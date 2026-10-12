# Characterisation tests for the leaflet maps *as the user sees them*.
#
# test-server.R pins the raw leafletProxy() traffic, which follows how the
# maps are updated: the Map page's overlays are added once, into their own
# panes, and then shown/hidden; its base layer is cached across sessions;
# the Spatial Datasets map is drawn once per basis and then recoloured in
# place. These tests replay the traffic through the leaflet.js model in
# helper-server.R (lf_state()) and snapshot what ends up on screen - visible
# layers per group with their content hashes, paint order, legends, view -
# so they show that the same things are drawn however they get there.
#
# The raw messages are snapshotted too (snap_messages()) for the toggle
# orders exercised here.

testthat::local_edition(3)

# Visible layer content only (no paint order, legends or view), for
# comparing states reached by different routes.
lf_content <- function(session, map_id) lf_visible(lf_state(session, map_id))
drop_groups <- function(content, groups) content[setdiff(names(content), groups)]

test_that("Map: visible layers after toggling them in different orders", {
  res <- new.env()
  run_server({
    start_session_lf(session)
    skip_messages(session)
    snap_lf(session, "basemap", "startup: subbasins only")

    # Order A: one layer at a time, bottom-up.
    send_lf(session, map_layers = c("subbasins", "measured_sites"))
    snap_messages(session, "A: + measured_sites")
    snap_lf(session, "basemap", "A: + measured_sites")
    send_lf(session, map_layers = c("subbasins", "measured_sites", "eco_risk"))
    snap_messages(session, "A: + eco_risk")
    snap_lf(session, "basemap", "A: + eco_risk")
    send_lf(session, map_layers = c("subbasins", "measured_sites", "eco_risk", "waterbodies"))
    snap_lf(session, "basemap", "A: + waterbodies")
    send_lf(session, map_layers = c("subbasins", "measured_sites", "eco_risk", "waterbodies", "opcat"))
    snap_messages(session, "A: + opcat")
    snap_lf(session, "basemap", "A: + opcat (5 layers)")
    res$five_a <- lf_content(session, "basemap")

    # Everything off, then the same five in one go, listed in another order.
    send_lf(session, map_layers = NULL)
    snap_messages(session, "all off (NULL)")
    snap_lf(session, "basemap", "all off")
    send_lf(session, map_layers = c("opcat", "waterbodies", "eco_risk", "measured_sites", "subbasins"))
    snap_messages(session, "B: same five at once")
    snap_lf(session, "basemap", "B: same five at once")
    expect_identical(lf_content(session, "basemap"), res$five_a)

    # Turning the ecological risk layer off takes its legend off too (it
    # used to stay, and every redraw added another one).
    send_lf(session, map_layers = c("opcat", "waterbodies", "measured_sites", "subbasins"))
    snap_messages(session, "B: - eco_risk")
    snap_lf(session, "basemap", "B: - eco_risk (legend removed)")

    # Subbasins off and on again: hideGroup/showGroup (the group is
    # repainted, but in its own pane, under every overlay).
    send_lf(session, map_layers = c("opcat", "waterbodies", "measured_sites"))
    snap_messages(session, "B: - subbasins")
    snap_lf(session, "basemap", "B: - subbasins")
    send_lf(session, map_layers = c("opcat", "waterbodies", "measured_sites", "subbasins"))
    snap_messages(session, "B: + subbasins again")
    snap_lf(session, "basemap", "B: + subbasins again")

    # A selected subbasin's highlight, then a toggle (the highlight's pane
    # keeps it above the layer switched on).
    send_lf(session, subbasin_search = "3310")
    snap_lf(session, "basemap", "subbasin 3310 selected")
    send_lf(session, map_layers = c("opcat", "waterbodies", "measured_sites", "subbasins", "chem_coverage"))
    snap_messages(session, "+ chem_coverage with a selection")
    snap_lf(session, "basemap", "+ chem_coverage with a selection")

    # Tile switch while overlays are on, then another toggle.
    send_lf(session, map_basemap = "esri_satellite")
    snap_lf(session, "basemap", "tiles -> esri_satellite")
    send_lf(session, map_layers = c("subbasins", "observed_hydro"))
    snap_messages(session, "subbasins + observed_hydro (satellite tiles)")
    snap_lf(session, "basemap", "subbasins + observed_hydro (satellite tiles)")
    res$sub_obs <- lf_content(session, "basemap")

    # Ecological risk with a chemical that has no risk file: no markers.
    send_lf(session, map_layers = c("subbasins", "observed_hydro", "eco_risk"))
    send_lf(session, eco_risk_chemical = "imidacloprid")
    snap_messages(session, "eco_risk on, chemical -> imidacloprid")
    snap_lf(session, "basemap", "eco_risk on with imidacloprid (no risk file: no markers, no legend)")
    send_lf(session, eco_risk_chemical = "fipronil")
    snap_lf(session, "basemap", "eco_risk back to fipronil")

    # Every layer, then back to subbasins only.
    send_lf(session, map_layers = c("subbasins", "opcat", "waterbodies", "measured_sites",
                                    "observed_hydro", "chem_coverage", "eco_risk"))
    snap_lf(session, "basemap", "all seven layers")
    res$all_seven <- lf_content(session, "basemap")
    send_lf(session, map_layers = "subbasins", map_basemap = "osm")
    snap_lf(session, "basemap", "back to subbasins only, osm tiles")
  })

  # A fresh session reaching the same layer sets by a different route.
  run_server({
    start_session_lf(session)
    skip_messages(session)
    send_lf(session, map_layers = c("eco_risk", "observed_hydro", "chem_coverage", "subbasins",
                                    "measured_sites", "waterbodies", "opcat"))
    snap_messages(session, "fresh session: all seven at once")
    # (session 1 had satellite tiles and a selection highlight at that point)
    expect_identical(drop_groups(lf_content(session, "basemap"), c("basetile", "selection")),
                     drop_groups(res$all_seven, c("basetile", "selection")))
    send_lf(session, map_layers = c("observed_hydro"))
    send_lf(session, map_layers = c("observed_hydro", "subbasins"))
    send_lf(session, map_basemap = "esri_satellite")
    # Same visible layers as order A's "subbasins + observed_hydro" step,
    # except that one also had a selection highlight (3310).
    expect_identical(drop_groups(lf_content(session, "basemap"), "selection"),
                     drop_groups(res$sub_obs, "selection"))
    snap_lf(session, "basemap", "fresh session: observed_hydro, then + subbasins, satellite tiles")
  })
})

test_that("Selection highlight, catchment rail framing and context-strip mini-maps", {
  run_server({
    start_session_lf(session)
    for (id in c("ctx_map_de", "ctx_map_site")) snap_lf(session, id, "startup")

    # Operational catchment rail: frames the catchment.
    send_lf(session, .events = list(opcat_pick = "Rye"))
    snap_lf(session, "basemap", "opcat_pick Rye (fitBounds)")

    send_lf(session, subbasin_search = "3310")
    snap_lf(session, "ctx_map_de", "subbasin 3310")
    # The three subbasin mini-maps are driven identically.
    expect_identical(lf_summary(lf_state(session, "ctx_map_sp")), lf_summary(lf_state(session, "ctx_map_de")))
    expect_identical(lf_summary(lf_state(session, "ctx_map_dl")), lf_summary(lf_state(session, "ctx_map_de")))

    send_lf(session, subbasin_search = "2592")
    snap_lf(session, "basemap", "subbasin 2592 highlighted")
    snap_lf(session, "ctx_map_de", "subbasin 2592")
    # A chemical-only subbasin (not drawn on the base layer) still gets the
    # highlight polygon.
    send_lf(session, .events = list(open_data_explorer = "765"))
    snap_lf(session, "basemap", "subbasin 765 (chemical-only) highlighted")
    snap_lf(session, "ctx_map_de", "subbasin 765 (chemical-only) via popup link")
    # A water body click does not change the selection: 765 stays
    # highlighted everywhere.
    send_lf(session, .events = list(basemap_shape_click = list(id = "GB104027057290", group = "waterbodies",
                                                               lat = 54.0, lng = -1.5, .nonce = 1)))
    snap_lf(session, "basemap", "after water body click (selection kept)")
    snap_lf(session, "ctx_map_sp", "after water body click (selection kept)")
    expect_identical(lf_summary(lf_state(session, "ctx_map_dl")), lf_summary(lf_state(session, "ctx_map_sp")))

    # Site Details locator: measured marker, modelled polygon, and back.
    send_lf(session, .events = list(open_site_details = "Ai1"))
    snap_lf(session, "ctx_map_site", "measured site Ai1")
    send_lf(session, .events = list(open_waterbody_details = "GB104027057290"))
    snap_lf(session, "ctx_map_site", "modelled water body GB104027057290")
    send_lf(session, .events = list(waterbody_pick = "GB104027069400"))
    snap_lf(session, "ctx_map_site", "modelled water body GB104027069400")
    send_lf(session, site_entity_type = "measured")
    snap_lf(session, "ctx_map_site", "back to measured (Ai1 kept)")
    send_lf(session, .events = list(site_pick = "Rye"))
    snap_lf(session, "ctx_map_site", "measured site Rye")
  })
})

test_that("Spatial Datasets map: decoded layers per variable type and the tile switch", {
  # Map state, the layer data behind it (canonical row order: Arrow scans /
  # a lazy Subbasin_Extremes do not fix one) and the context strip.
  snap_spatial <- function(session, step) {
    snap_lf(session, "prediction_map", step)
    snap_rx(session, "spatial_layer_data", ordered = FALSE)
    snap_outputs(session, step, c("ctx_value_sp", "ctx_rank_sp", "ctx_range_sp"))
  }
  run_server({
    start_session_lf(session)
    send_lf(session, subbasin_search = "3310")
    snap_spatial(session, "discharge SSP585|2070-2079 P99")
    send_lf(session, prediction_percentile_spatial = "1", prediction_period_spatial = "SSP245|2050-2059")
    snap_spatial(session, "discharge SSP245|2050-2059 P1")
    send_lf(session, prediction_variable_spatial = "water_temperature")
    snap_spatial(session, "water_temperature SSP245|2050-2059 P1")
    send_lf(session, prediction_variable_spatial = "chem_Zn", prediction_stat_spatial_chem = "90th")
    snap_spatial(session, "chem_Zn 90th")
    send_lf(session, prediction_variable_spatial = "wbchem_Zn")
    snap_spatial(session, "wbchem_Zn 90th")

    skip_messages(session)
    send_lf(session, sp_basemap = "esri_satellite")
    snap_messages(session, "sp_basemap esri_satellite")
    snap_lf(session, "prediction_map", "wbchem_Zn 90th, satellite tiles")
    # A new statistic recolours the map in place: the satellite tiles stay
    # (the whole map used to be rebuilt, with OSM tiles).
    send_lf(session, prediction_stat_spatial_chem = "mean")
    snap_lf(session, "prediction_map", "wbchem_Zn mean, recoloured in place (satellite tiles kept)")
    expect_identical(input$sp_basemap, "esri_satellite")
    # A new basis redraws the map, still on the chosen tiles.
    send_lf(session, prediction_variable_spatial = "discharge")
    snap_lf(session, "prediction_map", "discharge after wbchem_Zn (redrawn, satellite tiles kept)")
  })
})

test_that("Spatial Datasets map: a slice recoloured in place looks as if drawn afresh", {
  # Each slice is reached twice: by a change within the basis on the map
  # (recoloured, or for chemical subbasins drawn again, through the proxy),
  # then by switching to another basis and back (the map drawn afresh with
  # that slice). What is on screen must be the same.
  on_screen <- function(session) {
    st <- lf_state(session, "prediction_map")
    list(content = lf_effective(st), legends = lf_summary(st)$legends_and_controls, view = st$view)
  }
  run_server({
    start_session_lf(session)
    away <- c(hype_subbasin = "wbchem_Zn", chem_subbasin = "discharge", waterbody = "discharge")
    check <- function(variable, ...) {
      send_lf(session, ...)
      in_place <- on_screen(session)
      send_lf(session, prediction_variable_spatial = away[[spatial_basis_of(variable)]])
      send_lf(session, prediction_variable_spatial = variable)
      expect_identical(on_screen(session), in_place)
    }
    check("discharge", prediction_percentile_spatial = "1", prediction_period_spatial = "SSP245|2050-2059")
    check("water_temperature", prediction_variable_spatial = "water_temperature")
    send_lf(session, prediction_variable_spatial = "wbchem_Zn")
    check("wbchem_Zn", prediction_stat_spatial_chem = "90th")
    send_lf(session, prediction_variable_spatial = "chem_Zn")
    check("chem_Zn", prediction_stat_spatial_chem = "mean")
    check("chem_fipronil", prediction_variable_spatial = "chem_fipronil")
  })
})
