# Characterisation tests for the Shiny server, run through shiny::testServer()
# on a MockShinySession. Nothing is launched: no runApp(), no browser.
#
# The session-driving harness (recording mock session, browser emulation,
# snapshot descriptors, refactor hooks such as `ecomix_input_ids` and rx())
# lives in helper-server.R, shared with the other server test files.
#
# Tests marked "BUG (fixed in Phase 3): expected to change" record current,
# known-wrong behaviour on purpose.

testthat::local_edition(3)

# ---- tests ---------------------------------------------------------------------------

test_that("startup: default inputs, every output, startup messages", {
  run_server({
    start_session(session)
    snap_messages(session, "startup (defaults sent)")
    # The browser emulation must have filled the server-side selectizes.
    expect_identical(input$chem_d3d_site, "1")
    expect_identical(input$observation_variable, "Please select a subbasin")
    snap_outputs(session, "startup", all_output_ids)
    snap_rx(session, "downloader_preview_rows")
    snap_rx(session, "spatial_layer_data", ordered = FALSE)
    snap_rx(session, "chem_d3d_series_grouped")
    snap_rx(session, "chem_d3d_thresholds")
    snap_rx(session, "eco_risk_selected")
    snap_rx(session, "selected_opcat")
    snap_rx(session, "observation_choices")
  })
})

test_that("Map: layer toggles, eco risk layer, basemap and catchment rail (leafletProxy calls)", {
  run_server({
    start_session(session)
    snap_messages(session, "startup")

    send(session, map_layers = c("subbasins", "opcat"))
    snap_messages(session, "layers: + opcat")
    send(session, map_layers = c("subbasins", "opcat", "waterbodies"))
    snap_messages(session, "layers: + waterbodies")
    send(session, map_layers = c("subbasins", "opcat", "waterbodies", "measured_sites",
                                 "observed_hydro", "chem_coverage", "eco_risk"))
    snap_messages(session, "layers: all seven")
    send(session, map_layers = c("opcat", "waterbodies", "measured_sites",
                                 "observed_hydro", "chem_coverage", "eco_risk"))
    snap_messages(session, "layers: all but subbasins")
    send(session, map_layers = NULL)
    snap_messages(session, "layers: none (NULL)")
    send(session, map_layers = c("measured_sites", "subbasins"))
    snap_messages(session, "layers: measured_sites + subbasins")
    send(session, map_layers = c("chem_coverage", "observed_hydro", "subbasins"))
    snap_messages(session, "layers: chem_coverage + observed_hydro + subbasins")

    # Ecological risk layer and its chemical picker. Only fipronil has a
    # risk file today, so another chemical exercises the "no data" path.
    send(session, map_layers = c("subbasins", "eco_risk"))
    snap_messages(session, "layers: subbasins + eco_risk")
    send(session, eco_risk_chemical = "imidacloprid")
    snap_messages(session, "eco_risk_chemical -> imidacloprid (no risk file)")
    send(session, eco_risk_chemical = "fipronil")
    snap_messages(session, "eco_risk_chemical -> fipronil")
    send(session, map_layers = "subbasins")
    send(session, eco_risk_chemical = "imidacloprid")
    snap_messages(session, "eco_risk off, then chemical changed")

    # Basemap tile switcher.
    send(session, map_basemap = "esri_satellite")
    snap_messages(session, "map_basemap -> esri_satellite")
    send(session, map_basemap = "esri_gray")
    snap_messages(session, "map_basemap -> esri_gray")
    send(session, map_basemap = "osm")
    snap_messages(session, "map_basemap -> osm")

    # Operational catchment rail.
    send(session, .events = list(opcat_pick = "Rye"))
    snap_messages(session, "opcat_pick Rye")
    send(session, .events = list(opcat_pick = "Upper Aire"))
    snap_messages(session, "opcat_pick Upper Aire")
    send(session, .events = list(opcat_pick = "Not a catchment"))
    snap_messages(session, "opcat_pick unknown name")
    snap_outputs(session, "after rail picks", c("opcat_rail", ctx_subbasin_ids("map")))
  })
})

test_that("Map: subbasin selection via search, popup link and shape clicks", {
  run_server({
    start_session(session)
    snap_messages(session, "startup")

    send(session, subbasin_search = "3310")
    snap_messages(session, "subbasin_search 3310")
    expect_identical(rx(session, "rv"), 3310)
    snap_outputs(session, "subbasin 3310 selected",
                 c(ctx_subbasin_ids("map"), ctx_subbasin_ids("de"), ctx_subbasin_ids("sp"),
                   ctx_subbasin_ids("dl"), "opcat_rail"))
    snap_rx(session, "selected_opcat")
    snap_rx(session, "selected_lc")
    snap_rx(session, "selected_climate")
    snap_rx(session, "observation_choices")

    # Popup link "Open in Hydro Explorer" (string id from JS).
    send(session, .events = list(open_data_explorer = "353"))
    snap_messages(session, "open_data_explorer 353")
    expect_identical(input$main_nav, "Hydro Explorer")
    snap_outputs(session, "subbasin 353 via popup", c(ctx_subbasin_ids("de"), "opcat_rail"))

    # Context strip actions.
    send(session, .events = list(ctx_de_go = structure(1L, class = c("shinyActionButtonValue", "integer"))))
    snap_messages(session, "ctx_de_go")
    send(session, .events = list(ctx_map_go = structure(1L, class = c("shinyActionButtonValue", "integer"))))
    snap_messages(session, "ctx_map_go")

    # Shape click on a hydrology subbasin polygon (numeric layerId).
    send(session, .events = list(basemap_shape_click = list(id = 2592L, group = "Subbasins",
                                                            lat = 54.3, lng = -2.2, .nonce = 1)))
    snap_messages(session, "shape click: Subbasins 2592")
    expect_identical(rx(session, "rv"), 2592L)
    snap_outputs(session, "after shape click 2592", c(ctx_subbasin_ids("map"), "opcat_rail"))

    # Only a click on the Subbasins layer selects: a water body's id is its
    # EA code, not a subbasin, so the selection stays on 2592.
    send(session, .events = list(basemap_shape_click = list(id = "GB104027057290", group = "waterbodies",
                                                            lat = 54.0, lng = -1.5, .nonce = 2)))
    snap_messages(session, "shape click: waterbody GB104027057290 (ignored)")
    expect_identical(rx(session, "rv"), 2592L)
    snap_outputs(session, "after waterbody shape click (selection kept)",
                 c(ctx_subbasin_ids("map"), ctx_subbasin_ids("sp"), "opcat_rail"))

    # A click with no id (a polygon drawn without a layerId) is ignored too.
    # The selection highlight no longer takes clicks at all (it used to, and
    # cleared the selection). The search box still holds 3310 from the start,
    # so it is cleared first for the search to select 3310 again.
    send(session, subbasin_search = "")
    send(session, subbasin_search = "3310")
    send(session, .events = list(basemap_shape_click = list(id = NULL, group = "selection",
                                                            lat = 54.0, lng = -1.5, .nonce = 3)))
    snap_messages(session, "shape click: selection polygon, id NULL (ignored)")
    expect_identical(rx(session, "rv"), 3310)
    snap_outputs(session, "after selection-polygon click (selection kept)", c(ctx_subbasin_ids("map"), "opcat_rail"))
  })
})

test_that("Hydro Explorer: HYPE variables (subbasin mode)", {
  run_server({
    start_session(session)
    send(session, subbasin_search = "3310")
    snap_messages(session, "startup + subbasin 3310")
    snap_outputs(session, "3310 discharge, Baseline+SSP585, 2070s, p10/p50/p90, Absolute",
                 c(hydro_text_ids, hydro_plot_ids, ctx_subbasin_ids("de"), "ctx_map_de"))
    snap_rx(session, "selected_historical", ordered = FALSE)

    send(session, plot_type = "Relative")
    snap_messages(session, "plot_type Relative")
    snap_outputs(session, "3310 discharge Relative", hydro_plot_ids)

    send(session, scenario = c("Baseline", "SSP126", "SSP245", "SSP585"),
         prediction_period = c("2030-2039", "2070-2079"),
         prediction_percentile = c("p01", "p50", "p99"))
    snap_messages(session, "all scenarios, two periods, p01/p50/p99 (Relative)")
    # BUG: 7 scenario/period lines exceed the 6-colour ec_series palette, so
    # the Distributions (and, in Absolute mode, Monthly) plots currently
    # error with "Insufficient values in manual scale" - recorded as is.
    snap_outputs(session, "3310 discharge multi-scenario Relative", hydro_plot_ids)

    send(session, plot_type = "Absolute")
    snap_outputs(session, "3310 discharge multi-scenario Absolute", hydro_plot_ids)

    send(session, scenario = "SSP585", plot_type = "Relative")
    snap_outputs(session, "3310 discharge SSP585 only, Relative", hydro_plot_ids)
    send(session, scenario = "Baseline", plot_type = "Relative")
    snap_outputs(session, "3310 discharge Baseline only, Relative", hydro_plot_ids)
    # An empty multi-select is sent as NULL by the browser.
    send(session, prediction_percentile = NULL)
    snap_outputs(session, "3310 discharge no percentile selected", hydro_plot_ids)

    send(session, scenario = c("Baseline", "SSP585"), prediction_period = "2070-2079",
         prediction_percentile = c("p10", "p50", "p90"), plot_type = "Absolute",
         climate_variable = "Temperature", climate_resolution = "annual")
    snap_messages(session, "climate Temperature annual")
    snap_outputs(session, "3310 climate Temperature annual", c("climate_pill", "climate_plot"))
    send(session, climate_resolution = "monthly")
    snap_outputs(session, "3310 climate Temperature monthly", c("climate_pill", "climate_plot"))

    # Water quality variable: first year of each projection slice dropped.
    send(session, prediction_variable = "SS", climate_variable = "Precipitation")
    snap_messages(session, "prediction_variable SS")
    snap_outputs(session, "3310 SS Absolute", c(hydro_text_ids, hydro_plot_ids))
    send(session, prediction_variable = "water_temperature", plot_type = "Relative")
    snap_outputs(session, "3310 water_temperature Relative", c(hydro_text_ids, hydro_plot_ids))

    # Observations: 3310 has observed discharge.
    send(session, observation_variable = "discharge")
    snap_outputs(session, "3310 observation discharge", c("observation_pill", "observation_plot"))

    # 353: water quality observations (non-discharge branch).
    send(session, subbasin_search = "353")
    snap_messages(session, "subbasin 353")
    snap_rx(session, "observation_choices")
    snap_rx(session, "selected_historical", ordered = FALSE)
    snap_outputs(session, "353 default observation variable",
                 c("observation_pill", "observation_plot", ctx_subbasin_ids("de")))
    send(session, observation_variable = "Water temperature")
    snap_outputs(session, "353 observation Water temperature", c("observation_pill", "observation_plot"))

    # 765: a chemical-only subbasin outside the HYPE model.
    send(session, subbasin_search = "765")
    snap_messages(session, "subbasin 765 (no HYPE data)")
    snap_outputs(session, "765 HYPE plots", c(hydro_plot_ids, ctx_subbasin_ids("de")))

    # "Download this selection" for a HYPE variable.
    send(session, subbasin_search = "3310", prediction_variable = "discharge")
    snap_download(session, "download_selection", "3310 discharge", ordered = FALSE)
    send(session, prediction_variable = "IN")
    snap_download(session, "download_selection", "3310 IN", ordered = FALSE)
  })
})

test_that("Hydro Explorer: chemical variables (subbasin mode)", {
  run_server({
    start_session(session)
    send(session, subbasin_search = "3310")
    snap_messages(session, "startup + subbasin 3310")

    send(session, prediction_variable = "chem_fipronil")
    snap_messages(session, "prediction_variable chem_fipronil (Statistic relabel, Daily tab shown)")
    expect_identical(input$prediction_percentile, "median")
    snap_rx(session, "chem_selected_series")
    snap_outputs(session, "3310 fipronil median", c(hydro_text_ids, hydro_plot_ids))

    send(session, prediction_percentile = c("mean", "median", "90th"))
    snap_rx(session, "chem_selected_series")
    snap_outputs(session, "3310 fipronil mean/median/90th", hydro_plot_ids)
    snap_download(session, "download_selection", "3310 fipronil mean/median/90th")

    # Multi-site subbasin (duplicate dates).
    send(session, subbasin_search = "2592")
    snap_rx(session, "chem_selected_series")
    snap_outputs(session, "2592 fipronil mean/median/90th", hydro_plot_ids)
    snap_download(session, "download_selection", "2592 fipronil mean/median/90th")

    send(session, prediction_variable = "chem_Zn")
    snap_messages(session, "prediction_variable chem_Zn")
    snap_rx(session, "chem_selected_series")
    snap_outputs(session, "2592 Zn median", c(hydro_text_ids, hydro_plot_ids))

    # HYPE-only subbasin: no chemical data.
    send(session, subbasin_search = "7")
    snap_rx(session, "chem_selected_series")
    snap_outputs(session, "7 Zn (no chemical data)", hydro_plot_ids)

    # Back to a HYPE variable: Conditions restored, Daily tab hidden.
    send(session, prediction_variable = "discharge")
    snap_messages(session, "prediction_variable back to discharge")
    expect_identical(input$prediction_percentile, c("p10", "p50", "p90"))
  })
})

test_that("Hydro Explorer: water body mode", {
  run_server({
    start_session(session)
    send(session, subbasin_search = "3310")
    send(session, de_entity_type = "waterbody")
    snap_messages(session, "de_entity_type waterbody (variable choices -> chemicals)")
    # The variable select's options were replaced without a selection, so
    # the browser falls back to the first chemical.
    expect_identical(input$prediction_variable, "chem_6PPDQ")
    snap_outputs(session, "water body mode, no water body yet", c(hydro_text_ids, hydro_plot_ids))

    send(session, de_waterbody_search = "GB104027057290")
    snap_messages(session, "de_waterbody_search GB104027057290")
    snap_rx(session, "chem_selected_series")
    snap_outputs(session, "GB104027057290 6PPDQ median", c(hydro_text_ids, hydro_plot_ids))

    send(session, prediction_variable = "chem_cypermethrin", prediction_percentile = c("mean", "90th"))
    snap_rx(session, "chem_selected_series")
    snap_outputs(session, "GB104027057290 cypermethrin mean/90th", hydro_plot_ids)
    # The selection download is labelled with the Map's subbasin (rv() =
    # 3310 here) even though the rows are the water body's series.
    snap_download(session, "download_selection", "water body mode download (labelled with rv())")

    send(session, de_entity_type = "subbasin")
    snap_messages(session, "de_entity_type back to subbasin")
    expect_identical(input$prediction_variable, "discharge")
    snap_outputs(session, "back in subbasin mode", c(hydro_text_ids, hydro_plot_ids))
  })
})

test_that("Site Details: measured sites", {
  run_server({
    start_session(session)
    snap_outputs(session, "no site selected", c(site_ids, "measured_rail"))

    send(session, .events = list(open_site_details = "Ai1"))
    snap_messages(session, "open_site_details Ai1 (top-4 panels picked)")
    expect_identical(input$main_nav, "Site Details")
    snap_rx(session, "selected_measured_site_top_chemicals")
    snap_rx(session, "measured_chem_grid_data")
    snap_rx(session, "selected_measured_site_long")
    snap_outputs(session, "Ai1", c(site_ids, "measured_rail"))

    grid <- rx(session, "measured_chem_grid_data")
    detected <- grid[grid$status == "Detected", ][1, ]
    send(session, .events = list(site_chem_grid_hover = hover_at(
      session, "site_chem_grid", as.numeric(detected$Date), as.character(detected$parameter_label))))
    snap_outputs(session, "Ai1 hover on a detected cell", "site_chem_grid_tooltip")
    nd <- grid[grid$status == "Non-detect", ][1, ]
    if (nrow(nd) == 1 && !is.na(nd$Date)) {
      send(session, .events = list(site_chem_grid_hover = hover_at(
        session, "site_chem_grid", as.numeric(nd$Date), as.character(nd$parameter_label))))
      snap_outputs(session, "Ai1 hover on a non-detect cell", "site_chem_grid_tooltip")
    }

    send(session, .events = list(site_pick = "Ca2"))
    snap_messages(session, "site_pick Ca2")
    snap_outputs(session, "Ca2", c(site_ids, "measured_rail"))

    send(session, site_ts_chemical_2 = measured_physchem_cols[1])
    snap_outputs(session, "Ca2 panel 2 -> physicochemical", "site_time_series")
  })
})

test_that("Site Details: modelled water bodies", {
  run_server({
    start_session(session)
    send(session, .events = list(open_waterbody_details = "GB104027057290"))
    snap_messages(session, "open_waterbody_details GB104027057290")
    expect_identical(input$site_entity_type, "modelled")
    snap_rx(session, "selected_waterbody_top_chemicals")
    snap_rx(session, "wb_chem_grid_data", ordered = FALSE)
    snap_outputs(session, "GB104027057290 median", c(site_ids, "modelled_rail"),
                 order_dependent = "site_chem_grid")

    grid <- rx(session, "wb_chem_grid_data")
    cell <- grid[order(grid$date, as.character(grid$chem_label)), ][1, ]
    send(session, .events = list(site_chem_grid_hover = hover_at(
      session, "site_chem_grid", as.numeric(cell$date), as.character(cell$chem_label))))
    snap_outputs(session, "hover on a modelled cell", "site_chem_grid_tooltip")

    send(session, site_grid_wb_stat = "90th")
    snap_rx(session, "wb_chem_grid_data", ordered = FALSE)
    snap_outputs(session, "GB104027057290 90th", c("site_chem_grid", "site_time_series"),
                 order_dependent = "site_chem_grid")

    send(session, .events = list(waterbody_pick = "GB104027069400"))
    snap_messages(session, "waterbody_pick GB104027069400")
    snap_outputs(session, "GB104027069400", c(site_ids, "modelled_rail"),
                 order_dependent = "site_chem_grid")

    # Toggling back to Measured keeps each side's own selection.
    send(session, site_entity_type = "measured")
    snap_messages(session, "site_entity_type measured (no measured site yet)")
    snap_outputs(session, "measured side, nothing picked", site_ids)
  })
})

test_that("Chemical Explorer: chemical / site / grouping / hover", {
  run_server({
    start_session(session)
    snap_outputs(session, "default 6ppdq, first site", chem_d3d_ids)
    snap_rx(session, "chem_d3d_series_grouped")

    send(session, chem_d3d_key = "fipronil")
    snap_messages(session, "chem_d3d_key fipronil (site kept)")
    snap_rx(session, "chem_d3d_thresholds")
    snap_outputs(session, "fipronil site 1", chem_d3d_ids)

    send(session, chem_d3d_site = "504")
    snap_rx(session, "chem_d3d_thresholds")
    snap_outputs(session, "fipronil site 504", chem_d3d_ids)

    send(session, chem_d3d_grouping = "weekly")
    snap_rx(session, "chem_d3d_series_grouped")
    snap_outputs(session, "fipronil 504 weekly median", c("chem_d3d_conc_plot", "chem_d3d_sspaf_plot"))
    send(session, chem_d3d_grouping = "monthly", chem_d3d_group_stat = "p90")
    snap_rx(session, "chem_d3d_series_grouped")
    snap_outputs(session, "fipronil 504 monthly p90", c("chem_d3d_conc_plot", "chem_d3d_sspaf_plot"))

    # Imidacloprid has source apportionment: treemaps + hover day marker.
    send(session, chem_d3d_grouping = "daily", chem_d3d_group_stat = "median", chem_d3d_key = "imidacloprid")
    snap_messages(session, "chem_d3d_key imidacloprid")
    snap_outputs(session, "imidacloprid 504 (no hover yet)", chem_d3d_ids)

    # Hover on the concentration plot picks a day: day treemap + a vertical
    # marker drawn into both plots (the marker is planned to move
    # client-side, which will change these plot hashes).
    send(session, .events = list(chem_d3d_hover_conc = list(
      x = as.numeric(as.Date("2020-07-15")) + 0.3, y = 0.001,
      coords_css = list(x = 300, y = 200), coords_img = list(x = 300, y = 200), .nonce = 1)))
    snap_messages(session, "hover conc 2020-07-15")
    expect_identical(rx(session, "chem_d3d_hover_date"), as.Date("2020-07-15"))
    snap_outputs(session, "imidacloprid 504 hovered 2020-07-15", chem_d3d_ids)

    # Hover beyond the record is clamped to its last day; x NULL is ignored.
    send(session, .events = list(chem_d3d_hover_sspaf = list(
      x = as.numeric(as.Date("2024-01-01")), y = 0.1, .nonce = 2)))
    expect_identical(rx(session, "chem_d3d_hover_date"), as.Date("2022-12-31"))
    snap_outputs(session, "hovered beyond record (clamped)",
                 c("chem_d3d_treemap_day_title", "chem_d3d_treemap_day", "chem_d3d_conc_plot"))
    send(session, .events = list(chem_d3d_hover_conc = list(x = NULL, y = NULL, .nonce = 3)))
    expect_identical(rx(session, "chem_d3d_hover_date"), as.Date("2022-12-31"))

    # Changing chemical resets the hovered day.
    send(session, chem_d3d_key = "fipronil")
    expect_null(rx(session, "chem_d3d_hover_date"))
    snap_outputs(session, "fipronil after hover reset", chem_d3d_ids)

    # Map popup "View chemical time series" jump link.
    send(session, .events = list(open_chem_d3d_detail = "imidacloprid||356"))
    snap_messages(session, "open_chem_d3d_detail imidacloprid||356")
    expect_identical(input$main_nav, "Chemical Explorer")
    expect_identical(input$chem_d3d_site, "356")
    snap_outputs(session, "after jump link", chem_d3d_ids)

    # Selection carry-over when the tab is opened: subbasin 3793 has three
    # SITE_IDs (the first is taken).
    send(session, main_nav = "Map")
    send(session, subbasin_search = "3793")
    send(session, main_nav = "Chemical Explorer")
    snap_messages(session, "carry-over from subbasin 3793")
    expect_identical(input$chem_d3d_site, "366")
    # ...and from a Site Details water body.
    send(session, main_nav = "Map")
    send(session, .events = list(open_waterbody_details = "GB104027069500"))
    send(session, main_nav = "Chemical Explorer")
    snap_messages(session, "carry-over from water body GB104027069500")
    snap_outputs(session, "after carry-over", c("chem_d3d_threshold_note", "chem_d3d_conc_plot"))
  })
})

test_that("Ecological Risk detail tab", {
  run_server({
    start_session(session)
    eco <- eco_risk_data[["fipronil"]]
    has_ss <- !is_na_string(eco[["site_specific hc5"]])
    ss_site <- eco$site_id[which(has_ss)[1]]
    both_site <- eco$site_id[which(eco$risk_level == "Both")[1]]

    send(session, .events = list(open_eco_risk_detail = paste0("fipronil||", ss_site)))
    snap_messages(session, "open_eco_risk_detail (site-specific HC5 site)")
    expect_identical(input$main_nav, "Ecological Risk")
    snap_rx(session, "eco_risk_selected")
    snap_outputs(session, "site-specific site", "eco_risk_detail_card")

    send(session, .events = list(open_eco_risk_detail = paste0("fipronil||", both_site)))
    snap_outputs(session, "'Both' risk level site", "eco_risk_detail_card")

    send(session, .events = list(open_eco_risk_detail = "fipronil||999999999"))
    snap_outputs(session, "unknown site", "eco_risk_detail_card")
  })
})

test_that("Spatial Datasets: HYPE, subbasin chemical and water body chemical layers", {
  run_server({
    start_session(session)
    send(session, subbasin_search = "3310")
    snap_messages(session, "startup + subbasin 3310")
    snap_rx(session, "spatial_layer_data")
    snap_outputs(session, "discharge SSP585|2070-2079 P99", spatial_ids)

    send(session, prediction_percentile_spatial = "1")
    snap_rx(session, "spatial_layer_data")
    snap_outputs(session, "discharge SSP585|2070-2079 P1", spatial_ids)
    send(session, prediction_period_spatial = "Baseline|2000-2019", prediction_percentile_spatial = "50")
    snap_outputs(session, "discharge Baseline P50", spatial_ids)
    send(session, prediction_variable_spatial = "DOC", prediction_period_spatial = "SSP126|2040-2049")
    snap_outputs(session, "DOC SSP126|2040-2049 P50", spatial_ids)

    send(session, prediction_variable_spatial = "chem_fipronil")
    snap_rx(session, "spatial_layer_data", ordered = FALSE)
    snap_outputs(session, "chem_fipronil median", spatial_ids)
    send(session, prediction_stat_spatial_chem = "mean")
    snap_outputs(session, "chem_fipronil mean", spatial_ids)
    send(session, subbasin_search = "2592")
    snap_outputs(session, "chem_fipronil mean at multi-site 2592", spatial_ids)

    send(session, prediction_variable_spatial = "wbchem_fipronil")
    snap_rx(session, "spatial_layer_data", ordered = FALSE)
    snap_outputs(session, "wbchem_fipronil mean", spatial_ids)
    send(session, prediction_variable_spatial = "wbchem_cypermethrin", prediction_stat_spatial_chem = "90th")
    snap_outputs(session, "wbchem_cypermethrin 90th", spatial_ids)

    send(session, sp_basemap = "esri_gray")
    snap_messages(session, "sp_basemap esri_gray")
  })
})

test_that("Download: tabular previews and files", {
  run_server({
    start_session(session)
    snap_outputs(session, "no subbasin selected", download_ids)

    send(session, subbasin_search = "3310")
    snap_messages(session, "subbasin 3310")
    snap_rx(session, "downloader_tabular_data", ordered = FALSE)
    snap_rx(session, "downloader_preview_rows")
    snap_outputs(session, "3310 precip tabular", download_ids)
    snap_download(session, "download_data", "3310 precip csv", ordered = FALSE)
    send(session, dl_format = "xlsx")
    snap_outputs(session, "3310 precip xlsx", "ctx_size_dl")
    snap_download(session, "download_data", "3310 precip xlsx", ordered = FALSE)
    send(session, dl_format = "parquet")
    snap_download(session, "download_data", "3310 precip parquet", ordered = FALSE)

    send(session, dl_variable = "discharge", dl_format = "csv")
    snap_messages(session, "dl_variable discharge")
    snap_outputs(session, "3310 discharge tabular", download_ids)
    snap_download(session, "download_data", "3310 discharge csv", ordered = FALSE)

    send(session, dl_variable = "chem_fipronil")
    snap_rx(session, "downloader_tabular_data", ordered = FALSE)
    snap_outputs(session, "3310 chem_fipronil median tabular", download_ids)
    snap_download(session, "download_data", "3310 chem_fipronil median csv", ordered = FALSE)
    send(session, dl_chem_stat = "90th", dl_format = "parquet")
    snap_download(session, "download_data", "3310 chem_fipronil 90th parquet", ordered = FALSE)

    # Water body chemicals: always all modelled water bodies; picking one
    # also defaults the spatial layer to "Water bodies".
    send(session, dl_variable = "wbchem_fipronil", dl_format = "csv")
    snap_messages(session, "dl_variable wbchem_fipronil")
    expect_identical(input$dl_spatial_layer, "waterbodies")
    snap_outputs(session, "wbchem_fipronil 90th tabular", download_ids)
    snap_download(session, "download_data", "wbchem_fipronil 90th csv", ordered = FALSE)

    # Subbasin outside the HYPE model: empty download -> error.
    send(session, dl_variable = "discharge", subbasin_search = "765")
    snap_outputs(session, "765 discharge (no rows)", download_ids)
    snap_download(session, "download_data", "765 discharge csv (no rows)")
  })
})

test_that("Download: spatial previews and files", {
  run_server({
    start_session(session)
    send(session, subbasin_search = "3310", dl_data_type = "spatial")
    snap_messages(session, "dl_data_type spatial (format choices swapped)")
    expect_identical(input$dl_format, "gpkg")
    snap_rx(session, "downloader_spatial_data")
    snap_rx(session, "downloader_preview_rows")
    snap_outputs(session, "subbasins layer, precip", download_ids)
    snap_download(session, "download_data", "subbasins precip gpkg")
    send(session, dl_format = "shp")
    snap_download(session, "download_data", "subbasins precip shapefile zip")
    # BUG: GeoParquet fails on this machine's GDAL (no Parquet driver) - the
    # error is recorded as the current behaviour.
    send(session, dl_format = "geoparquet")
    snap_download(session, "download_data", "subbasins precip geoparquet")

    send(session, dl_spatial_layer = "catchment", dl_format = "gpkg")
    snap_rx(session, "downloader_spatial_data")
    snap_outputs(session, "catchment layer, precip", download_ids)
    snap_download(session, "download_data", "catchment precip gpkg")

    send(session, dl_variable = "chem_fipronil", dl_spatial_layer = "subbasins")
    snap_rx(session, "downloader_spatial_data")
    snap_outputs(session, "subbasins layer, chem_fipronil median", download_ids)
    snap_download(session, "download_data", "subbasins chem_fipronil gpkg")

    # Layer / variable mismatch -> empty layer -> download error.
    send(session, dl_spatial_layer = "waterbodies")
    snap_outputs(session, "waterbodies layer with a subbasin variable (mismatch)", download_ids)
    snap_download(session, "download_data", "mismatch gpkg")

    send(session, dl_variable = "wbchem_fipronil")
    snap_rx(session, "downloader_spatial_data")
    snap_outputs(session, "waterbodies layer, wbchem_fipronil median", download_ids)
    snap_download(session, "download_data", "waterbodies wbchem_fipronil gpkg")
    send(session, dl_format = "shp")
    snap_download(session, "download_data", "waterbodies wbchem_fipronil shapefile zip")
  })
})
