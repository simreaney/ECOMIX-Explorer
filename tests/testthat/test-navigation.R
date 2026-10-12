# Characterisation tests for navigation between tabs and the selection
# state it carries: popup links (open_data_explorer, open_site_details,
# open_waterbody_details, open_eco_risk_detail, open_chem_d3d_detail), the
# context-strip "go" links, rail picks, main_nav changes (Chemical Explorer
# carry-over) and shape clicks. After every event snap_state() records each
# selection reactiveVal (rv(), rv_measured_site(), rv_waterbody(),
# rv_de_waterbody(), ...) and the inputs the observers update; the messages
# that did it and the context strips are snapshotted alongside.

testthat::local_edition(3)

go <- structure(1L, class = c("shinyActionButtonValue", "integer"))

ctx_all_ids <- c(ctx_subbasin_ids("map"), ctx_subbasin_ids("de"), ctx_subbasin_ids("sp"),
                 ctx_subbasin_ids("dl"), "ctx_value_sp", "ctx_rank_sp", "ctx_range_sp",
                 "ctx_rows_dl", "ctx_combos_dl", "ctx_size_dl", "opcat_rail")

test_that("navigation: Map popups, context-strip links, shape clicks, Ecological Risk links", {
  run_server({
    start_session(session)
    skip_messages(session)
    snap_state(session, "startup")

    # Subbasin popup link -> Hydro Explorer.
    send(session, .events = list(open_data_explorer = "353"))
    snap_messages(session, "open_data_explorer 353")
    snap_state(session, "open_data_explorer 353")

    # Context-strip links, each from another tab.
    send(session, .events = list(ctx_de_go = go))
    snap_state(session, "ctx_de_go (Hydro Explorer -> Map)")
    send(session, .events = list(ctx_map_go = go))
    snap_state(session, "ctx_map_go (Map -> Hydro Explorer)")
    send(session, main_nav = "Spatial Datasets")
    send(session, .events = list(ctx_sp_go = go))
    snap_messages(session, "Spatial Datasets, then ctx_sp_go")
    snap_state(session, "ctx_sp_go (Spatial Datasets -> Map)")
    send(session, main_nav = "Download")
    send(session, .events = list(ctx_dl_go = go))
    snap_messages(session, "Download, then ctx_dl_go")
    snap_state(session, "ctx_dl_go (Download -> Map)")

    # Chemical coverage popup: a chemical-only subbasin outside HYPE.
    send(session, .events = list(open_data_explorer = "765"))
    snap_state(session, "open_data_explorer 765 (chemical-only subbasin)")
    snap_outputs(session, "context strips at 765", ctx_all_ids)

    # Observed hydrology popup (same input, id of a gauged subbasin).
    send(session, .events = list(open_data_explorer = "783"))
    snap_state(session, "open_data_explorer 783 (observed hydrology popup)")

    # An id with no subbasin behind it (no popup sends one today).
    send(session, .events = list(open_data_explorer = "999999"))
    snap_state(session, "open_data_explorer 999999 (unknown id)")
    snap_outputs(session, "context strips at an unknown id", ctx_all_ids)

    # A cleared search box is ignored.
    send(session, subbasin_search = "3310")
    send(session, subbasin_search = "")
    snap_state(session, "subbasin_search 3310, then cleared")

    # Shape click on the base layer (numeric layerId -> integer rv()).
    send(session, main_nav = "Map")
    send(session, .events = list(basemap_shape_click = list(id = 2592L, group = "Subbasins",
                                                            lat = 54.3, lng = -2.2, .nonce = 1)))
    snap_state(session, "shape click Subbasins 2592")
    snap_outputs(session, "context strips at 2592 (shape click)", ctx_all_ids)

    # Clicking a water body (here one without modelled data) leaves the
    # selection on 2592.
    send(session, .events = list(basemap_shape_click = list(id = "GB112071065640", group = "waterbodies",
                                                            lat = 54.0, lng = -1.5, .nonce = 2)))
    snap_state(session, "shape click on a water body without modelled data (ignored)")
    snap_outputs(session, "context strips after the water body click (selection kept)", ctx_all_ids)

    # Ecological Risk popup links.
    send(session, main_nav = "Map")
    eco <- eco_risk_data[["fipronil"]]
    site <- eco$site_id[which(eco$risk_level == "Chronic")[1]]

    send(session, .events = list(open_eco_risk_detail = paste0("fipronil||", site)))
    snap_state(session, "open_eco_risk_detail fipronil (Chronic site)")
    snap_outputs(session, "Chronic site card", "eco_risk_detail_card")

    # A chemical without a risk file.
    send(session, main_nav = "Map")
    send(session, .events = list(open_eco_risk_detail = "imidacloprid||1"))
    snap_state(session, "open_eco_risk_detail imidacloprid (no risk file)")
    snap_outputs(session, "no risk file card", "eco_risk_detail_card")

    # Malformed payload (no site part; no popup sends one): an NA site,
    # and the card errors today.
    send(session, .events = list(open_eco_risk_detail = "fipronil"))
    snap_state(session, "open_eco_risk_detail 'fipronil' (no site)")
    snap_outputs(session, "no site card", "eco_risk_detail_card")
  })
})

test_that("navigation: Site Details selection state, Chemical Explorer jump links and carry-over", {
  run_server({
    start_session(session)
    skip_messages(session)

    send(session, .events = list(open_site_details = "Ai1"))
    snap_messages(session, "open_site_details Ai1")
    snap_state(session, "open_site_details Ai1")
    send(session, .events = list(site_pick = "Ca2"))
    snap_state(session, "site_pick Ca2")

    send(session, .events = list(open_waterbody_details = "GB104027057290"))
    snap_messages(session, "open_waterbody_details GB104027057290")
    snap_state(session, "open_waterbody_details GB104027057290")
    send(session, .events = list(waterbody_pick = "GB104027069400"))
    snap_messages(session, "waterbody_pick GB104027069400")
    snap_state(session, "waterbody_pick GB104027069400")

    # Toggling sides keeps each side's selection and re-picks the panels.
    send(session, site_entity_type = "measured")
    snap_messages(session, "site_entity_type measured")
    snap_state(session, "site_entity_type measured (Ca2 kept)")
    snap_outputs(session, "measured side", c("ctx_idlabel_site", "ctx_id_site", "ctx_sub_site", "ctx_summary_site"))
    send(session, site_entity_type = "modelled")
    snap_state(session, "site_entity_type modelled (GB104027069400 kept)")
    snap_outputs(session, "modelled side", c("ctx_idlabel_site", "ctx_id_site", "ctx_sub_site", "ctx_summary_site"))

    # A measured-site popup link while the modelled side is showing.
    send(session, .events = list(open_site_details = "Rye"))
    snap_messages(session, "open_site_details Rye from the modelled side")
    snap_state(session, "open_site_details Rye from the modelled side")

    # Chemical Explorer jump links and carry-over.
    send(session, main_nav = "Map")
    snap_state(session, "before the jump links")

    # Jump link to another chemical: key switch + pending site + nav.
    send(session, .events = list(open_chem_d3d_detail = "fipronil||504"))
    snap_messages(session, "open_chem_d3d_detail fipronil||504")
    snap_state(session, "open_chem_d3d_detail fipronil||504")

    # Invalid links are ignored (req() fails): unknown site, unknown
    # chemical, no site part.
    send(session, main_nav = "Map")
    for (link in c("fipronil||999999", "nosuchchem||1", "fipronil")) {
      send(session, .events = list(open_chem_d3d_detail = link))
      snap_messages(session, paste("invalid open_chem_d3d_detail", link))
      snap_state(session, paste("invalid open_chem_d3d_detail", link))
    }

    # Carry-over on opening the tab: the most recent of the Map subbasin
    # and the Site Details water body wins.
    send(session, subbasin_search = "3793")
    send(session, main_nav = "Chemical Explorer")
    snap_messages(session, "carry-over: subbasin 3793 (three sites)")
    snap_state(session, "carry-over: subbasin 3793")

    send(session, main_nav = "Map")
    send(session, .events = list(open_waterbody_details = "GB104027057290"))
    send(session, main_nav = "Chemical Explorer")
    snap_state(session, "carry-over: water body GB104027057290 (more recent)")

    send(session, main_nav = "Map")
    send(session, subbasin_search = "2592")
    send(session, main_nav = "Chemical Explorer")
    snap_state(session, "carry-over: subbasin 2592 (more recent again)")

    # Subbasins with no site for this chemical leave the site alone.
    for (sb in c("765", "7")) {
      send(session, main_nav = "Map")
      send(session, subbasin_search = sb)
      send(session, main_nav = "Chemical Explorer")
      snap_state(session, paste("carry-over: subbasin", sb, "(no matching site)"))
    }

    # Jump link to the current chemical (only the site changes), then a
    # tab change: the link's site is not overridden by the carry-over.
    send(session, .events = list(open_chem_d3d_detail = "fipronil||356"))
    snap_state(session, "open_chem_d3d_detail fipronil||356 (same chemical)")
    send(session, main_nav = "Map")
    send(session, main_nav = "Chemical Explorer")
    snap_state(session, "Map and back after the jump link")
  })
})
