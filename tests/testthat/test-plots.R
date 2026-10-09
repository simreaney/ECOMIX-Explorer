# Characterisation tests for plot outputs, aimed at the planned
# bindCache() on renderPlot() and the client-side Chemical Explorer hover
# marker:
# * single-input changes (period, climate resolution, chemical statistic,
#   grouping statistic, water body), so a cache key that misses an input
#   shows up as a stale plot;
# * returning to an earlier state must give back the very same plot;
# * outputs that must *not* react to an input (water body mode ignores the
#   Map subbasin) are asserted unchanged.
#
# Plot descriptors (PNG hash + coordmap) come from snap_outputs() /
# desc_value() in helper-server.R.

testthat::local_edition(3)

plot_descs <- function(session, ids) {
  out <- lapply(ids, function(id) desc_value(out_val(session, id)))
  names(out) <- ids
  out
}

test_that("Hydro Explorer plots: one input at a time, round trips, water body mode", {
  run_server({
    start_session(session)
    skip_messages(session)
    # Water body mode before any Map subbasin is picked. Current
    # behaviour: every plot is NULL, even in water body mode (they all start
    # with `if (is.null(rv()))`).
    send(session, de_entity_type = "waterbody")
    send(session, de_waterbody_search = "GB104027057290")
    snap_outputs(session, "water body picked, no Map subbasin", c(hydro_text_ids, hydro_plot_ids))
    snap_rx(session, "chem_selected_series")
    send(session, de_entity_type = "subbasin")
    expect_identical(input$prediction_variable, "discharge")

    # Subbasin mode.
    send(session, subbasin_search = "3310")
    first <- plot_descs(session, c(hydro_plot_ids, "climate_plot"))

    send(session, prediction_period = "2030-2039")
    snap_outputs(session, "3310 discharge, period 2030-2039 only", hydro_plot_ids)
    send(session, prediction_period = "2070-2079")
    expect_identical(plot_descs(session, c(hydro_plot_ids, "climate_plot")), first)

    send(session, scenario = c("Baseline", "SSP126"))
    snap_outputs(session, "3310 discharge, scenarios Baseline + SSP126", hydro_plot_ids)
    send(session, prediction_percentile = c("p05", "p95"))
    snap_outputs(session, "3310 discharge, Baseline + SSP126, p05/p95", hydro_plot_ids)
    send(session, scenario = c("Baseline", "SSP585"), prediction_percentile = c("p10", "p50", "p90"))
    expect_identical(plot_descs(session, c(hydro_plot_ids, "climate_plot")), first)

    send(session, climate_resolution = "annual")
    snap_outputs(session, "3310 climate Precipitation annual", "climate_plot")
    send(session, climate_variable = "Temperature")
    send(session, climate_variable = "Precipitation", climate_resolution = "monthly")
    expect_identical(plot_descs(session, c(hydro_plot_ids, "climate_plot")), first)

    # Same inputs, another subbasin, then back.
    send(session, subbasin_search = "353")
    snap_outputs(session, "353 discharge defaults", c(hydro_plot_ids, "climate_plot"))
    send(session, subbasin_search = "3310")
    expect_identical(plot_descs(session, c(hydro_plot_ids, "climate_plot")), first)

    # Chemical statistic changed on its own.
    send(session, prediction_variable = "chem_imidacloprid")
    snap_outputs(session, "3310 imidacloprid median", hydro_plot_ids)
    chem_median <- plot_descs(session, hydro_plot_ids)
    send(session, prediction_percentile = "mean")
    snap_outputs(session, "3310 imidacloprid mean", hydro_plot_ids)
    send(session, prediction_percentile = "median")
    expect_identical(plot_descs(session, hydro_plot_ids), chem_median)

    # Water body mode again, now with a Map subbasin (the water body
    # picked above is kept).
    send(session, de_entity_type = "waterbody")
    snap_outputs(session, "GB104027057290 6PPDQ median (after a Map subbasin)", hydro_plot_ids)
    wb1 <- plot_descs(session, hydro_plot_ids)

    # The Map subbasin only gates the plots; another one changes nothing.
    send(session, subbasin_search = "2592")
    expect_identical(plot_descs(session, hydro_plot_ids), wb1)

    # Another water body, same chemical and statistic.
    send(session, de_waterbody_search = "GB104027069400")
    snap_rx(session, "chem_selected_series")
    snap_outputs(session, "GB104027069400 6PPDQ median", hydro_plot_ids)

    # Statistic on its own, then another chemical.
    send(session, prediction_percentile = "90th")
    snap_outputs(session, "GB104027069400 6PPDQ 90th", hydro_plot_ids)
    send(session, prediction_variable = "chem_Zn")
    snap_rx(session, "chem_selected_series")
    snap_outputs(session, "GB104027069400 Zn median", c(hydro_text_ids, hydro_plot_ids))

    # Back to the first water body and chemical.
    send(session, de_waterbody_search = "GB104027057290", prediction_variable = "chem_6PPDQ")
    expect_identical(plot_descs(session, hydro_plot_ids), wb1)
  })
})

test_that("Chemical Explorer and Site Details plots: single inputs, round trips, hover marker", {
  run_server({
    start_session(session)
    skip_messages(session)
    send(session, chem_d3d_key = "imidacloprid")
    send(session, chem_d3d_site = "504")
    ids <- c("chem_d3d_conc_plot", "chem_d3d_sspaf_plot")
    daily <- plot_descs(session, ids)

    send(session, chem_d3d_grouping = "monthly")
    snap_outputs(session, "imidacloprid 504 monthly median", ids)
    send(session, chem_d3d_group_stat = "p90")
    snap_rx(session, "chem_d3d_series_grouped")
    snap_outputs(session, "imidacloprid 504 monthly p90 (statistic only)", ids)
    send(session, chem_d3d_grouping = "weekly")
    snap_outputs(session, "imidacloprid 504 weekly p90", ids)
    send(session, chem_d3d_grouping = "daily", chem_d3d_group_stat = "median")
    expect_identical(plot_descs(session, ids), daily)

    # Site on its own, and back.
    send(session, chem_d3d_site = "356")
    snap_outputs(session, "imidacloprid 356 daily", c(ids, "chem_d3d_treemap_avg", "chem_d3d_threshold_note"))
    send(session, chem_d3d_site = "504")
    expect_identical(plot_descs(session, ids), daily)

    # The day marker is drawn into both plots on the server today. Planned
    # to move client-side: then hovering will no longer change these plots
    # (both flags below turn FALSE, and the hovered-plot hashes in
    # test-server.R change). The hovered day itself still drives the day
    # treemap.
    send(session, .events = list(chem_d3d_hover_conc = list(
      x = as.numeric(as.Date("2021-03-10")), y = 0.001, .nonce = 1)))
    hovered <- plot_descs(session, ids)
    expect_snapshot_value(list(
      hover_date = format(rx(session, "chem_d3d_hover_date")),
      conc_plot_changed_by_hover = !identical(hovered$chem_d3d_conc_plot, daily$chem_d3d_conc_plot),
      sspaf_plot_changed_by_hover = !identical(hovered$chem_d3d_sspaf_plot, daily$chem_d3d_sspaf_plot)
    ), style = "json2")
    snap_outputs(session, "imidacloprid 504 hovered 2021-03-10",
                 c("chem_d3d_treemap_day_title", "chem_d3d_treemap_day", "chem_d3d_treemap_avg"))

    # A chemical without source apportionment ignores hovers.
    send(session, chem_d3d_key = "fipronil")
    fip <- plot_descs(session, ids)
    send(session, .events = list(chem_d3d_hover_conc = list(
      x = as.numeric(as.Date("2021-03-10")), y = 0.001, .nonce = 2)))
    expect_null(rx(session, "chem_d3d_hover_date"))
    expect_identical(plot_descs(session, ids), fip)
    snap_outputs(session, "fipronil 504 after a hover (no source apportionment)",
                 c("chem_d3d_src_col", "chem_d3d_treemap_day_title"))

    # Site Details.
    send(session, .events = list(open_site_details = "Fo1"))
    snap_outputs(session, "Fo1", c("site_chem_grid", "site_time_series", "ctx_landcover_site"))
    fo1 <- plot_descs(session, c("site_chem_grid", "site_time_series"))
    panel4 <- cur_input(session, in_id("site_ts_chemical_4"))
    send(session, site_ts_chemical_4 = measured_physchem_cols[2])
    snap_outputs(session, "Fo1 panel 4 -> physicochemical", "site_time_series")
    send(session, site_ts_chemical_4 = panel4)
    expect_identical(plot_descs(session, c("site_chem_grid", "site_time_series")), fo1)

    send(session, .events = list(open_waterbody_details = "GB104027069400"))
    send(session, site_ts_chemical_1 = "Zn")
    snap_outputs(session, "GB104027069400 panel 1 -> Zn", "site_time_series")
    send(session, site_grid_wb_stat = "mean")
    snap_rx(session, "wb_chem_grid_data", ordered = FALSE)
    snap_outputs(session, "GB104027069400 mean", c("site_chem_grid", "site_time_series"),
                 order_dependent = "site_chem_grid")
  })
})
