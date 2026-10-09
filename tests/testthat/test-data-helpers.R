# Characterisation (golden-snapshot) tests for app.R's global scope: every
# startup lookup derived from the data, and the data-access helpers the
# server calls. They pin today's output so the optimisation phases (parquet
# re-layout, R/ split, bounded caches, lazy Subbasin_Extremes, download
# pushdown, ...) can prove they change nothing unintended.
#
# Row order: `ordered = FALSE` is used wherever the code does not define the
# order (Arrow dataset scans / Arrow-side distinct() and group_by() do not
# guarantee one - several of these were observed to change between runs);
# snap_obj() then sorts rows by all columns (and factor levels) before
# hashing. Everything else is hashed in its exact order.
#
# Representative ids (checked against the data):
#   subbasins  765, 2592, 3793 - several chemical SITE_IDs each (duplicate
#                                dates; today the duplicated rows are
#                                identical, so arrange(date) is still exact)
#              353, 3310       - single-site chemical subbasins that are also
#                                HYPE subbasins with observations
#   water bodies GB104027057290, GB104027069400 (both have modelled data)
#   chemicals  fipronil, imidacloprid (daily + D3D + source apportionment),
#              Zn (metal), cypermethrin / Metformin (chem_keys whose water
#              body parquet labels - "Cypermethrin" / "metformin" - differ
#              only in casing)

testthat::local_edition(3)

multi_site_subbasins <- c(765, 2592, 3793)
single_site_subbasins <- c(353, 3310)
rep_waterbodies <- c("GB104027057290", "GB104027069400")

# ---- loader -------------------------------------------------------------------

test_that("loader: only app.R's launch expressions were skipped", {
  expect_identical(Sys.getenv("ECOMIX_AUTORUN"), "0")
  skipped <- getOption("ecomix.tests.skipped")
  expect_true(length(skipped) >= 1)
  expect_true(all(grepl("shinyApp|runApp|^app$", skipped)))
  expect_true(is.function(get0("server", envir = globalenv())))
  expect_false(exists("app", envir = globalenv(), inherits = FALSE))
  expect_s3_class(get0("ui", envir = globalenv()), "shiny.tag.list")
  # Detection is by call content, not line numbers or text.
  expect_true(ecomix_skip_expr(quote(app <- shinyApp(ui = ui, server = server))))
  expect_true(ecomix_skip_expr(quote(if (identical(Sys.getenv("X"), "1")) shiny::runApp(app))))
  expect_true(ecomix_skip_expr(quote(app)))
  expect_false(ecomix_skip_expr(quote(server <- function(input, output, session) runApp())))
  expect_false(ecomix_skip_expr(quote(x <- df[, 1])))
  expect_false(ecomix_skip_expr(quote(application)))
})

# ---- startup lookups ----------------------------------------------------------

test_that("HYPE / climate startup lookups", {
  snap_obj(hype_subbasin_ids, "hype_subbasin_ids", ordered = FALSE)
  snap_obj(proj_periods, "proj_periods", ordered = FALSE)
  snap_obj(available_scenarios, "available_scenarios")
  snap_obj(prediction_period_choices, "prediction_period_choices")
  snap_obj(baseline_period, "baseline_period")
  snap_obj(scenario_default, "scenario_default")
  snap_obj(spatial_period_choices, "spatial_period_choices")
  snap_obj(df_observed_subbasins, "df_observed_subbasins")
  snap_obj(observed_hydro_vars, "observed_hydro_vars")
  snap_obj(observed_hydro_points, "observed_hydro_points")
  snap_obj(df_stats_climate, "df_stats_climate")
  # The plan filters df_stats_lc down to its "Upstream area" rows - pin both
  # the full table and the subset the app actually reads.
  snap_obj(df_stats_lc, "df_stats_lc")
  snap_obj(df_stats_lc[df_stats_lc$variable == "Upstream area", ], "df_stats_lc_upstream_area")
  # Read eagerly at startup today (planned to become lazy).
  snap_obj(df_map_input, "df_map_input")
})

test_that("map layer startup lookups (sf)", {
  snap_obj(subbasin_shp, "subbasin_shp")
  snap_obj(opcat_shp, "opcat_shp")
  snap_obj(df_subbasin_opcat, "df_subbasin_opcat")
  snap_obj(sf::st_drop_geometry(subbasin_opcat_shp), "subbasin_opcat_shp_attributes")
  snap_obj(canon_hash(sf::st_geometry(subbasin_opcat_shp)), "subbasin_opcat_shp_geometry_hash")
  snap_obj(hype_subbasin_shp, "hype_subbasin_shp")
  snap_obj(sf::st_drop_geometry(waterbody_opcat_shp), "waterbody_opcat_shp_attributes")
  snap_obj(canon_hash(sf::st_geometry(waterbody_opcat_shp)), "waterbody_opcat_shp_geometry_hash")
  snap_obj(waterbody_shp, "waterbody_shp")
  snap_obj(chem_coverage_shp, "chem_coverage_shp")
  snap_obj(opcat_levels, "opcat_levels")
  snap_obj(opcat_color_lookup, "opcat_color_lookup")
  snap_obj(opcat_counts, "opcat_counts")
  snap_obj(opcat_bounds, "opcat_bounds")
  snap_obj(lapply(opcat_bounds, num_txt), "opcat_bounds_values")
})

test_that("measured / modelled site startup lookups", {
  snap_obj(df_measured_raw, "df_measured_raw")
  snap_obj(measured_chemical_cols, "measured_chemical_cols")
  snap_obj(measured_physchem_cols, "measured_physchem_cols")
  snap_obj(df_measured_sites, "df_measured_sites")
  snap_obj(df_measured_long, "df_measured_long")
  snap_obj(df_measured_site_summary, "df_measured_site_summary")
  snap_obj(measured_parameter_choices, "measured_parameter_choices")
  snap_obj(measured_site_info, "measured_site_info")
  snap_obj(modelled_site_info, "modelled_site_info")
  snap_obj(measured_site_info[[1]], "measured_site_info_first")
  snap_obj(modelled_site_info[[1]], "modelled_site_info_first")
})

test_that("chemical startup lookups", {
  snap_obj(chem_keys, "chem_keys")
  snap_obj(chem_variable_choices, "chem_variable_choices")
  snap_obj(chem_subbasin_ids, "chem_subbasin_ids")
  expect_true(all(c(multi_site_subbasins, single_site_subbasins) %in% chem_subbasin_ids))

  # D3D (Chemical Explorer) datasets: Arrow handles, so pin their shape.
  snap_obj(lapply(chem_d3d_datasets, function(ds) list(
    label = ds$label, unit = ds$unit,
    conc_files = basename(ds$conc$files), sspaf_files = basename(ds$sspaf$files)
  )), "chem_d3d_datasets")
  snap_obj(chem_d3d_choices, "chem_d3d_choices")
  snap_obj(chem_d3d_site_ids, "chem_d3d_site_ids")
  snap_obj(chem_d3d_site_choices, "chem_d3d_site_choices")
  snap_obj(chem_d3d_site_choices[["fipronil"]], "chem_d3d_site_choices_fipronil")
  snap_obj(chem_site_wb, "chem_site_wb")
  snap_obj(lapply(chem_src_files, function(f) basename(unlist(f))), "chem_src_files")
  snap_obj(chem_wb_id_key, "chem_wb_id_key")

  # Ecological risk: one table per chemical file.
  snap_obj(basename(eco_risk_files), "eco_risk_files")
  snap_obj(eco_risk_choices, "eco_risk_choices")
  for (nm in names(eco_risk_data)) snap_obj(eco_risk_data[[nm]], paste0("eco_risk_data$", nm))

  # Water body (monthly) chemical dataset.
  snap_obj(wb_chem_raw_labels, "wb_chem_raw_labels", ordered = FALSE)
  snap_obj(wb_chem_key_map[order(names(wb_chem_key_map))], "wb_chem_key_map (sorted by raw label)")
  snap_obj(vapply(chem_keys, wb_chem_raw_label, character(1)), "wb_chem_raw_label()")
  snap_obj(wb_chem_waterbody_ids, "wb_chem_waterbody_ids")
  snap_obj(wbchem_variable_choices, "wbchem_variable_choices")
  snap_obj(modelled_parameter_choices, "modelled_parameter_choices")
})

test_that("widget choice lookups", {
  snap_obj(hype_variable_choices, "hype_variable_choices")
  snap_obj(prediction_variable_choices, "prediction_variable_choices")
  snap_obj(prediction_variable_choices_ext, "prediction_variable_choices_ext")
  snap_obj(prediction_variable_labels, "prediction_variable_labels")
  snap_obj(hype_pct_choices, "hype_pct_choices")
  snap_obj(chem_stat_choices, "chem_stat_choices")
  snap_obj(spatial_percentile_choices, "spatial_percentile_choices")
})

# ---- chemical (daily, by subbasin) helpers -------------------------------

test_that("get_chem_subbasin_series(): chemicals x stats x subbasins", {
  # Ordered by arrange(date). Multi-site subbasins carry one row per SITE_ID
  # per date; today those rows are identical, so the order is still exact.
  # ~0.2 s per call today (each is a filtered scan of the whole parquet).
  combos <- rbind(
    expand.grid(chem = c("fipronil", "imidacloprid"), stat = c("mean", "median", "90th"),
                stringsAsFactors = FALSE),
    data.frame(chem = "Zn", stat = "median")
  )
  for (i in seq_len(nrow(combos))) {
    for (sb in c(multi_site_subbasins, single_site_subbasins)) {
      snap_obj(get_chem_subbasin_series(combos$chem[i], combos$stat[i], sb),
               sprintf("get_chem_subbasin_series(%s, %s, %s)", combos$chem[i], combos$stat[i], sb))
    }
  }
  # A subbasin with no chemical predictions.
  snap_obj(get_chem_subbasin_series("fipronil", "mean", 1), "get_chem_subbasin_series(fipronil, mean, 1)")
})

test_that("get_chem_all_subbasins_daily() and get_chem_spatial_summary()", {
  snap_obj(get_chem_all_subbasins_daily("fipronil", "median"),
           "get_chem_all_subbasins_daily(fipronil, median)", ordered = FALSE)
  snap_obj(get_chem_all_subbasins_daily("Zn", "90th"),
           "get_chem_all_subbasins_daily(Zn, 90th)", ordered = FALSE)
  snap_obj(get_chem_spatial_summary("fipronil", "median"),
           "get_chem_spatial_summary(fipronil, median)", ordered = FALSE)
  snap_obj(get_chem_spatial_summary("Zn", "90th"),
           "get_chem_spatial_summary(Zn, 90th)", ordered = FALSE)
})

# ---- chemical (monthly, by water body) helpers -----------------------------

test_that("water body chemical helpers", {
  for (wb in rep_waterbodies) {
    for (chem in c("fipronil", "cypermethrin", "Metformin")) {
      snap_obj(get_wb_chem_series(chem, "median", wb),
               sprintf("get_wb_chem_series(%s, median, %s)", chem, wb))
    }
    snap_obj(get_wb_chem_series("Zn", "90th", wb), sprintf("get_wb_chem_series(Zn, 90th, %s)", wb))
    for (st in c("median", "mean")) {
      snap_obj(get_wb_chem_grid(wb, st), sprintf("get_wb_chem_grid(%s, %s)", wb, st), ordered = FALSE)
    }
  }
  snap_obj(get_wb_chem_all_waterbodies_monthly("fipronil", "median"),
           "get_wb_chem_all_waterbodies_monthly(fipronil, median)", ordered = FALSE)
  snap_obj(get_wb_chem_all_waterbodies_monthly("cypermethrin", "90th"),
           "get_wb_chem_all_waterbodies_monthly(cypermethrin, 90th)", ordered = FALSE)
  snap_obj(get_wb_chem_spatial_summary("fipronil", "median"),
           "get_wb_chem_spatial_summary(fipronil, median)", ordered = FALSE)
  snap_obj(get_wb_chem_spatial_summary("Metformin", "mean"),
           "get_wb_chem_spatial_summary(Metformin, mean)", ordered = FALSE)
  # The casing reconciliation must find rows for both mixed-case keys.
  expect_gt(nrow(get_wb_chem_series("cypermethrin", "median", rep_waterbodies[1])), 0)
  expect_gt(nrow(get_wb_chem_series("Metformin", "median", rep_waterbodies[1])), 0)
})

# ---- Chemical Explorer helpers -----------------------------------------------

test_that("Chemical Explorer helpers (D3D series, thresholds, source apportionment)", {
  for (chem in c("fipronil", "imidacloprid")) {
    for (site in c("1", "356", "504")) {
      snap_obj(get_chem_d3d_series(chem, site), sprintf("get_chem_d3d_series(%s, %s)", chem, site))
    }
  }
  # Thresholds: a site-specific site, a generic one, and a chemical with no
  # ecological risk file (NULL).
  eco <- eco_risk_data[["fipronil"]]
  has_ss <- !is_na_string(eco[["site_specific hc5"]])
  ss_site <- as.character(eco$site_id[which(has_ss)[1]])
  gen_site <- as.character(eco$site_id[which(!has_ss)[1]])
  snap_obj(list(ss_site = ss_site, gen_site = gen_site), "threshold sites")
  snap_obj(get_chem_d3d_thresholds("fipronil", ss_site), "get_chem_d3d_thresholds(fipronil, site-specific)")
  snap_obj(get_chem_d3d_thresholds("fipronil", gen_site), "get_chem_d3d_thresholds(fipronil, generic)")
  expect_null(get_chem_d3d_thresholds("imidacloprid", "1"))
  expect_null(get_chem_d3d_thresholds("fipronil", "no-such-site"))

  src <- get_chem_src("imidacloprid")
  snap_obj(src$dates, "get_chem_src(imidacloprid)$dates")
  for (nm in names(src$mats)) {
    snap_obj(src$mats[[nm]], paste0("get_chem_src(imidacloprid)$mats$", nm))
  }
  for (site in c("1", "356", "504")) {
    snap_obj(get_chem_src_split("imidacloprid", site), sprintf("get_chem_src_split(imidacloprid, %s)", site))
    snap_obj(get_chem_src_split("imidacloprid", site, as.Date("2020-07-15")),
             sprintf("get_chem_src_split(imidacloprid, %s, 2020-07-15)", site))
  }
  # Outside the apportionment record -> NULL.
  expect_null(get_chem_src_split("imidacloprid", "1", as.Date("2017-06-01")))
  # A site whose water body has no apportionment column (if any) -> NULL.
  no_wb <- chem_site_wb$SITE_ID[is.na(chem_site_wb$wb_id)][1]
  if (!is.na(no_wb)) expect_null(get_chem_src_split("imidacloprid", no_wb))
})

# ---- downloads ------------------------------------------------------------------

test_that("build_tabular_download() / get_tabular_download()", {
  for (v in c("discharge", "SS", "precip", "temp")) {
    snap_obj(build_tabular_download(v), sprintf("build_tabular_download(%s)", v), ordered = FALSE)
  }
  # get_tabular_download() is the cached wrapper: same content.
  expect_identical(canon_hash(canon_rows(get_tabular_download("discharge"))),
                   canon_hash(canon_rows(build_tabular_download("discharge"))))
  # Chemical variables are not routed through build_tabular_download() (the
  # Download page uses get_chem_all_subbasins_daily() instead); passing one
  # falls through to the HYPE branch and yields an empty table today.
  snap_obj(build_tabular_download("chem_fipronil"), "build_tabular_download(chem_fipronil)")
})

test_that("full_res_layer()", {
  for (nm in c("subbasins", "catchments", "waterbodies")) {
    lyr <- full_res_layer(nm)
    snap_obj(sf::st_drop_geometry(lyr), sprintf("full_res_layer(%s) attributes", nm))
    snap_obj(canon_hash(sf::st_geometry(lyr)), sprintf("full_res_layer(%s) geometry hash", nm))
    snap_obj(sf::st_crs(lyr)$epsg, sprintf("full_res_layer(%s) crs", nm))
  }
})

# ---- pure helpers -----------------------------------------------------------------

test_that("pure helper functions", {
  df_month <- ds_proj_month %>%
    dplyr::filter(subbasin == 3310L, variable == "discharge", ssp == "SSP585", period == "2070-2079") %>%
    dplyr::collect() %>%
    dplyr::arrange(month)
  snap_obj(pooled_percentiles(df_month, c(0.01, 0.5, 0.99)), "pooled_percentiles(3310 discharge SSP585 2070s)")
  snap_obj(treemap_layout(c(0.6, 0.3, 0.1)), "treemap_layout(0.6, 0.3, 0.1)")
  snap_obj(treemap_layout(c(0.05, 0.9, 0.05)), "treemap_layout(0.05, 0.9, 0.05)")
  snap_obj(pretty_unit(c("m3/s", "ug/l", "degC", "mm")), "pretty_unit")
  snap_obj(measured_parameter_label(measured_chemical_cols), "measured_parameter_label")
  snap_obj(compute_eco_risk_level(eco_risk_data[["fipronil"]]), "compute_eco_risk_level(fipronil)")
  snap_obj(build_eco_risk_popup(head(eco_risk_data[["fipronil"]], 5), "fipronil", "Fipronil"),
           "build_eco_risk_popup(fipronil, head 5)")
  snap_obj(pal_opcat(head(opcat_levels, 5)), "pal_opcat")
  expect_true(is_chem_variable("chem_fipronil"))
  expect_false(is_chem_variable("discharge"))
  expect_false(is_chem_variable(NULL))
  expect_true(is_wbchem_variable("wbchem_fipronil"))
  expect_false(is_wbchem_variable("chem_fipronil"))
  expect_identical(chem_key_from_variable("chem_Zn"), "Zn")
  expect_identical(wbchem_key_from_variable("wbchem_Zn"), "Zn")
  expect_identical(is_na_string(c("NA", " na ", "1", NA)), c(TRUE, TRUE, FALSE, TRUE))
})

# ---- float-tolerant companions ---------------------------------------------------

# The spatial summaries are means computed by Arrow over each group. If the
# parquet re-layout changes the order rows reach the aggregation, the last
# bits of a mean can change. These companions hash the same results with
# doubles rounded to 12 significant digits: if the exact snapshots above fail
# but these pass, the difference is floating-point summation noise only.
round_sig <- function(df, digits = 12) {
  for (nm in names(df)) if (is.double(df[[nm]])) df[[nm]] <- signif(df[[nm]], digits)
  df
}

test_that("spatial summaries at 12 significant digits (diagnostic companions)", {
  snap_obj(round_sig(get_chem_spatial_summary("fipronil", "median")),
           "get_chem_spatial_summary(fipronil, median) @ 12 sig. digits", ordered = FALSE)
  snap_obj(round_sig(get_chem_spatial_summary("Zn", "90th")),
           "get_chem_spatial_summary(Zn, 90th) @ 12 sig. digits", ordered = FALSE)
  snap_obj(round_sig(get_wb_chem_spatial_summary("fipronil", "median")),
           "get_wb_chem_spatial_summary(fipronil, median) @ 12 sig. digits", ordered = FALSE)
  snap_obj(round_sig(get_wb_chem_spatial_summary("Metformin", "mean")),
           "get_wb_chem_spatial_summary(Metformin, mean) @ 12 sig. digits", ordered = FALSE)
})
