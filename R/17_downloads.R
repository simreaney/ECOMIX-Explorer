# ---- Download page extracts ----
#
# Tabular downloads (and their previews) are one subbasin's rows, read with
# the subbasin pushed into the Arrow query and cached per selection. Spatial
# downloads summarise every subbasin's rows; those rows are read when needed
# and only their small summaries are cached.

# Monthly p50 rows (subbasin, scenario, period, month, value, unit) of one
# HYPE or climate forcing variable, values rounded to 3 decimals as
# downloaded: for every subbasin, or with `subbasin_id` for that subbasin
# only (each file is sorted by subbasin, so Arrow reads one or two row
# groups). Rows are put back in the files' own order, which is also
# (subbasin, scenario, period, month): an Arrow scan hands back a file's row
# groups in whatever order its threads finish, which made the order of the
# all-subbasin rows, and of the few subbasins split across two row groups,
# change from run to run.
build_tabular_download <- function(dl_variable, subbasin_id = NULL) {
  if (dl_variable %in% c("precip", "temp")) {
    dl_lookup <- c("precip" = "Precipitation", "temp" = "Temperature")
    dl_variable_label <- unname(dl_lookup[dl_variable])

    df_download <- ds_proj_forcing %>%
      filter(variable == dl_variable_label, time_aggregation == "monthly") %>%
      filter_subbasin(subbasin_id) %>%
      select(subbasin, period, month, p50, unit) %>%
      collect_quiet() %>%
      mutate("scenario" = forcing_scenario_label) %>%
      rename("value" = p50)
  } else {
    # dl_variable is the HYPE variable code itself (see widget_download_variable).
    df_download <- ds_proj_month %>%
      filter(variable == dl_variable) %>%
      filter_subbasin(subbasin_id) %>%
      select(subbasin, ssp, period, month, p50, unit) %>%
      collect() %>%
      rename("scenario" = ssp, "value" = p50)
  }

  df_download %>%
    mutate("value" = round(value, 3)) %>%
    dplyr::select(subbasin, scenario, period, month, value, unit) %>%
    dplyr::arrange(subbasin, scenario, period, month)
}

# One subbasin's rows - the Download page's tabular download and preview,
# and the Hydro Explorer's "Download this selection".
get_subbasin_tabular_download <- function(dl_variable, subbasin_id) {
  cache_fetch("tabular_download", dl_variable, subbasin_id,
              compute = function() build_tabular_download(dl_variable, subbasin_id))
}

# The spatial downloads' summaries of a download's rows: per subbasin (the
# subbasins layer, rounded) and over all rows (the catchment layer, rounded
# by the caller; NULL when there are no rows). A mean depends on the order
# its values are added in, so the rows must come in a fixed order -
# build_tabular_download() and get_chem_all_subbasins_daily() both return
# their files' stored order. (Over Arrow's varying scan order the catchment
# mean used to change in its last bits, never enough to change it rounded.)
summarise_download <- function(tab_data) {
  by_subbasin <- tab_data %>%
    mutate(subbasin = as.integer(subbasin)) %>%
    group_by(subbasin) %>%
    summarise(
      n_records = n(),
      value_mean = round(mean(value, na.rm = TRUE), 3),
      value_min = round(min(value, na.rm = TRUE), 3),
      value_max = round(max(value, na.rm = TRUE), 3),
      unit = dplyr::first(unit),
      .groups = "drop"
    )
  catchment <- if (nrow(tab_data) > 0) {
    tab_data %>%
      summarise(
        n_records = n(),
        value_mean = mean(value, na.rm = TRUE),
        value_min = min(value, na.rm = TRUE),
        value_max = max(value, na.rm = TRUE),
        unit = dplyr::first(unit)
      )
  }
  list(by_subbasin = by_subbasin, catchment = catchment)
}

# Summaries of every subbasin's rows for a HYPE / climate forcing variable
# (0.7M-1.8M rows) and for a subbasin chemical (~1M rows).
get_tabular_download_summary <- function(dl_variable) {
  cache_fetch("tabular_download_summary", dl_variable,
              compute = function() summarise_download(build_tabular_download(dl_variable)))
}

get_chem_download_summary <- function(chem_key, stat_val) {
  cache_fetch("chem_download_summary", chem_key, stat_val,
              compute = function() summarise_download(get_chem_all_subbasins_daily(chem_key, stat_val)))
}

# Spatial download formats. GeoParquet is written through GDAL's Parquet
# driver, which many GDAL builds (including common macOS and Debian ones)
# don't include - offering it there only produced a "Driver not available"
# error on download, so it is listed only when the driver is present.
spatial_download_formats <- c("Shapefile (.zip)" = "shp", "GeoParquet" = "geoparquet", "GPKG" = "gpkg")
if (!"Parquet" %in% sf::st_drivers(what = "vector")$name) {
  spatial_download_formats <- spatial_download_formats[spatial_download_formats != "geoparquet"]
}

# Keep only polygons with a summary value (mean, min or max) for the selected
# variable. Polygons without any records are dropped from the preview, the
# row count and the download.
spatial_rows_with_values <- function(df) {
  if (!all(c("value_mean", "value_min", "value_max") %in% names(df))) return(df)
  dplyr::filter(df, !(is.na(value_mean) & is.na(value_min) & is.na(value_max)))
}
