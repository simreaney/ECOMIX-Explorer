# ---- Chemical concentration predictions (monthly, by water body) ----
#
# The "modelled" counterpart of the chem_* dataset above: instead of a daily
# record resolved to a subbasin, this is a monthly (2018-2022) record
# resolved to an EA water body (waterbody_id, matching the "water_body"
# property already used by waterbody_shp/waterbody_opcat_shp). Built by
# scripts/build_chem_parquet.py::build_monthly_parquet() from the same style
# of per-chemical wide CSVs as the daily dataset. Kept as clearly-prefixed
# siblings (wb_chem_*/wbchem_*) rather than folding into the chem_* names
# above, since those already unambiguously mean "daily, by subbasin".
# chemical/stat are plain string columns (not dictionary-encoded) with one
# row group per chemical x statistic, so a chemical/stat filter skips every
# other row group on its min/max statistics, and distinct()/collect() across
# chemicals needs no casting.
wb_chem_dir <- here("data-chem/2_Modelling_results_by_waterbody_monthly_conc_ng_per_L")
wb_chem_dataset <- arrow::open_dataset(
  file.path(wb_chem_dir, "2_Modelling_results_by_waterbody_monthly_conc_ng_per_L.parquet")
)

# The modelled dataset's "chemical" values match chem_keys case-insensitively
# (only "Cypermethrin"/"cypermethrin" and "metformin"/"Metformin" actually
# differ in casing) - reconciled here from the data itself rather than a
# hand-maintained lookup, with a fail-fast check in case the two vocabularies
# ever diverge by more than a casing difference.
wb_chem_raw_labels <- wb_chem_dataset %>%
  dplyr::distinct(chemical) %>%
  dplyr::collect() %>%
  dplyr::pull(chemical)
wb_chem_key_map <- setNames(chem_keys[match(tolower(wb_chem_raw_labels), tolower(chem_keys))], wb_chem_raw_labels)
stopifnot(!anyNA(wb_chem_key_map))
wb_chem_raw_label <- function(chem_key) names(wb_chem_key_map)[wb_chem_key_map == chem_key][1]

# Water bodies with modelled data (336 of the 370 polygons in
# EA_Waterbodies_AOI.geojson) - the other 34 are drawn on the map but greyed
# out and not selectable anywhere else in the app.
wb_chem_waterbody_ids <- wb_chem_dataset %>%
  dplyr::distinct(waterbody_id) %>%
  dplyr::collect() %>%
  dplyr::pull(waterbody_id) %>%
  sort()

waterbody_opcat_shp <- waterbody_opcat_shp %>%
  dplyr::mutate(has_model_data = water_body %in% wb_chem_waterbody_ids)

# Display copy of the water bodies for the two maps that draw all 370 at once
# (Map page layer, Spatial Datasets water body chemicals): the same rows,
# order and attributes as waterbody_opcat_shp, but with the 20 m simplified
# geometry from scripts/build_map_layers.R, which roughly halves that payload.
# Everything that works with a single water body's shape keeps the 10 m one.
waterbody_map_shp <- local({
  map_geom <- read_sf(here("gis-data", "app", "waterbodies_map.gpkg"))
  out <- waterbody_opcat_shp
  sf::st_geometry(out) <- sf::st_geometry(map_geom)[match(out$water_body, map_geom$water_body)]
  out
})

# Per-waterbody, per-chemical monthly series - Site Details time series panel
# and Hydro Explorer's water-body mode.
wb_chem_series_cache <- new.env(parent = emptyenv())
get_wb_chem_series <- function(chem_key, stat_val, wb_id) {
  key <- paste(chem_key, stat_val, wb_id, sep = "||")
  if (!exists(key, envir = wb_chem_series_cache, inherits = FALSE)) {
    raw <- wb_chem_raw_label(chem_key)
    df <- wb_chem_dataset %>%
      dplyr::filter(chemical == raw, stat == stat_val, waterbody_id == wb_id) %>%
      dplyr::select(date, value = conc_ng_L) %>%
      dplyr::collect() %>%
      dplyr::arrange(date)
    df$year <- as.integer(format(df$date, "%Y"))
    df$month <- as.integer(format(df$date, "%m"))
    wb_chem_series_cache[[key]] <- df
  }
  wb_chem_series_cache[[key]]
}

# All 26 chemicals for one water body/statistic - Site Details occurrence
# heatmap.
wb_chem_grid_cache <- new.env(parent = emptyenv())
get_wb_chem_grid <- function(wb_id, stat_val) {
  key <- paste(wb_id, stat_val, sep = "||")
  if (!exists(key, envir = wb_chem_grid_cache, inherits = FALSE)) {
    # chemical comes back as a factor (levels in sorted order), the type the
    # parquet's former dictionary-encoded chemical column collected to.
    wb_chem_grid_cache[[key]] <- wb_chem_dataset %>%
      dplyr::filter(waterbody_id == wb_id, stat == stat_val) %>%
      dplyr::select(date, chemical, value = conc_ng_L) %>%
      dplyr::collect() %>%
      dplyr::mutate(chemical = factor(chemical, levels = sort(unique(chemical), method = "radix")),
                    chem_key = unname(wb_chem_key_map[as.character(chemical)]))
  }
  wb_chem_grid_cache[[key]]
}

# All-waterbody long series for one chemical/statistic - Download and
# the Spatial Datasets choropleth basis.
wb_chem_full_cache <- new.env(parent = emptyenv())
get_wb_chem_all_waterbodies_monthly <- function(chem_key, stat_val) {
  key <- paste(chem_key, stat_val, sep = "||")
  if (!exists(key, envir = wb_chem_full_cache, inherits = FALSE)) {
    raw <- wb_chem_raw_label(chem_key)
    wb_chem_full_cache[[key]] <- wb_chem_dataset %>%
      dplyr::filter(chemical == raw, stat == stat_val) %>%
      dplyr::select(date, waterbody_id, value = conc_ng_L) %>%
      dplyr::collect() %>%
      dplyr::mutate(unit = "ng/L")
  }
  wb_chem_full_cache[[key]]
}

# Time-averaged (2018-2022) value per water body for one chemical/statistic -
# the basis of the Spatial Datasets choropleth for modelled water bodies.
wb_chem_spatial_cache <- new.env(parent = emptyenv())
get_wb_chem_spatial_summary <- function(chem_key, stat_val) {
  key <- paste(chem_key, stat_val, sep = "||")
  if (!exists(key, envir = wb_chem_spatial_cache, inherits = FALSE)) {
    raw <- wb_chem_raw_label(chem_key)
    wb_chem_spatial_cache[[key]] <- wb_chem_dataset %>%
      dplyr::filter(chemical == raw, stat == stat_val) %>%
      dplyr::group_by(waterbody_id) %>%
      dplyr::summarise(value = mean(conc_ng_L, na.rm = TRUE), .groups = "drop") %>%
      dplyr::collect() %>%
      as.data.frame()
  }
  wb_chem_spatial_cache[[key]]
}

wbchem_variable_choices <- setNames(paste0("wbchem_", chem_keys), unname(chem_display_lookup[chem_keys]))
is_wbchem_variable <- function(v) !is.null(v) && nzchar(v) && grepl("^wbchem_", v)
wbchem_key_from_variable <- function(v) sub("^wbchem_", "", v)

# Site Details "Modelled" panel selector - plain chem_key values (unprefixed,
# like the measured chemical column names), no HYPE/physchem groups since
# modelled water bodies only ever have chemical data.
modelled_parameter_choices <- list(
  "Modelled chemicals" = setNames(chem_keys, unname(chem_display_lookup[chem_keys]))
)
