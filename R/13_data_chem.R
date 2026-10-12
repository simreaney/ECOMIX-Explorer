# ---- Chemical concentration predictions (daily, by subbasin) ----
#
# All chemical x summary-statistic series live in one long-format,
# Hive-partitioned parquet dataset - daily/chemical=<chem>/stat=<stat>/
# part-0.parquet with columns (date, site_id, subbasin, conc_ng_L), plus
# chemical/stat from the directory names - built by
# scripts/build_chem_parquet.py from the original per-chemical wide CSVs
# under data-chem/ (~76.5M rows total, ~700MB). arrow::open_dataset() reads
# it lazily: a chemical/stat filter only opens that one file, and since each
# file is sorted by (subbasin, site_id, date) in small row groups that end on
# subbasin boundaries, a subbasin filter only decodes the one row group
# holding it. That sort order is a storage detail, not a result order - the
# helpers below each arrange() their output explicitly. Unlike the HYPE
# datasets above, there's no SSP scenario or future period here - just a
# single 2018-2022 reconstructed daily record with three summary statistics
# (mean/median/90th) taking the place of p10/p50/p90. Variable ids are
# prefixed "chem_" so they can share the "Model variable" dropdown with the
# HYPE variables without colliding with hype_variable strings used elsewhere
# (e.g. in df_map_input).
chem_dir <- here("data-chem/1_DAILY_BY_SITE_conc_ng_L")
chem_dataset <- arrow::open_dataset(
  file.path(chem_dir, "daily"),
  partitioning = arrow::hive_partition(chemical = arrow::utf8(), stat = arrow::utf8())
)

# Display labels, aligned with the names already used on the measured Site
# Details page where the same substance appears there (e.g. "6PPD-Q",
# "Benzo[a]pyrene", "Erythromycin") so a chemical reads the same way across
# the app.
chem_display_lookup <- c(
  "6PPDQ" = "6PPD-Q", "Azoxystrobin" = "Azoxystrobin", "Cu" = "Cu",
  "Metformin" = "Metformin", "Zn" = "Zn", "anthracene" = "Anthracene",
  "azithromycin" = "Azithromycin", "benzoapyrene" = "Benzo[a]pyrene",
  "clarithromycin" = "Clarithromycin", "cypermethrin" = "Cypermethrin",
  "deltamethrin" = "Deltamethrin", "diclofenac" = "Diclofenac",
  "doramectin" = "Doramectin", "erithromycin" = "Erythromycin",
  "fipronil" = "Fipronil", "fluconazole" = "Fluconazole",
  "flufenacet" = "Flufenacet", "fluoranthene" = "Fluoranthene",
  "ibuprofen" = "Ibuprofen", "imidacloprid" = "Imidacloprid",
  "ivermectin" = "Ivermectin", "metazachlor" = "Metazachlor",
  "permethrin" = "Permethrin", "tebuconazole" = "Tebuconazole",
  "triallate" = "Triallate", "venlafaxine" = "Venlafaxine"
)

chem_keys <- sort(names(chem_display_lookup))
chem_variable_choices <- setNames(paste0("chem_", chem_keys), unname(chem_display_lookup[chem_keys]))
chem_variable_choices <- chem_variable_choices[order(names(chem_variable_choices))]

is_chem_variable <- function(v) !is.null(v) && nzchar(v) && grepl("^chem_", v)
chem_key_from_variable <- function(v) sub("^chem_", "", v)

chem_stat_choices <- c("Mean" = "mean", "Median" = "median", "90th percentile" = "90th")

# Real subbasin ids with chemical predictions - the same 537 for every
# chemical/statistic (the parquet already carries the resolved "subbasin" id
# alongside the chemical modelling's internal SITE_ID, so no separate key
# file/lookup is needed to translate between them here).
chem_subbasin_ids <- chem_dataset %>%
  dplyr::distinct(subbasin) %>%
  dplyr::collect() %>%
  dplyr::pull(subbasin) %>%
  sort()

chem_coverage_shp <- subbasin_shp %>% filter(Id %in% chem_subbasin_ids)

# Per-subbasin daily series (date, value) for one chemical/statistic -
# powering the Hydro Explorer projection plots and the "download this
# selection" / per-subbasin tabular download. Cheap enough (~0.02s) to read
# on demand and cache per subbasin actually visited, rather than eagerly
# loading all 78 x 537 series at startup.
chem_series_cache <- new.env(parent = emptyenv())
get_chem_subbasin_series <- function(chem_key, stat_val, subbasin_id) {
  key <- paste(chem_key, stat_val, subbasin_id, sep = "||")
  if (!exists(key, envir = chem_series_cache, inherits = FALSE)) {
    # A subbasin with several SITE_IDs has one row per site per date - ties
    # on date are broken by site_id (the order the old single-file layout,
    # stored by (site_id, date), happened to return them in).
    df <- chem_dataset %>%
      dplyr::filter(chemical == chem_key, stat == stat_val, subbasin == subbasin_id) %>%
      dplyr::select(date, site_id, value = conc_ng_L) %>%
      dplyr::collect() %>%
      dplyr::arrange(date, site_id) %>%
      dplyr::select(-site_id)
    df$year <- as.integer(format(df$date, "%Y"))
    df$month <- as.integer(format(df$date, "%m"))
    chem_series_cache[[key]] <- df
  }
  chem_series_cache[[key]]
}

# Full (all-subbasin) daily series for one chemical/statistic, long format -
# powers the Spatial Datasets time-averaged map and the Download page's
# "all subbasins" / catchment-level summaries. Cached per chemical/statistic
# actually requested. Rows are ordered by (site_id, date) - one site's whole
# series after another, the order the old single-file layout stored them in.
chem_full_cache <- new.env(parent = emptyenv())
get_chem_all_subbasins_daily <- function(chem_key, stat_val) {
  key <- paste(chem_key, stat_val, sep = "||")
  if (!exists(key, envir = chem_full_cache, inherits = FALSE)) {
    chem_full_cache[[key]] <- chem_dataset %>%
      dplyr::filter(chemical == chem_key, stat == stat_val) %>%
      dplyr::select(date, site_id, subbasin, value = conc_ng_L) %>%
      dplyr::collect() %>%
      dplyr::arrange(site_id, date) %>%
      dplyr::select(-site_id) %>%
      dplyr::mutate(unit = "ng/L")
  }
  chem_full_cache[[key]]
}

# Time-averaged (whole 2018-2022 record) value per subbasin for one
# chemical/statistic - the basis of the Spatial Datasets choropleth for
# chemicals, which (per product decision) has no period/scenario dimension.
chem_spatial_cache <- new.env(parent = emptyenv())
get_chem_spatial_summary <- function(chem_key, stat_val) {
  key <- paste(chem_key, stat_val, sep = "||")
  if (!exists(key, envir = chem_spatial_cache, inherits = FALSE)) {
    # Grouped by site_id (not subbasin): a handful of SITE_IDs in the
    # chemical modelling's key share the same real subbasin id, and rather
    # than averaging those together this keeps one row per SITE_ID's own
    # time-mean, matching the row-per-site-column shape the old per-file CSV
    # reads produced. Rows come back in site_id order (Arrow's grouped
    # output order is arbitrary). The means are reproducible to the last bit
    # only because scripts/build_chem_parquet.py never splits a site across
    # row groups: Arrow sums a group in row order within a scan batch, but
    # combines a group split over two batches in thread-finishing order.
    chem_spatial_cache[[key]] <- chem_dataset %>%
      dplyr::filter(chemical == chem_key, stat == stat_val) %>%
      dplyr::group_by(site_id, subbasin) %>%
      dplyr::summarise(value = mean(conc_ng_L, na.rm = TRUE), .groups = "drop") %>%
      dplyr::collect() %>%
      dplyr::arrange(site_id) %>%
      dplyr::select(subbasin, value) %>%
      as.data.frame()
  }
  chem_spatial_cache[[key]]
}
