# ---- HYPE hydrology and climate datasets ----
#
# Historical simulations at observation sites - ~3.9M daily rows, so read per
# subbasin on demand (selected_historical() in the server; one sorted file
# per variable, so that's a row-group lookup) rather than held in memory.
ds_historical_observations <- safe_open_dataset(data_path("data/DB_Historical_Sim_Obs"))
df_observed_subbasins <- cached_lookup("df_observed_subbasins", data_path("data/DB_Historical_Sim_Obs"), function() {
  ds_historical_observations %>%
    dplyr::distinct(subbasin, variable) %>%
    collect() %>%
    dplyr::arrange(subbasin, variable)
})

# One point per subbasin that has observed hydrology data, for the Map page's
# "Observed hydrology sites" layer - placed at the subbasin centroid since the
# observations themselves are subbasin-level, not a specific gauge location.
observed_hydro_vars <- df_observed_subbasins %>%
  group_by(subbasin) %>%
  summarise(variables = paste(sort(unique(variable)), collapse = ", "), .groups = "drop")

observed_hydro_centroids <- subbasin_opcat_shp %>%
  filter(Id %in% observed_hydro_vars$subbasin) %>%
  st_centroid()
observed_hydro_coords <- sf::st_coordinates(observed_hydro_centroids)
observed_hydro_points <- observed_hydro_centroids %>%
  sf::st_drop_geometry() %>%
  mutate(lon = observed_hydro_coords[, "X"], lat = observed_hydro_coords[, "Y"]) %>%
  left_join(observed_hydro_vars, by = c("Id" = "subbasin"))

# Keep Arrow dataset handles open once to avoid repeated metadata scans in renderers.
# All built by scripts/import_database_update.py from the HYPE/climate
# database delivery (see data/DB_readme.xlsx): one sorted parquet file per
# HYPE variable, so Arrow prunes per-subbasin reads on row-group statistics.
#   DB_Proj_Year/Month - HYPE projections, p01..p99 of daily values pooled
#                        over the 25 x 12 member ensemble (and, for Month,
#                        the years of the period)
#   DB_Proj_Forcing    - UKCP18 RCP8.5 climate, p10/p50/p90 across the 12
#                        regional members (annual and monthly)
ds_proj_forcing <- safe_open_dataset(data_path("data/DB_Proj_Forcing"))
ds_proj_year <- safe_open_dataset(data_path("data/DB_Proj_Year"))
ds_proj_month <- safe_open_dataset(data_path("data/DB_Proj_Month"))

# The climate projections are a single emissions scenario, so they carry no
# ssp column - this labels them wherever a scenario is shown.
forcing_scenario_label <- "UKCP18 RCP8.5"

# Percentile columns of the HYPE projection datasets, and their probabilities.
hype_pct_cols <- c("p01", "p05", "p10", "p25", "p50", "p75", "p90", "p95", "p99")
hype_pct_probs <- c(0.01, 0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95, 0.99)
hype_pct_choices <- c(
  "Extreme low (1st percentile)" = "p01",
  "Very low (5th percentile)" = "p05",
  "Low (10th percentile)" = "p10",
  "Lower quartile (25th percentile)" = "p25",
  "Average (50th percentile)" = "p50",
  "Upper quartile (75th percentile)" = "p75",
  "High (90th percentile)" = "p90",
  "Very high (95th percentile)" = "p95",
  "Extreme high (99th percentile)" = "p99"
)
hype_pct_default <- c("p10", "p50", "p90")

# Display form of the unit strings used in the data (from DB_readme.xlsx).
pretty_unit <- function(u) {
  dplyr::recode(u, "m3/s" = "m³/s", "ug/l" = "µg/l", "degC" = "°C")
}

# Scenario / period widget choices, read off the monthly projections (two
# dictionary columns, so this is cheap). Each future scenario shares one set
# of decade periods ("2070-2079"); Baseline has its own single period.
proj_periods <- cached_lookup("proj_periods", data_path("data/DB_Proj_Month"), function() {
  ds_proj_month %>% dplyr::distinct(ssp, period) %>% collect()
})
available_scenarios <- c("Baseline", sort(setdiff(unique(proj_periods$ssp), "Baseline")))
prediction_period_choices <- sort(unique(proj_periods$period[proj_periods$ssp != "Baseline"]))
baseline_period <- unique(proj_periods$period[proj_periods$ssp == "Baseline"])[1]

# Map input for the Spatial Datasets choropleth: P1/P50/P99 of daily values
# per subbasin x variable x scenario x period, precomputed by
# scripts/import_database_update.py from the monthly projections (the same
# pooled distribution the Hydro Explorer's cumulative frequency plot draws).
# Renamed to the column names the map's chemical branches share. A lazy Arrow
# query rather than a table in memory: the map shows one variable x period x
# percentile slice (3126 rows) of the 1.25M, and reading that slice takes
# about as long (~0.05 s) as filtering the table in memory did, which held
# ~80 MB for the life of the process.
map_input_file <- data_path("data/Subbasin_Extremes.gz.parquet")
ds_map_input <- arrow::open_dataset(map_input_file) %>%
  dplyr::transmute(hype_variable = variable, ssp, period, subbasin, unit,
                   prediction_percentile = percentile, value)

# One variable x scenario x period x percentile slice of ds_map_input, one
# row per subbasin. The file stores each slice in subbasin order, which the
# in-memory table kept, but a scan returns row batches in no fixed order, so
# the order is restored explicitly.
get_map_input_slice <- function(variable_val, ssp_val, period_val, percentile_val) {
  ds_map_input %>%
    dplyr::filter(hype_variable == variable_val, ssp == ssp_val, period == period_val,
                  prediction_percentile == percentile_val) %>%
    dplyr::collect() %>%
    dplyr::arrange(subbasin)
}

# Scenario x period combinations actually present in the extremes data,
# labelled the same way the Hydro Explorer labels them ("SSP585 (2070-2079)"),
# used to drive the Spatial Datasets "Period" dropdown.
spatial_period_choices <- cached_lookup("spatial_period_choices", map_input_file, function() {
  ds_map_input %>%
    dplyr::distinct(ssp, period) %>%
    dplyr::collect() %>%
    dplyr::arrange(period, ssp) %>%
    dplyr::mutate(label = paste0(ssp, " (", period, ")"),
                  value = paste(ssp, period, sep = "|")) %>%
    { setNames(.$value, .$label) }
})

spatial_percentile_choices <- c("Low extreme (P1)" = "1", "Median (P50)" = "50", "High extreme (P99)" = "99")

# Distribution of daily values pooled over all months of a period, from the
# monthly percentile rows of one subbasin x variable x scenario x period:
# each month is taken as linear between its percentile knots (with its mass
# below p01 / above p99 placed at those knots), weighted by days in month.
# Returns the pooled quantiles at probs_out - the same calculation
# scripts/import_database_update.py uses for the Spatial Datasets extremes.
pooled_percentiles <- function(df_month, probs_out) {
  q <- t(apply(as.matrix(df_month[, hype_pct_cols]), 1, sort))
  w <- c(31, 28.25, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)[df_month$month]
  w <- w / sum(w)
  n_k <- length(hype_pct_probs)
  # Pooled CDF at each knot, from the left (before any step) and the right.
  cdf_at <- function(x, left) {
    f <- vapply(seq_len(nrow(q)), function(m) {
      k <- if (left) sum(q[m, ] < x) else sum(q[m, ] <= x)
      if (k == 0) return(0)
      if (k == n_k) return(1)
      hype_pct_probs[k] + (x - q[m, k]) / (q[m, k + 1] - q[m, k]) *
        (hype_pct_probs[k + 1] - hype_pct_probs[k])
    }, numeric(1))
    sum(w * f)
  }
  knots <- sort(unique(as.vector(q)))
  v <- c(knots, knots)
  f <- c(vapply(knots, cdf_at, numeric(1), left = TRUE),
         vapply(knots, cdf_at, numeric(1), left = FALSE))
  o <- order(f, v)
  stats::approx(f[o], v[o], xout = probs_out, ties = min, rule = 2)$y
}
