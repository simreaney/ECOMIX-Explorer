# One-off validation script (not run as part of the Shiny app itself).
#
# Confirms the chemical parquet re-layout written by
# scripts/build_chem_parquet.py changes no data:
#   * daily-by-site: the old single file
#     (1_DAILY_BY_SITE_conc_ng_L/1_DAILY_BY_SITE_conc_ng_L.parquet, one row
#     group per chemical x stat) against the new Hive-partitioned dataset
#     (1_DAILY_BY_SITE_conc_ng_L/daily/chemical=*/stat=*/part-0.parquet), for
#     every chemical x stat - plus the app's chem helpers (taken verbatim from
#     R/13_data_chem.R) on the new dataset against today's queries on the old file;
#   * monthly-by-water-body: a backup of the parquet as it was before the
#     rebuild (chemical/stat dictionary-encoded) against the rebuilt file
#     (plain strings), plus the wb_chem helpers whose code changed.
# The old daily file is read one row group at a time
# (ParquetFileReader$ReadRowGroup) wherever possible, since any filtered
# query on it scans all ~76.5M rows.
#
# Usage (from the repo root; the old daily file must still be in place):
#   Rscript scripts/validate_chem_relayout.R <backup of the old monthly parquet>
# Prints a PASS/FAIL table and exits non-zero if any check fails. Without
# the monthly backup argument the monthly checks are reported as SKIP.

suppressPackageStartupMessages({
  library(arrow)
  library(dplyr)
  library(here)
})

old_daily_path <- here("data-chem/1_DAILY_BY_SITE_conc_ng_L/1_DAILY_BY_SITE_conc_ng_L.parquet")
new_daily_dir <- here("data-chem/1_DAILY_BY_SITE_conc_ng_L/daily")
args <- commandArgs(trailingOnly = TRUE)
old_monthly_path <- if (length(args) >= 1) normalizePath(args[1], mustWork = TRUE) else NA_character_

series_subbasins <- c(765, 2592, 3793, 353, 3310)  # 3 multi-site, 2 single-site
grid_waterbodies <- c("GB104027069400", "GB104027057290", "GB104027052280", "not-a-water-body")
# Arrow's default scan batch size: a row group larger than this would be
# split over several batches (see the get_chem_spatial_summary() check).
arrow_batch_rows <- 131072

# ---- result table -------------------------------------------------------------

results <- data.frame(check = character(), cases = integer(), failed = integer(),
                      result = character(), detail = character())
record <- function(check, ok, detail = "", skip = FALSE) {
  ok <- as.logical(ok)
  result <- if (skip) "SKIP" else if (length(ok) > 0 && all(ok %in% TRUE)) "PASS" else "FAIL"
  results[nrow(results) + 1, ] <<- list(check, length(ok), sum(!(ok %in% TRUE)), result, detail)
  cat(sprintf("[%s] %s (%d cases)%s\n", result, check, length(ok),
              if (nzchar(detail)) paste0(" - ", detail) else ""))
}
canon_df <- function(df, keys) {
  df <- as.data.frame(df)
  df <- df[do.call(order, c(unname(as.list(df[keys])), list(method = "radix"))), , drop = FALSE]
  rownames(df) <- NULL
  df
}

# ---- app helper definitions ----------------------------------------------------

# The helpers under test are evaluated from the app's own top-level
# assignments in R/ (not copies), in a small environment holding only what
# they need - loading the whole app would take far longer than this script.
app_exprs <- unlist(lapply(
  here("R", c("01_config.R", "01_data_access.R", "03_cache.R", "04_lookup_cache.R",
               "13_data_chem.R", "16_data_wb_chem.R")),
  function(f) as.list(parse(f, keep.source = FALSE))
))
app_env <- new.env(parent = globalenv())
eval_app_defs <- function(names) {
  for (nm in names) {
    hit <- Filter(function(e) is.call(e) && identical(e[[1]], as.name("<-")) &&
                    identical(e[[2]], as.name(nm)), app_exprs)
    if (length(hit) != 1) stop("expected exactly one top-level assignment to ", nm, " in R/")
    eval(hit[[1]], app_env)
  }
}
withr::with_dir(here(), eval_app_defs(c(
  "env_setting", "ecomix_data_root", "data_path", "filter_subbasin",
  "app_cache", "cache_fetch",
  "chem_dir", "chem_dataset", "chem_display_lookup", "chem_keys",
  "get_chem_subbasin_series", "read_chem_daily",
  "get_chem_all_subbasins_daily", "get_chem_spatial_summary"
)))

# Today's (pre-re-layout) query code, run against the old single file.
old_daily <- open_dataset(old_daily_path)
old_style_series <- function(chem_key, stat_val, subbasin_id) {
  df <- old_daily %>%
    dplyr::filter(chemical == chem_key, stat == stat_val, subbasin == subbasin_id) %>%
    dplyr::select(date, value = conc_ng_L) %>%
    dplyr::collect() %>%
    dplyr::arrange(date)
  df$year <- as.integer(format(df$date, "%Y"))
  df$month <- as.integer(format(df$date, "%m"))
  df
}
old_style_spatial <- function(chem_key, stat_val) {
  old_daily %>%
    dplyr::filter(chemical == chem_key, stat == stat_val) %>%
    dplyr::group_by(site_id, subbasin) %>%
    dplyr::summarise(value = mean(conc_ng_L, na.rm = TRUE), .groups = "drop") %>%
    dplyr::collect() %>%
    dplyr::arrange(site_id) %>%
    as.data.frame()
}

# ---- daily: old row groups vs new partitions ------------------------------------

cat("== daily-by-site ==\n")
old_reader <- ParquetFileReader$create(old_daily_path)
daily_cols <- c("date", "site_id", "subbasin", "conc_ng_L")
pairs <- data.frame(chemical = character(), stat = character())
chk <- list(one_pair = logical(), rows = logical(), sums = logical(), identical = logical(),
            old_order = logical(), layout = logical(), all_daily = logical(),
            spatial_seq = logical(), spatial_noise = logical())
spatial_diff_sites <- integer()
spatial_max_rel <- 0

for (i in seq_len(old_reader$num_row_groups) - 1L) {
  old_df <- as.data.frame(old_reader$ReadRowGroup(i))
  chem <- unique(as.character(old_df$chemical))
  st <- unique(as.character(old_df$stat))
  chk$one_pair <- c(chk$one_pair, length(chem) == 1 && length(st) == 1)
  chem <- chem[1]
  st <- st[1]
  pairs[nrow(pairs) + 1, ] <- list(chem, st)
  label <- paste(chem, st, sep = "/")
  old_df <- old_df[daily_cols]

  new_path <- file.path(new_daily_dir, paste0("chemical=", chem), paste0("stat=", st), "part-0.parquet")
  if (!file.exists(new_path)) {
    cat("  missing new partition for", label, "\n")
    for (k in setdiff(names(chk), c("one_pair", "spatial_noise"))) chk[[k]] <- c(chk[[k]], FALSE)
    next
  }
  new_reader <- ParquetFileReader$create(new_path)

  # Layout: row groups hold whole subbasins in increasing order (so a
  # subbasin, and hence a site, never spans two row groups) and each fits in
  # one scan batch.
  rg <- lapply(seq_len(new_reader$num_row_groups) - 1L, function(j) {
    sb <- as.vector(new_reader$ReadRowGroup(j, column_indices = 2L)$subbasin)
    c(min = min(sb), max = max(sb), n = length(sb), sorted = !is.unsorted(sb))
  })
  rg <- do.call(rbind, rg)
  chk$layout <- c(chk$layout, all(rg[, "sorted"] == 1) && all(rg[, "n"] <= arrow_batch_rows) &&
                    all(utils::tail(rg[, "min"], -1) > utils::head(rg[, "max"], -1)))

  new_df <- as.data.frame(new_reader$ReadTable())[daily_cols]

  chk$rows <- c(chk$rows, nrow(old_df) == nrow(new_df))
  old_ord <- order(old_df$site_id, old_df$date, method = "radix")
  chk$old_order <- c(chk$old_order, identical(old_ord, seq_len(nrow(old_df))))
  old_c <- canon_df(old_df, c("site_id", "date"))
  new_c <- canon_df(new_df, c("site_id", "date"))
  chk$sums <- c(chk$sums, identical(sum(old_c$conc_ng_L), sum(new_c$conc_ng_L)))
  ok <- identical(old_c, new_c)
  chk$identical <- c(chk$identical, ok)
  if (!ok) cat("  data mismatch for", label, "\n")

  # get_chem_all_subbasins_daily() on the new dataset == the old file's rows
  # in their stored order (what today's collect() returned when Arrow kept
  # file order).
  expected_all <- dplyr::as_tibble(old_c[c("date", "subbasin")]) %>%
    dplyr::mutate(value = old_c$conc_ng_L, unit = "ng/L")
  chk$all_daily <- c(chk$all_daily, identical(app_env$get_chem_all_subbasins_daily(chem, st), expected_all))

  # get_chem_spatial_summary(): reference = each site's values summed one by
  # one in date order in plain double arithmetic (vectorised across sites,
  # one date at a time), divided by the count - Arrow's per-batch mean.
  n_dates <- length(unique(old_c$date))
  sites <- unique(old_c$site_id)
  stopifnot(nrow(old_c) == n_dates * length(sites), !anyNA(old_c$conc_ng_L))
  m <- matrix(old_c$conc_ng_L, nrow = n_dates)
  acc <- numeric(ncol(m))
  for (d in seq_len(n_dates)) acc <- acc + m[d, ]
  seq_ref <- data.frame(subbasin = old_c$subbasin[match(sites, old_c$site_id)], value = acc / n_dates)
  new_sp <- app_env$get_chem_spatial_summary(chem, st)
  chk$spatial_seq <- c(chk$spatial_seq, identical(new_sp, seq_ref))

  # ... and against today's query on the old file (multi-threaded): sites
  # that straddled one of the old file's scan-batch boundaries were summed
  # as two partial sums, so may differ in the last bits.
  old_sp <- old_style_spatial(chem, st)
  same_keys <- identical(old_sp$subbasin, new_sp$subbasin)
  rel <- abs(old_sp$value - new_sp$value) / pmax(abs(old_sp$value), .Machine$double.xmin)
  spatial_diff_sites <- c(spatial_diff_sites, sum(old_sp$value != new_sp$value))
  spatial_max_rel <- max(spatial_max_rel, rel)
  chk$spatial_noise <- c(chk$spatial_noise, same_keys && all(rel < 1e-12))
  cat(sprintf("  %-26s %8d rows, %2d row groups, spatial sites differing from old query: %d\n",
              label, nrow(new_df), nrow(rg), sum(old_sp$value != new_sp$value)))
}

new_pairs <- local({
  dirs <- list.files(new_daily_dir, recursive = TRUE, pattern = "^part-0\\.parquet$")
  data.frame(chemical = sub("^chemical=([^/]+)/.*$", "\\1", dirs),
             stat = sub("^.*/stat=([^/]+)/.*$", "\\1", dirs))
})
record("daily: new partitions == old (chemical, stat) pairs",
       identical(canon_df(pairs, c("chemical", "stat")), canon_df(new_pairs, c("chemical", "stat"))),
       sprintf("old %d, new %d", nrow(pairs), nrow(new_pairs)))
record("daily: old row group holds one chemical x stat", chk$one_pair)
record("daily: row counts", chk$rows, sprintf("%s rows in total", format(old_reader$num_rows, big.mark = ",")))
record("daily: sum(conc_ng_L)", chk$sums)
record("daily: identical() ordered by (site_id, date)", chk$identical)
record("daily: old file stored in (site_id, date) order", chk$old_order)
record("daily: new row groups = whole subbasins, ascending, <= batch size", chk$layout)
record("get_chem_all_subbasins_daily() == old file rows in stored order", chk$all_daily)
record("get_chem_spatial_summary() == sequential per-site mean", chk$spatial_seq)
record("get_chem_spatial_summary() vs old query: float noise only", chk$spatial_noise,
       sprintf("%d of %d pairs have differing sites (max %d sites, max rel. diff %.2g)",
               sum(spatial_diff_sites > 0), length(spatial_diff_sites),
               max(c(0L, spatial_diff_sites)), spatial_max_rel))

# ---- daily: per-subbasin series ---------------------------------------------------

cat("== get_chem_subbasin_series() ==\n")
series_ok <- logical()
for (chem in app_env$chem_keys) {
  for (st in c("mean", "median", "90th")) {
    for (sb in series_subbasins) {
      ok <- identical(app_env$get_chem_subbasin_series(chem, st, sb), old_style_series(chem, st, sb))
      if (!ok) cat("  mismatch:", chem, st, sb, "\n")
      series_ok <- c(series_ok, ok)
    }
  }
}
record(sprintf("get_chem_subbasin_series() old query vs new helper (%d chemicals x 3 stats x subbasins %s)",
               length(app_env$chem_keys), paste(series_subbasins, collapse = ",")), series_ok)

# ---- monthly-by-water-body -------------------------------------------------------------

cat("== monthly-by-water-body ==\n")
if (is.na(old_monthly_path)) {
  record("monthly: backup vs new file", logical(), "no backup path given", skip = TRUE)
} else {
  withr::with_dir(here(), eval_app_defs(c(
    "LOOKUP_CACHE_VERSION", "lookup_cache_dir", "lookup_cache_log", "lookup_fingerprint", "cached_lookup",
    "wb_chem_dir", "wb_chem_file", "wb_chem_dataset", "wb_chem_raw_labels", "wb_chem_key_map",
    "get_wb_chem_grid", "wb_chem_raw_label", "get_wb_chem_spatial_summary"
  )))
  old_monthly <- open_dataset(old_monthly_path)
  new_reader <- ParquetFileReader$create(file.path(app_env$wb_chem_dir,
    "2_Modelling_results_by_waterbody_monthly_conc_ng_per_L.parquet"))

  old_types <- vapply(old_monthly$schema$fields, function(f) f$type$ToString(), character(1))
  new_types <- vapply(new_reader$GetSchema()$fields, function(f) f$type$ToString(), character(1))
  record("monthly: schema (chemical/stat dictionary -> string, rest unchanged)",
         identical(names(old_monthly$schema), names(new_reader$GetSchema())) &&
           identical(old_types[-(3:4)], new_types[-(3:4)]) && all(new_types[3:4] == "string"),
         paste(names(new_reader$GetSchema()), new_types, sep = ":", collapse = " "))

  as_plain <- function(df) {
    df <- as.data.frame(df)
    df$chemical <- as.character(df$chemical)
    df$stat <- as.character(df$stat)
    df
  }
  old_m <- as_plain(ParquetFileReader$create(old_monthly_path)$ReadTable())
  new_m <- as_plain(new_reader$ReadTable())
  keys <- c("chemical", "stat", "waterbody_id", "date")
  record("monthly: row counts", nrow(old_m) == nrow(new_m), format(nrow(new_m), big.mark = ","))
  record("monthly: identical() in canonical order", identical(canon_df(old_m, keys), canon_df(new_m, keys)))
  record("monthly: identical() in stored order", identical(old_m, new_m))
  record("monthly: one chemical x stat per row group",
         vapply(seq_len(new_reader$num_row_groups) - 1L, function(j) {
           t <- as_plain(new_reader$ReadRowGroup(j, column_indices = 2:3))
           length(unique(t$chemical)) == 1 && length(unique(t$stat)) == 1
         }, logical(1)),
         sprintf("%d row groups", new_reader$num_row_groups))

  old_labels <- old_monthly %>%
    dplyr::mutate(chemical = as.character(chemical)) %>%
    dplyr::distinct(chemical) %>%
    dplyr::collect() %>%
    dplyr::pull(chemical)
  record("wb_chem_raw_labels: same set of chemicals",
         identical(sort(old_labels, method = "radix"), sort(app_env$wb_chem_raw_labels, method = "radix")))

  # get_wb_chem_grid(): today's rows come back in Arrow scan order with
  # factor levels in scan order too, so compare with both canonicalised -
  # plus the column classes as they are.
  grid_ok <- logical()
  for (wb in grid_waterbodies) {
    for (st in c("mean", "median", "90th")) {
      old_g <- old_monthly %>%
        dplyr::filter(waterbody_id == wb, stat == st) %>%
        dplyr::select(date, chemical, value = conc_ng_L) %>%
        dplyr::collect() %>%
        dplyr::mutate(chem_key = unname(app_env$wb_chem_key_map[as.character(chemical)]))
      new_g <- app_env$get_wb_chem_grid(wb, st)
      canon_g <- function(g) {
        g$chemical <- factor(as.character(g$chemical), levels = sort(levels(g$chemical), method = "radix"))
        g[do.call(order, list(as.character(g$chemical), g$date, method = "radix")), ]
      }
      ok <- identical(lapply(old_g, class), lapply(new_g, class)) &&
        identical(canon_g(old_g), canon_g(new_g)) &&
        identical(levels(new_g$chemical), sort(levels(new_g$chemical), method = "radix"))
      if (!ok) cat("  grid mismatch:", wb, st, "\n")
      grid_ok <- c(grid_ok, ok)
    }
  }
  record("get_wb_chem_grid() old query vs new helper (canonical order, classes, levels)", grid_ok,
         paste(grid_waterbodies, collapse = ","))

  wb_sp_ok <- logical()
  for (k in seq_len(nrow(pairs))) {
    chem_key <- app_env$chem_keys[match(tolower(pairs$chemical[k]), tolower(app_env$chem_keys))]
    raw <- app_env$wb_chem_raw_label(chem_key)
    old_s <- old_monthly %>%
      dplyr::filter(chemical == raw, stat == pairs$stat[k]) %>%
      dplyr::group_by(waterbody_id) %>%
      dplyr::summarise(value = mean(conc_ng_L, na.rm = TRUE), .groups = "drop") %>%
      dplyr::collect() %>%
      as.data.frame()
    new_s <- app_env$get_wb_chem_spatial_summary(chem_key, pairs$stat[k])
    wb_sp_ok <- c(wb_sp_ok, identical(canon_df(old_s, "waterbody_id"), canon_df(new_s, "waterbody_id")))
  }
  record("get_wb_chem_spatial_summary() old query vs new helper", wb_sp_ok)
}

# ---- summary ----------------------------------------------------------------------------

cat("\nresult  cases failed  check\n")
for (k in seq_len(nrow(results))) {
  cat(sprintf("%-6s %6d %6d  %s%s\n", results$result[k], results$cases[k], results$failed[k],
              results$check[k], if (nzchar(results$detail[k])) paste0(" [", results$detail[k], "]") else ""))
}
if (any(results$result == "FAIL")) {
  cat("\nFAIL\n")
  quit(status = 1)
}
cat("\nAll checks passed.\n")
