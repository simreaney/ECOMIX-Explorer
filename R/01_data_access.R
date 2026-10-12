# Arrow dataset helpers shared by the data loaders in the files below.
safe_open_dataset <- function(dataset_path) {
  open_dataset(
    dataset_path,
    factory_options = list(
      exclude_invalid_files = TRUE,
      selector_ignore_prefixes = c(".", "_")
    )
  )
}

# Some parquet columns in DB_Proj_Forcing (e.g. p10/p50/p90) were written with
# a stray R "names" attribute (leftover from quantile() output), which Arrow
# can't round-trip and reports as "Invalid metadata$r" on every collect(). The
# values themselves are unaffected, so muffle just this known warning here
# rather than at every call site, while letting any other warning through.
collect_quiet <- function(x) {
  withCallingHandlers(
    collect(x),
    warning = function(w) {
      if (grepl("Invalid metadata\\$r", conditionMessage(w))) {
        invokeRestart("muffleWarning")
      }
    }
  )
}

# An Arrow query narrowed to one selected subbasin (unchanged when
# `subbasin_id` is NULL), so only that subbasin's row groups are read. The
# selection is normally numeric, but a click on any map polygon sets it, so a
# water body click leaves an EA code there (see test-server.R). Arrow refuses
# to compare the integer subbasin column with a string, whereas R's `==`,
# which the downloads used to filter with after reading every subbasin,
# compares as text and finds no row - so text is matched the way R would
# match it: "353" finds subbasin 353, anything else finds nothing.
filter_subbasin <- function(q, subbasin_id) {
  if (is.null(subbasin_id)) return(q)
  if (is.character(subbasin_id)) {
    n <- suppressWarnings(as.integer(subbasin_id))
    subbasin_id <- if (length(n) == 1 && !is.na(n) && identical(as.character(n), subbasin_id)) n else NA_integer_
  }
  dplyr::filter(q, subbasin == subbasin_id)
}
