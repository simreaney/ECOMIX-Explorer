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
