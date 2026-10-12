# Fills the app's startup lookup cache (see R/04_lookup_cache.R) ahead of
# time, e.g. as a step of a Docker image build, so the app's first start
# reads the lookups instead of computing them.
#
#   Rscript scripts/build_app_lookups.R [app_dir]
#
# app_dir defaults to the folder above this script. The script reads the same
# environment variables as the app: ECOMIX_DATA_ROOT (the data, by default the
# app directory) and ECOMIX_LOOKUP_CACHE_DIR (the cache, by default
# data/app_cache under the data root), so set them as the container will. It
# loads R/ exactly as shiny::runApp() does (the app directory as working
# directory, every R/*.R file in name order via shiny::loadSupport()), so each
# lookup is computed and fingerprinted as the app would do it. It stops with
# an error if any lookup could not be saved, e.g. because the cache directory
# is read-only.
#
# A cached lookup is only used while the data files behind it keep the same
# paths (relative to the data root), sizes and modification times. If the
# data reaches the container by another route than the one this script saw
# (copied into a volume, say), copy it with modification times preserved
# (cp -p, rsync -t), or run this script against the data where it is mounted.

args <- commandArgs(trailingOnly = TRUE)
app_dir <- if (length(args) >= 1) {
  args[1]
} else {
  script <- sub("^--file=", "", grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE))
  dirname(dirname(normalizePath(script)))
}
app_dir <- normalizePath(app_dir, mustWork = TRUE)

# Never let app code start the app.
Sys.setenv(ECOMIX_AUTORUN = "0")
setwd(app_dir)
app_env <- new.env(parent = globalenv())
shiny::loadSupport(app_dir, renv = app_env, globalrenv = NULL)

status <- unlist(as.list(app_env$lookup_cache_log))
status <- status[order(names(status))]
if (length(status) == 0) stop("No lookups went through cached_lookup() - is ", app_dir, " the app directory?")
cat("Lookup cache:", app_env$lookup_cache_dir, "\n")
cat(sprintf("  %-24s %s\n", names(status), status), sep = "")
unsaved <- names(status)[!status %in% c("loaded", "saved")]
if (length(unsaved) > 0) {
  stop("Could not write ", paste(unsaved, collapse = ", "), " to ", app_env$lookup_cache_dir,
       " - set ECOMIX_LOOKUP_CACHE_DIR to a writable directory.", call. = FALSE)
}
