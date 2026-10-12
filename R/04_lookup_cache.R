# ---- Startup lookup cache ----
#
# Several startup lookups (the subbasins each dataset covers, the scenario /
# period combinations, the SITE_ID -> water body join, ...) depend only on
# data files, yet each one runs an Arrow scan or a spatial join (about 1 s
# together) every time the app starts. cached_lookup() keeps each value in
# <cache dir>/<name>.rds with a fingerprint of what it was computed from:
#   - the path, size and modification time of every file under `sources`
#     (paths relative to the data root, so the same data mounted elsewhere
#     still matches; dot files are left out, as Arrow ignores them)
#   - the deparsed body of `compute` (comments and layout don't count)
#   - LOOKUP_CACHE_VERSION
#   - the R, arrow, dplyr and sf (GEOS) versions that do the computing.
# A start with a matching fingerprint reads the file. Any other start
# computes the value live, exactly as before, and rewrites the file, so a
# data update, an edited lookup or a package upgrade refreshes the cache
# without anyone having to clear it.
#
# The fingerprint cannot see code outside `compute` that shapes its inputs
# (e.g. how subbasin_shp is read, for chem_site_wb). Bump
# LOOKUP_CACHE_VERSION after changing such code.
#
# The cache lives in ECOMIX_LOOKUP_CACHE_DIR, by default data/app_cache under
# the data root. When it can't be written (e.g. a read-only data volume in
# the container) the lookups are simply computed at every start. To use the
# cache there, point ECOMIX_LOOKUP_CACHE_DIR at a writable directory or one
# baked into the image, and fill it with scripts/build_app_lookups.R.
LOOKUP_CACHE_VERSION <- 1L

lookup_cache_dir <- env_setting("ECOMIX_LOOKUP_CACHE_DIR", data_path("data", "app_cache"))

# What each cached_lookup() did at this start ("loaded", "saved" or
# "computed, not saved"), for scripts/build_app_lookups.R.
lookup_cache_log <- new.env(parent = emptyenv())

lookup_fingerprint <- function(sources, compute) {
  root <- paste0(ecomix_data_root, "/")
  files <- lapply(sources, function(src) {
    rel <- if (dir.exists(src)) sort(list.files(src, recursive = TRUE), method = "radix") else ""
    info <- file.info(if (dir.exists(src)) file.path(src, rel) else src, extra_cols = FALSE)
    list(source = if (startsWith(src, root)) substring(src, nchar(root) + 1L) else src,
         path = rel, size = info$size, mtime = as.numeric(info$mtime))
  })
  rlang::hash(list(
    version = LOOKUP_CACHE_VERSION,
    code = deparse(body(compute)),
    software = c(R.version.string,
                 vapply(c("arrow", "dplyr", "sf"), function(p) as.character(getNamespaceVersion(p)), ""),
                 sf::sf_extSoftVersion()[["GEOS"]]),
    files = files
  ))
}

cached_lookup <- function(name, sources, compute) {
  fingerprint <- lookup_fingerprint(sources, compute)
  file <- file.path(lookup_cache_dir, paste0(name, ".rds"))
  if (file.exists(file)) {
    cached <- tryCatch(readRDS(file), error = function(e) NULL, warning = function(w) NULL)
    if (is.list(cached) && identical(cached$fingerprint, fingerprint)) {
      lookup_cache_log[[name]] <- "loaded"
      return(cached$value)
    }
  }
  value <- compute()
  # Written to a temporary file and renamed into place, so another process
  # starting at the same moment never reads a half-written file.
  tmp <- tempfile(paste0(name, ".rds."), tmpdir = lookup_cache_dir)
  saved <- tryCatch({
    dir.create(lookup_cache_dir, recursive = TRUE, showWarnings = FALSE)
    saveRDS(list(fingerprint = fingerprint, value = value), tmp)
    file.rename(tmp, file)
  }, error = function(e) FALSE, warning = function(w) FALSE)
  if (!isTRUE(saved)) unlink(tmp)
  lookup_cache_log[[name]] <- if (isTRUE(saved)) "saved" else "computed, not saved"
  value
}
