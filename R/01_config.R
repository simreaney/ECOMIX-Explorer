# ---- Deployment configuration ----
#
# Every file the app reads from data/, data-chem/ and gis-data/ is resolved
# through data_path() against one data root: by default the app directory
# (Shiny makes it the working directory before sourcing R/ and app.R), or
# ECOMIX_DATA_ROOT when the data lives elsewhere, e.g. mounted read-only into
# a container. Normalised once here, so a later setwd() cannot move it.
#
# The ECOMIX_* settings here and in R/03_cache.R and R/04_lookup_cache.R are
# read with env_setting(), which treats a variable that is set but empty
# (`ECOMIX_DATA_ROOT=` in an env file, or a compose file substituting an
# unset host variable) as unset, so it falls back to its default.
env_setting <- function(var, default) {
  value <- Sys.getenv(var)
  if (nzchar(value)) value else default
}
ecomix_data_root <- normalizePath(env_setting("ECOMIX_DATA_ROOT", "."), mustWork = TRUE)
data_path <- function(...) file.path(ecomix_data_root, ...)

# Arrow's thread pools are per process and default to every core the process
# can see for computation (18 on the development machine) and 8 for I/O.
# Inside a container that core count is usually the host's, not the
# container's CPU allowance, and this one process serves every session, so a
# few concurrent scans would oversubscribe it. Both pools are capped instead:
# ECOMIX_ARROW_THREADS and ECOMIX_ARROW_IO_THREADS, 4 each by default.
local({
  thread_count <- function(var, default) {
    n <- suppressWarnings(as.integer(env_setting(var, default)))
    if (is.na(n) || n < 1L) {
      stop(var, " must be a positive whole number, not \"", Sys.getenv(var), "\"", call. = FALSE)
    }
    n
  }
  arrow::set_cpu_count(thread_count("ECOMIX_ARROW_THREADS", "4"))
  arrow::set_io_thread_count(thread_count("ECOMIX_ARROW_IO_THREADS", "4"))
})
