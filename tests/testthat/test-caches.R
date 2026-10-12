# Cache behaviour of the global data helpers, for the planned bounded
# caches: a cold call (cache miss), a warm call (hit) and a call after the
# cache was emptied must all return the same data, and interleaving many
# keys (the pattern that makes a bounded cache evict) must never hand back
# another key's entry. Values themselves are pinned in test-data-helpers.R;
# this file only compares calls with each other (plus a snapshot of the
# keys not covered there).
#
# Runs first (alphabetically) and empties every cache it can find, so it
# also checks that nothing depends on the caches having been filled in a
# particular order.

testthat::local_edition(3)

# Empty every cache object in globalenv(): objects named *_cache that are
# plain environments (today's caches), cachem-style objects (with a
# $reset() method) or anything else exposing reset()/clear(). Returns the
# names reset. If the bounded caches end up somewhere this cannot see,
# the test below fails - extend this function rather than dropping it.
ecomix_reset_caches <- function(envir = globalenv()) {
  done <- character()
  for (nm in ls(envir, pattern = "_cache$")) {
    obj <- get(nm, envir = envir)
    if (is.environment(obj) && !is.function(obj$reset) && !is.function(obj$clear)) {
      rm(list = ls(obj, all.names = TRUE), envir = obj)
    } else if (!is.null(obj) && is.function(obj$reset)) {
      obj$reset()
    } else if (!is.null(obj) && is.function(obj$clear)) {
      obj$clear()
    } else {
      next
    }
    done <- c(done, nm)
  }
  done
}

# Unordered results (Arrow scans) are compared in canonical row order.
hash_of <- function(x) canon_hash(canon_rows(x))

test_that("cached helpers: cold, warm and re-filled results are identical", {
  calls <- list(
    chem_series_multi_site = function() get_chem_subbasin_series("fipronil", "90th", 3793),
    chem_series_single_site = function() get_chem_subbasin_series("imidacloprid", "mean", 353),
    chem_all_subbasins = function() get_chem_all_subbasins_daily("imidacloprid", "mean"),
    chem_spatial = function() get_chem_spatial_summary("imidacloprid", "mean"),
    d3d_series = function() get_chem_d3d_series("imidacloprid", "356"),
    src_split = function() get_chem_src_split("imidacloprid", "356"),
    src_split_day = function() get_chem_src_split("imidacloprid", "356", as.Date("2019-05-01")),
    wb_series = function() get_wb_chem_series("Zn", "mean", "GB104027069400"),
    wb_grid = function() get_wb_chem_grid("GB104027069400", "90th"),
    wb_all = function() get_wb_chem_all_waterbodies_monthly("Zn", "mean"),
    wb_spatial = function() get_wb_chem_spatial_summary("Zn", "mean"),
    tabular = function() get_subbasin_tabular_download("SS", 3310),
    full_res = function() sf::st_drop_geometry(full_res_layer("catchments"))
  )
  reset <- ecomix_reset_caches()
  expect_true(all(c("app_cache", "full_res_cache") %in% reset),
              info = paste("caches reset:", paste(reset, collapse = ", ")))

  cold <- lapply(calls, function(f) hash_of(f()))
  warm <- lapply(calls, function(f) hash_of(f()))
  expect_identical(warm, cold)
  ecomix_reset_caches()
  refilled <- lapply(rev(calls), function(f) hash_of(f()))
  expect_identical(refilled[names(cold)], cold)
  snap_obj(cold, "cache-test helper results (hashes)")
})

test_that("cached helpers: interleaved keys never return another key's entry", {
  keys <- expand.grid(chem = c("fipronil", "Zn"), stat = c("mean", "90th"),
                      subbasin = c(765, 2592, 3793, 353, 3310), stringsAsFactors = FALSE)
  key_names <- sprintf("%s|%s|%s", keys$chem, keys$stat, keys$subbasin)
  ecomix_reset_caches()
  series <- function(i) get_chem_subbasin_series(keys$chem[i], keys$stat[i], keys$subbasin[i])
  first <- setNames(lapply(seq_len(nrow(keys)), function(i) canon_hash(series(i))), key_names)
  # Every key differs from every other (so a mix-up could not go unseen).
  expect_identical(anyDuplicated(unlist(first)), 0L)
  again <- setNames(lapply(rev(seq_len(nrow(keys))), function(i) canon_hash(series(i))), rev(key_names))
  expect_identical(again[key_names], first)
  # Interleave with the other cached helpers, then ask again.
  for (st in c("mean", "median", "90th")) {
    get_wb_chem_grid("GB104027057290", st)
    get_chem_spatial_summary("Zn", st)
  }
  third <- setNames(lapply(seq_len(nrow(keys)), function(i) canon_hash(series(i))), key_names)
  expect_identical(third, first)
  snap_obj(first, "get_chem_subbasin_series() hashes, fipronil/Zn x mean/90th x 5 subbasins")
})
