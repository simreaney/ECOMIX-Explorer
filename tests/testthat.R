# Characterisation test suite for ECOMIX Explorer (app.R).
#
# Run from the repo root, either of:
#   Rscript tests/testthat.R
#   Rscript -e 'testthat::test_dir("tests/testthat")'
# or interactively: testthat::test_dir("tests/testthat")
# A single file: testthat::test_file("tests/testthat/test-server.R")
#
# Nothing is launched: tests/testthat/helper-load.R sets ECOMIX_AUTORUN=0 and
# skips app.R's shinyApp()/runApp() expressions, then loads the app's global
# scope once into globalenv(); the server is exercised with shiny::testServer().
#
# Files (tests/testthat/):
#   helper-load.R      app loader, canon_hash(), canon_rows(), snap_obj()
#   helper-server.R    recording MockShinySession, browser emulation, output /
#                      message / download descriptors, snap_state(), the
#                      leaflet.js map model (lf_state(), snap_lf())
#   test-caches.R      cold / warm / re-filled cache equality
#   test-data-helpers.R  startup lookups and global data helpers
#   test-downloads.R   tabular + spatial downloads, every kind and format
#   test-maps.R        maps as drawn on screen (layer toggles, mini-maps,
#                      Spatial Datasets map)
#   test-navigation.R  popup / context links, tab carry-over, selection state
#   test-plots.R       single-input changes and round trips (plot caching),
#                      Chemical Explorer hover marker
#   test-server.R      the server tab by tab
#   test-ui.R          rendered UI HTML (random bslib ids normalised)
#
# The first run writes golden snapshots to tests/testthat/_snaps/; later runs
# compare against them. After an intended change, review with
#   testthat::snapshot_review(path = "tests/testthat")
# and accept with
#   testthat::snapshot_accept(path = "tests/testthat")

library(testthat)

Sys.setenv(ECOMIX_AUTORUN = "0", NOT_CRAN = "true")

script_dir <- local({
  file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(file_arg) == 1) dirname(normalizePath(sub("^--file=", "", file_arg))) else "tests"
})

test_dir(file.path(script_dir, "testthat"), stop_on_failure = TRUE)
