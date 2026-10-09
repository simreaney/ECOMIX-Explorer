# Characterisation tests for the download handlers, complementing the
# Download tests in test-server.R: every kind of tabular variable (HYPE
# water quantity / quality, climate forcing, subbasin chemical, water body
# chemical) per subbasin and per format, the multi-site and chemical-only
# subbasins, the Hydro Explorer "Download this selection" in each mode, and
# the spatial layer x format combinations not covered there. These pin the
# behaviour the planned download pushdown (filtering in Arrow instead of
# in R) must keep: same rows, columns, types and file names.
#
# Files are read back and hashed (see describe_download() in
# helper-server.R); rows coming from Arrow scans are canonicalised
# (`ordered = FALSE`) because today's order is not defined.
#
# Tests marked "BUG (fixed in Phase 3): expected to change" record current,
# known-wrong behaviour on purpose.

testthat::local_edition(3)

test_that("tabular downloads: every variable kind and format, and 'Download this selection'", {
  run_server({
    start_session(session)
    skip_messages(session)
    send(session, subbasin_search = "3310")

    # Climate forcing (the other forcing variable, precip, is in test-server.R).
    send(session, dl_variable = "temp")
    snap_outputs(session, "3310 temp", download_ids)
    snap_download(session, "download_data", "3310 temp csv", ordered = FALSE)

    # HYPE water quality, all three formats (parquet keeps column types).
    send(session, dl_variable = "SS")
    snap_rx(session, "downloader_tabular_data", ordered = FALSE)
    snap_outputs(session, "3310 SS", download_ids)
    snap_download(session, "download_data", "3310 SS csv", ordered = FALSE)
    send(session, dl_format = "xlsx")
    snap_download(session, "download_data", "3310 SS xlsx", ordered = FALSE)
    send(session, dl_format = "parquet")
    snap_download(session, "download_data", "3310 SS parquet", ordered = FALSE)
    send(session, dl_variable = "discharge")
    snap_download(session, "download_data", "3310 discharge parquet", ordered = FALSE)
    send(session, dl_format = "xlsx")
    snap_download(session, "download_data", "3310 discharge xlsx", ordered = FALSE)

    # Subbasin chemicals at a multi-site subbasin (one row per SITE_ID and
    # day), all three formats.
    send(session, subbasin_search = "2592", dl_variable = "chem_Zn", dl_chem_stat = "mean", dl_format = "csv")
    snap_rx(session, "downloader_tabular_data", ordered = FALSE)
    snap_outputs(session, "2592 chem_Zn mean", download_ids)
    snap_download(session, "download_data", "2592 chem_Zn mean csv", ordered = FALSE)
    send(session, dl_format = "xlsx")
    snap_download(session, "download_data", "2592 chem_Zn mean xlsx", ordered = FALSE)
    send(session, dl_format = "parquet")
    snap_download(session, "download_data", "2592 chem_Zn mean parquet", ordered = FALSE)

    # Three SITE_IDs.
    send(session, subbasin_search = "3793", dl_variable = "chem_fipronil", dl_chem_stat = "median", dl_format = "csv")
    snap_outputs(session, "3793 chem_fipronil median", download_ids)
    snap_download(session, "download_data", "3793 chem_fipronil median csv", ordered = FALSE)

    # A chemical-only subbasin (outside the HYPE model): chemicals and
    # climate forcing, but no HYPE rows.
    send(session, subbasin_search = "765")
    snap_outputs(session, "765 chem_fipronil median", download_ids)
    snap_download(session, "download_data", "765 chem_fipronil median csv", ordered = FALSE)
    send(session, dl_variable = "precip")
    snap_outputs(session, "765 precip", download_ids)
    snap_download(session, "download_data", "765 precip csv", ordered = FALSE)
    send(session, dl_variable = "SS")
    snap_download(session, "download_data", "765 SS csv (no rows)")

    # Water body chemicals (all water bodies), the formats not in test-server.R.
    send(session, dl_variable = "wbchem_cypermethrin", dl_chem_stat = "mean", dl_format = "xlsx")
    snap_outputs(session, "wbchem_cypermethrin mean", download_ids)
    snap_download(session, "download_data", "wbchem_cypermethrin mean xlsx", ordered = FALSE)
    send(session, dl_format = "parquet")
    snap_download(session, "download_data", "wbchem_cypermethrin mean parquet", ordered = FALSE)

    # Hydro Explorer "Download this selection". At the chemical-only
    # subbasin (still 765): chemicals download, HYPE variables are empty.
    send(session, prediction_variable = "chem_fipronil")
    send(session, prediction_percentile = c("mean", "90th"))
    snap_download(session, "download_selection", "765 fipronil mean/90th")
    send(session, prediction_variable = "discharge")
    snap_download(session, "download_selection", "765 discharge (no HYPE rows)")

    # Three SITE_IDs, a metal.
    send(session, subbasin_search = "3793", prediction_variable = "chem_Zn")
    snap_download(session, "download_selection", "3793 Zn median")

    # Water body mode, another water body than test-server.R.
    # BUG (fixed in Phase 3): expected to change - named after rv() (3793).
    send(session, de_entity_type = "waterbody")
    send(session, de_waterbody_search = "GB104027069400")
    send(session, prediction_variable = "chem_fipronil")
    send(session, prediction_percentile = c("median", "90th"))
    snap_download(session, "download_selection", "BUG water body GB104027069400 fipronil median/90th (named with rv())")
  })
})

test_that("spatial downloads: layer x format combinations", {
  run_server({
    start_session(session)
    skip_messages(session)
    send(session, subbasin_search = "3310", dl_data_type = "spatial")

    # Subbasins layer for a HYPE variable (all-subbasin summary).
    send(session, dl_variable = "discharge")
    snap_rx(session, "downloader_spatial_data")
    snap_outputs(session, "subbasins layer, discharge", download_ids)
    snap_download(session, "download_data", "subbasins discharge gpkg")

    # Catchment layer: HYPE and chemical summaries, every format.
    send(session, dl_spatial_layer = "catchment")
    snap_rx(session, "downloader_spatial_data")
    snap_download(session, "download_data", "catchment discharge gpkg")
    send(session, dl_variable = "chem_fipronil")
    snap_rx(session, "downloader_spatial_data")
    snap_outputs(session, "catchment layer, chem_fipronil median", download_ids)
    snap_download(session, "download_data", "catchment chem_fipronil gpkg")
    send(session, dl_format = "shp")
    snap_download(session, "download_data", "catchment chem_fipronil shapefile zip")
    # BUG: GeoParquet fails on this machine's GDAL (no Parquet driver).
    send(session, dl_format = "geoparquet")
    snap_download(session, "download_data", "catchment chem_fipronil geoparquet")

    # Water bodies layer, GeoParquet.
    send(session, dl_variable = "wbchem_Zn", dl_chem_stat = "90th")
    expect_identical(input$dl_spatial_layer, "waterbodies")
    snap_outputs(session, "waterbodies layer, wbchem_Zn 90th", download_ids)
    snap_download(session, "download_data", "waterbodies wbchem_Zn geoparquet")

    # Mismatch the other way round: a water body variable on the subbasins
    # layer -> empty layer -> download error.
    send(session, dl_spatial_layer = "subbasins", dl_format = "gpkg")
    snap_outputs(session, "subbasins layer with a water body variable (mismatch)", download_ids)
    snap_download(session, "download_data", "subbasins wbchem_Zn gpkg (mismatch)")
  })
})
