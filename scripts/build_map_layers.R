# One-off data-prep script (not run as part of the Shiny app itself).
#
# Builds the lightweight copies of the GIS layers that app.R's Leaflet maps
# draw. The source layers carry far more detail than a web map can show
# (e.g. ~974,000 vertices for the water bodies, a 36 MB JSON payload when
# that map layer is switched on), and the GeoJSON one also has to be parsed
# and reprojected at every app start. Each layer here is simplified in
# British National Grid where that pays off, has its coordinates rounded to
# 5 decimal places (~1 m - Leaflet otherwise ships ~15 significant digits
# per coordinate, half of every map payload), and is stored pre-projected to
# WGS84 as a GeoPackage under gis-data/app/.
#
# The full-resolution source layers are left untouched - app.R still uses
# them for spatial downloads, so exported geometry is never simplified.
#
# Re-run whenever one of the source layers below changes:
#   Rscript scripts/build_map_layers.R

library(sf)
library(dplyr)
library(here)

out_dir <- here("gis-data", "app")
dir.create(out_dir, showWarnings = FALSE)

# Simplification tolerance in metres (0 = keep the shape exactly). At zoom
# 14 one screen pixel is ~5.6 m at this latitude. Subbasins are left exact:
# their vertices sit 10-15 m off a straight line, so nothing is removed below
# 15 m and anything above visibly shifts these ~0.5 km2 polygons.
layers <- list(
  subbasins = list(
    read = function() read_sf(here("gis-data"), layer = "subbasins_wgs"),
    tolerance = 0
  ),
  waterbodies = list(
    read = function() read_sf(here("gis-data", "EA_Waterbodies_AOI.geojson")),
    tolerance = 10
  ),
  operational_catchments = list(
    read = function() read_sf(here("gis-data"), layer = "operational_catchments_wgs"),
    tolerance = 10
  )
)

n_vertices <- function(x) length(unlist(st_geometry(x))) / 2

for (name in names(layers)) {
  src <- layers[[name]]$read()
  bng <- st_transform(src, 27700)
  invalid <- !st_is_valid(bng)
  if (any(invalid)) bng[invalid, ] <- st_make_valid(bng[invalid, ])
  # Repairs can return geometry collections; keep the polygon parts, one type.
  bng <- st_cast(st_collection_extract(bng, "POLYGON"), "MULTIPOLYGON")

  # preserveTopology keeps every polygon (no small one collapses away).
  simple <- bng
  if (layers[[name]]$tolerance > 0) {
    simple <- st_simplify(simple, preserveTopology = TRUE, dTolerance = layers[[name]]$tolerance)
  }
  simple <- st_transform(simple, 4326)
  st_geometry(simple) <- st_as_sfc(st_as_binary(st_geometry(simple), precision = 1e5), crs = 4326)
  st_geometry(simple) <- "geometry"
  stopifnot(nrow(simple) == nrow(src), !any(st_is_empty(simple)))

  out_file <- file.path(out_dir, paste0(name, ".gpkg"))
  st_write(simple, out_file, delete_dsn = TRUE, quiet = TRUE)
  cat(sprintf("%-23s %6d features  %9s -> %8s vertices (%d repaired)  %5.1f MB\n",
              name, nrow(simple),
              format(n_vertices(src), big.mark = ","), format(n_vertices(simple), big.mark = ","),
              sum(invalid), file.size(out_file) / 1e6))
}
