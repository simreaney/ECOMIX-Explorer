## Join the per-site fipronil P90 summary stats to the subbasin polygons
## and write the result out as a shapefile.
##
## site_id -> subbasin comes from Subbasin_Siteids_Key.csv (also already
## carried in the summary table). subbasin matches the shapefile's
## `gridcode` field (== `Id`) 1:1.

suppressMessages({
  library(sf)
  library(arrow)
  library(dplyr)
})

summary_path <- "6_fipronil_site_p90_summary.parquet"
subbasins_path <- "../../gis-data/subbasins_bng.shp"
output_path <- "6_fipronil_site_p90_by_subbasin.shp"

site_summary <- read_parquet(summary_path)
subbasins <- st_read(subbasins_path, quiet = TRUE)

joined <- subbasins %>%
  inner_join(site_summary, by = c("gridcode" = "subbasin"))

cat(sprintf(
  "%d of %d subbasin polygons matched to a site summary\n",
  nrow(joined), nrow(subbasins)
))

st_write(joined, output_path, delete_layer = TRUE, quiet = TRUE)

cat(sprintf("Wrote %s\n", output_path))
