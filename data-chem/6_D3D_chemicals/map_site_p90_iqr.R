## Map the per-site fipronil P90 summary as centroid points over an Esri
## Dark Gray Canvas basemap:
##   - point colour = median daily P90 (log10 scale, diverging blue-red ramp)
##   - point size   = inverse of IQR (low IQR -> large point)
## Saved as a 300 dpi PNG.

suppressMessages({
  library(sf)
  library(ggplot2)
  library(ggspatial)
  library(rosm)
  library(dplyr)
  library(scales)
})

input_path <- "6_fipronil_site_p90_by_subbasin.shp"
output_path <- "6_fipronil_site_p90_iqr_map.png"

subbasins <- st_read(input_path, quiet = TRUE)

## Centroid of each subbasin polygon, reprojected to Web Mercator to match
## the basemap tiles.
centroids <- subbasins %>%
  st_centroid() %>%
  st_transform(3857) %>%
  mutate(
    log_median_p90 = log10(median_p90),
    log_iqr_p90 = log10(iqr_p90)
  )

## Esri "World Dark Gray Base" basemap, registered as a custom XYZ tile
## source (tiles are served as JPEG despite the extension-less URL).
esri_dark_gray_src <- rosm:::source_from_url_format(
  paste0(
    "https://services.arcgisonline.com/ArcGIS/rest/services/",
    "Canvas/World_Dark_Gray_Base/MapServer/tile/${z}/${y}/${x}"
  ),
  extension = "jpg",
  max_zoom = 16
)
register_tile_source(esridarkgray = esri_dark_gray_src)

## Diverging blue-red ramp (project data-viz palette's diverging pair,
## dark-surface steps for legibility against the dark basemap), centred on
## the across-site median so colour reads as "above/below typical site".
midpoint <- median(centroids$log_median_p90)

sci_label <- function(x) scales::label_scientific(digits = 1)(10^x)

p <- ggplot() +
  annotation_map_tile(type = "esridarkgray", zoomin = -1, progress = "none") +
  geom_sf(
    data = centroids,
    aes(size = log_iqr_p90, color = log_median_p90),
    alpha = 0.9,
    stroke = 1.4,
    shape = 21,
    fill = NA
  ) +
  scale_color_gradient2(
    low = "#3987e5",
    mid = "#c3c2b7",
    high = "#e66767",
    midpoint = midpoint,
    name = "Median daily\nP90 fipronil\n(ng/L)",
    labels = sci_label
  ) +
  scale_size_continuous(
    trans = "reverse",
    range = c(1.5, 8),
    name = "IQR of daily\nP90 fipronil\n(ng/L)",
    labels = sci_label,
    breaks = pretty(centroids$log_iqr_p90, n = 5)
  ) +
  annotation_scale(location = "bl", width_hint = 0.25) +
  annotation_north_arrow(
    location = "tr",
    which_north = "true",
    style = north_arrow_fancy_orienteering()
  ) +
  coord_sf(crs = 3857) +
  labs(
    title = "Fipronil: site-level median daily P90 and IQR",
    subtitle = "Point colour = median of daily P90; point size = inverse IQR (larger = more consistent)"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(face = "bold", size = 14, margin = margin(b = 4)),
    plot.subtitle = element_text(size = 10, color = "#52514e", margin = margin(b = 8)),
    legend.position = "right",
    legend.title = element_text(size = 9, face = "bold"),
    legend.text = element_text(size = 8),
    plot.margin = margin(10, 10, 10, 10)
  )

ggsave(output_path, plot = p, width = 10, height = 9, dpi = 300, bg = "white")

cat(sprintf("Wrote %s\n", output_path))
