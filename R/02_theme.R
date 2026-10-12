### 0. Design system (Modernist)
#
# Every colour, type and rule below comes from the Modernist token set; the
# matching CSS lives in styles.css and the two must be kept in step.

ec <- list(
  ground      = "#f3f2f2",
  surface     = "#eae9e9",
  ink         = "#201e1d",
  ink_muted   = "#444141",
  ink_dim     = "#605d5d",
  rule        = "#201e1d",
  rule_light  = "#d7d3d3",
  accent      = "#ec3013",
  accent_text = "#ae1800",
  accent_tint = "#ffe0d9",
  accent_ink  = "#7c1405"
)

# Discrete series palette. The system is mono - accent first for the series
# that carries the message, then ink and its neutral steps. Never more than
# one saturated line at a time.
ec_series <- c(ec$accent, ec$ink, ec$ink_dim, ec$accent_text, "#9b9797", "#b9b5b5")

scale_color_ecomix <- function(...) ggplot2::scale_color_manual(values = ec_series, ...)
scale_fill_ecomix  <- function(...) ggplot2::scale_fill_manual(values = ec_series, ...)

# ggplot theme matched to the interface: flat, flush left, one strong baseline
# rule, light grid, no panel border, legend top-left.
theme_ecomix <- function(base_size = 12) {
  ggplot2::theme_minimal(base_family = "Archivo", base_size = base_size) +
    ggplot2::theme(
      text             = ggplot2::element_text(colour = ec$ink),
      plot.title       = ggplot2::element_text(face = "bold", size = base_size, hjust = 0,
                                               margin = ggplot2::margin(b = 6)),
      plot.title.position = "plot",
      plot.caption     = ggplot2::element_text(colour = ec$ink_dim, size = base_size - 3, hjust = 0),
      plot.caption.position = "plot",
      panel.border     = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(colour = ec$rule_light, linewidth = 0.3),
      axis.line.x      = ggplot2::element_line(colour = ec$ink, linewidth = 0.7),
      axis.ticks       = ggplot2::element_blank(),
      axis.text        = ggplot2::element_text(colour = ec$ink_dim, size = base_size - 2),
      axis.title       = ggplot2::element_text(colour = ec$ink_dim, size = base_size - 2),
      legend.position  = "top",
      legend.justification = "left",
      legend.title     = ggplot2::element_blank(),
      legend.key.height = grid::unit(10, "pt"),
      legend.margin    = ggplot2::margin(0, 0, 2, 0),
      strip.text       = ggplot2::element_text(face = "bold", hjust = 0, colour = ec$ink),
      plot.background  = ggplot2::element_rect(fill = ec$ground, colour = NA),
      panel.background = ggplot2::element_rect(fill = ec$ground, colour = NA),
      plot.margin      = ggplot2::margin(4, 8, 4, 2)
    )
}

# Placeholder shown instead of a blank/empty chart whenever the selected
# subbasin has no rows for the current dataset - e.g. a subbasin that's in
# the chemical predictions but outside the 3126 subbasins the HYPE model
# itself covers (or vice versa). Without this, an empty result set renders
# as an unlabelled blank plot, which reads as a bug ("it flashed and
# vanished") rather than the data-coverage gap it actually is.
no_data_plot <- function(msg) {
  ggplot2::ggplot() +
    ggplot2::annotate("text", x = 0, y = 0, label = msg, size = 4.2, colour = ec$ink_dim) +
    ggplot2::theme_void() +
    ggplot2::theme(plot.background = ggplot2::element_rect(fill = ec$ground, colour = NA))
}

# Basemap tile choice shared by every full-size leaflet map (Map,
# Spatial Datasets). Works on both a fresh `leaflet()` widget and a
# `leafletProxy()`, since both dispatch through the same addTiles/
# addProviderTiles S3 methods - callers just pair it with `clearGroup
# ("basetile")` first when switching on an existing map.
add_basemap_tiles <- function(map, choice = "osm") {
  switch(choice,
    esri_satellite = addProviderTiles(map, leaflet::providers$Esri.WorldImagery, group = "basetile"),
    esri_gray      = addProviderTiles(map, leaflet::providers$Esri.WorldGrayCanvas, group = "basetile"),
    addTiles(map, group = "basetile")
  )
}
