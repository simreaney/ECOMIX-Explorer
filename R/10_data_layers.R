## Load required datasets

# Map layers are the lightweight display copies built by
# scripts/build_map_layers.R (pre-projected to WGS84, coordinates rounded to
# ~1 m, the water bodies and operational catchments simplified), which keeps
# app start and every Leaflet payload small. Spatial downloads use the
# full-resolution source layers instead, loaded on first use - see
# full_res_layer() below.

# HYPE Subbasins (modelling units)
subbasin_shp <- read_sf(data_path("gis-data", "app", "subbasins.gpkg"))

# Full-resolution source layers for spatial downloads, read once on demand
# (the water body GeoJSON alone takes ~1 s to parse) and cached outside the
# bounded app_cache (see R/03_cache.R). Each carries the same attributes as
# its display copy.
full_res_cache <- new.env(parent = emptyenv())
full_res_layer <- function(name) {
  if (is.null(full_res_cache[[name]])) {
    full_res_cache[[name]] <- switch(
      name,
      subbasins = read_sf(dsn = data_path("gis-data"), layer = "subbasins_wgs"),
      catchments = read_sf(dsn = data_path("gis-data"), layer = "catchments_wgs"),
      waterbodies = read_sf(data_path("gis-data", "EA_Waterbodies_AOI.geojson")) %>%
        st_transform(4326) %>%
        dplyr::select(water_body) %>%
        left_join(sf::st_drop_geometry(waterbody_opcat_shp), by = "water_body")
    )
  }
  full_res_cache[[name]]
}

# WFD surface water operational catchments (Environment Agency WFS),
# pre-filtered to the ones overlapping the study catchments, and each
# subbasin's dominant associated operational catchment (by shared area) -
# both produced by scripts/fetch_operational_catchments.R rather than
# fetched live, so the map doesn't depend on the WFS being reachable at app
# start.
opcat_shp <- read_sf(data_path("gis-data", "app", "operational_catchments.gpkg"))

df_subbasin_opcat <- read.csv(data_path("data/subbasin_operational_catchment.csv"))

# Subbasin polygons joined to their associated operational catchment, used to
# colour the Map page.
subbasin_opcat_shp <- subbasin_shp %>%
  left_join(df_subbasin_opcat, by = c("Id" = "subbasin"))

# Subbasins with HYPE hydrology output - the outlet subbasin (ds_sbbs in
# gis-data/riversegments_wgs) of each modelled river segment, 3126 of the
# 16,515 - which is what the Map page's Subbasins layer draws. Every HYPE
# subbasin appears under each variable/scenario, so one partition suffices.
hype_subbasin_ids <- cached_lookup("hype_subbasin_ids", data_path("data/DB_Proj_Month"), function() {
  safe_open_dataset(data_path("data/DB_Proj_Month")) %>%
    filter(variable == "discharge", ssp == "Baseline") %>%
    distinct(subbasin) %>%
    collect() %>%
    pull(subbasin) %>%
    sort()
})

hype_subbasin_shp <- subbasin_opcat_shp %>% filter(Id %in% hype_subbasin_ids)

# Version of the data the Map page's base layer is drawn from - the size and
# modification time of the files it is read from, and which subbasins it
# draws - used as its cache key (bindCache() in the server), so the layer is
# built once and then served to every session from Shiny's app cache.
basemap_data_version <- rlang::hash(list(
  lapply(c(data_path("gis-data", "app", "subbasins.gpkg"),
           data_path("data", "subbasin_operational_catchment.csv")), function(f) {
    info <- file.info(f, extra_cols = FALSE)
    c(info$size, as.numeric(info$mtime))
  }),
  hype_subbasin_ids
))

opcat_levels <- sort(unique(na.omit(subbasin_opcat_shp$opcat_name)))

# Operational catchments are coloured by river system, so the map reads as
# "which river am I looking at" rather than 50 unrelated hues. Rivers with
# several operational catchments along their length (source to mouth, listed
# upstream to downstream) share one hue and are told apart by shade; every
# other catchment gets its own hue. Matching is done against the exact
# catchment names rather than a substring, since a naive substring match
# ("Calder" in "Middle Ribble - Settle to Calder") would wrongly pull in
# catchments from a different river.
opcat_river_groups <- list(
  "Ure"     = c("Upper Ure", "Middle and Lower Ure"),
  "Swale"   = c("Upper Swale", "Middle Swale", "Lower Swale"),
  "Derwent" = c("Derwent Upper -  Derbyshire", "Derwent Middle - Derbyshire",
                "Upper Derwent Yorkshire", "Middle Derwent Yorkshire", "Lower Derwent Yorkshire"),
  "Wharfe"  = c("Upper Wharfe", "Middle Wharfe and Washburn", "Lower Wharfe"),
  "Aire"    = c("Upper Aire", "Middle Aire", "Lower Aire"),
  "Nidd"    = c("Upper Nidd", "Middle and Lower Nidd"),
  "Ouse"    = c("Upper Ouse Yorkshire", "Lower Ouse Yorkshire"),
  "Don"     = c("Upper Don", "Middle Don", "Lower Don"),
  "Calder"  = c("Upper Calder", "Middle Calder", "Lower Calder", "Calder")
)

# Any catchment actually present that isn't in a hardcoded group above still
# needs a colour, so it becomes its own single-catchment group. This also
# covers catchments in the data that this list doesn't yet know about.
opcat_grouped_names <- unlist(opcat_river_groups, use.names = FALSE)
opcat_other <- setdiff(opcat_levels, opcat_grouped_names)
opcat_groups <- c(opcat_river_groups, setNames(as.list(opcat_other), opcat_other))
opcat_groups <- opcat_groups[order(names(opcat_groups))]

# One evenly spaced hue per group, then each catchment in a group gets a
# shade of that hue (light tint -> deep shade, upstream -> downstream). The
# sweep skips the red/orange arc around the accent colour (hue ~8) so no
# catchment's base colour can be mistaken for the accent-highlighted
# selection.
n_groups <- length(opcat_groups)
group_hues <- seq(40, 340, length.out = n_groups)

opcat_color_lookup <- c()
for (i in seq_len(n_groups)) {
  members <- opcat_groups[[i]]
  n_members <- length(members)
  lum <- if (n_members == 1) 50 else seq(78, 38, length.out = n_members)
  chroma <- if (n_members == 1) 55 else seq(45, 70, length.out = n_members)
  opcat_color_lookup[members] <- grDevices::hcl(h = group_hues[i], c = chroma, l = lum)
}

# The accent is still reserved for the current selection, so the one bright
# red polygon on screen is always the thing the user picked.
pal_opcat <- function(names) {
  unname(opcat_color_lookup[names])
}

# Operational catchment index for the Map page rail: name, count of the
# hydrology subbasins the map draws (zero for a few catchments, which stay
# listed so they can still be zoomed to), and the bounding box used to zoom
# the map when a row is clicked.
opcat_counts <- df_subbasin_opcat %>%
  filter(!is.na(opcat_name)) %>%
  group_by(opcat_name) %>%
  summarise(n_subbasins = sum(subbasin %in% hype_subbasin_ids), .groups = "drop") %>%
  arrange(desc(n_subbasins))

opcat_bounds <- lapply(seq_len(nrow(opcat_shp)), function(i) {
  as.numeric(sf::st_bbox(opcat_shp[i, ]))
})
names(opcat_bounds) <- opcat_shp$opcat_name

# EA waterbody polygons (source-to-mouth reach units, finer-grained than
# operational catchments), each tagged with the operational catchment it
# overlaps most - see scripts/join_waterbody_opcat.R. Coloured with the same
# pal_opcat() lookup as subbasins so a waterbody and the catchment it sits in
# always read as the same hue.
waterbody_shp <- read_sf(data_path("gis-data", "app", "waterbodies.gpkg"))
df_waterbody_opcat <- read.csv(data_path("data/waterbody_operational_catchment.csv"))
waterbody_opcat_shp <- waterbody_shp %>%
  left_join(df_waterbody_opcat, by = "water_body")

# Table with climate information
df_stats_climate <- read.csv(data_path("data/subbasin_climate.csv"))
df_stats_climate$subbasin <- as.numeric(gsub("X", "", df_stats_climate$subbasin))

# Upstream area per subbasin (context strip). The file also holds ten land
# use shares per subbasin (90% of its rows), which nothing reads, so only the
# "Upstream area" rows are kept.
df_stats_lc <- read.csv(data_path("data/subbasin_lc.csv"))
df_stats_lc <- df_stats_lc[df_stats_lc$variable == "Upstream area", ]
df_stats_lc$subbasin <- as.numeric(gsub("X", "", df_stats_lc$subbasin))
