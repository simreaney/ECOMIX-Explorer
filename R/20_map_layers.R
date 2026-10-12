# ---- Leaflet map layers ----
#
# The pieces of the app's Leaflet maps that depend only on the data loaded
# above: the Map page's panes and overlays, the context-strip locator maps
# and the Spatial Datasets choropleth. The server decides when each is drawn.
# Every add_*() helper takes a leaflet() widget or a leafletProxy(), since
# both dispatch through the same leaflet methods.

## Map page

# One pane per overlay, so how the overlays stack no longer depends on the
# order they are switched on: areas below points, and the selection
# highlight above everything. When chemical coverage polygons were simply
# added after the site markers, they covered them and caught the clicks
# meant for 3 of the 19 measured sites and 44 of the 155 observed hydrology
# markers. Subbasins stay in Leaflet's overlayPane (z-index 400), between
# the tiles (200) and these; tooltips (650) and popups (700) stay on top.
map_panes <- c(opcat = 410, waterbodies = 415, chem_coverage = 420,
               measured_sites = 440, observed_hydro = 445, eco_risk = 450,
               selection = 460)

add_map_panes <- function(map) {
  for (pane in names(map_panes)) map <- addMapPane(map, pane, zIndex = map_panes[[pane]])
  map
}

# The Map page's overlays that never change once drawn, keyed by their
# "Layers" checkbox value, which is also their group and pane. Ecological
# risk depends on its chemical picker and is drawn by add_eco_risk_layer().
map_overlays <- list(
  opcat = function(map) {
    addPolygons(map, data = opcat_shp, fill = FALSE, color = ec$ink,
                weight = 2.5, opacity = 0.9, label = ~opcat_name, group = "opcat",
                options = pathOptions(pane = "opcat"))
  },
  waterbodies = function(map) {
    addPolygons(
      map,
      data = waterbody_map_shp,
      fill = TRUE,
      fillColor = ~ifelse(!has_model_data, ec$rule_light,
                          ifelse(is.na(opcat_name), "#e0dede", pal_opcat(opcat_name))),
      fillOpacity = ~ifelse(has_model_data, 0.45, 0.2),
      color = ec$ink,
      weight = 0.75,
      opacity = 0.5,
      layerId = ~water_body,
      label = ~paste0(water_bod0, ifelse(is.na(opcat_name), "", paste0(" · ", opcat_name)),
                      ifelse(has_model_data, "", " (no modelled data)")),
      popup = ~ifelse(
        has_model_data,
        paste0(
          "<strong>", water_bod0, "</strong>",
          ifelse(is.na(opcat_name), "", paste0("<br>", opcat_name)),
          "<br><a href='#' onclick=\"Shiny.setInputValue('open_waterbody_details', '",
          water_body,
          "', {priority: 'event'}); return false;\">Open in Site Details →</a>"
        ),
        paste0("<strong>", water_bod0, "</strong><br><em>No modelled chemical data available</em>")
      ),
      group = "waterbodies",
      options = pathOptions(pane = "waterbodies")
    )
  },
  measured_sites = function(map) {
    addCircleMarkers(
      map,
      data = df_measured_sites, lng = ~Longitude, lat = ~Latitude,
      layerId = ~Site_id,
      radius = 5, color = "#ffffff", weight = 1.5, fillColor = ec$ink,
      fillOpacity = 0.95, label = ~Site_full_name,
      popup = ~paste0(
        "<strong>", Site_full_name, "</strong>",
        "<br><a href='#' onclick=\"Shiny.setInputValue('open_site_details', '",
        Site_id,
        "', {priority: 'event'}); return false;\">Open in Site Details →</a>"
      ),
      group = "measured_sites",
      options = pathOptions(pane = "measured_sites")
    )
  },
  observed_hydro = function(map) {
    addCircleMarkers(
      map,
      data = observed_hydro_points, lng = ~lon, lat = ~lat,
      layerId = ~Id,
      radius = 5, color = ec$ink, weight = 1.5, fillColor = "#0f6659",
      fillOpacity = 0.9,
      popup = ~paste0(
        "<strong>Subbasin id: </strong>", Id,
        "<br><strong>Observed variables: </strong>", variables,
        "<br><a href='#' onclick=\"Shiny.setInputValue('open_data_explorer', '",
        Id,
        "', {priority: 'event'}); return false;\">Open in Hydro Explorer →</a>"
      ),
      group = "observed_hydro",
      options = pathOptions(pane = "observed_hydro")
    )
  },
  chem_coverage = function(map) {
    addPolygons(
      map,
      data = chem_coverage_shp, fill = TRUE, fillColor = "#2f6690", fillOpacity = 0.22,
      color = "#2f6690", weight = 1.5, opacity = 0.85,
      label = ~paste0("Subbasin ", Id, " · chemical data available"),
      # Most of these aren't hydrology subbasins, so aren't on the base
      # layer - this link is how they get selected from the map.
      popup = ~paste0(
        "<strong>Subbasin id: </strong>", Id,
        "<br><a href='#' onclick=\"Shiny.setInputValue('open_data_explorer', '",
        Id,
        "', {priority: 'event'}); return false;\">Open in Hydro Explorer →</a>"
      ),
      group = "chem_coverage",
      options = pathOptions(pane = "chem_coverage")
    )
  }
)

# Ecological risk markers for one chemical (`df` is its eco_risk_data entry).
add_eco_risk_layer <- function(map, df, chemical) {
  chemical_label <- names(eco_risk_choices)[eco_risk_choices == chemical]
  addCircleMarkers(
    map,
    data = df,
    lng = ~lon,
    lat = ~lat,
    layerId = ~site_id,
    radius = 6,
    color = "#ffffff",
    weight = 1.5,
    fillColor = ~eco_risk_pal(risk_level),
    fillOpacity = 0.9,
    label = ~paste0(chemical_label, " · ", risk_level, " · Site ", site_id),
    popup = ~popup_html,
    group = "eco_risk",
    options = pathOptions(pane = "eco_risk")
  )
}

# Its legend. It has a layerId, so drawing it again replaces it and
# removeControl("eco_risk_legend") takes it off; a legend without one was
# added again on every redraw and stayed after the layer was switched off.
# It has no group either: a legend's group only matters to Leaflet's own
# layers control, which this map doesn't use, and each one registers map
# event handlers that are never removed.
add_eco_risk_legend <- function(map, df) {
  addLegend(map, "bottomleft", pal = eco_risk_pal, values = df$risk_level,
            title = "Ecological risk", layerId = "eco_risk_legend")
}


## Context strip locator maps

# The selected subbasin in the accent, framed; with no selection, or an id
# with no subbasin polygon, the whole study area and no highlight.
ctx_subbasin_map <- function(subbasin_id) {
  map <- leaflet(options = leafletOptions(zoomControl = FALSE, attributionControl = FALSE,
                                          dragging = FALSE, scrollWheelZoom = FALSE,
                                          doubleClickZoom = FALSE)) %>%
    addTiles()
  shp <- if (is.null(subbasin_id)) NULL else subbasin_opcat_shp %>% filter(Id == subbasin_id)
  if (is.null(shp) || nrow(shp) == 0) return(setView(map, lng = -1.16, lat = 53.75, zoom = 7))
  bb <- as.numeric(sf::st_bbox(shp))
  map %>%
    addPolygons(data = shp, fillColor = ec$accent, fillOpacity = 0.9,
                color = ec$accent, weight = 2) %>%
    fitBounds(bb[1], bb[2], bb[3], bb[4])
}

# Site Details locator: the selected water body (`type` "modelled") or
# measured site, or the whole study area when that side has no selection.
ctx_site_map <- function(type, id) {
  map <- leaflet(options = leafletOptions(zoomControl = FALSE, attributionControl = FALSE,
                                          dragging = FALSE, scrollWheelZoom = FALSE)) %>%
    addTiles()
  if (!is.null(id) && identical(type, "modelled")) {
    shp <- waterbody_opcat_shp %>% filter(water_body == id)
    if (nrow(shp) == 1) {
      bb <- as.numeric(sf::st_bbox(shp))
      return(map %>%
               addPolygons(data = shp, fillColor = ec$accent, fillOpacity = 0.5,
                           color = ec$accent, weight = 2) %>%
               fitBounds(bb[1], bb[2], bb[3], bb[4]))
    }
  } else if (!is.null(id)) {
    row <- df_measured_sites %>% dplyr::filter(Site_id == id)
    if (nrow(row) == 1) {
      return(map %>%
               setView(lng = row$Longitude, lat = row$Latitude, zoom = 10) %>%
               addCircleMarkers(lng = row$Longitude, lat = row$Latitude,
                                radius = 6, color = ec$accent, weight = 2,
                                fillColor = ec$accent, fillOpacity = 1))
    }
  }
  setView(map, lng = -1.16, lat = 53.75, zoom = 7)
}


## Spatial Datasets map

# The polygons a Spatial Datasets variable is drawn on: the HYPE subbasins
# (hydrology and climate variables), the subbasins with chemical predictions
# or the modelled water bodies.
spatial_basis_of <- function(variable) {
  if (is_wbchem_variable(variable)) "waterbody" else if (is_chem_variable(variable)) "chem_subbasin" else "hype_subbasin"
}

# The two bases whose polygons stay on the map while their values change:
# every slice has at most one row per polygon (in practice every one of
# them: 3126 subbasins, 336 water bodies), matched on `key`. Each polygon is
# drawn with a string layerId, which is how the restyle finds it again
# (leaflet.js only indexes string layerIds). The 34 water bodies without
# modelled data never have a value, so they are not drawn at all.
spatial_basis_shp <- list(
  hype_subbasin = subbasin_shp %>%
    filter(Id %in% hype_subbasin_ids) %>%
    transmute(key = Id, layer_id = as.character(Id)),
  waterbody = waterbody_map_shp %>%
    filter(has_model_data) %>%
    transmute(key = water_body, layer_id = water_body)
)

spatial_legend_title <- function(basis, df) {
  if (nrow(df) == 0) {
    "No data"
  } else if (basis != "hype_subbasin") {
    stat_label <- names(chem_stat_choices)[match(df$prediction_percentile[1], chem_stat_choices)]
    paste0(stat_label, " conc. (ng/L)")
  } else {
    paste0("P", df$prediction_percentile[1], " (", pretty_unit(df$unit[1]), ")")
  }
}

# Style of every polygon of a restyled basis for one slice (`df`, from
# spatial_layer_data()): the colours the map has always used for the
# polygons with a row in the slice. The others are hidden with a zero fill
# and outline opacity, so they look exactly as they did when such polygons
# were left out of the map. Opacities are sent as one value when the slice
# covers every polygon, as it does today.
spatial_basis_style <- function(basis, df) {
  shp <- spatial_basis_shp[[basis]]
  key <- if (basis == "waterbody") df$waterbody_id else df$subbasin
  row <- match(shp$key, key)
  shown <- !is.na(row)
  values <- df$value[row]
  pal <- colorNumeric(palette = "viridis", domain = values[shown])
  shown_or_0 <- function(x) if (all(shown)) x else ifelse(shown, x, 0)
  list(
    layer_id = shp$layer_id,
    values = values[shown],
    pal = pal,
    style = list(fillColor = pal(values), fillOpacity = shown_or_0(0.75), opacity = shown_or_0(0.5))
  )
}

# Draws the polygons of `basis` coloured by `df`, with the legend. The
# chemical subbasins are drawn one polygon per row, as they always have
# been: a few subbasins have a row for each of several modelling sites, and
# their overlapping polygons cannot be told apart by layerId, so that basis
# is drawn again (clearGroup("values") first) rather than restyled.
add_spatial_values <- function(map, basis, df) {
  if (basis == "chem_subbasin") {
    shp <- left_join(subbasin_shp, df, by = c("Id" = "subbasin")) %>% filter(!is.na(hype_variable))
    values <- shp$value
    pal <- colorNumeric(palette = "viridis", domain = values)
    map <- addPolygons(map, data = shp, fillColor = pal(values), fillOpacity = 0.75,
                       color = ec$ink, weight = 0.6, group = "values")
  } else {
    s <- spatial_basis_style(basis, df)
    values <- s$values
    pal <- s$pal
    map <- addPolygons(map, data = spatial_basis_shp[[basis]], layerId = s$layer_id,
                       fillColor = s$style$fillColor, fillOpacity = s$style$fillOpacity,
                       opacity = s$style$opacity, color = ec$ink, weight = 0.6, group = "values")
  }
  addLegend(map, "bottomleft", pal = pal, values = values,
            title = spatial_legend_title(basis, df), opacity = 1, layerId = "sp_legend")
}

# Brings a Spatial Datasets map already showing `basis` to the slice `df`:
# new fill colours through the setShapeStyle method defined in the page
# head (app.R), or, for the chemical subbasins, the polygons drawn again.
update_spatial_values <- function(proxy, basis, df) {
  if (basis == "chem_subbasin") {
    return(proxy %>% clearGroup("values") %>% add_spatial_values(basis, df))
  }
  s <- spatial_basis_style(basis, df)
  proxy %>%
    invokeMethod(NULL, "setShapeStyle", s$layer_id, s$style) %>%
    addLegend("bottomleft", pal = s$pal, values = s$values,
              title = spatial_legend_title(basis, df), opacity = 1, layerId = "sp_legend")
}
