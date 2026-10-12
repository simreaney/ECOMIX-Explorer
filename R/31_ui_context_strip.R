### 0b. Context strip
#
# The selected subbasin used to exist only on the Map tab, and the Data
# Explorer restated four of its numbers in a row of value boxes that ate a
# whole band of the page. Both are replaced by one 96px strip repeated at the
# top of every panel: locator map, identity, inline stats, one action. Its
# geometry never changes; only which stats sit in the middle.
#
# page_navbar() has no slot above the panels, so the strip is the first child
# of each nav_panel(). Every panel therefore needs its own output ids - hence
# the `suffix` argument, mirrored by register_ctx() in the server.

ctx_stat <- function(label, value) {
  div(class = "ctx-stat",
      div(class = "ctx-label", label),
      div(class = "ctx-value", value))
}

ctx_strip <- function(suffix, stats, action = NULL, mini_map = TRUE,
                      id_label = "Subbasin") {
  div(
    class = "ctx",
    if (isTRUE(mini_map)) {
      div(class = "ctx-map", leafletOutput(paste0("ctx_map_", suffix), height = "94px"))
    },
    div(
      class = "ctx-id",
      div(class = "ctx-label", id_label),
      div(class = "ctx-id-value", textOutput(paste0("ctx_id_", suffix), inline = TRUE)),
      div(class = "ctx-id-sub", textOutput(paste0("ctx_sub_", suffix), inline = TRUE))
    ),
    div(class = "ctx-stats", stats),
    if (!is.null(action)) div(class = "ctx-act", action)
  )
}

# The four subbasin stats shared by Map / Hydro Explorer.
ctx_subbasin_stats <- function(suffix) {
  tagList(
    ctx_stat("Upstream area", textOutput(paste0("ctx_area_", suffix), inline = TRUE)),
    ctx_stat("Annual precip.", textOutput(paste0("ctx_precip_", suffix), inline = TRUE)),
    ctx_stat("Mean annual temp.", textOutput(paste0("ctx_temp_", suffix), inline = TRUE)),
    ctx_stat("Observations", uiOutput(paste0("ctx_obs_", suffix), inline = TRUE))
  )
}

# Segmented-control toggle between a "measured" (real samples) option and a
# "modelled" (model output) option, reused by the Site Details sidebar (which
# entity type is being browsed) and the Hydro Explorer sidebar (which spatial
# unit is being queried) - the two pages use different value/label pairs, so
# both are parameterized rather than hardcoded.
entity_type_toggle <- function(id, measured_panel, modelled_panel,
                               measured_label = "Measured", modelled_label = "Modelled",
                               values = c("measured", "modelled")) {
  div(class = "rail-toggle",
      navset_pill(id = id,
                  nav_panel(title = measured_label, value = values[1], measured_panel),
                  nav_panel(title = modelled_label, value = values[2], modelled_panel)))
}
