# ---- Download page extracts ----
#
# Cache expensive tabular download extracts per variable to avoid repeatedly
# scanning large Arrow datasets when users toggle between options.
tabular_download_cache <- new.env(parent = emptyenv())

build_tabular_download <- function(dl_variable) {
  if (dl_variable %in% c("precip", "temp")) {
    dl_lookup <- c("precip" = "Precipitation", "temp" = "Temperature")
    dl_variable_label <- unname(dl_lookup[dl_variable])

    df_download <- ds_proj_forcing %>%
      filter(variable == dl_variable_label, time_aggregation == "monthly") %>%
      select(subbasin, period, month, variable, p50, unit) %>%
      collect_quiet() %>%
      mutate("scenario" = forcing_scenario_label) %>%
      rename("value" = p50)
  } else {
    # dl_variable is the HYPE variable code itself (see widget_download_variable).
    df_download <- ds_proj_month %>%
      filter(variable == dl_variable) %>%
      select(subbasin, ssp, period, month, p50, unit) %>%
      collect() %>%
      rename("scenario" = ssp, "value" = p50)
  }

  df_download %>%
    mutate("value" = round(value, 3)) %>%
    dplyr::select(subbasin, scenario, period, month, value, unit)
}

get_tabular_download <- function(dl_variable) {
  if (!exists(dl_variable, envir = tabular_download_cache, inherits = FALSE)) {
    tabular_download_cache[[dl_variable]] <- build_tabular_download(dl_variable)
  }
  tabular_download_cache[[dl_variable]]
}
