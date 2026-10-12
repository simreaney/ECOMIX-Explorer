# ---- Ecological risk thresholds (by subbasin) ----
#
# Each CSV under data-chem/5_EcologicalRisk/ is one chemical's per-site
# ecological risk assessment - currently just fipronil, with more chemicals
# expected to land here over time as one file each, sharing the same layout
# (EASTING/NORTHING/site_id, exposure summary stats, and a set of yes/no/na
# "<...>_risk_threshold_<...>" columns comparing exposure against generic and
# site-specific HC5 standards). Files are picked up by folder listing so
# adding a chemical is just adding a CSV, no code change. Sites are keyed by
# the same SITE_ID used in the daily chemical modelling above, so a single
# lookup against chem_site_key resolves every file to a subbasin.
eco_risk_dir <- data_path("data-chem/5_EcologicalRisk")

eco_risk_files <- list.files(eco_risk_dir, pattern = "_risk\\.csv$", full.names = TRUE)
names(eco_risk_files) <- sub("_risk\\.csv$", "", basename(eco_risk_files))

# Readable labels for the four standard threshold comparisons - shown in
# each site's map popup alongside its site-specific counterpart (ss_<col>)
# where a site-specific HC5 was available. Only columns actually present in
# a given file are used, so a future file with a different set of threshold
# columns still renders (just without these specific lines).
eco_risk_flag_labels <- c(
  acute_risk_threshold_p90       = "Acute risk (90th percentile exposure)",
  chronic_risk_threshold_p90     = "Chronic risk (90th percentile exposure)",
  acute_risk_threshold_average   = "Acute risk (average exposure)",
  chronic_risk_threshold_average = "Chronic risk (average exposure)"
)

is_na_string <- function(x) is.na(x) | tolower(trimws(x)) == "na"

# Overall risk level per site - None/Acute/Chronic/Both - from whichever
# acute/chronic comparison (90th-percentile or average exposure) trips
# first, preferring the site-specific HC5 threshold columns (ss_*) over the
# generic ones whenever a site-specific HC5 exists for that site. Unlike the
# popup's generic scan over every "*risk_threshold*" column, this needs to
# know specifically which columns mean acute vs. chronic, so a future
# chemical file needs the same acute/chronic/generic/ss_ column layout for
# this to keep working.
compute_eco_risk_level <- function(df) {
  has_ss          <- !is_na_string(df[["site_specific hc5"]])
  generic_acute   <- df$acute_risk_threshold_p90   == "yes" | df$acute_risk_threshold_average   == "yes"
  generic_chronic <- df$chronic_risk_threshold_p90 == "yes" | df$chronic_risk_threshold_average == "yes"
  ss_acute        <- df$ss_acute_risk_threshold_p90   == "yes" | df$ss_acute_risk_threshold_average   == "yes"
  ss_chronic      <- df$ss_chronic_risk_threshold_p90 == "yes" | df$ss_chronic_risk_threshold_average == "yes"
  eff_acute   <- ifelse(has_ss, ss_acute, generic_acute)
  eff_chronic <- ifelse(has_ss, ss_chronic, generic_chronic)
  factor(
    dplyr::case_when(
      eff_acute & eff_chronic ~ "Both",
      eff_acute               ~ "Acute",
      eff_chronic              ~ "Chronic",
      TRUE                     ~ "None"
    ),
    levels = c("None", "Acute", "Chronic", "Both")
  )
}

# Colour palette for the four risk levels, built only from the app's
# existing design tokens (a light-to-dark ramp along the single accent hue)
# so the map stays inside the "mono + one accent" design system - shared by
# the map markers and the map legend. Acute vs. Chronic aren't actually
# ordered relative to each other (two independent flags, not severity
# steps); this tint/accent assignment is a deliberate but arbitrary choice,
# easy to swap once the detail page design is finalised.
eco_risk_pal <- colorFactor(
  c(ec$rule_light, ec$accent_tint, ec$accent, ec$accent_ink),
  levels = c("None", "Acute", "Chronic", "Both")
)

build_eco_risk_popup <- function(df, chemical_key, chemical_label) {
  flag_cols <- intersect(names(eco_risk_flag_labels), names(df))
  # The D3D time series (Chemical Explorer tab) only exists for chemicals also
  # present in chem_d3d_datasets (data-chem/7_chemicals), but the two lists aren't guaranteed to stay in
  # lockstep as more chemicals are added to either one independently.
  has_d3d <- chemical_key %in% names(chem_d3d_datasets)
  vapply(seq_len(nrow(df)), function(i) {
    lines <- vapply(flag_cols, function(col) {
      ss_col <- paste0("ss_", col)
      ss_val <- if (ss_col %in% names(df)) df[[ss_col]][i] else NA
      ss_txt <- if (!is.null(ss_val) && !is_na_string(ss_val)) {
        paste0(" · site-specific: ", ss_val)
      } else ""
      paste0(eco_risk_flag_labels[[col]], ": ", df[[col]][i], ss_txt)
    }, character(1))
    chem_data_link <- if (has_d3d) {
      paste0(
        "<br><a href='#' onclick=\"Shiny.setInputValue('open_chem_d3d_detail', '",
        chemical_key, "||", df$site_id[i],
        "', {priority: 'event'}); return false;\">View chemical time series →</a>"
      )
    } else ""
    paste0(
      "<strong>", chemical_label, " · ecological risk</strong>",
      "<br>Site ", df$site_id[i], " · Subbasin ", df$subbasin[i],
      "<br>", paste(lines, collapse = "<br>"),
      "<br><a href='#' onclick=\"Shiny.setInputValue('open_eco_risk_detail', '",
      chemical_key, "||", df$site_id[i],
      "', {priority: 'event'}); return false;\">View ecological risk detail →</a>",
      chem_data_link
    )
  }, character(1))
}

eco_risk_data <- lapply(names(eco_risk_files), function(nm) {
  df <- read.csv(eco_risk_files[[nm]], check.names = FALSE)
  df <- df %>% dplyr::inner_join(chem_site_key, by = c("site_id" = "SITE_ID"))

  # EASTING/NORTHING are British National Grid (EPSG:27700, confirmed via
  # gis-data/subbasins_bng.prj) - transform to WGS84 for the leaflet map,
  # the same CRS used the other direction for the Download page's export
  # (see the st_transform(df_spatial, 27700) call further down this file).
  pts <- sf::st_as_sf(df, coords = c("EASTING", "NORTHING"), crs = 27700) %>%
    sf::st_transform(4326)
  coords <- sf::st_coordinates(pts)
  df$lon <- coords[, "X"]
  df$lat <- coords[, "Y"]

  df$risk_level <- compute_eco_risk_level(df)
  df$popup_html <- build_eco_risk_popup(df, nm, tools::toTitleCase(nm))
  df
})
names(eco_risk_data) <- names(eco_risk_files)

# One choice per chemical file for the Map rail's chemical picker.
eco_risk_choices <- names(eco_risk_data)
names(eco_risk_choices) <- tools::toTitleCase(eco_risk_choices)
