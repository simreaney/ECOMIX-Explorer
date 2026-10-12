# ---- Chemical / physicochemical measured dataset ----
#
# Raw measured data is read as all-character columns so that "ND"
# (non-detect) values sit alongside numeric concentrations without read.csv
# silently coercing an entire analyte column to NA.
df_measured_raw <- read.csv(
  here("data/measured/comix_monitoring_data_Dashboard.csv"),
  colClasses = "character",
  check.names = FALSE
)
colnames(df_measured_raw)[1] <- "Site_id" # first header cell carries a stray BOM

# The file's final row is a footnote about the ND/NA convention, not a real
# measured record - drop any row without a parseable latitude.
df_measured_raw <- df_measured_raw[
  !is.na(suppressWarnings(as.numeric(df_measured_raw$Latitude))),
]

measured_meta_cols <- c("Site_id", "Site_full_name", "Latitude", "Longitude", "Week_Month", "Date")
measured_physchem_cols <- tail(setdiff(names(df_measured_raw), measured_meta_cols), 7)
measured_chemical_cols <- setdiff(names(df_measured_raw), c(measured_meta_cols, measured_physchem_cols))

df_measured_raw$Date <- as.Date(df_measured_raw$Date, format = "%d/%m/%Y")
df_measured_raw$Latitude <- as.numeric(df_measured_raw$Latitude)
df_measured_raw$Longitude <- as.numeric(df_measured_raw$Longitude)

# Strip the stray unit suffix baked into the "6PPD-Q" column name for display only.
measured_parameter_label <- function(x) sub("_ng L⁻¹$", "", x)

# One row per site, for the measured sites map.
df_measured_sites <- df_measured_raw %>%
  dplyr::select(Site_id, Site_full_name, Latitude, Longitude) %>%
  dplyr::distinct(Site_id, .keep_all = TRUE)

# Tidy long format: one row per site/date/parameter, with a detection status
# derived from the raw string ("ND" -> non-detect, blank -> no sample taken).
df_measured_long <- df_measured_raw %>%
  tidyr::pivot_longer(
    cols = dplyr::all_of(c(measured_chemical_cols, measured_physchem_cols)),
    names_to = "parameter",
    values_to = "value_raw"
  ) %>%
  dplyr::mutate(
    parameter_group = ifelse(parameter %in% measured_physchem_cols,
                              "Physicochemical parameter", "Organic micropollutant"),
    parameter_label = measured_parameter_label(parameter),
    status = dplyr::case_when(
      is.na(value_raw) | value_raw == "" ~ "No sample",
      value_raw == "ND" ~ "Non-detect",
      TRUE ~ "Detected"
    ),
    value_num = dplyr::case_when(
      status == "Detected" ~ suppressWarnings(as.numeric(value_raw)),
      status == "Non-detect" ~ 0,
      TRUE ~ NA_real_
    )
  ) %>%
  dplyr::select(Site_id, Site_full_name, Latitude, Longitude, Date,
                parameter, parameter_label, parameter_group, status, value_num)

# Per-site summary powering the Site Details page's site table and its
# context strip: how often anything was detected, and the largest
# concentration seen.
df_measured_site_summary <- df_measured_long %>%
  dplyr::filter(parameter_group == "Organic micropollutant") %>%
  dplyr::group_by(Site_id, Site_full_name) %>%
  dplyr::summarise(
    n_screened = sum(status %in% c("Detected", "Non-detect")),
    n_detected = sum(status == "Detected"),
    detection_rate = ifelse(n_screened > 0, round(100 * n_detected / n_screened), NA_real_),
    max_value = suppressWarnings(max(value_num[status == "Detected"], na.rm = TRUE)),
    n_weeks = dplyr::n_distinct(Date),
    .groups = "drop"
  ) %>%
  dplyr::mutate(max_value = ifelse(is.finite(max_value), round(max_value, 1), NA_real_)) %>%
  dplyr::arrange(dplyr::desc(detection_rate))

# Grouped choices for the Site Details time series chemical/parameter
# selectors below (measured mode).
measured_parameter_choices <- list(
  "Organic micropollutants" = setNames(measured_chemical_cols, measured_parameter_label(measured_chemical_cols)),
  "Physicochemical parameters" = setNames(measured_physchem_cols, measured_physchem_cols)
)

# Short background information for a measured site or modelled water body
# (dummy placeholder data - see the file header of sitesInfo.txt /
# waterbodyInfo.txt). Parsed once at startup into a named list keyed by
# Site_id or water_body id. The name field accepts either "Site" (measured
# blocks) or "Water body" (modelled blocks); "Operational catchment" and
# landcover/population are optional and simply come back NA/empty when a
# block doesn't have them (true for every modelled block).
parse_entity_info <- function(path) {
  lines <- readLines(path, encoding = "UTF-8")
  lines <- lines[!(grepl("^\\s*#", lines) & !grepl("^### ", lines))] # drop comment lines, keep block headers
  block_starts <- grep("^### ", lines)

  entity_info <- list()
  for (i in seq_along(block_starts)) {
    entity_id <- sub("^### ", "", lines[block_starts[i]])
    start <- block_starts[i] + 1
    end <- if (i < length(block_starts)) block_starts[i + 1] - 1 else length(lines)
    block_lines <- lines[start:end]
    block_lines <- block_lines[nzchar(trimws(block_lines))]

    # Split each "Key: Value" line on the first colon only, so summary text
    # containing colons is not truncated.
    parsed <- regmatches(block_lines, regexec("^([^:]+):\\s*(.*)$", block_lines))
    keys <- vapply(parsed, `[`, character(1), 2)
    values <- vapply(parsed, `[`, character(1), 3)
    names(values) <- trimws(keys)

    landcover_idx <- grepl("^Land cover", names(values))
    landcover_names <- gsub("^Land cover - | \\(%\\)$", "", names(values)[landcover_idx])

    name <- unname(values["Site"])
    if (is.na(name)) name <- unname(values["Water body"])

    entity_info[[entity_id]] <- list(
      name = name,
      summary = unname(values["Summary"]),
      population = unname(values["Population (catchment)"]),
      opcat = unname(values["Operational catchment"]),
      landcover = setNames(as.numeric(values[landcover_idx]), landcover_names)
    )
  }
  entity_info
}

measured_site_info <- parse_entity_info(here("data/measured/sitesInfo.txt"))
modelled_site_info <- tryCatch(
  parse_entity_info(here("data/modelled/waterbodyInfo.txt")),
  error = function(e) {
    warning("data/modelled/waterbodyInfo.txt not found - run scripts/build_waterbody_info.R first. ",
            "Modelled water body summaries will be unavailable.")
    list()
  }
)
