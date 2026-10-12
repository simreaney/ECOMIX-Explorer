# ---- Modelled chemical percentile time series (daily, by site) ----
#
# Each chemical under data-chem/7_chemicals/ is a pair of parquet files named
# "<chemical>_conc_by_SITE_ID_p10_p50_p90.parquet" (concentration, ug/L) and
# "<chemical>_ssPAF_by_SITE_ID_p10_p50_p90.parquet" (ssPAF, the potentially
# affected fraction of species), both with columns Date, SITE_ID and
# <measure>_p10/_p50/_p90 (25 model realizations collapsed to percentiles).
# The first word of the file name is the chemical and the second the measure,
# so adding a chemical is just dropping in a new pair of files - this table
# is discovered from the folder at startup, no code changes. A chemical
# missing either half of its pair is skipped.
#
# Declared here, before the Ecological risk section below, so
# build_eco_risk_popup() can reference chem_d3d_datasets when deciding
# whether to show a "View chemical time series" link.
chem_d3d_dir <- here("data-chem/7_chemicals")
chem_d3d_datasets <- local({
  files <- list.files(chem_d3d_dir, pattern = "\\.parquet$", full.names = TRUE)
  parts <- strsplit(basename(files), "_")
  chem_orig <- vapply(parts, `[`, character(1), 1)
  chem  <- tolower(chem_orig)
  kind  <- tolower(vapply(parts, `[`, character(1), 2))
  out <- list()
  for (ch in sort(unique(chem))) {
    conc_f  <- files[chem == ch & kind == "conc"]
    sspaf_f <- files[chem == ch & kind == "sspaf"]
    if (length(conc_f) != 1 || length(sspaf_f) != 1) next
    out[[ch]] <- list(
      # Keep the file name's own capitalisation (e.g. "6PPDQ"), only
      # capitalising a fully lower-case name ("imidacloprid" -> "Imidacloprid").
      label = {
        nm <- chem_orig[chem == ch][1]
        if (nm == tolower(nm)) tools::toTitleCase(nm) else nm
      },
      conc = arrow::open_dataset(conc_f),
      sspaf = arrow::open_dataset(sspaf_f),
      unit = "\u00b5g/L"
    )
  }
  out
})
chem_d3d_choices <- setNames(names(chem_d3d_datasets), sapply(chem_d3d_datasets, `[[`, "label"))

# Site ids available per chemical, for the Chemical Explorer page's site
# selector - queried once at startup rather than re-scanning the parquet
# every time the chemical selector changes.
chem_d3d_site_ids <- lapply(chem_d3d_datasets, function(ds) {
  ids <- ds$conc %>% dplyr::distinct(SITE_ID) %>% dplyr::collect() %>% dplyr::pull(SITE_ID)
  ids[order(suppressWarnings(as.numeric(ids)))]
})

# SITE_ID -> subbasin (Subbasin_Siteids_Key.csv) -> EA water body (the
# water body polygon containing the subbasin's representative point), so the
# site selector can show water body names and a Map/Site Details selection
# can be carried over. A few subbasins hold several SITE_IDs, and a few
# sites fall outside every water body polygon (no name, labelled "Site <id>").
chem_site_wb <- local({
  key <- read.csv(file.path(chem_dir, "Subbasin_Siteids_Key.csv"), fileEncoding = "UTF-8-BOM")
  key$SITE_ID <- as.character(key$SITE_ID)
  pts <- subbasin_shp %>% dplyr::filter(Id %in% key$subbasin)
  s2_was <- sf::sf_use_s2(FALSE)
  on.exit(sf::sf_use_s2(s2_was), add = TRUE)
  pts <- sf::st_point_on_surface(sf::st_make_valid(sf::st_geometry(pts))) %>%
    sf::st_as_sf() %>% dplyr::mutate(subbasin = pts$Id)
  wb <- sf::st_make_valid(waterbody_shp[, c("water_body", "water_bod0")])
  hit <- sf::st_join(pts, sf::st_transform(wb, sf::st_crs(pts)), join = sf::st_intersects) %>%
    sf::st_drop_geometry() %>%
    dplyr::distinct(subbasin, .keep_all = TRUE) %>%
    dplyr::transmute(subbasin, wb_code = water_body, wb_name = water_bod0)
  dplyr::left_join(key, hit, by = "subbasin")
})

# Named choices (label = water body name, value = SITE_ID) for the selector.
# Names shared by several sites get the site id appended to stay distinct.
chem_d3d_site_choices <- lapply(chem_d3d_site_ids, function(ids) {
  d <- chem_site_wb[match(ids, chem_site_wb$SITE_ID), ]
  nm <- ifelse(is.na(d$wb_name), paste("Site", ids), d$wb_name)
  dup <- duplicated(nm) | duplicated(nm, fromLast = TRUE)
  nm[dup] <- paste0(nm[dup], " (site ", ids[dup], ")")
  setNames(ids, nm)
})

# ---- Source apportionment (share of each source in a site's concentration) ----
#
# data-chem/6_Source_apportionment/ holds, per chemical, one wide CSV per
# source named "<Chemical>_<source>_source_apportion_waterbodies.csv", with
# source one of dog_swim / pet_CSO / pet_WWTP. Rows are consecutive days (no
# date column - the last row is 2022-12-31, the end of the concentration
# record) and columns are water body ids (id_waterbody, see
# marthaWBid2EAWBid.csv); each cell is that source's fraction of the total,
# so the three sources sum to 1. A chemical only gets the Chemical Explorer
# treemaps if it has all three files. Sites take the split of the water body
# they sit in (chem_site_wb). Only 2018-01-01 onwards (the plotted period) is
# kept, and files are read on first use then cached per chemical.
chem_src_dir <- here("data-chem/6_Source_apportionment")
chem_src_last_date <- as.Date("2022-12-31")
chem_src_first_date <- as.Date("2018-01-01")
chem_src_labels <- c(dog_swim = "Dog swimming", pet_CSO = "Pet CSOs", pet_WWTP = "Pet WWTP")
chem_src_fill <- c(dog_swim = ec$accent, pet_CSO = ec$ink, pet_WWTP = "#9b9797")
chem_src_text <- c(dog_swim = "#ffffff", pet_CSO = "#ffffff", pet_WWTP = ec$ink)

chem_src_files <- local({
  files <- list.files(chem_src_dir, pattern = "_source_apportion_waterbodies\\.csv$", full.names = TRUE)
  chem <- tolower(sub("_.*$", "", basename(files)))
  src <- sub("^[^_]+_(.*)_source_apportion_waterbodies\\.csv$", "\\1", basename(files))
  out <- list()
  for (ch in intersect(unique(chem), names(chem_d3d_datasets))) {
    f <- setNames(files[chem == ch][match(names(chem_src_labels), src[chem == ch])], names(chem_src_labels))
    if (!anyNA(f)) out[[ch]] <- as.list(f)
  }
  out
})

chem_wb_id_key <- read.csv(here("data-chem/2_Modelling_results_by_waterbody_monthly_conc_ng_per_L/marthaWBid2EAWBid.csv"),
                           fileEncoding = "UTF-8-BOM")
chem_site_wb$wb_id <- as.character(chem_wb_id_key$id_waterbody[match(chem_site_wb$wb_code, chem_wb_id_key$water_body)])

chem_src_cache <- new.env(parent = emptyenv())
get_chem_src <- function(chem_key) {
  if (!exists(chem_key, envir = chem_src_cache, inherits = FALSE)) {
    mats <- lapply(chem_src_files[[chem_key]], function(f) {
      m <- as.matrix(arrow::read_csv_arrow(f))
      dates <- chem_src_last_date - rev(seq_len(nrow(m)) - 1L)
      keep <- dates >= chem_src_first_date
      structure(m[keep, , drop = FALSE], dates = dates[keep])
    })
    chem_src_cache[[chem_key]] <- list(mats = mats, dates = attr(mats[[1]], "dates"))
  }
  chem_src_cache[[chem_key]]
}

# Source shares (named dog_swim/pet_CSO/pet_WWTP, summing to 1) for one site:
# the mean over the whole period when `date` is NULL, otherwise that day.
# NULL if the site has no water body column or no data for that day.
get_chem_src_split <- function(chem_key, site_id_val, date = NULL) {
  wb <- chem_site_wb$wb_id[match(site_id_val, chem_site_wb$SITE_ID)]
  src <- get_chem_src(chem_key)
  if (is.na(wb) || !wb %in% colnames(src$mats[[1]])) return(NULL)
  rows <- if (is.null(date)) seq_along(src$dates) else which(src$dates == date)
  if (length(rows) == 0) return(NULL)
  v <- vapply(src$mats, function(m) mean(m[rows, wb], na.rm = TRUE), numeric(1))
  if (anyNA(v) || sum(v) <= 0) return(NULL)
  v / sum(v)
}

# Squarified treemap layout (Bruls et al.) of positive `vals` in the unit
# square; returns x0/x1/y0/y1 per value in the input order.
treemap_layout <- function(vals) {
  ord <- order(vals, decreasing = TRUE)
  v <- vals[ord] / sum(vals)
  out <- data.frame(x0 = 0, x1 = 0, y0 = 0, y1 = 0)[rep(1, length(v)), ]
  x0 <- 0; y0 <- 0; w <- 1; h <- 1
  i <- 1
  worst <- function(row, side) {
    s <- sum(row)
    max(side^2 * max(row) / s^2, s^2 / (side^2 * min(row)))
  }
  while (i <= length(v)) {
    side <- min(w, h)
    n <- i
    while (n < length(v) && worst(v[i:(n + 1)], side) <= worst(v[i:n], side)) n <- n + 1
    row <- v[i:n]
    t <- sum(row) / (w * h)  # fraction of the remaining area this row takes
    if (w >= h) {  # lay the row out as a column on the left
      cw <- w * t
      ys <- y0 + h * c(0, cumsum(row / sum(row)))
      for (k in seq_along(row)) out[i + k - 1, ] <- c(x0, x0 + cw, ys[k], ys[k + 1])
      x0 <- x0 + cw; w <- w - cw
    } else {  # lay the row out as a strip along the bottom
      rh <- h * t
      xs <- x0 + w * c(0, cumsum(row / sum(row)))
      for (k in seq_along(row)) out[i + k - 1, ] <- c(xs[k], xs[k + 1], y0, y0 + rh)
      y0 <- y0 + rh; h <- h - rh
    }
    i <- n + 1
  }
  out[order(ord), ]
}

# Per-(chemical, site) daily series (Date, conc_p10/p50/p90, sspaf_p10/p50/p90),
# cached per selection actually visited. Concentrations are already in ug/L,
# the same unit as the HC5 thresholds in eco_risk_data.
chem_d3d_series_cache <- new.env(parent = emptyenv())
get_chem_d3d_series <- function(chem_key, site_id_val) {
  key <- paste(chem_key, site_id_val, sep = "||")
  if (!exists(key, envir = chem_d3d_series_cache, inherits = FALSE)) {
    ds <- chem_d3d_datasets[[chem_key]]
    conc <- ds$conc %>%
      dplyr::filter(SITE_ID == site_id_val) %>%
      dplyr::collect() %>%
      dplyr::transmute(Date = as.Date(Date),
                       conc_p10 = conc_ug_L_p10, conc_p50 = conc_ug_L_p50, conc_p90 = conc_ug_L_p90)
    sspaf <- ds$sspaf %>%
      dplyr::filter(SITE_ID == site_id_val) %>%
      dplyr::collect() %>%
      dplyr::transmute(Date = as.Date(Date),
                       sspaf_p10 = ssPAF_p10, sspaf_p50 = ssPAF_p50, sspaf_p90 = ssPAF_p90)
    chem_d3d_series_cache[[key]] <- dplyr::full_join(conc, sspaf, by = "Date") %>% dplyr::arrange(Date)
  }
  chem_d3d_series_cache[[key]]
}

# Acute/chronic "levels of concern" for one chemical/site, preferring the
# site-specific HC5 / HC5-over-10 over the generic value - exactly the same
# has_ss preference logic as compute_eco_risk_level() below. Reuses
# eco_risk_data rather than re-reading fipronil_assemblage_specific_SSD_stats.xlsx:
# data-chem/5_EcologicalRisk/<chem>_risk.csv already carries these same
# threshold columns (verified derived from that xlsx's HC50/SD via the
# standard log-normal HC5 formula), keyed by the same site_id used here.
get_chem_d3d_thresholds <- function(chem_key, site_id_val) {
  df <- eco_risk_data[[chem_key]]
  if (is.null(df)) return(NULL)
  row <- df[as.character(df$site_id) == as.character(site_id_val), ]
  if (nrow(row) == 0) return(NULL)
  has_ss <- !is_na_string(row[["site_specific hc5"]])
  list(
    acute   = if (has_ss) as.numeric(row[["site_specific hc5"]]) else as.numeric(row$generic_hc5),
    chronic = if (has_ss) as.numeric(row[["site_specific hc5/10"]]) else as.numeric(row[["generic_hc5_divided by 10"]]),
    source  = if (has_ss) "site-specific" else "generic"
  )
}
