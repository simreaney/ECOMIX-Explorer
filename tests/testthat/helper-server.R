# Shared harness for the server characterisation tests (test-server.R and
# the other test-*.R files that drive the server): a recording
# MockShinySession, browser emulation, output/message/download descriptors
# and the snapshot helpers built on them. testthat sources helper-*.R files
# alphabetically before the tests, so helper-load.R (the app itself) is
# already loaded when this runs.
#
# How the session is driven
# -------------------------
# * testServer() starts with every input NULL, so each test first sends the
#   inputs a real browser sends on connect (`ui_defaults`, read off the UI /
#   widget definitions in app.R).
# * MockShinySession turns sendCustomMessage / sendInputMessage /
#   registerDataObj / tab-visibility messages into no-ops. The recording
#   session below captures them instead, so leafletProxy() traffic
#   ("leaflet-calls" custom messages, sent when the session flushes) and
#   update*Input() calls are snapshotted step by step.
# * Browser emulation (apply_client_updates()): after every flush, an
#   update*Input()/nav_select() message carrying a `value` is applied back to
#   the input, as the browser would; a select whose options are replaced
#   without a value falls back to its first option (that is what the browser
#   does once selectize re-initialises). Values equal to the current one are
#   not re-sent (the client only sends changes), while events (clicks,
#   hovers, popup links) always are.
# * MockShinySession evaluates *every* output at each flush (a real session
#   suspends hidden ones), so "hidden tab" outputs are characterised too.
#   Plot outputs are rendered at the mock's fixed 600 x 400 px; their PNG
#   data URI is hashed (deterministic on one machine) next to the coordmap
#   domain, and the reactive feeding each plot is snapshotted separately.
#
# Refactor hooks
# --------------
# * When the server is split into modules, map un-namespaced input ids to
#   their namespaced ids in `ecomix_input_ids`; outputs are resolved
#   automatically (an output "x" is found as "<ns>-x" if unique) and map ids
#   in recorded messages are reduced to their last "-" component, so the
#   module prefix does not churn the snapshots.
# * rx() reads reactives from the top-level server environment; once a
#   reactive moves into a module it errors loudly so the test is re-pointed
#   rather than silently dropped.
#
# Tests marked "BUG (fixed in Phase 3): expected to change" record current,
# known-wrong behaviour on purpose.

# ---- refactor hooks -------------------------------------------------------------

ecomix_input_ids <- c()  # e.g. c(map_layers = "map-map_layers") after the module split

in_id <- function(id) {
  if (id %in% names(ecomix_input_ids)) unname(ecomix_input_ids[[id]]) else id
}

strip_ns <- function(id) if (is.null(id)) NULL else sub("^.*-", "", id)

defined_outputs <- function(session) {
  names(.subset2(session, ".__enclos_env__")$private$outs)
}

resolve_output <- function(session, id) {
  outs <- defined_outputs(session)
  if (id %in% outs) return(id)
  hit <- outs[endsWith(outs, paste0("-", id))]
  if (length(hit) == 1) hit else id
}

rx <- function(session, name) {
  if (!exists(name, envir = session$env, inherits = FALSE)) {
    stop("Reactive '", name, "' is no longer in the top-level server environment ",
         "(moved into a module?) - re-point this test at its new home.")
  }
  f <- get(name, envir = session$env, inherits = FALSE)
  if (is.function(f)) f() else f
}

rx_safe <- function(session, name) {
  tryCatch(rx(session, name), error = function(e) {
    if (grepl("no longer in the top-level", conditionMessage(e))) stop(e)
    structure(list(error = paste(class(e), collapse = "/"), message = conditionMessage(e)),
              class = "ecomix_error")
  })
}

# ---- UI defaults --------------------------------------------------------------------

# Value a browser sends for an untouched actionButton/actionLink.
action0 <- structure(0L, class = c("shinyActionButtonValue", "integer"))

ui_defaults <- list(
  # Navigation (first panel / first pill of each navset)
  main_nav = "Map",
  site_entity_type = "measured",
  de_entity_type = "subbasin",
  projection_tabs = "Yearly",
  chem_d3d_grouping = "daily",
  # Map
  subbasin_search = "",                 # selectize, no choices yet
  map_layers = "subbasins",
  eco_risk_chemical = "fipronil",
  map_basemap = "osm",
  ctx_map_go = action0,
  # Site Details (popover inputs are bound at page load - bslib moves the
  # popover template into the DOM on connect)
  site_grid_wb_stat = "median",
  site_ts_chemical_1 = measured_chemical_cols[1],  # first choice: "6PPD-Q_ng L⁻¹"
  site_ts_chemical_2 = measured_chemical_cols[1],
  site_ts_chemical_3 = measured_chemical_cols[1],
  site_ts_chemical_4 = measured_chemical_cols[1],
  site_grid_opts = action0,
  site_ts_opts = action0,
  # Hydro Explorer
  de_waterbody_search = "",
  scenario = c("Baseline", "SSP585"),
  prediction_period = "2070-2079",
  plot_type = "Absolute",
  prediction_percentile = c("p10", "p50", "p90"),
  prediction_variable = "discharge",
  climate_variable = "Precipitation",
  # selected = "Monthly" is a label, not a value, so the browser falls back
  # to the first option's value.
  climate_resolution = "monthly",
  observation_variable = "",
  climate_opts = action0,
  observation_opts = action0,
  ctx_de_go = action0,
  # Chemical Explorer
  chem_d3d_key = "6ppdq",
  chem_d3d_site = "",
  chem_d3d_group_stat = "median",
  # Spatial Datasets
  prediction_variable_spatial = "discharge",
  prediction_period_spatial = "SSP585|2070-2079",
  prediction_percentile_spatial = "99",
  prediction_stat_spatial_chem = "median",
  sp_basemap = "osm",
  ctx_sp_go = action0,
  # Download
  dl_variable = "precip",
  dl_chem_stat = "median",
  dl_data_type = "tabular",
  dl_spatial_layer = "subbasins",
  dl_format = "csv",
  ctx_dl_go = action0
)

# ---- recording mock session --------------------------------------------------------

new_rec_session <- function() {
  session <- shiny::MockShinySession$new()
  log <- new.env(parent = emptyenv())
  log$m <- list()
  log$applied <- 0L
  log$snapped <- 0L
  log$warnings <- character()
  rec <- function(entry) log$m[[length(log$m) + 1L]] <- entry
  session$sendCustomMessage <- function(type, message) {
    rec(list(kind = "custom", type = type, message = message))
  }
  session$sendInputMessage <- function(inputId, message) {
    rec(list(kind = "input", id = inputId, message = message))
  }
  session$sendChangeTabVisibility <- function(inputId, target, type) {
    rec(list(kind = "tab", id = inputId, target = target, type = type))
  }
  session$registerDataObj <- function(name, data, filterFunc) {
    rec(list(kind = "dataobj", id = name, data = data))
    paste0("mock-dataobj/", name)
  }
  for (nm in c("sendInsertUI", "sendRemoveUI", "sendModal", "sendNotification",
               "sendInsertTab", "sendRemoveTab", "updateQueryString", "reload")) {
    local({
      kind <- nm
      session[[kind]] <- function(...) rec(list(kind = kind, args = list(...)))
    })
  }
  session$userData$ecomix_log <- log
  session
}

ses_log <- function(session) session$userData$ecomix_log

with_warnings <- function(session, expr) {
  log <- ses_log(session)
  withCallingHandlers(expr, warning = function(w) {
    log$warnings <- c(log$warnings, conditionMessage(w))
    invokeRestart("muffleWarning")
  })
}

cur_input <- function(session, id) session$input[[id]]

html_unescape <- function(x) {
  x <- gsub("&lt;", "<", x, fixed = TRUE)
  x <- gsub("&gt;", ">", x, fixed = TRUE)
  x <- gsub("&quot;", "\"", x, fixed = TRUE)
  x <- gsub("&#39;", "'", x, fixed = TRUE)
  gsub("&amp;", "&", x, fixed = TRUE)
}

first_option_value <- function(options_html) {
  m <- regmatches(options_html, regexpr("<option value=\"[^\"]*\"", options_html))
  if (length(m) == 0) return(NULL)
  html_unescape(sub("^<option value=\"([^\"]*)\"$", "\\1", m))
}

# Apply update*Input() / nav_select() messages back to the inputs, as the
# browser would (see the header comment). Repeats while updates trigger more.
apply_client_updates <- function(session, max_rounds = 6) {
  log <- ses_log(session)
  for (round in seq_len(max_rounds)) {
    idx <- seq_along(log$m)
    new <- log$m[idx > log$applied]
    log$applied <- length(log$m)
    upd <- list()
    for (m in new) {
      if (!identical(m$kind, "input")) next
      msg <- m$message
      val <- if (!is.null(msg$value) && length(msg$value) > 0) {
        as.character(unlist(msg$value))
      } else if (!is.null(msg$options)) {
        first_option_value(as.character(msg$options))
      } else {
        NULL
      }
      if (!is.null(val)) upd[[m$id]] <- val
    }
    if (length(upd) > 0) {
      same <- vapply(names(upd), function(id) identical(upd[[id]], cur_input(session, id)), logical(1))
      upd <- upd[!same]
    }
    if (length(upd) == 0) break
    with_warnings(session, do.call(session$setInputs, upd))
  }
  invisible(session)
}

# Send inputs: values equal to the current input are dropped (the browser
# only sends changes); `.events` are always sent (priority: "event").
send <- function(session, ..., .events = list()) {
  vals <- list(...)
  if (length(vals) > 0) {
    keep <- vapply(names(vals), function(id) !identical(vals[[id]], cur_input(session, in_id(id))), logical(1))
    vals <- vals[keep]
  }
  all <- c(vals, .events)
  if (length(all) > 0) {
    names(all) <- vapply(names(all), in_id, character(1))
    with_warnings(session, do.call(session$setInputs, all))
  }
  apply_client_updates(session)
}

start_session <- function(session) {
  do.call(send, c(list(session), ui_defaults))
}

# ---- message / output descriptors ---------------------------------------------------

summ_leaflet_call <- function(call) {
  args <- call$args
  out <- list(method = call$method)
  strs <- unlist(lapply(args, function(a) if (is.character(a) && length(a) == 1) a))
  if (length(strs) > 0) out$strings <- paste(strs, collapse = " | ")
  grp <- unlist(lapply(args, function(a) if (is.list(a) && is.character(a$group)) a$group))
  if (length(grp) > 0) out$option_group <- paste(grp, collapse = " | ")
  if (call$method %in% c("fitBounds", "setView", "flyTo", "flyToBounds")) {
    nums <- unlist(Filter(is.numeric, args))
    out$values <- paste(num_txt(nums), collapse = " ")
  }
  if (length(args) > 0) out$first_arg_length <- length(args[[1]])
  out$args_hash <- canon_hash(args)
  out
}

summ_data <- function(data, ordered = TRUE) {
  if (is.data.frame(data)) {
    d <- if (ordered) data else canon_rows(data)
    out <- list(dim = paste(dim(d), collapse = " x "), names = paste(names(d), collapse = " | "),
                hash = canon_hash(d))
    if (!ordered) out$row_order <- "canonicalised"
    vals <- if ("value" %in% names(d)) d$value else d[[1]]
    out$head <- paste(utils::head(as.character(vals), 5), collapse = " | ")
    out
  } else {
    list(class = paste(class(data), collapse = "/"), hash = canon_hash(data))
  }
}

summ_msg <- function(m) {
  switch(m$kind,
    custom = if (identical(m$type, "leaflet-calls")) {
      list(leaflet = strip_ns(m$message$id), calls = lapply(m$message$calls, summ_leaflet_call))
    } else {
      list(custom = m$type, hash = canon_hash(m$message))
    },
    input = {
      msg <- m$message
      out <- list(input = strip_ns(m$id), fields = paste(sort(names(msg)), collapse = " | "))
      if (!is.null(msg$label)) out$label <- as.character(msg$label)
      if (!is.null(msg$value)) out$value <- paste(as.character(unlist(msg$value)), collapse = " | ")
      if (!is.null(msg$options)) {
        opts <- as.character(msg$options)
        out$n_options <- lengths(regmatches(opts, gregexpr("<option ", opts)))
        out$options_hash <- canon_hash(opts)
      }
      out
    },
    tab = list(tab = strip_ns(m$id), target = m$target, action = m$type),
    # The DT preview's rows come straight from Arrow scans whose order is not
    # defined, so its data is canonicalised; selectize choices are ordered.
    dataobj = list(dataobj = strip_ns(m$id),
                   data = summ_data(m$data, ordered = !identical(strip_ns(m$id), "data_table"))),
    list(other = m$kind, hash = canon_hash(m$args))
  )
}

# Target of a recorded message: the map, input, tab set or data object it
# addresses.
msg_target <- function(m) {
  switch(m$kind,
    custom = if (identical(m$type, "leaflet-calls")) paste0("leaflet:", strip_ns(m$message$id))
             else paste0("custom:", m$type),
    input = paste0("input:", strip_ns(m$id)),
    tab = paste0("tab:", strip_ns(m$id)),
    dataobj = paste0("dataobj:", strip_ns(m$id)),
    paste0("other:", m$kind)
  )
}

# Snapshot everything the session sent since the previous snap, plus any
# warnings raised while flushing.
#
# Messages are grouped by target (sorted by target, original order kept
# within each target). Messages to different targets commute, and the order
# observers fire in within one flush - hence how their messages interleave -
# is not something the app defines: it follows reactive context ids and
# observer creation order (see run_server()), which the module split will
# change. The order of messages to the *same* target is kept and pinned.
snap_messages <- function(session, step) {
  log <- ses_log(session)
  idx <- seq_along(log$m)
  new <- log$m[idx > log$snapped]
  if (length(new) > 1) {
    tgt <- vapply(new, msg_target, character(1))
    new <- new[order(tgt, seq_along(new), method = "radix")]
  }
  log$snapped <- length(log$m)
  warns <- unique(log$warnings)
  log$warnings <- character()
  desc <- list(step = step, n_messages = length(new), messages = lapply(new, summ_msg))
  if (length(warns) > 0) desc$warnings <- warns
  expect_snapshot_value(desc, style = "json2")
}

# Mark everything recorded so far as seen and drop pending warnings, so the
# next snap_messages() starts here (for tests whose startup is already
# pinned by test-server.R).
skip_messages <- function(session) {
  log <- ses_log(session)
  log$snapped <- length(log$m)
  log$warnings <- character()
  invisible(session)
}

flatten_txt <- function(x) {
  v <- unlist(x)
  if (is.null(v)) return("")
  vals <- if (is.numeric(v)) num_txt(v) else as.character(v)
  paste(paste0(names(v), "=", vals), collapse = " ")
}

desc_value <- function(val) {
  if (inherits(val, "ecomix_error")) return(unclass(val))
  if (is.null(val)) return("NULL")
  if (inherits(val, "json")) {
    js <- as.character(val)
    methods <- regmatches(js, gregexpr("\"method\":\"[A-Za-z]+\"", js))[[1]]
    return(list(widget_hash = canon_hash(js), nchar = nchar(js),
                methods = paste(sub("\"method\":\"([A-Za-z]+)\"", "\\1", methods), collapse = " > ")))
  }
  if (is.list(val) && !is.null(val$src)) {
    panels <- lapply(val$coordmap$panels, function(p) {
      list(domain = flatten_txt(p$domain), mapping = flatten_txt(p$mapping))
    })
    return(list(png_hash = canon_hash(val$src), size = paste(val$width, "x", val$height),
                alt = val$alt %||% "", panels = panels))
  }
  if (is.list(val) && !is.null(val$html)) {
    html <- as.character(val$html)
    out <- list(html_hash = canon_hash(html), nchar = nchar(html))
    out$html <- if (nchar(html) <= 1500) html else paste0(substr(html, 1, 600), " ...[truncated]")
    deps <- vapply(val$deps, function(d) paste0(d$name, "@", d$version), character(1))
    if (length(deps) > 0) out$deps <- paste(deps, collapse = " | ")
    return(out)
  }
  if (is.character(val)) return(paste(val, collapse = "\n"))
  list(class = paste(class(val), collapse = "/"), hash = canon_hash(val))
}

out_val <- function(session, id) {
  tryCatch(session$getOutput(resolve_output(session, id)), error = function(e) {
    structure(list(error = paste(class(e), collapse = "/"), message = conditionMessage(e)),
              class = "ecomix_error")
  })
}

# `order_dependent`: plot outputs whose pixels depend on the (undefined) row
# order of their Arrow-sourced data - e.g. the modelled occurrence grid,
# where overlapping tile borders are painted in data order. Their PNG hash
# is not compared (it changed between identical runs); the coordmap still
# is, and the feeding reactive is snapshotted in canonical order.
snap_outputs <- function(session, step, ids, order_dependent = character()) {
  vals <- lapply(ids, function(id) {
    d <- desc_value(out_val(session, id))
    if (id %in% order_dependent && is.list(d) && !is.null(d$png_hash)) {
      d$png_hash <- "not compared: pixels depend on Arrow scan order"
    }
    d
  })
  names(vals) <- ids
  expect_snapshot_value(list(step = step, outputs = vals), style = "json2")
}

snap_rx <- function(session, name, ordered = TRUE) {
  v <- rx_safe(session, name)
  if (inherits(v, "ecomix_error")) {
    expect_snapshot_value(list(reactive = name, value = unclass(v)), style = "json2")
  } else {
    snap_obj(v, paste0(name, "()"), ordered = ordered)
  }
}

# Run a downloadHandler output and describe the file it writes. Contents are
# read back and hashed rather than the raw bytes, since xlsx/gpkg/zip embed
# timestamps.
describe_download <- function(path, ordered) {
  ext <- tolower(tools::file_ext(path))
  data <- switch(ext,
    csv = utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE),
    xlsx = as.data.frame(readxl::read_excel(path)),
    parquet = as.data.frame(arrow::read_parquet(path)),
    gpkg = sf::st_read(path, quiet = TRUE),
    zip = {
      dir <- tempfile("ecomix_zip_")
      utils::unzip(path, exdir = dir, junkpaths = TRUE)
      shp <- list.files(dir, pattern = "\\.shp$", full.names = TRUE)
      zipped <- sort(basename(list.files(dir)))
      structure(sf::st_read(shp[1], quiet = TRUE), zipped_files = zipped)
    },
    NULL
  )
  out <- list(file = basename(path), ext = ext)
  if (!is.null(attr(data, "zipped_files"))) out$zipped_files <- paste(attr(data, "zipped_files"), collapse = " | ")
  if (inherits(data, "sf")) {
    out$crs_epsg <- sf::st_crs(data)$epsg
    attrs <- sf::st_drop_geometry(data)
    out$attributes <- summ_data(attrs, ordered = ordered)
    out$geometry_hash <- canon_hash(sf::st_geometry(data))
    out$geometry_type <- paste(unique(as.character(sf::st_geometry_type(data))), collapse = "/")
  } else if (is.data.frame(data)) {
    d <- if (ordered) data else canon_rows(data)
    out$dim <- paste(dim(d), collapse = " x ")
    out$names <- paste(names(d), collapse = " | ")
    out$types <- paste(vapply(d, function(c) class(c)[1], ""), collapse = " | ")
    out$hash <- canon_hash(d)
    if (!ordered) out$row_order <- "canonicalised (sorted by all columns)"
    out$head <- ecomix_head_values(d)
  } else {
    out$bytes_hash <- canon_hash(readBin(path, "raw", file.size(path)))
  }
  out
}

snap_download <- function(session, id, step, ordered = TRUE) {
  # Only warnings raised by the download itself are reported here; any left
  # over from earlier flushes stay queued for the next snap_messages().
  log <- ses_log(session)
  pending <- log$warnings
  log$warnings <- character()
  res <- NULL
  # capture.output(): utils::zip() (shapefile download) prints to stdout.
  utils::capture.output(res <- tryCatch(
    with_warnings(session, session$getOutput(resolve_output(session, id))),
    error = function(e) e
  ))
  desc <- if (inherits(res, "error")) {
    list(error = paste(class(res), collapse = "/"), message = conditionMessage(res))
  } else {
    describe_download(res, ordered)
  }
  warns <- unique(log$warnings)
  log$warnings <- pending
  out <- list(step = step, download = id, result = desc)
  if (length(warns) > 0) out$warnings <- warns
  expect_snapshot_value(out, style = "json2")
}

# Hover payload for a plotOutput at a data point, built from the rendered
# plot's coordmap (pixel position from the panel's domain -> range mapping),
# in the shape the browser sends.
hover_at <- function(session, plot_id, x, y) {
  val <- session$getOutput(resolve_output(session, plot_id))
  p <- val$coordmap$panels[[1]]
  lim <- p$domain$discrete_limits
  xn <- if (!is.null(lim$x) && is.character(x)) match(x, lim$x) else as.numeric(x)
  yn <- if (!is.null(lim$y) && is.character(y)) match(y, lim$y) else as.numeric(y)
  sc <- function(v, d0, d1, r0, r1) r0 + (v - d0) / (d1 - d0) * (r1 - r0)
  px <- sc(xn, p$domain$left, p$domain$right, p$range$left, p$range$right)
  py <- sc(yn, p$domain$bottom, p$domain$top, p$range$bottom, p$range$top)
  list(x = xn, y = yn, coords_css = list(x = px, y = py), coords_img = list(x = px, y = py),
       img_css_ratio = list(x = 1, y = 1), mapping = p$mapping, domain = p$domain,
       range = p$range, log = p$log, .nonce = 1)
}

# Reactive context ids come from one process-wide counter, and shiny
# invalidates a reactive's dependents in *string* order of their ids
# (Dependents$invalidate(): values(sort = TRUE), so "1000" sorts before
# "999"). The order observers fire in within a flush therefore depended on
# how many contexts earlier tests had created - the same session gave
# differently ordered messages depending on which test files ran first.
# Every test session starts the counter at the same 8-digit value, so ids
# have a fixed width (string order = creation order) and a session behaves
# the same whatever ran before it. Ids are reused across sessions, which is
# safe: earlier sessions are closed and the app has no reactive objects
# shared between sessions.
reset_reactive_ids <- function() {
  env <- shiny:::.getReactiveEnvironment()
  if (!is.integer(env$.nextId)) stop("shiny's reactive id counter changed - update reset_reactive_ids()")
  env$.nextId <- 10000000L
  invisible()
}

# testServer() quotes its `expr`; forward the caller's block with {{ }} so it
# is evaluated in testServer's mask (session / input / output of the server).
run_server <- function(expr) {
  reset_reactive_ids()
  session <- new_rec_session()
  shiny::testServer(server, {{ expr }}, session = session)
}

# Output id groups.
ctx_subbasin_ids <- function(suffix) {
  paste0(c("ctx_id_", "ctx_sub_", "ctx_area_", "ctx_precip_", "ctx_temp_", "ctx_obs_"), suffix)
}
hydro_plot_ids <- c("climate_plot", "observation_plot", "projections_yearly_plot",
                    "projections_monthly_plot", "projections_cfc_plot", "projections_daily_plot")
hydro_text_ids <- c("climate_pill", "observation_pill", "projection_title")
site_ids <- c("ctx_idlabel_site", "ctx_id_site", "ctx_sub_site", "ctx_summary_site",
              "ctx_landcover_site", "site_chem_grid_note", "site_chem_grid", "site_time_series",
              "site_chem_grid_tooltip")
chem_d3d_ids <- c("chem_d3d_threshold_note", "chem_d3d_conc_plot", "chem_d3d_sspaf_plot",
                  "chem_d3d_src_col", "chem_d3d_treemap_avg", "chem_d3d_treemap_day_title",
                  "chem_d3d_treemap_day")
spatial_ids <- c("prediction_map", "ctx_value_sp", "ctx_rank_sp", "ctx_range_sp")
download_ids <- c("ctx_rows_dl", "ctx_combos_dl", "ctx_size_dl", "dl_preview_note", "data_table")
all_output_ids <- c(
  ctx_subbasin_ids("map"), ctx_subbasin_ids("de"), ctx_subbasin_ids("sp"), ctx_subbasin_ids("dl"),
  "ctx_map_de", "ctx_map_sp", "ctx_map_dl", "ctx_map_site",
  "basemap", "opcat_rail", "measured_rail", "modelled_rail", "eco_risk_detail_card",
  site_ids, hydro_text_ids, hydro_plot_ids, chem_d3d_ids, spatial_ids, download_ids
)

# ---- selection state ---------------------------------------------------------------

# reactiveVals that carry a selection between tabs. Read with rx(), so one
# that moves into a module fails loudly (re-point it) instead of vanishing.
state_rvals <- c("rv", "rv_measured_site", "rv_waterbody", "rv_de_waterbody",
                 "rv_eco_risk_chemical", "rv_eco_risk_site", "chem_d3d_pending_site",
                 "chem_d3d_skip_carry", "last_wb_sel", "chem_d3d_hover_date")

# Inputs the navigation / selection observers write to (via update*Input and
# nav_select messages, applied back by the browser emulation).
state_inputs <- c("main_nav", "site_entity_type", "de_entity_type", "projection_tabs",
                  "subbasin_search", "de_waterbody_search", "prediction_variable",
                  "prediction_percentile", "observation_variable", "chem_d3d_key",
                  "chem_d3d_site", paste0("site_ts_chemical_", 1:4),
                  "dl_variable", "dl_data_type", "dl_spatial_layer", "dl_format")

# One-line text for a value, keeping its class (rv() holding 3310, 3310L or
# an EA code string are different states - a type change must show).
val_txt <- function(v) {
  if (inherits(v, "ecomix_error")) return(paste("ERROR:", v$message))
  if (is.null(v)) return("NULL")
  if (is.list(v)) {
    return(paste0("list(", paste(names(v), vapply(v, val_txt, character(1)), sep = "=", collapse = ", "), ")"))
  }
  vals <- if (is.numeric(v) && !inherits(v, "Date")) num_txt(v) else as.character(v)
  paste0("<", paste(class(v), collapse = "/"), "> ",
         if (length(v) == 0) "(empty)" else paste(ifelse(is.na(vals), "NA", vals), collapse = " | "))
}

# Snapshot the selection state: every selection reactiveVal and the inputs
# the navigation observers drive.
snap_state <- function(session, step) {
  rvals <- lapply(state_rvals, function(nm) val_txt(rx_safe(session, nm)))
  names(rvals) <- state_rvals
  inputs <- lapply(state_inputs, function(id) val_txt(cur_input(session, in_id(id))))
  names(inputs) <- state_inputs
  expect_snapshot_value(list(step = step, reactive_values = rvals, inputs = inputs), style = "json2")
}

# ---- leaflet map model --------------------------------------------------------------
#
# snap_messages() pins *how* a map is updated (the raw leafletProxy()
# traffic). The planned map work (add-once layers in panes, a cached
# basemap, a restyled Spatial map) changes that traffic on purpose, so
# lf_state() replays the traffic the way leaflet.js does and reports what is
# *on screen*: the visible layers per group, their paint order, the legends
# and the view. Two implementations that put the same things on the map give
# the same lf_state(), however they get there.
#
# leaflet.js semantics modelled (leaflet 2.2.x, htmlwidgets/assets/leaflet.js,
# LayerManager and ControlStore):
# * Layers are added per feature. A feature whose layerId is a *string*
#   replaces any earlier feature with the same (category, layerId); numeric
#   ids are not indexed by leaflet.js and never replace anything.
#   Categories: tile (addTiles/addProviderTiles), shape (polygons, polylines,
#   rectangles, circles), marker (markers, circle markers).
# * Each group has one container, created visible on first use - also by
#   hideGroup()/showGroup(), which create it when missing. Layers added to a
#   hidden container stay hidden; clearGroup() empties a container but keeps
#   it, and its visibility.
# * Legends are controls. addLegend() without a layerId always adds another
#   one (leaflet.js gives it a fresh stamp id). hideGroup()/clearGroup() do
#   not remove a legend, even one given a group (only zoom-based group
#   visibility and the layers control fire the events that do).
# * Paint order (stacking within a pane): a batch goes on top when it is
#   added to a visible container, or when its hidden container is shown.
# * A renderLeaflet() re-render replaces the whole map (leaflet.js discards
#   stashed proxy calls), so proxy calls are replayed from the start of the
#   send_lf() during which the widget output last changed.
# * setShapeStyle(layerIds, style) - the app's own leaflet.js method (page
#   head in app.R) - restyles the live shapes with those string layerIds;
#   each style entry is one value for all or one per id. A restyled batch
#   reports a `restyle` hash, and lf_effective() gives each shape's style as
#   it is now, however it got there.
#
# Each add call is one "batch". Its content is split into short hashes so a
# diff says what changed: geom (coordinates / tile url), ids (layerId),
# popup, label, style (scalar options and other per-call arguments), vstyle
# (per-feature options, e.g. a data-driven fillColor), with `pane` taken out
# of the options and reported on its own.

lf_maps <- c("basemap", "prediction_map", "ctx_map_de", "ctx_map_sp", "ctx_map_dl", "ctx_map_site")

lf_json <- function(x) jsonlite::parse_json(shiny:::toJSON(x), simplifyVector = FALSE)

# Short content hash; "-" for an absent argument (NULL, or a list of NULLs).
# Hashes JSON-parsed values directly with rlang::hash() (plain lists of
# numbers and UTF-8 strings, so canon_hash()'s normalisation is not needed -
# and it is slow on deeply nested geometry).
short_hash <- function(x) {
  if (length(x) == 0 || (is.list(x) && all(vapply(x, function(el) length(el) == 0, logical(1))))) return("-")
  substr(rlang::hash(x), 1, 10)
}

# Call descriptors are cached across sessions, keyed by a hash of the call
# as R sent it (the same layer is re-sent on every toggle).
lf_cache <- new.env(parent = emptyenv())

# Argument positions (1-based) of the leaflet.js methods that add layers.
lf_layer_methods <- list(
  addTiles         = list(cat = "tile",   geom = 1,   id = 2, group = 3, options = 4),
  addProviderTiles = list(cat = "tile",   geom = 1,   id = 2, group = 3, options = 4),
  addWMSTiles      = list(cat = "tile",   geom = 1,   id = 2, group = 3, options = 4),
  addPolygons      = list(cat = "shape",  geom = 1,   id = 2, group = 3, options = 4, popup = 5:6, label = 7:8),
  addPolylines     = list(cat = "shape",  geom = 1,   id = 2, group = 3, options = 4, popup = 5:6, label = 7:8),
  addRectangles    = list(cat = "shape",  geom = 1:4, id = 5, group = 6, options = 7, popup = 8:9, label = 10:11),
  addCircles       = list(cat = "shape",  geom = 1:2, id = 4, group = 5, options = 6, popup = 7:8, label = 9:10),
  addCircleMarkers = list(cat = "marker", geom = 1:2, id = 4, group = 5, options = 6, popup = 9:10, label = 11:12),
  addMarkers       = list(cat = "marker", geom = 1:2, id = 4, group = 5, options = 6, popup = 7:8, label = 11:12)
)

lf_is_array <- function(v) is.list(v) && is.null(names(v))

# State-independent description of one call (args already JSON-shaped, the
# way the browser receives them).
lf_describe_call <- function(method, args) {
  spec <- lf_layer_methods[[method]]
  if (is.null(spec)) return(list(method = method, args = args))
  get <- function(i) if (length(args) >= i) args[[i]] else NULL
  n <- if (spec$cat == "tile") 1L else max(1L, length(get(spec$geom[1])))
  if (spec$cat != "tile" && !lf_is_array(get(spec$geom[1]))) n <- 1L
  ids_arg <- get(spec$id)
  ids <- if (is.null(ids_arg)) rep(NA_character_, n) else {
    v <- if (lf_is_array(ids_arg)) ids_arg else list(ids_arg)
    v <- rep_len(v, n)
    vapply(v, function(el) if (is.character(el) && length(el) == 1) el else NA_character_, character(1))
  }
  grp <- get(spec$group)
  group <- if (is.null(grp)) NA_character_ else as.character(unlist(grp))[1]
  opts <- get(spec$options) %||% list()
  pane <- if (is.null(opts$pane)) "-" else as.character(opts$pane)
  opts$pane <- NULL
  vec <- vapply(opts, lf_is_array, logical(1))
  used <- c(spec$geom, spec$id, spec$group, spec$options, spec$popup, spec$label)
  extra <- args[setdiff(seq_along(args), used)]
  comp <- c(
    geom = short_hash(lapply(spec$geom, get)),
    ids = short_hash(ids_arg),
    popup = short_hash(if (!is.null(spec$popup)) lapply(spec$popup, get)),
    label = short_hash(if (!is.null(spec$label)) lapply(spec$label, get)),
    style = short_hash(list(opts[!vec], extra)),
    vstyle = short_hash(opts[vec])
  )
  # Leaflet's default pane: tiles in tilePane, markers in markerPane, vector
  # paths (polygons, lines, circles, circle markers) in overlayPane.
  default_pane <- if (spec$cat == "tile") "tilePane" else if (method == "addMarkers") "markerPane" else "overlayPane"
  list(method = method, layer = TRUE, cat = spec$cat, n = n, ids = ids, group = group,
       pane = pane, paint_pane = if (pane == "-") default_pane else pane,
       comp = comp, key = paste(method, paste(comp, collapse = "/")), opts = opts)
}

lf_new_state <- function() {
  list(batches = list(), groups = list(), controls = list(), paint = integer(),
       view = "-", panes = character(), other = character(), map_options = "-")
}

lf_num <- function(x) paste(num_txt(unlist(x)), collapse = " ")

lf_group_batches <- function(st, g) {
  which(vapply(st$batches, function(b) identical(b$group, g), logical(1)))
}

lf_apply <- function(st, d) {
  m <- d$method
  if (isTRUE(d$layer)) {
    # string layerIds replace earlier features with the same (category, id)
    new_ids <- d$ids[!is.na(d$ids)]
    if (length(new_ids) > 0) {
      for (j in seq_along(st$batches)) {
        b <- st$batches[[j]]
        if (b$cat != d$cat || !any(b$alive)) next
        hit <- b$alive & !is.na(b$ids) & b$ids %in% new_ids
        if (any(hit)) st$batches[[j]]$alive[hit] <- FALSE
      }
    }
    b <- d
    b$alive <- rep(TRUE, d$n)
    st$batches[[length(st$batches) + 1L]] <- b
    visible <- TRUE
    if (!is.na(d$group)) {
      if (is.null(st$groups[[d$group]])) st$groups[[d$group]] <- TRUE
      visible <- st$groups[[d$group]]
    }
    if (visible) st$paint <- c(st$paint, length(st$batches))
    return(st)
  }
  a <- d$args
  arg_groups <- function() as.character(unlist(a[[1]]))
  switch(m,
    clearGroup = for (g in arg_groups()) {
      for (j in lf_group_batches(st, g)) st$batches[[j]]$alive[] <- FALSE
    },
    hideGroup = for (g in arg_groups()) {
      st$groups[[g]] <- FALSE
      st$paint <- setdiff(st$paint, lf_group_batches(st, g))
    },
    showGroup = for (g in arg_groups()) {
      was <- st$groups[[g]]
      st$groups[[g]] <- TRUE
      if (isFALSE(was)) st$paint <- c(st$paint, lf_group_batches(st, g))
    },
    clearShapes = , clearMarkers = , clearTiles = {
      cat <- c(clearShapes = "shape", clearMarkers = "marker", clearTiles = "tile")[[m]]
      for (j in seq_along(st$batches)) if (st$batches[[j]]$cat == cat) st$batches[[j]]$alive[] <- FALSE
    },
    removeShape = , removeMarker = , removeTiles = {
      cat <- c(removeShape = "shape", removeMarker = "marker", removeTiles = "tile")[[m]]
      ids <- as.character(unlist(a[[1]]))
      for (j in seq_along(st$batches)) {
        b <- st$batches[[j]]
        if (b$cat == cat) st$batches[[j]]$alive[!is.na(b$ids) & b$ids %in% ids] <- FALSE
      }
    },
    addLegend = , addControl = {
      opts <- if (m == "addLegend") a[[1]] else list(html = a[[1]], position = a[[2]], classes = a[[4]])
      id <- if (m == "addLegend") opts$layerId else a[[3]]
      desc <- if (m == "addLegend") {
        paste0("legend '", paste(unlist(opts$title), collapse = " "), "' ", opts$position %||% "-",
               " type=", opts$type %||% "-", " labels=", length(unlist(opts$labels)),
               " group=", opts$group %||% "-", " #", short_hash(opts[setdiff(names(opts), "layerId")]))
      } else {
        paste0("control ", opts$position %||% "-", " #", short_hash(opts))
      }
      if (is.null(id)) {
        st$controls[[length(st$controls) + 1L]] <- list(id = NA_character_, desc = desc)
      } else {
        ids <- vapply(st$controls, function(cc) cc$id, character(1))
        st$controls <- st$controls[!(ids %in% as.character(id))]
        st$controls[[length(st$controls) + 1L]] <- list(id = as.character(id), desc = desc)
      }
    },
    removeControl = {
      ids <- vapply(st$controls, function(cc) cc$id, character(1))
      st$controls <- st$controls[!(ids %in% as.character(unlist(a[[1]])))]
    },
    setShapeStyle = {
      ids <- as.character(unlist(a[[1]]))
      style <- a[[2]]
      for (j in seq_along(st$batches)) {
        b <- st$batches[[j]]
        if (b$cat != "shape" || !any(b$alive)) next
        hit <- which(b$alive & !is.na(b$ids) & b$ids %in% ids)
        if (length(hit) == 0) next
        pos <- match(b$ids[hit], ids)
        rs <- b$restyle %||% list()
        for (k in names(style)) {
          cur <- rs[[k]] %||% rep(NA_character_, b$n)
          cur[hit] <- if (lf_is_array(style[[k]])) lf_chr(style[[k]])[pos] else lf_chr(style[[k]])
          rs[[k]] <- cur
        }
        st$batches[[j]]$restyle <- rs
      }
    },
    clearControls = st$controls <- list(),
    setView = , flyTo = st$view <- paste(m, lf_num(a[[1]]), "zoom", lf_num(a[[2]])),
    fitBounds = , flyToBounds = st$view <- paste(m, lf_num(a[1:4])),
    createMapPane = st$panes[[as.character(a[[1]])]] <- lf_num(a[[2]]),
    st$other <- c(st$other, paste(m, short_hash(a)))
  )
  st
}

# Descriptors for the initial widget (cached by its JSON) and for each
# recorded proxy message (cached by log position).
lf_widget_calls <- function(json) {
  key <- paste0("widget:", rlang::hash(json))
  if (is.null(lf_cache[[key]])) {
    w <- jsonlite::parse_json(json, simplifyVector = FALSE)
    calls <- lapply(w$x$calls, function(cl) lf_describe_call(cl$method, cl$args))
    if (!is.null(w$x$setView)) calls <- c(list(list(method = "setView", args = w$x$setView)), calls)
    if (!is.null(w$x$fitBounds)) calls <- c(list(list(method = "fitBounds", args = w$x$fitBounds)), calls)
    lf_cache[[key]] <- list(calls = calls, options = short_hash(w$x$options))
  }
  lf_cache[[key]]
}

lf_proxy_calls <- function(message) {
  lapply(message$calls, function(cl) {
    key <- paste0("proxy:", rlang::hash(list(cl$method, cl$args)))
    if (is.null(lf_cache[[key]])) lf_cache[[key]] <- lf_describe_call(cl$method, lf_json(cl$args))
    lf_cache[[key]]
  })
}

# On-screen state of map `map_id` now: the current widget output, then every
# proxy call for it recorded since its last re-render (see send_lf()).
lf_state <- function(session, map_id) {
  log <- ses_log(session)
  st <- lf_new_state()
  widget <- out_val(session, map_id)
  if (inherits(widget, "json")) {
    wc <- lf_widget_calls(as.character(widget))
    st$map_options <- wc$options
    for (d in wc$calls) st <- lf_apply(st, d)
  } else {
    st$other <- c(st$other, paste("widget:", paste(unlist(desc_value(widget)), collapse = " ")))
  }
  base <- log$lf_base[[map_id]] %||% 0L
  for (i in seq_along(log$m)) {
    if (i <= base) next
    m <- log$m[[i]]
    if (!identical(m$kind, "custom") || !identical(m$type, "leaflet-calls")) next
    if (!identical(strip_ns(m$message$id), map_id)) next
    for (d in lf_proxy_calls(m$message)) st <- lf_apply(st, d)
  }
  st
}

# JSON-parsed scalars / arrays as text (NA for null), so a style value
# compares the same whichever call set it.
lf_chr <- function(x) {
  if (!lf_is_array(x)) x <- list(x)
  vapply(x, function(el) if (length(el) == 0) NA_character_ else as.character(el), character(1))
}

lf_batch_txt <- function(b) {
  paste0(b$method, " n=", sum(b$alive), if (!all(b$alive)) paste0("/", b$n, " alive#", short_hash(b$alive)),
         " pane=", b$pane, " ", paste(names(b$comp), b$comp, sep = "=", collapse = " "),
         if (!is.null(b$restyle)) paste0(" restyle=", short_hash(lapply(b$restyle, function(v) v[b$alive]))))
}

# Each live shape's style now: its options when added, overridden by any
# setShapeStyle() since (one text vector per option, over the live shapes).
lf_style_now <- function(b) {
  keys <- sort(union(names(b$opts), names(b$restyle)))
  vals <- lapply(keys, function(k) {
    v <- if (is.null(b$opts[[k]])) rep(NA_character_, b$n) else rep_len(lf_chr(b$opts[[k]]), b$n)
    r <- b$restyle[[k]]
    if (!is.null(r)) v[!is.na(r)] <- r[!is.na(r)]
    v[b$alive]
  })
  stats::setNames(vals, keys)
}

# Visible content as lf_visible(), but with each batch's current style in
# place of the style it was added with, so a map restyled in place and one
# drawn afresh with the same values compare equal.
lf_effective <- function(st) {
  live <- vapply(st$batches, function(b) any(b$alive), logical(1))
  vis <- vapply(st$batches, function(b) is.na(b$group) || isTRUE(st$groups[[b$group]]), logical(1))
  keep <- which(live & vis)
  grp <- vapply(st$batches[keep], function(b) if (is.na(b$group)) "(no group)" else b$group, character(1))
  out <- lapply(split(keep, grp), function(js) vapply(st$batches[js], function(b) {
    paste0(b$method, " n=", sum(b$alive), " pane=", b$pane, " geom=", b$comp[["geom"]],
           " ids=", short_hash(b$ids[b$alive]), " popup=", b$comp[["popup"]], " label=", b$comp[["label"]],
           " style_now=", short_hash(lf_style_now(b)))
  }, character(1)))
  out[order(names(out))]
}

# What is visible: per group (sorted; "(no group)" for ungrouped layers) the
# live batches in insertion order. Hidden containers are left out - whether
# a hidden layer exists is an implementation detail, not something on screen.
lf_visible <- function(st) {
  live <- vapply(st$batches, function(b) any(b$alive), logical(1))
  vis <- vapply(st$batches, function(b) is.na(b$group) || isTRUE(st$groups[[b$group]]), logical(1))
  keep <- which(live & vis)
  grp <- vapply(st$batches[keep], function(b) if (is.na(b$group)) "(no group)" else b$group, character(1))
  out <- lapply(split(keep, grp), function(js) vapply(st$batches[js], lf_batch_txt, character(1)))
  out[order(names(out))]
}

lf_summary <- function(st) {
  live <- vapply(st$batches, function(b) any(b$alive), logical(1))
  vis <- vapply(st$batches, function(b) is.na(b$group) || isTRUE(st$groups[[b$group]]), logical(1))
  paint <- st$paint[live[st$paint] & vis[st$paint]]
  # Paint order per pane (panes stack by their own z-index).
  paint_txt <- if (length(paint)) {
    panes <- vapply(st$batches[paint], function(b) b$paint_pane, character(1))
    labels <- vapply(st$batches[paint], function(b) {
      paste0(if (is.na(b$group)) "(no group)" else b$group, ":", b$method)
    }, character(1))
    vapply(split(labels, factor(panes, levels = unique(panes))), paste, character(1), collapse = " > ")
  } else "-"
  if (length(paint)) paint_txt <- paint_txt[order(names(paint_txt))]
  list(
    view = st$view,
    map_options = st$map_options,
    visible_groups = lf_visible(st),
    paint_order = as.list(paint_txt),
    legends_and_controls = if (length(st$controls)) vapply(st$controls, function(cc) cc$desc, character(1)) else "-",
    panes = if (length(st$panes)) paste(names(st$panes), st$panes, sep = "=", collapse = " | ") else "-",
    other_calls = if (length(st$other)) st$other else "-"
  )
}

snap_lf <- function(session, map_id, step) {
  expect_snapshot_value(c(list(step = step, map = map_id), lf_summary(lf_state(session, map_id))),
                        style = "json2")
}

# send() that also notes, per map, where its latest render starts in the log
# (a widget re-render discards earlier proxy calls).
send_lf <- function(session, ...) {
  log <- ses_log(session)
  if (is.null(log$lf_base)) log$lf_base <- list()
  if (is.null(log$lf_key)) log$lf_key <- list()
  before <- length(log$m)
  send(session, ...)
  for (id in lf_maps) {
    v <- out_val(session, resolve_output(session, id))
    k <- if (inherits(v, "json")) rlang::hash(as.character(v)) else "no widget"
    if (!identical(k, log$lf_key[[id]])) {
      log$lf_base[[id]] <- if (is.null(log$lf_key[[id]])) 0L else before
      log$lf_key[[id]] <- k
    }
  }
  invisible(session)
}

start_session_lf <- function(session) do.call(send_lf, c(list(session), ui_defaults))
