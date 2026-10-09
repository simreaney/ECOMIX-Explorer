#!/usr/bin/env Rscript
# ECOMIX Explorer performance benchmark
# =====================================
#
# Usage (from the repo root):
#
#   Rscript scripts/bench_app.R [label] [out.csv]
#
# Prints a markdown table of metrics and, when out.csv is given, appends one
# row per metric (label, metric, value, unit) to it. Run it before and after
# each optimisation phase with a different label, e.g.
#
#   Rscript scripts/bench_app.R baseline bench/results.csv
#   Rscript scripts/bench_app.R phase1   bench/results.csv
#
# Environment variables:
#   BENCH_GROUPS   comma-separated subset of groups to run (default: all), e.g.
#                  BENCH_GROUPS=startup,chem Rscript scripts/bench_app.R
#   BENCH_VERBOSE  "1" to echo each child process's log
#
# How it works
# ------------
# Each measurement group runs in a FRESH `Rscript` child process (this same
# file, invoked with --child <group> <result.rds>) so RSS figures are not
# polluted by earlier groups. The child loads the app the same way Shiny does
# (R/*.R sourced alphabetically when R/ exists, then app.R's top-level
# expressions minus shinyApp()/runApp() and the trailing bare `app`) with
# ECOMIX_AUTORUN=0, so no server or browser is ever started. On macOS/Linux
# the child is wrapped in /usr/bin/time to get its peak RSS.
#
# Server-side measurements use shiny::testServer() on the real `server`
# function with a MockShinySession that is modified in two ways:
#   * Outputs are GATED: every output starts hidden and is only rendered once
#     the bench "shows" it. A plain MockShinySession renders every output on
#     every flush (outputs are eager observers there), whereas a browser
#     session suspends hidden-tab outputs - gating keeps each timing about
#     the one output being measured.
#   * session$sendCustomMessage is captured, so leafletProxy() traffic can be
#     measured (bytes = nchar of the JSON shiny would send).
# The app-level cache (session$appcache) is shared between the mock sessions
# of one process, as it is between browser sessions of one app process, so a
# bindCache(cache = "app") added later shows up in the second-session timings.
#
# "bytes" metrics count the JSON the client would receive for that step: the
# serialised values of the outputs that re-rendered plus any custom (e.g.
# leaflet-calls) messages. A refactor that moves work from re-rendering an
# output to a leafletProxy() update still shows up comparably.
#
# Every metric is wrapped in tryCatch: a failure records NA and is listed at
# the end of the report instead of aborting the run. Ids/defaults the bench
# relies on are collected in bench_cfg / bench_ui_defaults() below - update
# them there if a refactor renames inputs (e.g. module namespaces).

# ---------------------------------------------------------------------------
# Configuration
# ---------------------------------------------------------------------------

bench_cfg <- list(
  chem_key        = "fipronil",          # daily-by-subbasin chemical
  chem_var        = "chem_fipronil",     # its Model variable id
  wbchem_var      = "wbchem_fipronil",   # modelled-by-water-body variable id
  sb_single       = 353,                 # chem + HYPE subbasin, single SITE_ID
  sb_multi        = c(765, 2592, 3793),  # subbasins with several chem SITE_IDs
  sb_plot         = 2592,                # HYPE + chem subbasin used for plots (multi-site)
  sb_alt          = 353,                 # another HYPE + chem subbasin (for revisits)
  waterbody       = "GB104027052280",    # modelled water body
  waterbody_alt   = "GB104027057290",
  measured_site   = "Ai1",               # measured site (Site Details)
  measured_alt    = "Ai2",
  d3d_key         = "imidacloprid",      # Chemical Explorer chemical with source apportionment
  d3d_site        = NULL,                # NULL = 1st / 2nd site offered for d3d_key
  d3d_site_alt    = NULL,
  hover_dates     = as.Date(c("2020-06-15", "2021-03-01")),
  spatial_pcts    = c("50", "1", "99"),
  spatial_dl_var  = "water_temperature", # variable for the spatial download (not otherwise used)
  # Input/output id renames, e.g. after a move into Shiny modules:
  # list(basemap = "map-basemap", map_layers = "map-map_layers"). Every id
  # the bench uses (inputs it sets and outputs it shows/reads) goes through
  # bench_id(), so a rename only needs an entry here.
  id_map          = list()
)

bench_id <- function(id) {
  vapply(id, function(x) bench_cfg$id_map[[x]] %|||% x, character(1), USE.NAMES = FALSE)
}

# Initial input values a browser would send for the UI defaults (testServer
# starts with every input NULL). Kept literal so the bench does not depend
# on global object names that may move during refactors.
bench_ui_defaults <- function() {
  list(
    main_nav = "Map",
    subbasin_search = "",
    map_layers = "subbasins",
    eco_risk_chemical = "fipronil",
    map_basemap = "osm",
    site_entity_type = "measured",
    site_grid_wb_stat = "median",
    de_entity_type = "subbasin",
    de_waterbody_search = "",
    scenario = c("Baseline", "SSP585"),
    climate_variable = "Precipitation",
    climate_resolution = "monthly",
    prediction_variable = "discharge",
    prediction_percentile = c("p10", "p50", "p90"),
    prediction_period = "2070-2079",
    plot_type = "Absolute",
    chem_d3d_key = "6ppdq",
    chem_d3d_site = "",
    chem_d3d_grouping = "daily",
    chem_d3d_group_stat = "median",
    prediction_variable_spatial = "discharge",
    prediction_period_spatial = "SSP585|2070-2079",
    prediction_percentile_spatial = "99",
    prediction_stat_spatial_chem = "median",
    dl_variable = "precip",
    dl_chem_stat = "median",
    dl_data_type = "tabular",
    dl_spatial_layer = "subbasins",
    dl_format = "csv"
  )
}

bench_groups_all <- c("startup", "chem", "basemap", "spatial", "layers",
                      "plots", "downloads", "exercise_all")

# ---------------------------------------------------------------------------
# Small utilities
# ---------------------------------------------------------------------------

bench_now <- function() proc.time()[["elapsed"]]

# Resident set size of this process in MB (ps reports KB on macOS and Linux).
bench_rss_mb <- function() {
  out <- tryCatch(
    suppressWarnings(system2("ps", c("-o", "rss=", "-p", Sys.getpid()), stdout = TRUE, stderr = FALSE)),
    error = function(e) character()
  )
  v <- suppressWarnings(as.numeric(trimws(out[1])))
  if (length(v) == 0 || is.na(v)) {
    if (requireNamespace("ps", quietly = TRUE)) return(ps::ps_memory_info()[["rss"]] / 1024^2)
    return(NA_real_)
  }
  v / 1024
}

# OS thread count of this process.
bench_os_threads <- function() {
  if (identical(Sys.info()[["sysname"]], "Darwin")) {
    out <- system2("ps", c("-M", "-p", Sys.getpid()), stdout = TRUE, stderr = FALSE)
    return(length(out) - 1)
  }
  out <- system2("ps", c("-o", "nlwp=", "-p", Sys.getpid()), stdout = TRUE, stderr = FALSE)
  as.numeric(trimws(out[1]))
}

bench_heap_mb <- function() sum(gc()[, 2])

bench_pool <- function() {
  if (!requireNamespace("arrow", quietly = TRUE)) return(c(alloc = NA_real_, peak = NA_real_))
  p <- arrow::default_memory_pool()
  c(alloc = p$bytes_allocated / 1024^2, peak = p$max_memory / 1024^2)
}

`%|||%` <- function(a, b) if (is.null(a)) b else a

# ---------------------------------------------------------------------------
# Metric recording (child process)
# ---------------------------------------------------------------------------

bench_state <- new.env(parent = emptyenv())
bench_state$rows <- list()
bench_state$failures <- character()
bench_state$silent <- FALSE

bench_record <- function(metric, value, unit) {
  if (isTRUE(bench_state$silent)) return(invisible())
  bench_state$rows[[length(bench_state$rows) + 1L]] <-
    data.frame(metric = metric, value = as.numeric(value)[1], unit = unit, stringsAsFactors = FALSE)
  invisible()
}

bench_fail <- function(metric, unit, e) {
  msg <- if (inherits(e, "condition")) conditionMessage(e) else as.character(e)
  msg <- gsub("[\r\n]+", " ", msg)
  if (!nzchar(msg)) msg <- paste0("<", class(e)[1], ">")
  message("[bench] FAILED ", metric, ": ", msg)
  if (isTRUE(bench_state$silent)) return(invisible())
  bench_record(metric, NA_real_, unit)
  bench_state$failures <- c(bench_state$failures, paste0(metric, ": ", msg))
  invisible()
}

# Time `expr`; record the elapsed seconds under `metric`; return its value
# (NULL on error, which is recorded as NA).
bench_time <- function(metric, expr) {
  t0 <- bench_now()
  ok <- TRUE
  val <- tryCatch(force(expr), error = function(e) {
    ok <<- FALSE
    bench_fail(metric, "s", e)
    NULL
  })
  if (ok) bench_record(metric, bench_now() - t0, "s")
  invisible(val)
}

# Record a numeric value computed by `expr`.
bench_value <- function(metric, unit, expr) {
  ok <- TRUE
  val <- tryCatch(as.numeric(force(expr))[1], error = function(e) {
    ok <<- FALSE
    bench_fail(metric, unit, e)
    NA_real_
  })
  if (ok) bench_record(metric, val, unit)
  invisible(val)
}

# Run a setup block whose failure should be reported (in the failure list,
# without a metric row) but not stop the group.
bench_try <- function(label, expr) {
  tryCatch(force(expr), error = function(e) bench_note_failure(label, e))
  invisible()
}

bench_note_failure <- function(label, e) {
  msg <- gsub("[\r\n]+", " ", conditionMessage(e))
  message("[bench] FAILED ", label, ": ", msg)
  if (!isTRUE(bench_state$silent)) {
    bench_state$failures <- c(bench_state$failures, paste0(label, ": ", msg))
  }
  invisible()
}

# ---------------------------------------------------------------------------
# App loader (same order as shiny's own R/ autoloading)
# ---------------------------------------------------------------------------

bench_load_app <- function(root = getwd(), env = globalenv()) {
  Sys.setenv(ECOMIX_AUTORUN = "0")
  rdir <- file.path(root, "R")
  if (dir.exists(rdir)) {
    for (f in sort(list.files(rdir, pattern = "\\.[Rr]$", full.names = TRUE))) {
      source(f, local = env, encoding = "UTF-8")
    }
  }
  exprs <- parse(file.path(root, "app.R"), keep.source = FALSE, encoding = "UTF-8")
  for (e in exprs) {
    if (is.symbol(e) && identical(as.character(e), "app")) next
    if (any(c("shinyApp", "runApp") %in% all.names(e))) next
    eval(e, env)
  }
  invisible(env)
}

# ---------------------------------------------------------------------------
# Payload sizes
# ---------------------------------------------------------------------------

bench_json_bytes <- function(x) {
  if (is.null(x)) return(0)
  as.numeric(nchar(shiny:::toJSON(x), type = "bytes"))
}

# Bytes of the JSON a browser would receive for one output value.
bench_output_bytes <- function(value) {
  if (is.null(value)) return(0)
  bench_json_bytes(list(values = list(x = value)))
}

bench_msg_bytes <- function(type, message) {
  bench_json_bytes(list(custom = stats::setNames(list(message), type)))
}

# ---------------------------------------------------------------------------
# Gated mock session
# ---------------------------------------------------------------------------

bench_new_session <- function() {
  sess <- shiny::MockShinySession$new()
  if (is.null(bench_state$app_cache)) bench_state$app_cache <- cachem::cache_mem(max_size = 200 * 1024^2)
  sess$appcache <- bench_state$app_cache

  st <- new.env(parent = emptyenv())
  st$gates <- list()
  st$renders <- list()
  st$msgs <- list()

  gate <- function(name) {
    g <- st$gates[[name]]
    if (is.null(g)) {
      g <- shiny::reactiveVal(FALSE, label = paste0("bench_gate_", name))
      st$gates[[name]] <- g
    }
    g
  }

  orig_define <- sess$defineOutput
  gated_define <- function(name, func, label) {
    if (is.function(func)) {
      force(name)
      g <- gate(name)
      f <- func
      wrapped <- function(...) {
        if (!isTRUE(g())) shiny::req(FALSE)
        st$renders[[name]] <- (st$renders[[name]] %|||% 0L) + 1L
        f(...)
      }
      attributes(wrapped) <- attributes(func)
      func <- wrapped
    }
    orig_define(name, func, label)
  }
  unlockBinding("defineOutput", sess)
  assign("defineOutput", gated_define, envir = sess)
  lockBinding("defineOutput", sess)

  # Serialised here (inside the timed step), as a real session would.
  if (bindingIsLocked("sendCustomMessage", sess)) unlockBinding("sendCustomMessage", sess)
  assign("sendCustomMessage", function(type, message) {
    st$msgs[[length(st$msgs) + 1L]] <- list(type = type, bytes = bench_msg_bytes(type, message))
    invisible()
  }, envir = sess)

  list(
    session = sess,
    state = st,
    show = function(...) for (n in bench_id(c(...))) gate(n)(TRUE),
    hide = function(...) for (n in bench_id(c(...))) gate(n)(FALSE),
    get = function(n) sess$getOutput(bench_id(n))
  )
}

# Run fn() and report time, bytes sent to the client (re-rendered outputs +
# custom messages) and how many outputs re-rendered.
bench_step <- function(bs, fn) {
  st <- bs$state
  before <- st$renders
  n_msg <- length(st$msgs)
  t0 <- bench_now()
  fn()
  elapsed <- bench_now() - t0
  changed <- names(st$renders)[vapply(names(st$renders), function(n) {
    is.null(before[[n]]) || st$renders[[n]] > before[[n]]
  }, logical(1))]
  out_bytes <- sum(vapply(changed, function(n) {
    v <- tryCatch(bs$session$getOutput(n), error = function(e) NULL)
    bench_output_bytes(v)
  }, numeric(1)))
  new_msgs <- if (length(st$msgs) > n_msg) st$msgs[(n_msg + 1L):length(st$msgs)] else list()
  msg_bytes <- sum(vapply(new_msgs, function(m) m$bytes, numeric(1)))
  list(time = elapsed, out_bytes = out_bytes, msg_bytes = msg_bytes,
       bytes = out_bytes + msg_bytes, n_msgs = length(new_msgs), outputs = changed)
}

# Record a step as <prefix>_s and <prefix>_bytes (plus optional extras).
bench_record_step <- function(prefix, bs, fn, bytes = TRUE, n_outputs = FALSE, n_msgs = FALSE) {
  res <- tryCatch(bench_step(bs, fn), error = function(e) {
    bench_fail(paste0(prefix, "_s"), "s", e)
    NULL
  })
  if (is.null(res)) return(invisible(NULL))
  bench_record(paste0(prefix, "_s"), res$time, "s")
  if (bytes) bench_record(paste0(prefix, "_bytes"), res$bytes, "bytes")
  if (n_outputs) bench_record(paste0(prefix, "_outputs"), length(res$outputs), "count")
  if (n_msgs) bench_record(paste0(prefix, "_msgs"), res$n_msgs, "count")
  invisible(res)
}

# Run fn(bs) inside testServer(server) with a gated session; returns the
# time the server function itself took.
bench_with_app <- function(fn, label) {
  bs <- bench_new_session()
  bs$state$t0 <- bench_now()
  server_fn <- get("server", envir = globalenv())
  tryCatch(
    shiny::testServer(server_fn, {
      bs$state$server_init <- bench_now() - bs$state$t0
      fn(bs)
    }, session = bs$session),
    error = function(e) bench_note_failure(paste0(label, " (session aborted)"), e)
  )
  invisible(bs)
}

bench_set_list <- function(bs, vals) {
  names(vals) <- bench_id(names(vals))
  do.call(bs$session$setInputs, vals)
}

bench_set <- function(bs, ...) bench_set_list(bs, list(...))

bench_set_defaults <- function(bs) bench_set_list(bs, bench_ui_defaults())

# Show one output, flush, read it; fail if it rendered nothing.
bench_show_read <- function(bs, name) {
  bs$show(name)
  bs$session$flushReact()
  v <- bs$get(name)
  if (is.null(v)) stop("output$", name, " rendered NULL")
  v
}

# Render time for an output, then the time to render the same selection
# again after visiting another one ("revisit": select A, then B, then A).
# The output is hidden while away, so only the final A render is timed.
# renderPlot() already memoises an unchanged plot, so revisiting is what
# bindCache() would short-circuit. `away`/`back` set the inputs for B / A;
# without them the output is just hidden and re-shown.
bench_plot <- function(prefix, bs, name, away = NULL, back = NULL) {
  v <- bench_time(paste0(prefix, "_s"), {
    v <- bench_show_read(bs, name)
    if (is.list(v) && is.null(v$src)) stop("output$", name, " returned no image")
    v
  })
  bench_try(paste0(prefix, "_revisit (away/back)"), {
    bs$hide(name)
    bs$session$flushReact()
    if (is.function(away)) away()
    if (is.function(back)) back()
  })
  bench_time(paste0(prefix, "_revisit_s"), bench_show_read(bs, name))
  bench_try(paste0(prefix, " (hide)"), {
    bs$hide(name)
    bs$session$flushReact()
  })
  invisible(v)
}

# ---------------------------------------------------------------------------
# Measurement groups (run in a child process after the app is loaded)
# ---------------------------------------------------------------------------

bench_group_startup <- function(ctx) {
  bench_record("startup.rss_before_load_mb", ctx$rss_before, "MB")
  bench_record("startup.load_s", ctx$load_s, "s")
  bench_value("startup.rss_mb", "MB", bench_rss_mb())
  bench_value("startup.heap_mb", "MB", bench_heap_mb())
  bench_value("startup.rss_after_gc_mb", "MB", bench_rss_mb())
  pool <- bench_pool()
  bench_value("startup.arrow_pool_mb", "MB", pool[["alloc"]])
  bench_value("startup.arrow_pool_peak_mb", "MB", pool[["peak"]])
  bench_value("startup.os_threads", "count", bench_os_threads())
  bench_value("startup.arrow_cpu_threads", "count", arrow::cpu_count())
  bench_value("startup.arrow_io_threads", "count", arrow::io_thread_count())
  bench_value("startup.global_objects", "count", {
    nms <- ls(globalenv(), all.names = TRUE)
    sum(!startsWith(nms, "bench_") & !startsWith(nms, ".") & nms != "%|||%")
  })
}

# A lookup that silently returns nothing would make its timing meaningless.
bench_nonempty <- function(df) {
  if (is.null(df) || NROW(df) == 0) stop("lookup returned 0 rows")
  df
}

bench_group_chem <- function(ctx) {
  cfg <- bench_cfg
  f <- function(...) bench_nonempty(get("get_chem_subbasin_series", envir = globalenv())(...))
  stats3 <- c("mean", "median", "90th")

  invisible(gc()); r0 <- bench_rss_mb()
  df <- bench_time("chem.series_cold_s", f(cfg$chem_key, "mean", cfg$sb_single))
  bench_value("chem.series_cold_rss_delta_mb", "MB", bench_rss_mb() - r0)
  bench_value("chem.series_rows", "rows", nrow(df))
  bench_time("chem.series_warm_s", f(cfg$chem_key, "mean", cfg$sb_single))

  # 3-stat path (mean/median/90th) on a subbasin with several SITE_IDs.
  sb <- cfg$sb_multi[1]
  invisible(gc()); r0 <- bench_rss_mb()
  res <- bench_time("chem.series_3stat_cold_s",
                    lapply(stats3, function(s) f(cfg$chem_key, s, sb)))
  bench_value("chem.series_3stat_rss_delta_mb", "MB", bench_rss_mb() - r0)
  bench_value("chem.series_3stat_rows", "rows", sum(vapply(res, nrow, numeric(1))))
  bench_time("chem.series_3stat_warm_s",
             lapply(stats3, function(s) f(cfg$chem_key, s, sb)))

  # The other multi-SITE_ID subbasins (duplicate dates).
  for (sb in cfg$sb_multi[-1]) {
    df <- bench_time(paste0("chem.series_sb", sb, "_cold_s"), f(cfg$chem_key, "median", sb))
    bench_value(paste0("chem.series_sb", sb, "_rows"), "rows", nrow(df))
  }

  # Monthly-by-water-body and Chemical Explorer series lookups.
  wb <- function(...) bench_nonempty(get("get_wb_chem_series", envir = globalenv())(...))
  df <- bench_time("chem.wb_series_cold_s", wb(cfg$chem_key, "median", cfg$waterbody))
  bench_value("chem.wb_series_rows", "rows", nrow(df))
  bench_time("chem.wb_series_warm_s", wb(cfg$chem_key, "median", cfg$waterbody))
  d3d <- function(...) bench_nonempty(get("get_chem_d3d_series", envir = globalenv())(...))
  df <- bench_time("chem.d3d_series_cold_s", d3d(cfg$d3d_key, bench_d3d_site()))
  bench_value("chem.d3d_series_rows", "rows", nrow(df))

  bench_value("chem.end_rss_mb", "MB", bench_rss_mb())
  pool <- bench_pool()
  bench_value("chem.arrow_pool_mb", "MB", pool[["alloc"]])
  bench_value("chem.arrow_pool_peak_mb", "MB", pool[["peak"]])
}

# i = 1: the site measured; i = 2: the "away" site for the revisit.
bench_d3d_site <- function(i = 1) {
  fixed <- if (i == 1) bench_cfg$d3d_site else bench_cfg$d3d_site_alt
  if (!is.null(fixed)) return(fixed)
  choices <- get0("chem_d3d_site_choices", envir = globalenv())
  ids <- choices[[bench_cfg$d3d_key]]
  if (length(ids) == 0) {
    ids <- get0("chem_d3d_site_ids", envir = globalenv())[[bench_cfg$d3d_key]]
  }
  if (length(ids) < i) stop("no Chemical Explorer sites found for ", bench_cfg$d3d_key)
  unname(as.character(ids))[i]
}

bench_group_basemap <- function(ctx) {
  for (i in 1:2) {
    p <- paste0("basemap.s", i)
    bench_with_app(function(bs) {
      bench_record(paste0(p, "_server_init_s"), bs$state$server_init, "s")
      init <- bench_record_step(paste0(p, "_init_flush"), bs, function() bench_set_defaults(bs))
      res <- bench_record_step(paste0(p, "_render"), bs, function() bench_show_read(bs, "basemap"),
                               bytes = (i == 1))
      if (!is.null(init) && !is.null(res)) {
        bench_record(paste0(p, "_first_read_total_s"),
                     bs$state$server_init + init$time + res$time, "s")
      }
    }, label = p)
  }
}

bench_group_spatial <- function(ctx) {
  cfg <- bench_cfg
  bench_with_app(function(bs) {
    bench_set_defaults(bs)
    bench_record_step("spatial.first_render", bs, function() bench_show_read(bs, "prediction_map"))
    for (i in seq_along(cfg$spatial_pcts)) {
      bench_record_step(paste0("spatial.pct_change_", i), bs, function() {
        bench_set(bs, prediction_percentile_spatial = cfg$spatial_pcts[i])
        bs$get("prediction_map")
      })
    }
    bench_record_step("spatial.period_change", bs, function() {
      bench_set(bs, prediction_period_spatial = "SSP245|2070-2079")
      bs$get("prediction_map")
    })
    bench_record_step("spatial.chem_render", bs, function() {
      bench_set(bs, prediction_variable_spatial = cfg$chem_var)
      bs$get("prediction_map")
    })
    bench_record_step("spatial.chem_stat_change", bs, function() {
      bench_set(bs, prediction_stat_spatial_chem = "mean")
      bs$get("prediction_map")
    })
    bench_record_step("spatial.wbchem_render", bs, function() {
      bench_set(bs, prediction_variable_spatial = cfg$wbchem_var)
      bs$get("prediction_map")
    })
  }, label = "spatial")
}

bench_group_layers <- function(ctx) {
  cfg <- bench_cfg
  bench_with_app(function(bs) {
    bench_set_defaults(bs)
    bench_show_read(bs, "basemap")
    toggles <- list(
      waterbodies_on    = c("subbasins", "waterbodies"),
      measured_sites_on = c("subbasins", "waterbodies", "measured_sites"),
      waterbodies_off   = c("subbasins", "measured_sites"),
      waterbodies_reon  = c("subbasins", "waterbodies", "measured_sites")
    )
    for (nm in names(toggles)) {
      bench_record_step(paste0("layers.", nm), bs, function() {
        bench_set(bs, map_layers = toggles[[nm]])
      }, n_msgs = TRUE)
    }
    # Selecting a subbasin: highlight on the basemap + every context mini map.
    bench_record_step("layers.select_subbasin", bs, function() {
      bench_set(bs, subbasin_search = as.character(cfg$sb_plot))
    }, n_msgs = TRUE)
  }, label = "layers")
}

bench_group_plots <- function(ctx) {
  cfg <- bench_cfg
  bench_with_app(function(bs) {
    bench_set_defaults(bs)
    sb_a <- function() bench_set(bs, subbasin_search = as.character(cfg$sb_plot))
    sb_b <- function() bench_set(bs, subbasin_search = as.character(cfg$sb_alt))
    sb_a()

    hype <- c(climate = "climate_plot", yearly = "projections_yearly_plot",
              monthly = "projections_monthly_plot", cfc = "projections_cfc_plot",
              daily = "projections_daily_plot")
    for (nm in names(hype)) {
      bench_plot(paste0("plots.", nm, "_hype"), bs, hype[[nm]], away = sb_b, back = sb_a)
    }

    # Switching Model variable to a chemical: the browser then reports the
    # relabelled "Conditions" selector's new value ("median").
    bench_try("plots.switch_to_chem",
              bench_set(bs, prediction_variable = cfg$chem_var, prediction_percentile = "median"))
    for (nm in c("yearly", "monthly", "cfc", "daily")) {
      bench_plot(paste0("plots.", nm, "_chem"), bs, hype[[nm]], away = sb_b, back = sb_a)
    }

    # Site Details occurrence grid, measured site then modelled water body.
    bench_try("plots.site_measured",
              bench_set(bs, site_entity_type = "measured", site_pick = cfg$measured_site))
    bench_plot("plots.site_chem_grid_measured", bs, "site_chem_grid",
               away = function() bench_set(bs, site_pick = cfg$measured_alt),
               back = function() bench_set(bs, site_pick = cfg$measured_site))
    bench_try("plots.site_modelled",
              bench_set(bs, site_entity_type = "modelled", waterbody_pick = cfg$waterbody))
    bench_plot("plots.site_chem_grid_modelled", bs, "site_chem_grid",
               away = function() bench_set(bs, waterbody_pick = cfg$waterbody_alt),
               back = function() bench_set(bs, waterbody_pick = cfg$waterbody))

    # Chemical Explorer.
    bench_try("plots.d3d_select",
              bench_set(bs, chem_d3d_key = cfg$d3d_key, chem_d3d_site = bench_d3d_site(1)))
    bench_plot("plots.chem_d3d_conc", bs, "chem_d3d_conc_plot",
               away = function() bench_set(bs, chem_d3d_site = bench_d3d_site(2)),
               back = function() bench_set(bs, chem_d3d_site = bench_d3d_site(1)))

    # The rest of the Chemical Explorer tab, then hover events over the
    # concentration plot (each picks a day: marker + day treemap update).
    d3d_tab <- c("chem_d3d_conc_plot", "chem_d3d_sspaf_plot", "chem_d3d_threshold_note",
                 "chem_d3d_src_col", "chem_d3d_treemap_avg", "chem_d3d_treemap_day",
                 "chem_d3d_treemap_day_title")
    bench_record_step("plots.chem_d3d_tab", bs, function() {
      bs$show(d3d_tab)
      bs$session$flushReact()
    }, n_outputs = TRUE)
    for (i in seq_along(cfg$hover_dates)) {
      x <- as.numeric(cfg$hover_dates[i])
      bench_record_step(paste0("plots.chem_d3d_hover_", i), bs, function() {
        bench_set(bs, chem_d3d_hover_conc = list(x = x, y = 0.001))
      }, n_outputs = TRUE)
    }
    bs$hide(d3d_tab)
  }, label = "plots")
}

bench_group_downloads <- function(ctx) {
  cfg <- bench_cfg
  bench_with_app(function(bs) {
    bench_set_defaults(bs)
    bench_set(bs, subbasin_search = as.character(cfg$sb_plot))
    bs$show("download_data")
    bs$session$flushReact()

    dl <- function(prefix) {
      invisible(gc()); r0 <- bench_rss_mb()
      path <- bench_time(paste0(prefix, "_s"), bs$get("download_data"))
      bench_value(paste0(prefix, "_rss_delta_mb"), "MB", bench_rss_mb() - r0)
      bench_value(paste0(prefix, "_file_bytes"), "bytes", file.size(path))
    }
    vars <- c(discharge = "discharge", precip = "precip", chem = cfg$chem_var)
    for (nm in names(vars)) {
      bench_try(paste0("downloads.tabular_", nm, "_inputs"),
                bench_set(bs, dl_variable = vars[[nm]], dl_chem_stat = "median",
                          dl_data_type = "tabular", dl_format = "csv"))
      dl(paste0("downloads.tabular_", nm))
    }
    bench_try("downloads.spatial_inputs",
              bench_set(bs, dl_variable = cfg$spatial_dl_var, dl_data_type = "spatial",
                        dl_spatial_layer = "subbasins", dl_format = "gpkg"))
    dl("downloads.spatial_subbasins")
  }, label = "downloads")
}

bench_group_exercise_all <- function(ctx) {
  t0 <- bench_now()
  bench_state$silent <- TRUE
  for (g in c("chem", "basemap", "spatial", "layers", "plots", "downloads")) {
    tryCatch(bench_group_fns[[g]](ctx), error = function(e) message("[bench] exercise ", g, ": ", conditionMessage(e)))
  }
  bench_state$silent <- FALSE
  bench_record("exercise_all.exercise_s", bench_now() - t0, "s")
  bench_value("exercise_all.rss_mb", "MB", bench_rss_mb())
  bench_value("exercise_all.heap_mb", "MB", bench_heap_mb())
  bench_value("exercise_all.rss_after_gc_mb", "MB", bench_rss_mb())
  pool <- bench_pool()
  bench_value("exercise_all.arrow_pool_mb", "MB", pool[["alloc"]])
  bench_value("exercise_all.arrow_pool_peak_mb", "MB", pool[["peak"]])
}

bench_group_fns <- list(
  startup = bench_group_startup,
  chem = bench_group_chem,
  basemap = bench_group_basemap,
  spatial = bench_group_spatial,
  layers = bench_group_layers,
  plots = bench_group_plots,
  downloads = bench_group_downloads,
  exercise_all = bench_group_exercise_all
)

# ---------------------------------------------------------------------------
# Child entry point
# ---------------------------------------------------------------------------

bench_child <- function(group, out_file) {
  Sys.setenv(ECOMIX_AUTORUN = "0")
  on.exit({
    rows <- if (length(bench_state$rows)) do.call(rbind, bench_state$rows) else
      data.frame(metric = character(), value = numeric(), unit = character())
    saveRDS(list(rows = rows, failures = bench_state$failures), out_file)
  }, add = TRUE)

  ctx <- list(rss_before = bench_rss_mb())
  t0 <- bench_now()
  loaded <- tryCatch({
    suppressPackageStartupMessages(bench_load_app())
    TRUE
  }, error = function(e) {
    bench_note_failure(paste0(group, ".load_app"), e)
    FALSE
  })
  ctx$load_s <- bench_now() - t0
  if (!loaded) return(invisible())

  fn <- bench_group_fns[[group]]
  if (is.null(fn)) stop("unknown bench group: ", group)
  tryCatch(fn(ctx), error = function(e) bench_note_failure(paste0(group, " (group aborted)"), e))
  invisible()
}

# ---------------------------------------------------------------------------
# Parent: run groups in fresh processes, report
# ---------------------------------------------------------------------------

bench_script_path <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", args[grepl("^--file=", args)])
  if (length(f) == 0) return(file.path(getwd(), "scripts", "bench_app.R"))
  normalizePath(f[1])
}

bench_peak_rss_mb <- function(log_lines) {
  # macOS /usr/bin/time -l: "  123456789  maximum resident set size" (bytes)
  hit <- grep("maximum resident set size", log_lines, value = TRUE)
  if (length(hit)) {
    v <- suppressWarnings(as.numeric(sub("^\\s*([0-9]+).*", "\\1", hit[length(hit)])))
    return(v / 1024^2)
  }
  # GNU time -v: "Maximum resident set size (kbytes): 123456"
  hit <- grep("Maximum resident set size", log_lines, value = TRUE)
  if (length(hit)) {
    v <- suppressWarnings(as.numeric(sub(".*:\\s*([0-9]+).*", "\\1", hit[length(hit)])))
    return(v / 1024)
  }
  NA_real_
}

bench_run_group <- function(group, script) {
  res_file <- tempfile(paste0("bench_", group, "_"), fileext = ".rds")
  log_file <- tempfile(paste0("bench_", group, "_"), fileext = ".log")
  rscript <- file.path(R.home("bin"), "Rscript")
  child_args <- c(shQuote(script), "--child", group, shQuote(res_file))
  time_bin <- "/usr/bin/time"
  sysname <- Sys.info()[["sysname"]]
  use_time <- file.exists(time_bin) && sysname %in% c("Darwin", "Linux")
  t0 <- bench_now()
  status <- if (use_time) {
    system2(time_bin, c(if (sysname == "Darwin") "-l" else "-v", shQuote(rscript), child_args),
            stdout = log_file, stderr = log_file)
  } else {
    system2(rscript, child_args, stdout = log_file, stderr = log_file)
  }
  wall <- bench_now() - t0
  log_lines <- if (file.exists(log_file)) readLines(log_file, warn = FALSE) else character()
  if (identical(Sys.getenv("BENCH_VERBOSE"), "1")) cat(log_lines, sep = "\n")

  out <- if (file.exists(res_file)) readRDS(res_file) else NULL
  rows <- if (!is.null(out)) out$rows else data.frame(metric = character(), value = numeric(), unit = character())
  failures <- if (!is.null(out)) out$failures else character()
  if (is.null(out) || !identical(as.integer(status), 0L)) {
    failures <- c(failures, sprintf("%s: child process exited with status %s; last log lines: %s",
                                    group, status, paste(utils::tail(log_lines, 8), collapse = " | ")))
  }
  rows <- rbind(rows, data.frame(
    metric = paste0(group, c(".process_wall_s", ".process_peak_rss_mb")),
    value = c(wall, bench_peak_rss_mb(log_lines)),
    unit = c("s", "MB"), stringsAsFactors = FALSE))
  list(rows = rows, failures = failures)
}

bench_format_value <- function(v, unit) {
  if (is.na(v)) return("NA")
  out <- switch(unit,
    s = if (abs(v) < 0.01) formatC(v, format = "g", digits = 2) else formatC(v, format = "f", digits = 3),
    MB = formatC(v, format = "f", digits = 1),
    formatC(round(v), format = "d", big.mark = ",")
  )
  trimws(out)
}

bench_main <- function(args) {
  label <- if (length(args) >= 1) args[1] else "run"
  out_csv <- if (length(args) >= 2) args[2] else NA_character_
  if (!file.exists("app.R")) stop("Run from the repo root (app.R not found in ", getwd(), ")")
  Sys.setenv(ECOMIX_AUTORUN = "0")

  groups <- bench_groups_all
  sel <- trimws(strsplit(Sys.getenv("BENCH_GROUPS", ""), ",")[[1]])
  sel <- sel[nzchar(sel)]
  if (length(sel)) {
    unknown <- setdiff(sel, groups)
    if (length(unknown)) stop("Unknown BENCH_GROUPS: ", paste(unknown, collapse = ", "))
    groups <- groups[groups %in% sel]
  }

  script <- bench_script_path()
  sha <- tryCatch(system2("git", c("rev-parse", "--short", "HEAD"), stdout = TRUE, stderr = FALSE)[1],
                  error = function(e) NA_character_)
  dirty <- tryCatch(length(system2("git", c("status", "--porcelain", "--", "app.R", "R"),
                                   stdout = TRUE, stderr = FALSE)) > 0,
                    error = function(e) NA)

  t_all <- bench_now()
  all_rows <- list()
  all_fail <- character()
  for (g in groups) {
    message(sprintf("[bench] %-13s ...", g))
    r <- bench_run_group(g, script)
    message(sprintf("[bench] %-13s done in %.1f s", g, r$rows$value[r$rows$metric == paste0(g, ".process_wall_s")]))
    all_rows[[g]] <- r$rows
    all_fail <- c(all_fail, r$failures)
  }
  rows <- do.call(rbind, all_rows)
  rownames(rows) <- NULL

  cat("\n## ECOMIX Explorer benchmark: ", label, "\n\n", sep = "")
  cat(sprintf("- date: %s\n- git: %s%s\n- R: %s, shiny %s, leaflet %s, arrow %s\n- total runtime: %.1f s\n\n",
              format(Sys.time(), "%Y-%m-%d %H:%M"), sha, if (isTRUE(dirty)) " (app code modified)" else "",
              getRversion(), utils::packageVersion("shiny"), utils::packageVersion("leaflet"),
              utils::packageVersion("arrow"), bench_now() - t_all))
  cat("| metric | value | unit |\n|---|---:|---|\n")
  for (i in seq_len(nrow(rows))) {
    cat(sprintf("| %s | %s | %s |\n", rows$metric[i],
                bench_format_value(rows$value[i], rows$unit[i]), rows$unit[i]))
  }
  cat("\n")
  if (length(all_fail)) {
    cat("### Failed metrics (", length(all_fail), ")\n\n", sep = "")
    cat(paste0("- ", all_fail), sep = "\n")
    cat("\n")
  } else {
    cat("All metrics succeeded.\n")
  }

  if (!is.na(out_csv)) {
    dir.create(dirname(out_csv), showWarnings = FALSE, recursive = TRUE)
    out <- data.frame(label = label, metric = rows$metric, value = rows$value, unit = rows$unit,
                      stringsAsFactors = FALSE)
    utils::write.table(out, out_csv, sep = ",", row.names = FALSE, qmethod = "double",
                       append = file.exists(out_csv), col.names = !file.exists(out_csv))
    cat("\nAppended ", nrow(out), " rows to ", out_csv, "\n", sep = "")
  }
  invisible(rows)
}

# ---------------------------------------------------------------------------
# Dispatch
# ---------------------------------------------------------------------------

# Only when run as a script (`Rscript scripts/bench_app.R ...`); sourcing the
# file just defines the bench_* functions.
if (sys.nframe() == 0L && !interactive()) {
  bench_args <- commandArgs(trailingOnly = TRUE)
  if (length(bench_args) >= 1 && identical(bench_args[1], "--child")) {
    bench_child(bench_args[2], bench_args[3])
  } else {
    bench_main(bench_args)
  }
}
