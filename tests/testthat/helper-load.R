# Characterisation-test harness: app loader and snapshot helpers.
#
# testthat sources every helper-*.R before the test files, so this file is
# where the app gets loaded - once per test run, into globalenv(), the same
# environment `shiny::runApp()` gives app.R's top-level code. Nothing here
# starts a server or opens a browser: ECOMIX_AUTORUN is forced to "0" and the
# top-level `shinyApp(...)` / `runApp(...)` expressions are skipped entirely.
#
# The loader is deliberately content-driven rather than line-number-driven so
# it keeps working while app.R is refactored (global code moved into R/*.R,
# server split into modules, ...):
#   1. R/*.R (if the directory exists) are sourced alphabetically into
#      globalenv(), mimicking shiny::loadSupport();
#   2. app.R is parsed with keep.source and each top-level expression is
#      evaluated into globalenv(), except expressions that call shinyApp() or
#      runApp() (outside any function definition) and a bare `app` symbol.

# ---- locate the repo root -------------------------------------------------

ecomix_find_root <- function(start = getwd()) {
  env_root <- Sys.getenv("ECOMIX_ROOT", "")
  if (nzchar(env_root)) return(normalizePath(env_root, mustWork = TRUE))
  dir <- normalizePath(start, mustWork = TRUE)
  repeat {
    if (file.exists(file.path(dir, "app.R")) && dir.exists(file.path(dir, "data"))) {
      return(dir)
    }
    parent <- dirname(dir)
    if (identical(parent, dir)) {
      stop("Could not find the ECOMIX-Explorer repo root (a folder holding app.R and data/) ",
           "above ", start, ". Set the ECOMIX_ROOT environment variable.")
    }
    dir <- parent
  }
}

# ---- loader ---------------------------------------------------------------

# TRUE if `expr` calls any function in `fns` (bare or pkg::-qualified),
# without descending into function definitions - a function *body* that
# happens to mention runApp() is a definition, not a launch.
ecomix_calls_any <- function(expr, fns) {
  if (!is.call(expr)) return(FALSE)
  head <- expr[[1]]
  if (identical(head, as.name("function"))) return(FALSE)
  head_name <- if (is.name(head)) {
    as.character(head)
  } else if (is.call(head) && length(head) == 3 && is.name(head[[1]]) &&
             as.character(head[[1]]) %in% c("::", ":::")) {
    as.character(head[[3]])
  } else {
    NA_character_
  }
  if (!is.na(head_name) && head_name %in% fns) return(TRUE)
  for (i in seq_along(expr)[-1]) {
    if (identical(expr[[i]], quote(expr = ))) next  # empty argument, e.g. x[, 1]
    if (ecomix_calls_any(expr[[i]], fns)) return(TRUE)
  }
  is.call(head) && ecomix_calls_any(head, fns)
}

ecomix_skip_expr <- function(expr) {
  (is.name(expr) && identical(as.character(expr), "app")) ||
    ecomix_calls_any(expr, c("shinyApp", "runApp", "shinyAppDir", "shinyAppFile"))
}

load_ecomix <- function(root = ecomix_find_root(), envir = globalenv(), force = FALSE) {
  if (isTRUE(getOption("ecomix.tests.loaded")) && !force) return(invisible(envir))

  # Never let a sourced app.R launch itself.
  Sys.setenv(ECOMIX_AUTORUN = "0")

  # shiny itself setwd()s to the app dir before sourcing, so relative paths
  # and here::here() resolve against the repo root.
  old_wd <- setwd(root)
  on.exit(setwd(old_wd), add = TRUE)

  # 1. R/*.R support files (alphabetical, like shiny::loadSupport()).
  r_dir <- file.path(root, "R")
  if (dir.exists(r_dir)) {
    r_files <- sort(list.files(r_dir, pattern = "\\.[rR]$", full.names = TRUE))
    for (f in r_files) sys.source(f, envir = envir, keep.source = TRUE, toplevel.env = envir)
  }

  # 2. app.R, one top-level expression at a time.
  exprs <- parse(file.path(root, "app.R"), keep.source = TRUE, encoding = "UTF-8")
  skipped <- character(0)
  for (i in seq_along(exprs)) {
    e <- exprs[[i]]
    if (ecomix_skip_expr(e)) {
      skipped <- c(skipped, paste(trimws(deparse(e)), collapse = " "))
      next
    }
    eval(e, envir = envir)
  }

  options(ecomix.tests.loaded = TRUE, ecomix.tests.root = root,
          ecomix.tests.skipped = skipped)
  invisible(envir)
}

# ---- hashing ----------------------------------------------------------------

# Re-mark every string as UTF-8 (recursively through lists / data frames /
# factor levels / names). Serialisation records each string's declared
# encoding, so the same text read via read.csv (native) and via Arrow (UTF-8)
# would otherwise hash differently even though the values are identical.
ecomix_norm_enc <- function(x) {
  if (is.environment(x) || is.function(x) || is.null(x)) return(x)
  nms <- names(x)
  if (is.character(x)) {
    x[] <- enc2utf8(x)
  } else if (is.factor(x)) {
    levels(x) <- enc2utf8(levels(x))
  } else if (is.list(x)) {
    for (i in seq_along(x)) {
      if (!is.null(x[[i]])) x[[i]] <- ecomix_norm_enc(x[[i]])
    }
  }
  if (!is.null(nms)) names(x) <- enc2utf8(nms)
  x
}

# Content hash that is insensitive to *how* the object was materialised:
# version-2 serialisation expands ALTREP vectors (Arrow's lazy columns,
# compact integer sequences), so a lazy and an eager read of identical data
# hash the same. sf objects are hashed the same way (geometry included).
canon_hash <- function(x) {
  x <- unserialize(serialize(x, NULL, version = 2))
  rlang::hash(ecomix_norm_enc(x))
}

# Factor with its levels sorted (codes remapped to match). Arrow's
# dictionary-encoded columns (e.g. `chemical` in the water body parquet) come
# back as factors whose level order depends on which file/row group the scan
# reached first, which is not deterministic.
canon_factor <- function(f) {
  factor(as.character(f), levels = sort(unique(levels(f)), method = "radix"))
}

# Canonical order for data whose order the code under test does not define:
# Arrow dataset scans and Arrow-side distinct()/group_by() results do not
# guarantee a row order (verified: hype_subbasin_ids, wb_chem_raw_labels,
# get_chem_all_subbasins_daily(), get_chem_spatial_summary(),
# get_wb_chem_grid() and build_tabular_download() all change order between
# runs). Data frames: factor levels sorted, rows sorted by every atomic
# column left to right, row names reset. Vectors: sorted (by value, then
# name).
canon_rows <- function(df) {
  if (is.factor(df)) df <- canon_factor(df)
  if (is.atomic(df) && !is.data.frame(df)) {
    if (length(df) < 2) return(df)
    keys <- list(if (is.factor(df)) as.character(df) else df)
    if (!is.null(names(df))) keys <- c(keys, list(names(df)))
    return(df[do.call(order, c(keys, list(method = "radix", na.last = TRUE)))])
  }
  if (!is.data.frame(df)) return(df)
  for (nm in names(df)) {
    if (is.factor(df[[nm]])) df[[nm]] <- canon_factor(df[[nm]])
  }
  if (nrow(df) < 2) return(df)
  is_sf <- inherits(df, "sf")
  key <- if (is_sf) sf::st_drop_geometry(df) else df
  key <- as.data.frame(key)
  keep <- vapply(key, function(col) is.atomic(col) || is.factor(col), logical(1))
  key <- key[keep]
  if (ncol(key) == 0) return(df)
  ord <- do.call(order, c(unname(lapply(key, function(col) {
    if (is.factor(col)) as.character(col) else col
  })), list(method = "radix", na.last = TRUE)))
  out <- df[ord, , drop = FALSE]
  if (!is_sf && inherits(out, "tbl_df")) return(out)
  rownames(out) <- NULL
  out
}

# ---- snapshot helper ----------------------------------------------------------

# Numbers are written into snapshots as 8-significant-digit strings: the
# json2 style only keeps 8 decimal places, which would otherwise make tiny
# concentrations fail testthat's serialise/deserialise round-trip check (the
# exact values are covered by the hash anyway).
num_txt <- function(v) {
  v <- as.numeric(v)
  out <- formatC(v, digits = 8, format = "g")
  out[is.na(v)] <- NA_character_
  trimws(out)
}

# One-line summary of a column/vector: class, NA count and range (numbers,
# dates) or number of distinct values (strings, factors, logicals). Kept to a
# single string per column so json2 snapshots stay short and diff cleanly.
ecomix_col_summary <- function(col) {
  if (inherits(col, "sfc")) {
    return(paste0(
      "sfc ", paste(unique(as.character(sf::st_geometry_type(col))), collapse = "/"),
      "; epsg ", sf::st_crs(col)$epsg %||% NA,
      "; bbox ", paste(num_txt(unname(as.numeric(sf::st_bbox(col)))), collapse = " "),
      "; hash ", canon_hash(col)
    ))
  }
  parts <- c(paste(class(col), collapse = "/"), paste("NA", sum(is.na(col))))
  if (is.numeric(col) || inherits(col, c("Date", "POSIXt"))) {
    v <- col[!is.na(col)]
    if (length(v) > 0) {
      fmt <- function(z) if (is.numeric(z)) num_txt(z) else as.character(z)
      parts <- c(parts, paste("min", fmt(min(v))), paste("max", fmt(max(v))))
      if (is.numeric(v)) parts <- c(parts, paste("mean", num_txt(mean(as.numeric(v)))))
    }
  } else if (is.character(col) || is.factor(col) || is.logical(col)) {
    parts <- c(parts, paste("distinct", length(unique(col))))
    if (is.factor(col)) parts <- c(parts, paste("levels", nlevels(col)))
  }
  paste(parts, collapse = "; ")
}

# First rows of a data frame, one " | "-joined string per column.
ecomix_head_values <- function(df, n = 3) {
  h <- utils::head(df, n)
  if (inherits(h, "sf")) h <- sf::st_drop_geometry(h)
  h <- as.data.frame(h)
  lapply(h, function(col) {
    v <- if (is.list(col)) rep("<list>", length(col))
         else if (is.numeric(col)) num_txt(col)
         else as.character(col)
    paste(ifelse(is.na(v), "<NA>", v), collapse = " | ")
  })
}

# Snapshot a compact, diff-friendly description of `x`: a canonical content
# hash plus enough structure (class, dims, names, a one-line summary per
# column and the first rows) that a failing snapshot shows *what* changed.
#   ordered = FALSE canonicalises row order (see canon_rows()) before hashing
#   and heading - use it wherever the code does not define the row order.
snap_obj <- function(x, name = NULL, ordered = TRUE) {
  if (!ordered) x <- canon_rows(x)
  desc <- list(name = name, hash = canon_hash(x), class = class(x))
  if (!ordered) desc$row_order <- "canonicalised (sorted by all columns)"
  if (is.data.frame(x)) {
    desc$dim <- dim(x)
    desc$names <- names(x)
    if (inherits(x, "sf")) {
      desc$attr_hash <- canon_hash(sf::st_drop_geometry(x))
      desc$geometry_hash <- canon_hash(sf::st_geometry(x))
    }
    desc$columns <- lapply(x, ecomix_col_summary)
    desc$head <- ecomix_head_values(x)
  } else {
    desc$length <- length(x)
    if (!is.null(dim(x))) {
      desc$dim <- dim(x)
      desc$dimnames_head <- lapply(dimnames(x), function(d) paste(utils::head(d, 10), collapse = " | "))
    }
    if (!is.null(names(x))) desc$names <- paste(utils::head(names(x), 50), collapse = " | ")
    if (is.atomic(x) && !is.null(x)) {
      desc$summary <- ecomix_col_summary(x)
      v <- utils::head(unname(x), 10)
      v <- if (is.numeric(v)) num_txt(v) else as.character(v)
      desc$head <- paste(ifelse(is.na(v), "<NA>", v), collapse = " | ")
    } else if (is.list(x) && length(x) > 0) {
      hashes <- vapply(x, canon_hash, character(1))
      # Small lists of scalars/short vectors: show the values; otherwise the
      # per-element hashes (first 50) locate which element changed.
      small <- all(vapply(x, function(el) is.null(el) || (is.atomic(el) && length(el) <= 10), logical(1)))
      if (small && length(x) <= 20) {
        desc$values <- lapply(x, function(el) {
          if (is.null(el)) return("NULL")
          v <- if (is.numeric(el)) num_txt(el) else as.character(el)
          paste(ifelse(is.na(v), "<NA>", v), collapse = " | ")
        })
      } else {
        desc$element_hashes <- as.list(utils::head(hashes, 50))
      }
    }
  }
  testthat::expect_snapshot_value(desc, style = "json2")
}

# ---- load the app (once per test run) ------------------------------------

# Snapshot expectations silently *skip* when testthat thinks it is on CRAN
# (any non-interactive run without NOT_CRAN=true, e.g. Rscript). This suite
# is nothing but snapshots, so force them on.
Sys.setenv(NOT_CRAN = "true")

# Keep plotting off the default (PDF) device so no Rplots.pdf is written.
if (is.null(grDevices::dev.list())) grDevices::pdf(NULL)

# Determinism shim for ggplot2 4.0.0 legend order (test-only; app unchanged).
# Legends are ordered by a hash of each guide's title/labels. When a guide's
# title is NULL (e.g. `labs(linetype = NULL)` in the Hydro Explorer
# projection plots) the element is absent from the guide's params, so
# ggplot2's eval_tidy(quote(title), data = params) falls through the search
# path to graphics::title() - and that closure hashes differently in every R
# process. The result: in the real app the colour/linetype legend order of
# those plots flips at random between sessions (cosmetic), and their PNG
# hashes would be unstable. Binding a constant `title` in globalenv (searched
# before package:graphics; calls to title() still find the function) makes
# the order deterministic. Harmless if ggplot2 fixes this.
assign("title", NULL, envir = globalenv())

load_ecomix()
