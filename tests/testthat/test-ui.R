# Characterisation of the rendered UI (page_navbar -> HTML), for the phases
# that move global code (widget definitions, choices built from the data)
# into R/*.R: the page must render to the same HTML.
#
# bslib/shiny give navs, cards and sidebars random numeric ids
# (data-tabsetid="2727", tab-2727-1, navbar-collapse-5812, bslib-card-1186,
# bslib-sidebar-1221) that change on every render. They are replaced by
# placeholders numbered in order of first appearance (R1, R2, ...), which
# keeps the structure (which tab link points at which pane) comparable.
#
# The module split will change input/output ids on purpose: review the diff
# with testthat::snapshot_review() and accept it there.

testthat::local_edition(3)

normalise_ui_html <- function(html) {
  pat <- "(data-tabsetid=\"|tab-|navbar-collapse-|bslib-card-|bslib-sidebar-)([0-9]+)"
  m <- gregexpr(pat, html, perl = TRUE)
  toks <- regmatches(html, m)[[1]]
  if (length(toks) == 0) return(html)
  nums <- sub(pat, "\\2", toks, perl = TRUE)
  ids <- setNames(paste0("R", seq_along(unique(nums))), unique(nums))
  regmatches(html, m) <- list(paste0(sub(pat, "\\1", toks, perl = TRUE), ids[nums]))
  html
}

test_that("UI renders to the same HTML (random ids normalised)", {
  rendered <- htmltools::renderTags(ui)
  html <- normalise_ui_html(as.character(rendered$html))
  # Rendering twice gives the same normalised HTML.
  expect_identical(normalise_ui_html(as.character(htmltools::renderTags(ui)$html)), html)

  ids <- regmatches(html, gregexpr("\\bid=\"[^\"]+\"", html))[[1]]
  ids <- sort(unique(sub("^id=\"(.*)\"$", "\\1", ids)))
  outputs <- regmatches(html, gregexpr("class=\"[^\"]*shiny-[a-z-]*output[^\"]*\"[^>]*id=\"[^\"]+\"|id=\"[^\"]+\"[^>]*class=\"[^\"]*shiny-[a-z-]*output", html))[[1]]
  deps <- vapply(rendered$dependencies, function(d) paste0(d$name, "@", d$version), character(1))
  expect_snapshot_value(list(
    html_hash = canon_hash(html),
    nchar = nchar(html),
    element_ids = paste(ids, collapse = " | "),
    n_output_elements = length(outputs),
    dependencies = paste(deps, collapse = " | ")
  ), style = "json2")

  path <- file.path(tempdir(), "ui.html")
  writeLines(html, path, useBytes = TRUE)
  expect_snapshot_file(path, "ui.html", compare = compare_file_text)
})
