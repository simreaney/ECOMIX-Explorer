# ECOMIX Explorer - Shiny app entry point.
#
# Shiny sources every file in R/ (in file-name order) before this file, so the
# data loading, helper functions and widget definitions live there; this file
# holds the UI, the server and the app object. Start it with shiny::runApp()
# from the repo root, or `Rscript app.R`.

# Run directly (`Rscript app.R`): R/ isn't loaded at this point, so hand over
# to runApp(), which loads R/ and then sources this file again. Shiny (and so
# Shiny Server, ShinyProxy, Docker and the RStudio Run button) evaluates the
# file inside a function frame, where this block is skipped - the old
# end-of-file runApp() call instead started a second, nested server there
# unless ECOMIX_AUTORUN=0 was set. ECOMIX_AUTORUN=0 still disables it.
if (sys.nframe() == 0L && !identical(Sys.getenv("ECOMIX_AUTORUN"), "0")) {
  app_file <- sub("^--file=", "", grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE))
  shiny::runApp(
    if (length(app_file) > 0) dirname(normalizePath(app_file[1])) else ".",
    host = getOption("shiny.host", "127.0.0.1"),
    port = getOption("shiny.port", NULL),
    launch.browser = TRUE
  )
  quit(save = "no")
}

### 1. User Interface

ui <- page_navbar(
  id = "main_nav",
  title = "ECOMIX Explorer",
  fillable = TRUE,
  theme = bs_theme(
    version = 5,
    bg = ec$ground,
    fg = ec$ink,
    primary = ec$accent,
    base_font = font_google("Archivo"),
    heading_font = font_google("Archivo"),
    "border-radius" = "0",
    "card-border-width" = "0",
    "card-box-shadow" = "none",
    "font-size-base" = "0.8125rem"
  ),
  # styles.css and help.htm (Help panel) ship with the app, not the data, so
  # they are read relative to the app directory - Shiny's working directory
  # while it sources this file - rather than through data_path().
  header = tags$head(
    includeCSS("styles.css"),
    # Restyles polygons already on a Leaflet map, found by their string
    # layerId, instead of drawing them again - how the Spatial Datasets map
    # takes a new slice (update_spatial_values() in R/20_map_layers.R).
    # Registered as a leaflet.js method so leafletProxy() can call it
    # (leaflet::invokeMethod()) in order with its other calls; `this` is the
    # map. Each entry of `style` is a Leaflet path option, either one value
    # for every polygon or one per id.
    tags$script(HTML("
      $(function() {
        LeafletWidget.methods.setShapeStyle = function(layerIds, style) {
          var ids = [].concat(layerIds);
          for (var i = 0; i < ids.length; i++) {
            var layer = this.layerManager.getLayer('shape', ids[i]);
            if (!layer) continue;
            var s = {};
            for (var key in style) s[key] = Array.isArray(style[key]) ? style[key][i] : style[key];
            layer.setStyle(s);
          }
        };
      });
    ")),
    # Chemical Explorer: marks the selected day (server message
    # "chem_d3d_marker", days since 1970-01-01 or null) on both time series
    # as a 1px line over the plot image, so hovering doesn't re-render the
    # plots. Each plot's coordmap (sent with every render) maps the day to a
    # pixel position; the line spans the plot panel, like the geom_vline it
    # replaces, and ignores the pointer so hovering still reaches the image.
    tags$script(HTML("
      $(function() {
        var ids = ['chem_d3d_conc_plot', 'chem_d3d_sspaf_plot'];
        var coordmaps = {}, day = null;
        function draw(id) {
          var el = document.getElementById(id);
          if (!el) return;
          var img = el.querySelector('img');
          var line = el.querySelector('.chem-d3d-marker');
          var cm = coordmaps[id];
          var panel = cm && cm.panels && cm.panels[0];
          var lo = panel && Math.min(panel.domain.left, panel.domain.right);
          var hi = panel && Math.max(panel.domain.left, panel.domain.right);
          if (day === null || !panel || !img || !img.clientWidth || day < lo || day > hi) {
            if (line) line.style.display = 'none';
            return;
          }
          if (!line) {
            line = document.createElement('div');
            line.className = 'chem-d3d-marker';
            line.style.cssText = 'position:absolute; width:1px; pointer-events:none; background:#ae1800;';
            if (getComputedStyle(el).position === 'static') el.style.position = 'relative';
            el.appendChild(line);
          }
          var sx = img.clientWidth / cm.dims.width, sy = img.clientHeight / cm.dims.height;
          var d = panel.domain, r = panel.range;
          var x = r.left + (day - d.left) / (d.right - d.left) * (r.right - r.left);
          line.style.left = (img.offsetLeft + x * sx) + 'px';
          line.style.top = (img.offsetTop + r.top * sy) + 'px';
          line.style.height = ((r.bottom - r.top) * sy) + 'px';
          line.style.display = 'block';
        }
        // shiny:value fires before the new image is in the page, so redraw
        // once it has been swapped in (and again once it has loaded).
        $(document).on('shiny:value', function(e) {
          if (ids.indexOf(e.name) < 0) return;
          coordmaps[e.name] = e.value && e.value.coordmap;
          setTimeout(function() {
            draw(e.name);
            var img = document.querySelector('#' + e.name + ' img');
            if (img && !img.complete) img.addEventListener('load', function() { draw(e.name); }, { once: true });
          }, 0);
        });
        Shiny.addCustomMessageHandler('chem_d3d_marker', function(msg) {
          day = (msg.day === null || msg.day === undefined || msg.day === 'NA') ? null : Number(msg.day);
          ids.forEach(draw);
        });
        $(window).on('resize', function() { ids.forEach(draw); });
      });
    "))
  ),

  # Panel 1: Map - choose a location
  nav_panel(
    title = "Map",
    ctx_strip(
      "map",
      ctx_subbasin_stats("map"),
      action = actionLink("ctx_map_go", "Open in explorer \u2192"),
      mini_map = FALSE
    ),
    layout_sidebar(
      fillable = TRUE,
      sidebar = sidebar(
        width = 320,
        title = "Find a location",
        selectizeInput(
          "subbasin_search",
          label = NULL,
          choices = NULL,
          options = list(placeholder = "Search subbasin id", maxOptions = 50)
        ),
        checkboxGroupInput(
          "map_layers",
          label = "Layers",
          choices = c("Subbasins with hydrology" = "subbasins",
                      "Operational catchment boundaries" = "opcat",
                      "Waterbodies (modelled)" = "waterbodies",
                      "Measured sites" = "measured_sites",
                      "Observed hydrology sites" = "observed_hydro",
                      "Chemical data coverage" = "chem_coverage",
                      "Ecological risk" = "eco_risk"),
          selected = "subbasins"
        ),
        conditionalPanel(
          condition = "input.map_layers && input.map_layers.includes('eco_risk')",
          selectInput(
            "eco_risk_chemical",
            label = "Ecological risk · chemical",
            choices = eco_risk_choices,
            selected = eco_risk_choices[1]
          )
        ),
        selectInput(
          "map_basemap",
          label = "Basemap",
          choices = c("Streets (OSM)" = "osm",
                      "Satellite (Esri)" = "esri_satellite",
                      "Light gray (Esri)" = "esri_gray"),
          selected = "osm"
        ),
        div(class = "rail-head",
            paste0("Operational catchments \u00b7 ", nrow(opcat_counts))),
        div(class = "rail-list", uiOutput("opcat_rail"))
      ),
      div(
        class = "map-wrap",
        leafletOutput("basemap", width = "100%", height = "100%"),
        tags$div(
          class = "map-note",
          "Data compiled by York, Durham and Sheffield Universities (2026). Operational catchment boundaries and waterbodies \u00a9 Environment Agency copyright and/or database right 2026, licensed under the Open Government Licence v3.0"
        )
      )
    )
  ),

  ## Panel 2: Site detail view - measured sites and modelled water bodies
  nav_panel(
    title = "Site Details",
    ctx_strip(
      "site",
      tagList(
        ctx_stat("Summary", div(style = "font-size:13px; font-weight:400; line-height:1.45; white-space:normal;",
                                textOutput("ctx_summary_site", inline = TRUE))),
        ctx_stat("Land cover", uiOutput("ctx_landcover_site", inline = TRUE))
      ),
      mini_map = TRUE,
      id_label = textOutput("ctx_idlabel_site", inline = TRUE)
    ),
    layout_sidebar(
      fillable = TRUE,
      sidebar = sidebar(
        width = 220,
        title = "Sites",
        entity_type_toggle(
          "site_entity_type",
          measured_panel = div(class = "rail-list", uiOutput("measured_rail")),
          modelled_panel = div(class = "rail-list", uiOutput("modelled_rail"))
        )
      ),
      layout_columns(
        col_widths = c(7, 5),
        gap = "0px",
        card(
          full_screen = TRUE,
          card_header(
            "Chemical occurrence",
            conditionalPanel(
              condition = "input.site_entity_type == 'modelled'",
              popover(
                actionLink("site_grid_opts", "Statistic \u25be", class = "pill"),
                title = "Occurrence grid",
                selectInput("site_grid_wb_stat", "Statistic", choices = chem_stat_choices, selected = "median")
              )
            ),
            span(class = "panel-note", textOutput("site_chem_grid_note", inline = TRUE))
          ),
          div(
            style = "position: relative; height: 100%;",
            plotOutput(
              "site_chem_grid",
              height = "100%",
              hover = hoverOpts(id = "site_chem_grid_hover", delay = 60, delayType = "debounce")
            ),
            uiOutput("site_chem_grid_tooltip")
          )
        ),
        card(
          full_screen = TRUE,
          card_header(
            "Time series",
            popover(
              actionLink("site_ts_opts", "Choose panels \u25be", class = "pill"),
              title = "Time series panels",
              selectInput("site_ts_chemical_1", "Panel 1", choices = measured_parameter_choices),
              selectInput("site_ts_chemical_2", "Panel 2", choices = measured_parameter_choices),
              selectInput("site_ts_chemical_3", "Panel 3", choices = measured_parameter_choices),
              selectInput("site_ts_chemical_4", "Panel 4", choices = measured_parameter_choices)
            ),
            span(class = "panel-note", "widest concentration range at this site")
          ),
          plotOutput("site_time_series", height = "100%")
        )
      )
    )
  ),

  ## Panel 3: Hydro explorer
  nav_panel(
    title = "Hydro Explorer",
    ctx_strip(
      "de",
      ctx_subbasin_stats("de"),
      action = actionLink("ctx_de_go", "Change location"),
      mini_map = TRUE
    ),
    layout_sidebar(
      fillable = TRUE,

      # ONE control rail for the whole page. Everything that used to live in
      # a sidebar nested inside each plot card sits here; the two remaining
      # per-panel choices (which climate variable, which observed variable)
      # are popovers in their own card headers.
      sidebar = sidebar(
        width = 248,
        title = "Controls \u00b7 all panels",
        entity_type_toggle(
          "de_entity_type",
          measured_panel = NULL,
          modelled_panel = selectizeInput(
            "de_waterbody_search",
            label = NULL,
            choices = NULL,
            options = list(placeholder = "Search water body", maxOptions = 50)
          ),
          measured_label = "Subbasin",
          modelled_label = "Water body",
          values = c("subbasin", "waterbody")
        ),
        conditionalPanel(
          condition = "input.de_entity_type == 'subbasin'",
          widget_scenario,
          # One shared period control for the whole page - it used to be
          # duplicated (a separate "Periods" dropdown fed only the Climate
          # panel), which looked like two controls doing the same job. Kept
          # outside the conditionalPanel below since the Climate panel needs
          # it regardless of which Model variable is selected; only
          # Absolute/Relative is HYPE-only (chemicals are a single historical
          # daily record with no future period or baseline to compare against).
          widget_prediction_period,
          conditionalPanel(
            condition = "input.prediction_variable && input.prediction_variable.indexOf('chem_') !== 0",
            widget_plot_type
          )
        ),
        widget_prediction_percentile,
        tags$hr(class = "hr"),
        widget_prediction_variable,
        downloadButton("download_selection", "Download this selection")
      ),

      layout_columns(
        col_widths = c(6, 6, 12),
        row_heights = c(1, 1.22),
        gap = "0px",

        # Climate
        card(
          full_screen = TRUE,
          card_header(
            "Climate",
            popover(
              actionLink("climate_opts", textOutput("climate_pill", inline = TRUE), class = "pill"),
              title = "Climate panel",
              widget_climate_variable,
              widget_climate_resolution
            )
          ),
          plotOutput("climate_plot", height = "100%")
        ),

        # Simulations vs observations
        card(
          full_screen = TRUE,
          card_header(
            "Simulated vs observed",
            popover(
              actionLink("observation_opts", textOutput("observation_pill", inline = TRUE), class = "pill"),
              title = "Observation panel",
              widget_observed_variable
            )
          ),
          plotOutput("observation_plot", height = "100%")
        ),

        # Projections - views of the same selection. "Daily" only makes
        # sense for chemicals (HYPE output here is monthly/yearly/percentile
        # only, no daily series) - it's shown/hidden server-side based on
        # the selected Model variable rather than always being present.
        navset_card_pill(
          id = "projection_tabs",
          full_screen = TRUE,
          title = textOutput("projection_title", inline = TRUE),
          nav_panel("Yearly", plotOutput("projections_yearly_plot", height = "100%")),
          nav_panel("Monthly", plotOutput("projections_monthly_plot", height = "100%")),
          nav_panel("Distributions", plotOutput("projections_cfc_plot", height = "100%")),
          nav_panel("Daily", plotOutput("projections_daily_plot", height = "100%"))
        )
      )
    )
  ),

  ## Panel 4: Chemical explorer - D3D-modelled daily percentile time series
  ## (see chem_d3d_datasets above): concentration and ssPAF, with acute/chronic "level of concern"
  ## threshold lines. No ctx_strip/subbasin selection here - the D3D data is
  ## keyed by its own site_id, a different id space to the map's EA
  ## water-body codes, so this page carries its own site selector instead.
  nav_panel(
    title = "Chemical Explorer",
    layout_sidebar(
      fillable = TRUE,
      sidebar = sidebar(
        width = 260,
        title = "Controls",
        selectInput(
          "chem_d3d_key",
          "Chemical",
          choices = chem_d3d_choices,
          selected = chem_d3d_choices[1]
        ),
        selectizeInput(
          "chem_d3d_site",
          "Water body",
          choices = NULL,
          # maxOptions must cover every site actually offered (537 today) -
          # selectize.js silently truncates the dropdown/search results to
          # this many entries, which otherwise makes most water bodies look
          # missing even though they're all present in `choices`.
          options = list(placeholder = "Search water body", maxOptions = 600)
        ),
        tags$hr(class = "hr"),
        div(class = "rail-toggle",
            navset_pill(
              id = "chem_d3d_grouping",
              nav_panel(title = "Daily", value = "daily", NULL),
              nav_panel(title = "Weekly", value = "weekly", NULL),
              nav_panel(title = "Monthly", value = "monthly", NULL)
            )),
        conditionalPanel(
          condition = "input.chem_d3d_grouping != 'daily'",
          radioButtons(
            "chem_d3d_group_stat",
            "Aggregate using",
            choices = c("Median" = "median", "90th percentile" = "p90"),
            selected = "median"
          )
        )
      ),
      div(
        class = "html-fill-item html-fill-container",
        style = "flex-direction: row;",
        div(
          class = "html-fill-item html-fill-container",
          style = "flex: 1 1 0; min-width: 0; gap: 1rem; min-height: 0;",
          card(
            full_screen = TRUE,
            card_header(
              "Concentration · 10th–90th percentile range",
              span(class = "panel-note", textOutput("chem_d3d_threshold_note", inline = TRUE))
            ),
            plotOutput("chem_d3d_conc_plot", height = "100%",
                       hover = hoverOpts("chem_d3d_hover_conc", delay = 80, delayType = "debounce", nullOutside = FALSE))
          ),
          card(
            full_screen = TRUE,
            card_header("ssPAF (potentially affected fraction of species) · 10th–90th percentile range"),
            plotOutput("chem_d3d_sspaf_plot", height = "100%",
                       hover = hoverOpts("chem_d3d_hover_sspaf", delay = 80, delayType = "debounce", nullOutside = FALSE))
          )
        ),
        # Source apportionment treemaps - only rendered for chemicals that
        # have source apportionment files (see chem_src_files).
        uiOutput("chem_d3d_src_col", class = "html-fill-item",
                 style = "flex: 0 0 auto; overflow-y: auto; min-height: 0;")
      )
    )
  ),

  ## Panel 5: Ecological risk detail - reached by clicking a point on the
  ## Map's Ecological risk layer. Deliberately a single minimal card for
  ## now (no ctx_strip, no sidebar) - the design here is expected to change
  ## once the underlying risk assessment content is finalised.
  nav_panel(
    title = "Ecological Risk",
    card(
      full_screen = TRUE,
      card_header("Ecological risk · site detail"),
      uiOutput("eco_risk_detail_card")
    )
  ),

  # Panel 6: Spatial mapping
  nav_panel(
    title = "Spatial Datasets",
    ctx_strip(
      "sp",
      tagList(
        ctx_stat("Value here", textOutput("ctx_value_sp", inline = TRUE)),
        ctx_stat("Rank", textOutput("ctx_rank_sp", inline = TRUE)),
        ctx_stat("Study-area range", textOutput("ctx_range_sp", inline = TRUE))
      ),
      action = actionLink("ctx_sp_go", "Change location"),
      mini_map = TRUE
    ),
    layout_sidebar(
      fillable = TRUE,
      sidebar = sidebar(
        width = 248,
        title = "Map layer",
        widget_prediction_variable_spatial,
        conditionalPanel(
          condition = paste0(
            "input.prediction_variable_spatial && ",
            "input.prediction_variable_spatial.indexOf('chem_') !== 0 && ",
            "input.prediction_variable_spatial.indexOf('wbchem_') !== 0"
          ),
          widget_spatial_period,
          widget_spatial_percentile
        ),
        conditionalPanel(
          condition = paste0(
            "input.prediction_variable_spatial && ",
            "(input.prediction_variable_spatial.indexOf('chem_') === 0 || ",
            "input.prediction_variable_spatial.indexOf('wbchem_') === 0)"
          ),
          widget_spatial_chem_stat
        ),
        selectInput(
          "sp_basemap",
          label = "Basemap",
          choices = c("Streets (OSM)" = "osm",
                      "Satellite (Esri)" = "esri_satellite",
                      "Light gray (Esri)" = "esri_gray"),
          selected = "osm"
        )
      ),
      div(
        class = "map-wrap",
        leafletOutput("prediction_map", width = "100%", height = "100%")
      )
    )
  ),

  # Panel 7: Download of data (or tables)
  nav_panel(
    title = "Download",
    ctx_strip(
      "dl",
      tagList(
        ctx_stat("Rows in selection", textOutput("ctx_rows_dl", inline = TRUE)),
        ctx_stat("Scenarios \u00d7 periods", textOutput("ctx_combos_dl", inline = TRUE)),
        ctx_stat("Estimated size", textOutput("ctx_size_dl", inline = TRUE))
      ),
      action = actionLink("ctx_dl_go", "Change location"),
      mini_map = TRUE
    ),
    layout_sidebar(
      fillable = TRUE,
      sidebar = sidebar(
        width = 280,
        title = "Build a download",
        widget_download_variable,
        conditionalPanel(
          condition = paste0(
            "input.dl_variable && ",
            "(input.dl_variable.indexOf('chem_') === 0 || input.dl_variable.indexOf('wbchem_') === 0)"
          ),
          widget_download_chem_stat
        ),
        widget_download_data_type,
        widget_download_spatial_layer,
        widget_download_format,
        helpText("Monthly p50 values for the current selection (full daily values for chemical concentrations; water body variables are always exported for all 336 modelled water bodies, since this page has no water body picker). Units travel with the file."),
        downloadButton("download_data", "Download data")
      ),
      card(
        card_header("Preview", span(class = "panel-note", textOutput("dl_preview_note", inline = TRUE))),
        DT::dataTableOutput("data_table")
      )
    )
  ),

  ## PANEL 8: Help
  nav_panel(
    title = "Help",
    div(style = "padding: 24px; max-width: 760px;", includeHTML("help.htm"))
  ),

  nav_spacer(),

  nav_menu(
    title = "Links",
    align = "right",
    nav_item(tags$a("York", href = "https://www.york.ac.uk")),
    nav_item(tags$a("Durham", href = "https://durham.ac.uk")),
    nav_item(tags$a("Sheffield", href = "https://sheffield.ac.uk")),
  )
)


### 2. Server

server <- function(input, output, session) {

  ### NAVBAR 1 - MAP ###

  # use reactive values to store the id from observing the shape click
  rv <- reactiveVal()

  # Selected measured site (Site_id) and modelled water body (water_body
  # code), set by clicking a marker/polygon on the Map; drive the Site
  # Details tab. Kept as two separate reactiveVals (rather than one shared id
  # + a type flag) so toggling between Measured/Modelled on Site Details
  # preserves each side's own last selection instead of clobbering it.
  rv_measured_site <- reactiveVal()
  rv_waterbody <- reactiveVal()

  # Selected ecological risk point (chemical key + site_id), set by clicking
  # a marker's popup link on the Map's Ecological risk layer; drives the
  # Ecological Risk detail tab. Two parallel reactiveVals rather than one
  # compound value, matching the rv_measured_site/rv_waterbody idiom above.
  rv_eco_risk_chemical <- reactiveVal()
  rv_eco_risk_site <- reactiveVal()

  selected_climate <- reactive({
    req(rv())
    df_stats_climate %>% filter(subbasin == rv())
  })

  selected_lc <- reactive({
    req(rv())
    df_stats_lc %>% filter(subbasin == rv())
  })

  selected_historical <- reactive({
    req(rv())
    sb <- as.integer(rv())
    ds_historical_observations %>% filter(subbasin == sb) %>% collect()
  })

  chem_stat_labels <- setNames(names(chem_stat_choices), unname(chem_stat_choices))

  # Daily series (subbasin mode) or monthly series (water body mode) for the
  # selected chemical and statistic (or statistics - the "Conditions"
  # selector allows more than one), feeding the chemical branch of the three
  # Projections plots and the per-subbasin downloads. Because every
  # Projections renderer already routes through this one reactive whenever
  # is_chem_variable() is true, the water-body branch below is the only
  # change those renderers need.
  chem_selected_series <- reactive({
    var <- input$prediction_variable
    req(is_chem_variable(var))
    chem_key <- chem_key_from_variable(var)
    # "Conditions" briefly still holds the HYPE p10/p50/p90 selection for one
    # reactive tick after prediction_variable switches to a chemical (the
    # observer that relabels it to mean/median/90th hasn't landed yet) - drop
    # anything that isn't a real chemical statistic so that tick renders
    # nothing instead of trying to open e.g. "..._p50_conc_by_SITE_ID.csv".
    stats <- intersect(unique(input$prediction_percentile), unname(chem_stat_choices))
    req(length(stats) > 0)

    if (identical(input$de_entity_type, "waterbody")) {
      req(rv_de_waterbody())
      dplyr::bind_rows(lapply(stats, function(s) {
        get_wb_chem_series(chem_key, s, rv_de_waterbody()) %>% dplyr::mutate(stat = s)
      }))
    } else {
      req(rv())
      if (!(rv() %in% chem_subbasin_ids)) return(data.frame())
      dplyr::bind_rows(lapply(stats, function(s) {
        get_chem_subbasin_series(chem_key, s, rv()) %>% dplyr::mutate(stat = s)
      }))
    }
  })

  # ---- Chemical Explorer page (D3D percentile time series) ----

  # Site id to land on once the site-choice list for the target chemical has
  # been (re)populated - normally NULL (defaults to the first site); set
  # briefly by the Map's "View chemical time series" popup link below so the
  # jump lands on the right site even though the site list is
  # chemical-specific and gets rebuilt by the observer just below whenever
  # input$chem_d3d_key changes (mirrors rv_eco_risk_chemical/rv_eco_risk_site
  # further down, the same pattern for the Ecological Risk jump-link).
  chem_d3d_pending_site <- reactiveVal(NULL)

  # Site choices depend on which chemical is selected - updated via
  # server-side selectize (500+ sites) rather than sending the whole list to
  # the browser, mirroring the "de_waterbody_search" pattern above. Runs on
  # session start too (default input$chem_d3d_key from the UI), so the site
  # selector is already populated the first time this tab is opened.
  observeEvent(input$chem_d3d_key, {
    req(input$chem_d3d_key)
    ids <- chem_d3d_site_choices[[input$chem_d3d_key]]
    pending <- chem_d3d_pending_site()
    current <- isolate(input$chem_d3d_site)
    # Keep the current water body when switching chemical (if the new chemical
    # has it); a pending jump-link site wins, otherwise fall back to the first.
    target <- if (!is.null(pending) && pending %in% ids) pending
              else if (!is.null(current) && nzchar(current) && current %in% ids) current
              else ids[1]
    chem_d3d_pending_site(NULL)
    updateSelectizeInput(session, "chem_d3d_site",
                         choices = ids, selected = target, server = TRUE)
  })

  # Open the Chemical Explorer tab from the Map's ecological-risk popup link,
  # which passes "<chemical key>||<site_id>" - same "||" delimiter and
  # Shiny.setInputValue mechanism as open_eco_risk_detail below. Sets
  # chem_d3d_site directly (covers today's single-chemical case, where
  # updateSelectInput below is a same-value no-op that won't re-trigger the
  # observer above) and stages chem_d3d_pending_site too (covers a future
  # multi-chemical case, where input$chem_d3d_key genuinely changes and the
  # observer above would otherwise reset the site choice back to ids[1] on
  # the next reactive flush).
  observeEvent(input$open_chem_d3d_detail, {
    parts <- strsplit(input$open_chem_d3d_detail, "\\|\\|")[[1]]
    chem_key <- parts[1]
    site <- parts[2]
    req(chem_key %in% names(chem_d3d_datasets), !is.na(site), site %in% chem_d3d_site_ids[[chem_key]])
    ids <- chem_d3d_site_choices[[chem_key]]
    chem_d3d_pending_site(site)
    chem_d3d_skip_carry(TRUE)
    updateSelectInput(session, "chem_d3d_key", selected = chem_key)
    updateSelectizeInput(session, "chem_d3d_site",
                         choices = ids, selected = site, server = TRUE)
    bslib::nav_select("main_nav", selected = "Chemical Explorer", session = session)
  })

  # Carry the Map / Site Details selection over when the Chemical Explorer
  # tab is opened. last_wb_sel records whichever of the Map's subbasin (rv)
  # or Site Details' modelled water body (rv_waterbody) changed most
  # recently; both resolve to SITE_IDs through chem_site_wb. The popup jump
  # link above sets chem_d3d_skip_carry so its explicit site wins.
  chem_d3d_skip_carry <- reactiveVal(FALSE)
  last_wb_sel <- reactiveVal(NULL)
  observeEvent(rv(), { last_wb_sel(list(type = "subbasin", value = rv())) }, ignoreNULL = TRUE)
  observeEvent(rv_waterbody(), { last_wb_sel(list(type = "waterbody", value = rv_waterbody())) }, ignoreNULL = TRUE)

  observeEvent(input$main_nav, {
    req(input$main_nav == "Chemical Explorer")
    if (isTRUE(chem_d3d_skip_carry())) { chem_d3d_skip_carry(FALSE); return() }
    sel <- last_wb_sel()
    req(!is.null(sel), input$chem_d3d_key)
    hits <- if (sel$type == "subbasin") {
      chem_site_wb$SITE_ID[chem_site_wb$subbasin == as.numeric(sel$value)]
    } else {
      chem_site_wb$SITE_ID[which(chem_site_wb$wb_code == sel$value)]
    }
    ids <- chem_d3d_site_choices[[input$chem_d3d_key]]
    hits <- hits[hits %in% ids]
    req(length(hits) > 0)
    updateSelectizeInput(session, "chem_d3d_site", choices = ids, selected = hits[1], server = TRUE)
  })

  chem_d3d_series_raw <- reactive({
    req(input$chem_d3d_key, input$chem_d3d_site)
    get_chem_d3d_series(input$chem_d3d_key, input$chem_d3d_site)
  })

  # Weekly/monthly grouping bins by calendar week/month and applies the
  # chosen statistic to each percentile series independently, so the ribbon
  # stays a real (smoothed) p10-p90 band rather than a min/max envelope.
  chem_d3d_series_grouped <- reactive({
    df <- chem_d3d_series_raw()
    grouping <- input$chem_d3d_grouping %||% "daily"
    if (nrow(df) == 0 || grouping == "daily") return(df)
    unit <- if (grouping == "weekly") "week" else "month"
    stat_fun <- if (identical(input$chem_d3d_group_stat, "p90")) {
      function(v) quantile(v, 0.9, na.rm = TRUE, names = FALSE)
    } else {
      function(v) median(v, na.rm = TRUE)
    }
    df %>%
      dplyr::mutate(Date = as.Date(cut(Date, unit))) %>%
      dplyr::group_by(Date) %>%
      dplyr::summarise(dplyr::across(dplyr::everything(), stat_fun), .groups = "drop")
  })

  chem_d3d_thresholds <- reactive({
    req(input$chem_d3d_key, input$chem_d3d_site)
    get_chem_d3d_thresholds(input$chem_d3d_key, input$chem_d3d_site)
  })

  output$chem_d3d_threshold_note <- renderText({
    thr <- chem_d3d_thresholds()
    if (is.null(thr)) "no threshold data for this chemical/site" else paste0(thr$source, " HC5 thresholds")
  })

  # Shared ribbon + median line for one measure ("conc" or "sspaf").
  chem_d3d_band_plot <- function(df, measure, log_y = FALSE) {
    d <- data.frame(x = df$Date,
                    lo = df[[paste0(measure, "_p10")]],
                    mid = df[[paste0(measure, "_p50")]],
                    hi = df[[paste0(measure, "_p90")]])
    # Exact zeros are undefined on a log scale, so they become gaps.
    if (log_y) d[c("lo", "mid", "hi")] <- lapply(d[c("lo", "mid", "hi")], function(v) ifelse(v > 0, v, NA_real_))
    ggplot(d, aes(x = x, y = mid)) +
      geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.18, colour = NA, fill = ec$accent) +
      geom_line(colour = ec$accent, linewidth = 0.8)
  }

  output$chem_d3d_conc_plot <- renderPlot({
    df <- chem_d3d_series_grouped()
    if (nrow(df) == 0) return(no_data_plot("No data for this site"))
    ds <- chem_d3d_datasets[[input$chem_d3d_key]]
    thr <- chem_d3d_thresholds()

    # Concentrations sit orders of magnitude below any threshold line, so a
    # log10 axis is used (zero days show as gaps).
    p <- chem_d3d_band_plot(df, "conc", log_y = TRUE)
    if (!is.null(thr)) {
      df_thr <- data.frame(
        label = factor(c("Acute HC5", "Chronic HC5/10"), levels = c("Acute HC5", "Chronic HC5/10")),
        value = c(thr$acute, thr$chronic)
      )
      p <- p +
        geom_hline(data = df_thr, aes(yintercept = value, linetype = label), colour = ec$ink, linewidth = 0.5) +
        scale_linetype_manual(values = c("Acute HC5" = "dashed", "Chronic HC5/10" = "dotted"))
    }
    p +
      scale_x_date(expand = expansion(mult = c(0.01, 0.03))) +
      scale_y_log10() +
      labs(x = NULL, y = paste0(ds$label, " concentration [", ds$unit, ", log scale]"),
           caption = "Gaps mark exact-zero days (undefined on a log scale).") +
      theme_ecomix()
  }) %>%
    bindCache(input$chem_d3d_key, input$chem_d3d_site, input$chem_d3d_grouping, input$chem_d3d_group_stat,
            sizePolicy = function(dims) dims)

  output$chem_d3d_sspaf_plot <- renderPlot({
    df <- chem_d3d_series_grouped()
    if (nrow(df) == 0) return(no_data_plot("No data for this site"))
    ds <- chem_d3d_datasets[[input$chem_d3d_key]]
    chem_d3d_band_plot(df, "sspaf") +
      scale_x_date(expand = expansion(mult = c(0.01, 0.03))) +
      scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
      labs(x = NULL, y = paste0(ds$label, " ssPAF")) +
      theme_ecomix()
  }) %>%
    bindCache(input$chem_d3d_key, input$chem_d3d_site, input$chem_d3d_grouping, input$chem_d3d_group_stat,
            sizePolicy = function(dims) dims)

  # ---- Source apportionment treemaps ----
  # Hovering either time series picks a day (the last hovered day sticks once
  # the cursor leaves); until then, and whenever the chemical changes, both
  # treemaps show the period average.
  chem_d3d_has_src <- reactive(isTRUE(input$chem_d3d_key %in% names(chem_src_files)))
  chem_d3d_hover_date <- reactiveVal(NULL)
  set_hover_date <- function(h) {
    if (is.null(h$x) || !chem_d3d_has_src()) return()
    d <- as.Date(round(h$x), origin = "1970-01-01")
    d <- min(max(d, chem_src_first_date), chem_src_last_date)
    if (!identical(d, chem_d3d_hover_date())) chem_d3d_hover_date(d)
  }
  observeEvent(input$chem_d3d_hover_conc, set_hover_date(input$chem_d3d_hover_conc))
  observeEvent(input$chem_d3d_hover_sspaf, set_hover_date(input$chem_d3d_hover_sspaf))
  observeEvent(input$chem_d3d_key, chem_d3d_hover_date(NULL), ignoreInit = TRUE)

  output$chem_d3d_src_col <- renderUI({
    req(chem_d3d_has_src())
    sq <- function(id) div(style = "width: 100%; aspect-ratio: 1 / 1;", plotOutput(id, width = "100%", height = "100%"))
    div(
      style = "width: 320px; margin-left: 1rem; display: flex; flex-direction: column; gap: 1rem;",
      card(fill = FALSE, card_header("Average source apportionment"), sq("chem_d3d_treemap_avg")),
      card(fill = FALSE, card_header(textOutput("chem_d3d_treemap_day_title", inline = TRUE)), sq("chem_d3d_treemap_day"))
    )
  })

  chem_d3d_treemap <- function(split, empty_msg) {
    if (is.null(split)) return(no_data_plot(empty_msg))
    d <- data.frame(src = names(split), share = as.numeric(split))
    d <- d[d$share > 0, ]
    lay <- treemap_layout(d$share)
    d <- cbind(d, lay)
    d$label <- paste0(chem_src_labels[d$src], "\n", sprintf("%.1f%%", 100 * d$share))
    d$cx <- (d$x0 + d$x1) / 2
    d$cy <- (d$y0 + d$y1) / 2
    # Skip labels on tiles too small to hold them; the legend-free design
    # still lists them in the caption.
    d$show <- (d$x1 - d$x0) > 0.2 & (d$y1 - d$y0) > 0.12
    small <- d[!d$show, ]
    ggplot(d) +
      geom_rect(aes(xmin = x0, xmax = x1, ymin = y0, ymax = y1, fill = src),
                colour = ec$ground, linewidth = 1.5) +
      geom_text(data = d[d$show, ], aes(cx, cy, label = label, colour = src),
                family = "Archivo", size = 3.6, lineheight = 0.95) +
      scale_fill_manual(values = chem_src_fill) +
      scale_colour_manual(values = chem_src_text) +
      coord_fixed(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
      labs(caption = if (nrow(small) > 0) paste(
        paste0(chem_src_labels[small$src], " ", sprintf("%.1f%%", 100 * small$share)), collapse = " · ")) +
      theme_void(base_family = "Archivo") +
      theme(legend.position = "none",
            plot.background = element_rect(fill = ec$ground, colour = NA),
            plot.caption = element_text(colour = ec$ink_dim, size = 8, hjust = 0))
  }

  output$chem_d3d_treemap_avg <- renderPlot({
    req(chem_d3d_has_src(), input$chem_d3d_site)
    chem_d3d_treemap(get_chem_src_split(input$chem_d3d_key, input$chem_d3d_site),
                     "No source apportionment for this site")
  }) %>%
    bindCache(input$chem_d3d_key, input$chem_d3d_site, sizePolicy = function(dims) dims)

  output$chem_d3d_treemap_day_title <- renderText({
    d <- chem_d3d_hover_date()
    if (is.null(d)) "Selected day · hover a graph to choose" else paste("Source apportionment ·", format(d, "%d %b %Y"))
  })

  output$chem_d3d_treemap_day <- renderPlot({
    req(chem_d3d_has_src(), input$chem_d3d_site)
    d <- chem_d3d_hover_date()
    chem_d3d_treemap(get_chem_src_split(input$chem_d3d_key, input$chem_d3d_site, d),
                     "No source apportionment for this day")
  }) %>%
    bindCache(input$chem_d3d_key, input$chem_d3d_site, chem_d3d_hover_date(), sizePolicy = function(dims) dims)

  # The selected day is marked on both time series in the browser (the
  # chem_d3d_marker script in the page head) rather than drawn into them: a
  # hover used to re-render both full plots on the server (~0.3 s an event).
  # NA (JSON null) hides the marker.
  observe({
    d <- chem_d3d_hover_date()
    session$sendCustomMessage("chem_d3d_marker", list(day = if (is.null(d)) NA else as.numeric(d)))
  })

  selected_opcat <- reactive({
    if (is.null(rv())) return(NA_character_)
    df_tmp <- df_subbasin_opcat %>% filter(subbasin == rv())
    if (nrow(df_tmp) == 0 || is.na(df_tmp$opcat_name[1])) NA_character_ else df_tmp$opcat_name[1]
  })

  # A click on a hydrology subbasin selects it. Leaflet reports clicks on
  # every polygon on the map, so the others are ignored: a water body's id is
  # its EA code, not a subbasin, and polygons drawn without a layerId
  # (catchment boundaries, chemical coverage) report a NULL id, which would
  # clear the selection.
  observeEvent(input$basemap_shape_click, {
    click <- input$basemap_shape_click
    if (identical(click$group, "Subbasins") && !is.null(click$id)) rv(click$id)
  })

  # Search box on the Map rail
  updateSelectizeInput(session, "subbasin_search",
                       choices = sort(unique(subbasin_opcat_shp$Id)),
                       selected = character(0), server = TRUE)

  observeEvent(input$subbasin_search, {
    req(nzchar(input$subbasin_search))
    rv(as.numeric(input$subbasin_search))
  }, ignoreInit = TRUE)

  # Open Hydro Explorer from map popup link and keep selected subbasin in sync.
  observeEvent(input$open_data_explorer, {
    rv(as.numeric(input$open_data_explorer))
    bslib::nav_select("main_nav", selected = "Hydro Explorer", session = session)
  })

  # Context strip actions
  observeEvent(input$ctx_map_go, {
    bslib::nav_select("main_nav", selected = "Hydro Explorer", session = session)
  })
  observeEvent(input$ctx_de_go, {
    bslib::nav_select("main_nav", selected = "Map", session = session)
  })
  observeEvent(input$ctx_sp_go, {
    bslib::nav_select("main_nav", selected = "Map", session = session)
  })
  observeEvent(input$ctx_dl_go, {
    bslib::nav_select("main_nav", selected = "Map", session = session)
  })

  ## Reactive selection of observed variables
  observation_choices <- reactive({
    if (is.null(rv())) {
      c("Please select a subbasin")
    } else {
      df_tmp <- df_observed_subbasins %>% filter(subbasin == rv())
      if (nrow(df_tmp) == 0) {
        c("No Observations available")
      } else {
        df_tmp[["variable"]]
      }
    }
  })

  observeEvent(observation_choices(), {
    updateSelectizeInput(
      session,
      inputId = "observation_variable",
      choices = observation_choices(),
      selected = NULL
    )
  })

  # Base map. Subbasins are drawn in the neutral ramp so the accent can mark
  # the current selection unambiguously. Only the subbasins with hydrology
  # output are drawn; the others (climate/chemical data only) stay reachable
  # through the search box and the chemical coverage layer.
  #
  # The widget is the same for every session and reads no reactive value, so
  # it is built once (3126 polygons with popups: ~1.4 s of R and 4.3 MB) and
  # then served from Shiny's app cache, shared by every session of the one R
  # process, keyed by the version of the data it is drawn from (see
  # R/10_data_layers.R). Everything that changes on it - overlays, selection,
  # tiles, framing - goes through leafletProxy() below. It also creates the
  # overlay panes (add_map_panes()).
  output$basemap <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = TRUE)) %>%
      add_basemap_tiles("osm") %>%
      setView(lng = -1.16, lat = 53.75, zoom = 8.5) %>%
      add_map_panes() %>%
      addPolygons(data = hype_subbasin_shp,
                  fill = TRUE,
                  fillColor = ~ifelse(is.na(opcat_name), "#e0dede", pal_opcat(opcat_name)),
                  fillOpacity = 0.55,
                  color = ec$ink,
                  opacity = 0.45,
                  weight = 1,
                  popup = ~paste0(
                    "<strong>Subbasin id: </strong>", Id,
                    "<br><strong>Operational catchment: </strong>", ifelse(is.na(opcat_name), "None", opcat_name),
                    "<br><a href='#' onclick=\"Shiny.setInputValue('open_data_explorer', '",
                    Id,
                    "', {priority: 'event'}); return false;\">Open in Hydro Explorer</a>"
                  ),
                  layerId = ~Id,
                  group = "Subbasins")
  }) %>%
    bindCache(basemap_data_version, cache = "app")

  # Layer toggles from the rail (replacing leaflet's own layers control, so
  # the map surface carries no floating chrome of its own). Each overlay is
  # added to the map the first time it is switched on, into its own pane,
  # and from then on only shown or hidden: clearing and redrawing every
  # overlay that was on cost ~1.3 s and up to 4 MB per click. Which overlays
  # this session's map already holds:
  map_overlays_added <- reactiveVal(character())

  observeEvent(input$map_layers, {
    layers <- input$map_layers
    proxy <- leafletProxy("basemap")
    if ("subbasins" %in% layers) {
      proxy %>% showGroup("Subbasins")
    } else {
      proxy %>% hideGroup("Subbasins")
    }
    for (layer in setdiff(intersect(names(map_overlays), layers), map_overlays_added())) {
      map_overlays[[layer]](proxy)
      map_overlays_added(c(map_overlays_added(), layer))
    }
    shown <- intersect(map_overlays_added(), layers)
    hidden <- setdiff(map_overlays_added(), layers)
    if (length(shown) > 0) proxy %>% showGroup(shown)
    if (length(hidden) > 0) proxy %>% hideGroup(hidden)
    draw_eco_risk_layer()
  }, ignoreNULL = FALSE)

  # Ecological risk layer - shared by its layer toggle and its chemical
  # picker, since either one changing can change it. Its markers are drawn
  # again only when the chemical differs from the one last drawn on this
  # session's map, and its legend is on the map exactly while the layer is
  # (some chemicals have no risk file: no markers, no legend).
  eco_risk_drawn <- reactiveVal(list(on = FALSE, chemical = NULL))

  draw_eco_risk_layer <- function() {
    on <- "eco_risk" %in% input$map_layers
    chemical <- input$eco_risk_chemical
    drawn <- eco_risk_drawn()
    if (!on && !drawn$on) return()
    if (on && drawn$on && identical(chemical, drawn$chemical)) return()
    proxy <- leafletProxy("basemap")
    if (!on) {
      proxy %>% hideGroup("eco_risk") %>% removeControl("eco_risk_legend")
    } else {
      df <- eco_risk_data[[chemical]]
      if (!identical(chemical, drawn$chemical)) {
        proxy %>% clearGroup("eco_risk")
        if (!is.null(df)) proxy %>% add_eco_risk_layer(df, chemical)
        drawn$chemical <- chemical
      }
      proxy %>% showGroup("eco_risk")
      if (is.null(df)) {
        proxy %>% removeControl("eco_risk_legend")
      } else {
        proxy %>% add_eco_risk_legend(df)
      }
    }
    drawn$on <- on
    eco_risk_drawn(drawn)
  }

  # Redraw when the ecological risk chemical picker changes (the layer
  # toggle above only fires on map_layers changes, not this).
  observeEvent(input$eco_risk_chemical, draw_eco_risk_layer(), ignoreInit = TRUE)

  # Basemap tile switcher.
  observeEvent(input$map_basemap, {
    leafletProxy("basemap") %>%
      clearGroup("basetile") %>%
      add_basemap_tiles(input$map_basemap)
  }, ignoreInit = TRUE)

  # Open Site Details from a measured site marker's popup link, same
  # "click marker -> pick from its popup" pattern as the subbasin/observed-
  # hydrology "Open in Hydro Explorer" links.
  observeEvent(input$open_site_details, {
    rv_measured_site(input$open_site_details)
    bslib::nav_select("site_entity_type", selected = "measured", session = session)
    bslib::nav_select("main_nav", selected = "Site Details", session = session)
  })

  # Open Site Details from a water body polygon's popup link, landing on the
  # "Modelled" side of the Site Details toggle with that water body selected.
  observeEvent(input$open_waterbody_details, {
    rv_waterbody(input$open_waterbody_details)
    bslib::nav_select("site_entity_type", selected = "modelled", session = session)
    bslib::nav_select("main_nav", selected = "Site Details", session = session)
  })

  # Open the Ecological Risk detail tab from an eco-risk marker's popup
  # link, which passes "<chemical key>||<site_id>".
  observeEvent(input$open_eco_risk_detail, {
    parts <- strsplit(input$open_eco_risk_detail, "\\|\\|")[[1]]
    rv_eco_risk_chemical(parts[1])
    rv_eco_risk_site(as.integer(parts[2]))
    bslib::nav_select("main_nav", selected = "Ecological Risk", session = session)
  })

  # Ecological Risk detail tab - single row for the selected chemical/site,
  # NULL until a point has been picked from the Map's Ecological risk layer.
  eco_risk_selected <- reactive({
    chem <- rv_eco_risk_chemical()
    site <- rv_eco_risk_site()
    if (is.null(chem) || is.null(site)) return(NULL)
    df <- eco_risk_data[[chem]]
    if (is.null(df)) return(NULL)
    row <- df[df$site_id == site, ]
    if (nrow(row) == 0) NULL else row
  })

  output$eco_risk_detail_card <- renderUI({
    row <- eco_risk_selected()
    if (is.null(row)) {
      return(div(class = "panel-note",
                 "Select a point on the Map's Ecological risk layer to see its detail here."))
    }

    fmt_conc <- function(x) formatC(x, format = "g", digits = 3)
    hc5_txt <- function(val) if (is_na_string(val)) "Not available" else val

    tagList(
      div(
        class = "ctx-stats",
        ctx_stat("Chemical", tools::toTitleCase(rv_eco_risk_chemical())),
        ctx_stat("Site id", row$site_id),
        ctx_stat("Subbasin", row$subbasin),
        ctx_stat("Risk level",
                 span(style = paste0("color:", eco_risk_pal(row$risk_level), "; font-weight:600;"),
                      as.character(row$risk_level)))
      ),
      div(
        class = "ctx-stats",
        ctx_stat("Easting", row$EASTING),
        ctx_stat("Northing", row$NORTHING),
        ctx_stat("Days of data", row$n_days)
      ),
      div(
        class = "ctx-stats",
        ctx_stat("Median 90th-pctile conc. (µg/L)", fmt_conc(row$median_of_p90_ug)),
        ctx_stat("95th-pctile of 90th-pctile conc. (µg/L)", fmt_conc(row$p95_of_p90_ug)),
        ctx_stat("Median conc. (µg/L)", fmt_conc(row$median_of_median_ug)),
        ctx_stat("95th-pctile of median conc. (µg/L)", fmt_conc(row$p95_of_median_ug))
      ),
      div(
        class = "ctx-stats",
        ctx_stat("Generic HC5", fmt_conc(row$generic_hc5)),
        ctx_stat("Generic HC5/10", fmt_conc(row[["generic_hc5_divided by 10"]])),
        ctx_stat("Site-specific HC5", hc5_txt(row[["site_specific hc5"]])),
        ctx_stat("Site-specific HC5/10", hc5_txt(row[["site_specific hc5/10"]]))
      ),
      tags$h6("Threshold comparisons", style = "margin-top:12px;"),
      tagList(lapply(names(eco_risk_flag_labels), function(col) {
        ss_col <- paste0("ss_", col)
        ss_val <- if (ss_col %in% names(row)) row[[ss_col]] else NA
        ss_txt <- if (!is.null(ss_val) && !is_na_string(ss_val)) paste0(" · site-specific: ", ss_val) else ""
        div(style = paste0("font-size:12px; color:", ec$ink_dim, "; margin-bottom:4px;"),
            paste0(eco_risk_flag_labels[[col]], ": ", row[[col]], ss_txt))
      }))
    )
  })

  # Highlight the selected subbasin in the accent, above every overlay. It
  # takes no clicks, so a click on the selected subbasin reaches the subbasin
  # beneath (a click on the highlight itself used to clear the selection).
  # An id with no subbasin polygon leaves nothing highlighted, as on the
  # context-strip maps.
  observeEvent(rv(), {
    shp <- subbasin_opcat_shp %>% filter(Id == rv())
    proxy <- leafletProxy("basemap") %>% clearGroup("selection")
    if (nrow(shp) == 0) return()
    proxy %>%
      addPolygons(data = shp, fill = TRUE, fillColor = ec$accent, fillOpacity = 0.35,
                  color = ec$accent, weight = 3, opacity = 1, group = "selection",
                  options = pathOptions(interactive = FALSE, pane = "selection"))
  })

  # Operational catchment rail: click a row to frame that catchment.
  output$opcat_rail <- renderUI({
    active <- selected_opcat()
    tagList(lapply(seq_len(nrow(opcat_counts)), function(i) {
      nm <- opcat_counts$opcat_name[i]
      div(
        class = paste("rail-row", if (!is.na(active) && identical(active, nm)) "is-active" else ""),
        onclick = paste0("Shiny.setInputValue('opcat_pick', ", jsonlite::toJSON(nm, auto_unbox = TRUE),
                         ", {priority:'event'})"),
        div(class = "rail-row-label",
            span(class = "rail-swatch", style = paste0("background:", pal_opcat(nm), ";")),
            span(nm)),
        span(class = "rail-num", opcat_counts$n_subbasins[i])
      )
    }))
  })

  observeEvent(input$opcat_pick, {
    bb <- opcat_bounds[[input$opcat_pick]]
    req(!is.null(bb))
    leafletProxy("basemap") %>% fitBounds(bb[1], bb[2], bb[3], bb[4])
  })


  ### CONTEXT STRIP ###
  #
  # One registration call per panel; every panel gets its own output ids
  # because nav_panel() renders all panels into the DOM at once.

  ctx_area <- reactive({
    if (is.null(rv())) return("\u2014")
    df <- selected_lc()
    v <- df$value[df$variable == "Upstream area"]
    if (length(v) == 0) return("\u2014")
    paste0(round(v / 1e6, 2), " km\u00b2")
  })

  ctx_precip <- reactive({
    if (is.null(rv())) return("\u2014")
    paste0(round(selected_climate()$precip[1], 0), " mm")
  })

  ctx_temp <- reactive({
    if (is.null(rv())) return("\u2014")
    paste0(round(selected_climate()$maat[1], 1), " \u00b0C")
  })

  register_ctx_subbasin <- function(suffix, mini_map = TRUE) {
    output[[paste0("ctx_id_", suffix)]] <- renderText({
      if (is.null(rv())) "No location" else as.character(rv())
    })
    output[[paste0("ctx_sub_", suffix)]] <- renderText({
      if (is.null(rv())) return("Pick a subbasin on the map")
      nm <- selected_opcat()
      if (is.na(nm)) "No operational catchment" else nm
    })
    output[[paste0("ctx_area_", suffix)]] <- renderText(ctx_area())
    output[[paste0("ctx_precip_", suffix)]] <- renderText(ctx_precip())
    output[[paste0("ctx_temp_", suffix)]] <- renderText(ctx_temp())
    output[[paste0("ctx_obs_", suffix)]] <- renderUI({
      if (is.null(rv())) return(span(style = "font-size:13px; font-weight:400; color:#605d5d;", "\u2014"))
      vars <- df_observed_subbasins %>% filter(subbasin == rv()) %>% pull(variable)
      if (length(vars) == 0) {
        return(span(style = "font-size:13px; font-weight:400; color:#605d5d;", "none at this subbasin"))
      }
      tagList(lapply(vars, function(v) span(class = "ctx-tag", v)))
    })

    # The locator map is rendered from the selection rather than updated
    # through leafletProxy(): the selection is made on the Map page while
    # these maps sit on hidden tabs, and leaflet.js drops proxy calls for a
    # map that has not been rendered yet, so a first visit showed no
    # highlight. Cached by the selection, in the app cache like the base map.
    if (isTRUE(mini_map)) {
      output[[paste0("ctx_map_", suffix)]] <- renderLeaflet(ctx_subbasin_map(rv())) %>%
        bindCache(rv())
    }
  }

  register_ctx_subbasin("map", mini_map = FALSE)
  register_ctx_subbasin("de")
  register_ctx_subbasin("sp")
  register_ctx_subbasin("dl")


  ### SITE DETAILS ###

  # Resets the four time series selectors to the active entity's top-range
  # chemicals, so switching sites/water bodies or toggling Measured/Modelled
  # gives a fresh, sensible default rather than carrying over the previous
  # selection's picks.
  observeEvent(list(input$site_entity_type, rv_measured_site(), rv_waterbody()), {
    is_modelled <- identical(input$site_entity_type, "modelled")
    if (is_modelled) {
      req(rv_waterbody())
      choices <- modelled_parameter_choices
      top4 <- selected_waterbody_top_chemicals()$chem_key
      fallback <- chem_keys
    } else {
      req(rv_measured_site())
      choices <- measured_parameter_choices
      top4 <- selected_measured_site_top_chemicals()$parameter
      fallback <- measured_chemical_cols
    }
    if (length(top4) < 4) {
      top4 <- c(top4, setdiff(fallback, top4))[seq_len(4)]
    }
    for (i in seq_len(4)) {
      updateSelectInput(session, paste0("site_ts_chemical_", i), choices = choices, selected = top4[i])
    }
  }, ignoreNULL = FALSE)

  # Full measured record (all parameters, all weeks) for the selected site
  selected_measured_site_long <- reactive({
    req(rv_measured_site())
    df_measured_long %>% dplyr::filter(Site_id == rv_measured_site())
  })

  # Site Details context strips (site/water-body-scoped, so registered
  # separately from the subbasin strips above). Both entity types share one
  # set of ctx_* outputs, branching internally on input$site_entity_type.
  measured_site_display_name <- reactive({
    if (is.null(rv_measured_site())) return(NULL)
    row <- df_measured_sites %>% filter(Site_id == rv_measured_site())
    if (nrow(row) == 0) NULL else row$Site_full_name[1]
  })

  waterbody_display_name <- reactive({
    if (is.null(rv_waterbody())) return(NULL)
    row <- waterbody_opcat_shp %>% sf::st_drop_geometry() %>% filter(water_body == rv_waterbody())
    if (nrow(row) == 0) NULL else row$water_bod0[1]
  })

  output$ctx_idlabel_site <- renderText({
    if (identical(input$site_entity_type, "modelled")) "Modelled water body" else "Measured site"
  })

  output$ctx_id_site <- renderText({
    if (identical(input$site_entity_type, "modelled")) {
      waterbody_display_name() %||% "No water body"
    } else {
      measured_site_display_name() %||% "No site"
    }
  })
  output$ctx_sub_site <- renderText({
    if (identical(input$site_entity_type, "modelled")) {
      if (is.null(rv_waterbody())) return("Pick a water body in the list")
      row <- waterbody_opcat_shp %>% sf::st_drop_geometry() %>% filter(water_body == rv_waterbody())
      if (nrow(row) == 0) return(rv_waterbody())
      paste0(rv_waterbody(), ifelse(is.na(row$opcat_name[1]), "", paste0(" · ", row$opcat_name[1])))
    } else {
      if (is.null(rv_measured_site())) return("Pick a site in the list")
      row <- df_measured_sites %>% filter(Site_id == rv_measured_site())
      paste0(rv_measured_site(), " · ", round(row$Latitude[1], 4), ", ", round(row$Longitude[1], 4))
    }
  })
  output$ctx_summary_site <- renderText({
    if (identical(input$site_entity_type, "modelled")) {
      if (is.null(rv_waterbody())) return("Select a modelled water body to see its background.")
      info <- modelled_site_info[[rv_waterbody()]]
      if (is.null(info)) return(paste("No water body information available yet for", rv_waterbody()))
      info$summary
    } else {
      if (is.null(rv_measured_site())) return("Select a measured site to see its background.")
      info <- measured_site_info[[rv_measured_site()]]
      if (is.null(info)) return(paste("No site information available yet for", rv_measured_site()))
      info$summary
    }
  })
  output$ctx_landcover_site <- renderUI({
    is_modelled <- identical(input$site_entity_type, "modelled")
    active_id <- if (is_modelled) rv_waterbody() else rv_measured_site()
    if (is.null(active_id)) return(NULL)
    info <- if (is_modelled) modelled_site_info[[rv_waterbody()]] else measured_site_info[[rv_measured_site()]]
    if (is.null(info) || length(info$landcover) == 0) return(NULL)

    lc <- info$landcover
    shades <- grDevices::colorRampPalette(c(ec$accent_text, ec$accent_tint))(length(lc))
    tagList(
      div(style = "display:flex; height:14px; margin-top:2px;",
          lapply(seq_along(lc), function(i) {
            div(style = paste0("width:", lc[i], "%; background:", shades[i], ";"))
          })),
      div(style = "display:flex; flex-wrap:wrap; gap:10px; margin-top:6px; font-size:10px; font-weight:400; color:#605d5d;",
          lapply(seq_along(lc), function(i) span(paste0(names(lc)[i], " ", lc[i]))))
    )
  })

  # Small locator map in the Site Details context strip: the selection on
  # the side (Measured/Modelled) being browsed. Rendered from it and cached
  # by it, like the subbasin locator maps (see register_ctx_subbasin()).
  site_locator <- reactive({
    if (identical(input$site_entity_type, "modelled")) {
      list(type = "modelled", id = rv_waterbody())
    } else {
      list(type = "measured", id = rv_measured_site())
    }
  })

  output$ctx_map_site <- renderLeaflet(do.call(ctx_site_map, site_locator())) %>%
    bindCache(site_locator())

  # Measured sites rail (replaces the full-width selectInput)
  output$measured_rail <- renderUI({
    active <- rv_measured_site()
    sites <- df_measured_sites %>% arrange(Site_full_name)
    tagList(lapply(seq_len(nrow(sites)), function(i) {
      id <- sites$Site_id[i]
      div(
        class = paste("rail-row", if (!is.null(active) && identical(active, id)) "is-active" else ""),
        onclick = paste0("Shiny.setInputValue('site_pick', ", jsonlite::toJSON(id, auto_unbox = TRUE),
                         ", {priority:'event'})"),
        span(sites$Site_full_name[i])
      )
    }))
  })

  observeEvent(input$site_pick, { rv_measured_site(input$site_pick) })

  # Modelled water bodies rail - only the 336 with modelled data are listed
  # (the other 34 geojson polygons are shown greyed-out on the Map but are
  # never selectable here or anywhere else in the app).
  output$modelled_rail <- renderUI({
    active <- rv_waterbody()
    wbs <- waterbody_opcat_shp %>% sf::st_drop_geometry() %>%
      dplyr::filter(has_model_data) %>% dplyr::arrange(water_bod0)
    tagList(lapply(seq_len(nrow(wbs)), function(i) {
      id <- wbs$water_body[i]
      div(
        class = paste("rail-row", if (!is.null(active) && identical(active, id)) "is-active" else ""),
        onclick = paste0("Shiny.setInputValue('waterbody_pick', ", jsonlite::toJSON(id, auto_unbox = TRUE),
                         ", {priority:'event'})"),
        span(wbs$water_bod0[i])
      )
    }))
  })

  observeEvent(input$waterbody_pick, { rv_waterbody(input$waterbody_pick) })

  # The four organic micropollutants with the greatest concentration range
  # at the selected measured site, used to auto-pick the time series panels.
  selected_measured_site_top_chemicals <- reactive({
    selected_measured_site_long() %>%
      dplyr::filter(parameter_group == "Organic micropollutant") %>%
      dplyr::group_by(parameter, parameter_label) %>%
      dplyr::summarise(
        range_val = {
          rng <- suppressWarnings(range(value_num, na.rm = TRUE))
          if (all(is.finite(rng))) diff(rng) else NA_real_
        },
        .groups = "drop"
      ) %>%
      dplyr::filter(is.finite(range_val)) %>%
      dplyr::arrange(dplyr::desc(range_val)) %>%
      dplyr::slice_head(n = 4)
  })

  # The four modelled chemicals with the greatest concentration range at the
  # selected water body (at the median statistic), mirroring the measured
  # equivalent above.
  selected_waterbody_top_chemicals <- reactive({
    req(rv_waterbody())
    get_wb_chem_grid(rv_waterbody(), "median") %>%
      dplyr::group_by(chem_key) %>%
      dplyr::summarise(
        range_val = {
          rng <- suppressWarnings(range(value, na.rm = TRUE))
          if (all(is.finite(rng))) diff(rng) else NA_real_
        },
        .groups = "drop"
      ) %>%
      dplyr::filter(is.finite(range_val)) %>%
      dplyr::arrange(dplyr::desc(range_val)) %>%
      dplyr::slice_head(n = 4)
  })

  # Chemical x week occurrence grid data for the selected measured site.
  # Colour is a log-scaled concentration relative to that parameter's own
  # maximum at this site (0-1). Non-detects sit at the bottom of the scale;
  # missing samples are a distinct flat grey via NA.
  measured_chem_grid_data <- reactive({
    df_grid <- selected_measured_site_long() %>%
      dplyr::group_by(parameter) %>%
      dplyr::mutate(
        max_detected = suppressWarnings(max(value_num[status == "Detected"], na.rm = TRUE)),
        max_detected = ifelse(is.finite(max_detected) & max_detected > 0, max_detected, 1),
        color_value = dplyr::if_else(
          status == "No sample",
          NA_real_,
          log1p(pmax(value_num, 0)) / log1p(max_detected)
        )
      ) %>%
      dplyr::ungroup()

    parameter_order <- df_grid %>%
      dplyr::distinct(parameter_label, parameter_group) %>%
      dplyr::arrange(parameter_group, parameter_label) %>%
      dplyr::pull(parameter_label)
    df_grid$parameter_label <- factor(df_grid$parameter_label, levels = rev(parameter_order))
    df_grid
  })

  # Chemical x month concentration grid data for the selected modelled water
  # body, at the chosen statistic. Colour is a log-scaled concentration
  # relative to that chemical's own maximum at this water body (0-1) - there
  # is no detect/non-detect concept here, every cell has a modelled value.
  wb_chem_grid_data <- reactive({
    req(rv_waterbody())
    stat_val <- input$site_grid_wb_stat %||% "median"
    df_grid <- get_wb_chem_grid(rv_waterbody(), stat_val) %>%
      dplyr::group_by(chem_key) %>%
      dplyr::mutate(
        max_val = suppressWarnings(max(value, na.rm = TRUE)),
        max_val = ifelse(is.finite(max_val) & max_val > 0, max_val, 1),
        color_value = log1p(pmax(value, 0)) / log1p(max_val)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(chem_label = unname(chem_display_lookup[chem_key]))

    chem_order <- df_grid %>% dplyr::distinct(chem_label) %>% dplyr::arrange(chem_label) %>% dplyr::pull(chem_label)
    df_grid$chem_label <- factor(df_grid$chem_label, levels = rev(chem_order))
    df_grid
  })

  output$site_chem_grid_note <- renderText({
    if (identical(input$site_entity_type, "modelled")) {
      "colour = concentration relative to this water body's own maximum"
    } else {
      "colour = concentration relative to this site's own maximum; grey = no sample"
    }
  })

  output$site_chem_grid <- renderPlot({
    if (identical(input$site_entity_type, "modelled")) {
      req(rv_waterbody())
      df_grid <- wb_chem_grid_data()

      ggplot(df_grid, aes(x = date, y = chem_label, fill = color_value)) +
        geom_tile(colour = ec$ground, linewidth = 0.4) +
        scale_fill_gradientn(
          colours = c(ec$rule_light, "#ffc4b8", "#ff563c", ec$accent_text),
          na.value = ec$surface,
          limits = c(0, 1),
          guide = guide_colourbar(barheight = grid::unit(8, "pt"), barwidth = grid::unit(90, "pt"))
        ) +
        scale_x_date(expand = c(0, 0), date_labels = "%b %Y") +
        labs(x = NULL, y = NULL, fill = NULL) +
        theme_ecomix(11) +
        theme(
          panel.grid = element_blank(),
          axis.line.x = element_blank(),
          axis.text.y = element_text(size = 9, colour = ec$ink),
          axis.text.x = element_text(size = 9, angle = 45, hjust = 1)
        )
    } else {
      df_grid <- measured_chem_grid_data()

      ggplot(df_grid, aes(x = Date, y = parameter_label, fill = color_value)) +
        geom_tile(colour = ec$ground, linewidth = 0.4) +
        scale_fill_gradientn(
          colours = c(ec$rule_light, "#ffc4b8", "#ff563c", ec$accent_text),
          na.value = ec$surface,
          limits = c(0, 1),
          guide = guide_colourbar(barheight = grid::unit(8, "pt"), barwidth = grid::unit(90, "pt"))
        ) +
        scale_x_date(expand = c(0, 0), date_labels = "%b %Y") +
        labs(x = NULL, y = NULL, fill = NULL) +
        theme_ecomix(11) +
        theme(
          panel.grid = element_blank(),
          axis.line.x = element_blank(),
          axis.text.y = element_text(size = 9, colour = ec$ink),
          axis.text.x = element_text(size = 9, angle = 45, hjust = 1)
        )
    }
  })

  # Hover tooltip for the occurrence/concentration grid.
  output$site_chem_grid_tooltip <- renderUI({
    hover <- input$site_chem_grid_hover
    req(hover)

    if (identical(input$site_entity_type, "modelled")) {
      point <- nearPoints(wb_chem_grid_data(), hover,
                           xvar = "date", yvar = "chem_label",
                           threshold = 15, maxpoints = 1)
      if (nrow(point) == 0) return(NULL)
      detail_text <- paste0("Value: ", round(point$value[1], 3), " ng/L")
      label_text <- as.character(point$chem_label[1])
      date_text <- format(point$date[1], "%b %Y")
    } else {
      point <- nearPoints(measured_chem_grid_data(), hover,
                           xvar = "Date", yvar = "parameter_label",
                           threshold = 15, maxpoints = 1)
      if (nrow(point) == 0) return(NULL)
      detail_text <- switch(point$status[1],
        "Detected" = paste0("Value: ", round(point$value_num[1], 3)),
        "Non-detect" = "Not detected (below LOD)",
        "No sample" = "No sample taken"
      )
      label_text <- as.character(point$parameter_label[1])
      date_text <- format(point$Date[1], "%d %b %Y")
    }

    style <- paste0(
      "position:absolute; z-index:1000; pointer-events:none; ",
      "left:", hover$coords_css$x + 12, "px; top:", hover$coords_css$y + 12, "px; ",
      "background:", ec$ground, "; border:2px solid ", ec$ink, "; ",
      "padding:6px 10px; font-size:12px; white-space:nowrap;"
    )

    div(
      style = style,
      tags$strong(label_text), tags$br(),
      date_text, tags$br(),
      detail_text
    )
  })

  # Stacked time series for the four chemicals chosen in the popover.
  output$site_time_series <- renderPlot({
    if (identical(input$site_entity_type, "modelled")) {
      req(rv_waterbody())
      chem_key_vals <- vapply(seq_len(4), function(i) input[[paste0("site_ts_chemical_", i)]] %||% "", character(1))
      req(all(nzchar(chem_key_vals)))
      stat_val <- input$site_grid_wb_stat %||% "median"

      df_ts <- dplyr::bind_rows(lapply(chem_key_vals, function(k) {
        get_wb_chem_series(k, stat_val, rv_waterbody()) %>%
          dplyr::mutate(panel_label = unname(chem_display_lookup[k]))
      }))
      panel_levels <- unname(chem_display_lookup[chem_key_vals])
      df_ts$panel_label <- factor(df_ts$panel_label, levels = panel_levels)

      ggplot(df_ts, aes(x = date, y = value)) +
        geom_line(colour = ec$accent, na.rm = TRUE) +
        geom_point(colour = ec$accent, size = 1.6, na.rm = TRUE) +
        scale_x_date(expand = expansion(mult = c(0.02, 0.06))) +
        facet_wrap(vars(panel_label), ncol = 1, scales = "free_y") +
        labs(x = NULL, y = NULL, caption = "Monthly modelled concentration, 2018-2022.") +
        theme_ecomix(11)
    } else {
      params <- vapply(seq_len(4), function(i) input[[paste0("site_ts_chemical_", i)]] %||% "", character(1))
      req(all(nzchar(params)))

      df_ts <- dplyr::bind_rows(lapply(seq_along(params), function(i) {
        selected_measured_site_long() %>%
          dplyr::filter(parameter == params[i]) %>%
          dplyr::mutate(panel_label = measured_parameter_label(params[i]))
      }))
      panel_levels <- unique(measured_parameter_label(params))
      df_ts$panel_label <- factor(df_ts$panel_label, levels = panel_levels)

      ggplot(df_ts, aes(x = Date, y = value_num)) +
        geom_line(colour = ec$accent, na.rm = TRUE) +
        geom_point(aes(colour = status), size = 1.6, na.rm = TRUE) +
        scale_colour_manual(
          breaks = c("Detected", "Non-detect"),
          values = c("Detected" = ec$accent, "Non-detect" = ec$ink_dim)
        ) +
        scale_x_date(expand = expansion(mult = c(0.02, 0.06))) +
        facet_wrap(vars(panel_label), ncol = 1, scales = "free_y") +
        labs(x = NULL, y = NULL, caption = "Gaps indicate no sample taken.") +
        theme_ecomix(11)
    }
  })


  ### NAVBAR 2 - DATA EXPLORER ###

  # Selected water body for Hydro Explorer's "Water body" mode - section-local
  # (unlike rv_waterbody on Site Details) since nothing outside this section
  # needs it.
  rv_de_waterbody <- reactiveVal()

  # Whether the Hydro Explorer has a location to show: the picked water body
  # in Water body mode, the Map's subbasin otherwise. The projection plots
  # wait on this rather than on rv(), which in Water body mode left them
  # blank until a Map subbasin was also picked (and redrew them whenever it
  # changed).
  hydro_location_ready <- reactive({
    if (identical(input$de_entity_type, "waterbody")) !is.null(rv_de_waterbody()) else !is.null(rv())
  })

  # bindCache() keys for the Hydro Explorer plots. A cached renderer runs only
  # when its key changes, so each key lists everything the renderer (and the
  # reactives it calls, such as chem_selected_series()) reads - leaving one
  # out would freeze the plot on that input. The cache is the app-wide one,
  # so a plot drawn for one user is reused for any other with the same
  # selection.
  projection_plot_key <- reactive(list(
    input$de_entity_type, rv(), rv_de_waterbody(), input$prediction_variable,
    input$scenario, input$prediction_period, input$prediction_percentile, input$plot_type
  ))
  # The climate plot's annual view ignores the Scenarios and Periods
  # selectors, and the monthly view only uses whether Baseline is selected.
  climate_plot_periods_key <- reactive({
    if (identical(input$climate_resolution, "annual")) NULL
    else list("Baseline" %in% input$scenario, unique(input$prediction_period))
  })

  updateSelectizeInput(session, "de_waterbody_search",
                       choices = waterbody_opcat_shp %>% sf::st_drop_geometry() %>%
                         dplyr::filter(has_model_data) %>% dplyr::arrange(water_bod0) %>%
                         { setNames(.$water_body, .$water_bod0) },
                       selected = character(0), server = TRUE)

  observeEvent(input$de_waterbody_search, {
    req(nzchar(input$de_waterbody_search))
    rv_de_waterbody(input$de_waterbody_search)
  }, ignoreInit = TRUE)

  # Model variable choices are HYPE + subbasin chemicals in Subbasin mode,
  # chemicals only in Water body mode (there's no per-water-body hydrology).
  observeEvent(input$de_entity_type, {
    if (identical(input$de_entity_type, "waterbody")) {
      updateSelectInput(session, "prediction_variable", choices = chem_variable_choices)
    } else {
      updateSelectInput(session, "prediction_variable", choices = prediction_variable_choices)
    }
  }, ignoreInit = TRUE)

  # Card-header pill labels, so the header states the current choice rather
  # than a sidebar restating it.
  output$climate_pill <- renderText({
    res <- if (identical(input$climate_resolution, "annual")) "Yearly" else "Monthly"
    paste0(input$climate_variable, " \u00b7 ", res, " \u25be")
  })

  output$observation_pill <- renderText({
    v <- input$observation_variable
    if (is.null(v) || !nzchar(v)) "Choose a variable \u25be" else paste0(v, " \u25be")
  })

  output$projection_title <- renderText({
    var <- input$prediction_variable
    lbl <- prediction_variable_labels[[var]] %||% "output"
    if (is_chem_variable(var)) paste0(lbl, " concentration") else paste0("Projected ", tolower(lbl))
  })

  # "Daily" is only meaningful for chemicals - the HYPE datasets in this app
  # are monthly/yearly/percentile only, with no daily series behind them.
  # Default Model variable is a HYPE one, so start with the tab hidden.
  bslib::nav_hide("projection_tabs", target = "Daily", session = session)

  # The "Conditions" selector doubles up as the percentile picker for HYPE
  # variables (p10/p50/p90) and the statistic picker for chemicals (mean/
  # median/90th, matching the data-chem file suffixes) - swap its choices
  # when the Model variable crosses that boundary.
  observeEvent(input$prediction_variable, {
    if (is_chem_variable(input$prediction_variable)) {
      updateSelectInput(session, "prediction_percentile",
                        label = "Statistic",
                        choices = chem_stat_choices,
                        selected = "median")
      bslib::nav_show("projection_tabs", target = "Daily", session = session)
    } else {
      updateSelectInput(session, "prediction_percentile",
                        label = "Conditions",
                        choices = hype_pct_choices,
                        selected = hype_pct_default)
      bslib::nav_hide("projection_tabs", target = "Daily", session = session)
    }
  }, ignoreInit = TRUE)

  ## Plot 1: Climate
  output$climate_plot <- renderPlot({
    if (identical(input$de_entity_type, "waterbody")) {
      return(no_data_plot("Not available in Water body mode"))
    }
    if (is.null(rv())) return(NULL)

    sub_subbasin <- rv()
    sub_climate_variable <- if (is.null(input$climate_variable) || !nzchar(input$climate_variable)) "Precipitation" else input$climate_variable
    sub_climate_resolution <- if (is.null(input$climate_resolution) || !nzchar(input$climate_resolution)) "monthly" else input$climate_resolution
    # The climate projections are one scenario (UKCP18 RCP8.5) rather than
    # per-SSP, so the Scenarios selector only decides whether the baseline
    # period is drawn; the projection periods apply as selected.
    sub_scenarios <- unique(input$scenario)
    sub_periods <- unique(input$prediction_period)
    if ("Baseline" %in% sub_scenarios) {
      sub_periods <- c(baseline_period, sub_periods)
    }

    df_plot <- ds_proj_forcing %>%
      filter(
        variable == sub_climate_variable,
        subbasin %in% sub_subbasin,
        time_aggregation == sub_climate_resolution) %>%
      collect_quiet()

    if (nrow(df_plot) == 0) {
      return(no_data_plot("No climate model data for this subbasin"))
    }

    if (sub_climate_resolution == "monthly") {
      df_plot <- df_plot %>% filter(period %in% sub_periods) %>%
        mutate("scenario" = ifelse(period == baseline_period,
                                   paste0("Baseline (", period, ")"),
                                   paste0(forcing_scenario_label, " (", period, ")")))
      df_plot$xaxis <- df_plot$month
      xlab <- "Month"
      caption <- "Median across the 12 UKCP18 members; ribbon = 10th-90th percentile across years in the period."
    }
    if (sub_climate_resolution == "annual") {
      df_plot$scenario <- forcing_scenario_label
      df_plot$xaxis <- df_plot$year
      xlab <- "Year"
      caption <- "Median of the 12 UKCP18 members; ribbon = 10th-90th percentile across members."
    }
    if (nrow(df_plot) == 0) {
      return(no_data_plot("No climate model data for the selected period"))
    }
    ylab <- paste0(sub_climate_variable, " [", pretty_unit(unique(df_plot$unit)), "]")

    ggplot(df_plot, aes(x = xaxis, y = p50, color = scenario, fill = scenario)) +
      geom_ribbon(aes(ymin = p10, ymax = p90), alpha = 0.18, colour = NA) +
      geom_line(linewidth = 0.8) +
      scale_color_ecomix() +
      scale_fill_ecomix() +
      scale_x_continuous(expand = c(0, 0)) +
      scale_y_continuous(expand = c(0, 0)) +
      labs(x = xlab, y = ylab, caption = caption) +
      theme_ecomix()
  }) %>%
    bindCache(input$de_entity_type, rv(), input$climate_variable, input$climate_resolution,
            climate_plot_periods_key(), sizePolicy = function(dims) dims)

  ## Plot 2 - Observations
  output$observation_plot <- renderPlot({
    if (identical(input$de_entity_type, "waterbody")) {
      return(no_data_plot("Not available in Water body mode"))
    }
    if (is.null(rv())) return(NULL)

    sub_observation_variable <- input$observation_variable
    if (is.null(sub_observation_variable) || !nzchar(sub_observation_variable)) {
      sub_observation_variable <- observation_choices()[1]
    }

    df_data <- selected_historical()
    df_plot <- df_data[df_data$variable == sub_observation_variable, ]

    if (identical(sub_observation_variable, "discharge")) {

      df_tmp <- df_plot %>% dplyr::select(-prediction_percentile, -variable, -sim_P10, -sim_P50, -sim_P90) %>%
        rename("low" = obs_min, "med" = obs, "high" = obs_max) %>% mutate("type" = "Observation")
      df_plot <- df_plot %>% dplyr::select(-prediction_percentile, -variable, -obs_min, -obs_max, -obs) %>%
        rename("low" = sim_P10, "med" = sim_P50, "high" = sim_P90) %>% mutate("type" = "Simulation")
      df_plot <- rbind(df_plot, df_tmp)

      ggplot(df_plot, aes(x = date, y = med, color = type, fill = type)) +
        geom_ribbon(aes(ymin = low, ymax = high), alpha = 0.25, colour = NA) +
        geom_line(linewidth = 0.7) +
        scale_color_manual(values = c("Simulation" = ec$accent, "Observation" = ec$ink)) +
        scale_fill_manual(values = c("Simulation" = ec$accent, "Observation" = ec$ink)) +
        scale_x_date(expand = c(0, 0)) +
        scale_y_continuous(limits = c(0, max(df_plot$high, na.rm = TRUE) * 1.1), expand = c(0, 0)) +
        labs(x = NULL, y = "Discharge [m\u00b3/s]",
             caption = paste0(unique(df_plot$station_label), " (Station ", unique(df_plot$id_station), ")")) +
        theme_ecomix()

    } else {

      if (nrow(df_plot) == 0 || all(is.na(c(df_plot$sim_P90, df_plot$obs)))) {
        return(NULL)
      }
      upper <- max(c(df_plot$sim_P90, df_plot$obs), na.rm = TRUE)
      ylab <- paste0(unique(df_plot$variable), " [", unique(df_plot$unit), "]")

      ggplot(df_plot, aes(x = date, y = sim_P50)) +
        geom_ribbon(aes(ymin = sim_P10, ymax = sim_P90), fill = ec$accent, alpha = 0.2) +
        geom_line(colour = ec$accent, linewidth = 0.7) +
        geom_point(aes(x = date, y = obs), shape = 15, colour = ec$ink, size = 1.6) +
        scale_x_date(expand = c(0, 0)) +
        scale_y_continuous(limits = c(0, upper * 1.1), expand = c(0, 0)) +
        labs(x = NULL, y = ylab,
             caption = paste0(unique(df_plot$station_label), " (Station ", unique(df_plot$id_station), ")")) +
        theme_ecomix()
    }
  }) %>%
    bindCache(input$de_entity_type, rv(), input$observation_variable, observation_choices()[1],
            sizePolicy = function(dims) dims)

  ## Plot 3: Yearly Projections
  output$projections_yearly_plot <- renderPlot({
    if (!hydro_location_ready()) return(NULL)

    sub_subbasin <- rv()
    # Only chemicals have water body series; HYPE variables need the subbasin.
    if (is.null(sub_subbasin) && !is_chem_variable(input$prediction_variable[1])) return(NULL)
    sub_variable <- input$prediction_variable[1]

    # Chemicals: a single 2018-2022 daily record with no scenario/period, so
    # "Yearly" here is the annual mean of the daily values per statistic
    # (Mean/Median/90th) rather than an SSP projection.
    if (is_chem_variable(sub_variable)) {
      df_chem <- chem_selected_series()
      if (nrow(df_chem) == 0) return(no_data_plot("No chemical data for this subbasin"))

      df_yearly <- df_chem %>%
        dplyr::group_by(year, stat) %>%
        dplyr::summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
        dplyr::mutate(stat_label = factor(chem_stat_labels[stat], levels = unname(chem_stat_labels)))

      lbl <- prediction_variable_labels[[sub_variable]] %||% sub_variable
      return(
        ggplot(df_yearly, aes(x = year, y = value, color = stat_label)) +
          geom_line(linewidth = 0.8) +
          geom_point(size = 1.6) +
          scale_x_continuous(expand = c(0, 0), breaks = unique(df_yearly$year)) +
          scale_color_ecomix() +
          labs(x = "Year", y = paste0(lbl, " [ng/L]"),
               caption = "Annual mean of the daily 2018-2022 record.") +
          theme_ecomix()
      )
    }

    sub_scenarios <- unique(input$scenario)
    sub_percentiles <- intersect(hype_pct_cols, input$prediction_percentile)
    req(length(sub_percentiles) > 0)
    sub_plot_type <- unique(input$plot_type)
    lbl <- prediction_variable_labels[[sub_variable]] %||% sub_variable

    df_projections_year <- ds_proj_year %>%
      filter(subbasin %in% sub_subbasin,
             variable == sub_variable,
             ssp %in% c("Baseline", sub_scenarios)) %>%
      collect()

    # Water quality variables: drop the first year of each projection slice
    # (2020, 2030, ..., 2070) - it is model spin-up, which is why their
    # monthly slices start a year in (2071_2079). Baseline is one continuous
    # run, so it keeps every year.
    if (sub_variable %in% hype_wq_variables) {
      df_projections_year <- df_projections_year %>% filter(ssp == "Baseline" | year %% 10 != 0)
    }

    # This subbasin may simply be outside the 3126 subbasins the HYPE model
    # covers (most subbasins on the map, and nearly all of the chemical
    # ones, are) - show that plainly instead of a blank/vanishing chart.
    if (nrow(df_projections_year) == 0) {
      return(no_data_plot("No hydrology model data for this subbasin"))
    }

    baseline_years <- range(df_projections_year$year[df_projections_year$ssp == "Baseline"])

    if (sub_plot_type == "Absolute") {

      df_plot <- df_projections_year %>% filter(ssp %in% sub_scenarios)
      df_plot <- df_plot[!(df_plot$ssp == "Baseline" & df_plot$year > 2020), ]
      if (nrow(df_plot) == 0) return(no_data_plot("No data for the selected scenarios"))

      # Ribbon spans the lowest to highest selected percentile.
      df_plot$band_low <- do.call(pmin, df_plot[sub_percentiles])
      df_plot$band_high <- do.call(pmax, df_plot[sub_percentiles])
      df_lines <- df_plot %>%
        tidyr::pivot_longer(dplyr::all_of(sub_percentiles), names_to = "percentile", values_to = "value") %>%
        mutate(percentile_label = factor(percentile, levels = hype_pct_cols, labels = names(hype_pct_choices)))

      ylab <- paste0(lbl, " [", pretty_unit(unique(df_plot$unit)), "]")
      ggplot(df_lines, aes(x = year, color = ssp, fill = ssp)) +
        geom_ribbon(data = df_plot, aes(ymin = band_low, ymax = band_high), alpha = 0.12, colour = NA) +
        geom_line(aes(y = value, linetype = percentile_label), linewidth = 0.8) +
        scale_x_continuous(expand = c(0, 0)) +
        scale_color_ecomix() +
        scale_fill_ecomix() +
        labs(x = "Year", y = ylab, linetype = NULL,
             caption = "Percentiles of daily values within each year, pooled over the model ensemble.") +
        theme_ecomix()
    } else {

      df_proj <- df_projections_year %>% filter(ssp != "Baseline")
      if (nrow(df_proj) == 0) return(no_data_plot("Select a future scenario to compare with the baseline"))
      base_mean <- df_projections_year %>% filter(ssp == "Baseline") %>%
        summarise(dplyr::across(dplyr::all_of(sub_percentiles), mean)) %>%
        unlist()

      df_plot <- df_proj %>%
        tidyr::pivot_longer(dplyr::all_of(sub_percentiles), names_to = "percentile", values_to = "value") %>%
        mutate(anomaly = value - base_mean[percentile],
               percentile_label = factor(percentile, levels = hype_pct_cols, labels = names(hype_pct_choices)))

      ylab <- paste0("Change to baseline: ", lbl, " [", pretty_unit(unique(df_plot$unit)), "]")

      ggplot(df_plot, aes(x = year, y = anomaly, color = ssp, linetype = percentile_label)) +
        geom_hline(yintercept = 0, colour = ec$ink, linewidth = 0.5) +
        geom_line(linewidth = 0.8) +
        scale_x_continuous(expand = c(0, 0)) +
        scale_color_ecomix() +
        labs(x = "Year", y = ylab, linetype = NULL,
             caption = paste0("Change against the ", baseline_years[1], "-", baseline_years[2], " baseline mean.")) +
        theme_ecomix()
    }
  }) %>%
    bindCache(projection_plot_key(), sizePolicy = function(dims) dims)

  ## Plot 4: Monthly Projections
  output$projections_monthly_plot <- renderPlot({
    if (!hydro_location_ready()) return(NULL)

    sub_subbasin <- rv()
    # Only chemicals have water body series; HYPE variables need the subbasin.
    if (is.null(sub_subbasin) && !is_chem_variable(input$prediction_variable[1])) return(NULL)
    sub_variable <- input$prediction_variable[1]

    # Chemicals: monthly climatology across the whole 2018-2022 record
    # (mean per calendar month, ribbon = range across the covered years) per
    # statistic, in place of an SSP/period monthly projection.
    if (is_chem_variable(sub_variable)) {
      df_chem <- chem_selected_series()
      if (nrow(df_chem) == 0) return(no_data_plot("No chemical data for this subbasin"))

      df_monthly <- df_chem %>%
        dplyr::group_by(month, stat) %>%
        dplyr::summarise(value_mean = mean(value, na.rm = TRUE),
                         value_min = min(value, na.rm = TRUE),
                         value_max = max(value, na.rm = TRUE), .groups = "drop") %>%
        dplyr::mutate(stat_label = factor(chem_stat_labels[stat], levels = unname(chem_stat_labels)))

      lbl <- prediction_variable_labels[[sub_variable]] %||% sub_variable
      return(
        ggplot(df_monthly, aes(x = month, y = value_mean, color = stat_label, fill = stat_label)) +
          geom_ribbon(aes(ymin = value_min, ymax = value_max), alpha = 0.18, colour = NA) +
          geom_line(linewidth = 0.8) +
          scale_x_continuous(expand = c(0, 0), breaks = 1:12) +
          scale_color_ecomix() +
          scale_fill_ecomix() +
          labs(x = "Month", y = paste0(lbl, " [ng/L]"),
               caption = "Monthly climatology across 2018-2022; ribbon = range across years.") +
          theme_ecomix()
      )
    }

    sub_scenarios <- unique(input$scenario)
    sub_percentiles <- intersect(hype_pct_cols, input$prediction_percentile)
    req(length(sub_percentiles) > 0)
    sub_periods <- unique(input$prediction_period)
    if ("Baseline" %in% sub_scenarios) {
      sub_periods <- c(baseline_period, sub_periods)
    }
    sub_plot_type <- unique(input$plot_type)
    lbl <- prediction_variable_labels[[sub_variable]] %||% sub_variable

    # Baseline is always fetched (whatever the period selection) as the
    # reference for the Relative view.
    df_projections_month <- ds_proj_month %>%
      filter(subbasin %in% sub_subbasin,
             variable == sub_variable,
             ssp %in% c("Baseline", sub_scenarios),
             period %in% c(baseline_period, sub_periods)) %>%
      collect()

    if (nrow(df_projections_month) == 0) {
      return(no_data_plot("No hydrology model data for this subbasin"))
    }

    if (sub_plot_type == "Absolute") {

      df_plot <- df_projections_month %>%
        filter(ssp %in% sub_scenarios, period %in% sub_periods) %>%
        mutate("scenario" = paste0(ssp, " (", period, ")"))
      if (nrow(df_plot) == 0) return(no_data_plot("No data for the selected scenarios/periods"))

      # Ribbon spans the lowest to highest selected percentile.
      df_plot$band_low <- do.call(pmin, df_plot[sub_percentiles])
      df_plot$band_high <- do.call(pmax, df_plot[sub_percentiles])
      df_lines <- df_plot %>%
        tidyr::pivot_longer(dplyr::all_of(sub_percentiles), names_to = "percentile", values_to = "value") %>%
        mutate(percentile_label = factor(percentile, levels = hype_pct_cols, labels = names(hype_pct_choices)))

      ylab <- paste0(lbl, " [", pretty_unit(unique(df_plot$unit)), "]")
      ggplot(df_lines, aes(x = month, color = scenario, fill = scenario)) +
        geom_ribbon(data = df_plot, aes(ymin = band_low, ymax = band_high), alpha = 0.12, colour = NA) +
        geom_line(aes(y = value, linetype = percentile_label), linewidth = 0.8) +
        scale_x_continuous(expand = c(0, 0), breaks = 1:12) +
        scale_color_ecomix() +
        scale_fill_ecomix() +
        labs(x = "Month", y = ylab, linetype = NULL,
             caption = "Percentiles of daily values in each calendar month, pooled over the period and model ensemble.") +
        theme_ecomix()

    } else {

      df_proj <- df_projections_month %>% filter(ssp != "Baseline", period %in% sub_periods)
      if (nrow(df_proj) == 0) return(no_data_plot("Select a future scenario to compare with the baseline"))
      df_base <- df_projections_month %>% filter(ssp == "Baseline") %>%
        tidyr::pivot_longer(dplyr::all_of(sub_percentiles), names_to = "percentile", values_to = "base")

      df_plot <- df_proj %>%
        tidyr::pivot_longer(dplyr::all_of(sub_percentiles), names_to = "percentile", values_to = "value") %>%
        left_join(df_base %>% dplyr::select(month, percentile, base), by = c("month", "percentile")) %>%
        mutate(anomaly = value - base,
               scenario = paste0(ssp, " (", period, ")"),
               percentile_label = factor(percentile, levels = hype_pct_cols, labels = names(hype_pct_choices)))

      ylab <- paste0("Change to baseline: ", lbl, " [", pretty_unit(unique(df_plot$unit)), "]")

      ggplot(df_plot, aes(x = month, y = anomaly, color = scenario, linetype = percentile_label)) +
        geom_hline(yintercept = 0, colour = ec$ink, linewidth = 0.5) +
        geom_line(linewidth = 0.8) +
        scale_x_continuous(expand = c(0, 0), breaks = 1:12) +
        scale_color_ecomix() +
        labs(x = "Month", y = ylab, linetype = NULL,
             caption = paste0("Change against the ", baseline_period, " baseline.")) +
        theme_ecomix()
    }
  }) %>%
    bindCache(projection_plot_key(), sizePolicy = function(dims) dims)

  ## Plot 5: Cumulative Frequency Curves
  output$projections_cfc_plot <- renderPlot({
    if (!hydro_location_ready()) return(NULL)

    sub_subbasin <- rv()
    # Only chemicals have water body series; HYPE variables need the subbasin.
    if (is.null(sub_subbasin) && !is_chem_variable(input$prediction_variable[1])) return(NULL)
    sub_variable <- input$prediction_variable[1]

    # Chemicals: empirical cumulative frequency curve of the daily
    # concentration values themselves (per statistic), rather than the
    # ensemble percentile spread the HYPE variables use.
    if (is_chem_variable(sub_variable)) {
      df_chem <- chem_selected_series()
      if (nrow(df_chem) == 0) return(no_data_plot("No chemical data for this subbasin"))

      probs <- seq(1, 99, by = 1)
      df_cfc <- df_chem %>%
        dplyr::group_by(stat) %>%
        dplyr::reframe(p = probs, value = as.numeric(stats::quantile(value, probs = probs / 100, na.rm = TRUE))) %>%
        dplyr::mutate(stat_label = factor(chem_stat_labels[stat], levels = unname(chem_stat_labels)))

      lbl <- prediction_variable_labels[[sub_variable]] %||% sub_variable
      return(
        ggplot(df_cfc, aes(x = p, y = value, color = stat_label)) +
          geom_line(linewidth = 0.8) +
          scale_x_continuous(expand = c(0, 0), breaks = c(0, 25, 50, 75, 100),
                             labels = c("0", "25", "50 (Median)", "75", "100")) +
          scale_y_log10() +
          scale_color_ecomix() +
          labs(x = "Cumulative frequency [%]", y = paste0(lbl, " [ng/L]")) +
          theme_ecomix()
      )
    }

    sub_scenarios <- unique(input$scenario)
    sub_periods <- unique(input$prediction_period)
    if ("Baseline" %in% sub_scenarios) {
      sub_periods <- c(baseline_period, sub_periods)
    }

    df_month <- ds_proj_month %>%
      filter(subbasin %in% sub_subbasin,
             variable == sub_variable,
             ssp %in% sub_scenarios,
             period %in% sub_periods) %>%
      collect()

    if (nrow(df_month) == 0) {
      return(no_data_plot("No hydrology model data for this subbasin"))
    }

    probs <- seq(1, 99, by = 1)
    df_plot <- df_month %>%
      dplyr::group_by(ssp, period, unit) %>%
      dplyr::group_modify(~ tibble::tibble(percentile = probs,
                                           value = pooled_percentiles(.x, probs / 100))) %>%
      dplyr::ungroup() %>%
      mutate("scenario" = paste0(ssp, " (", period, ")"))
    lbl <- prediction_variable_labels[[sub_variable]] %||% sub_variable
    ylab <- paste0(lbl, " [", pretty_unit(unique(df_plot$unit)), "]")

    ggplot(df_plot, aes(x = percentile, y = value, color = scenario)) +
      geom_line(linewidth = 0.8) +
      scale_x_continuous(expand = c(0, 0), breaks = c(0, 25, 50, 75, 100),
                         labels = c("0", "25", "50 (Median)", "75", "100")) +
      scale_y_log10() +
      scale_color_ecomix() +
      labs(x = "Cumulative frequency [%]", y = ylab,
           caption = "Daily values pooled over all months of the period and the model ensemble.") +
      theme_ecomix()
  }) %>%
    bindCache(projection_plot_key(), sizePolicy = function(dims) dims)

  ## Plot 6: Daily chemical concentration series (chemicals only - the HYPE
  ## datasets in this app have no daily granularity, only monthly/yearly/
  ## percentile summaries, so this tab is hidden via nav_hide/nav_show
  ## whenever a HYPE variable is selected).
  output$projections_daily_plot <- renderPlot({
    if (!hydro_location_ready()) return(NULL)

    sub_variable <- input$prediction_variable[1]
    if (!is_chem_variable(sub_variable)) {
      return(no_data_plot("Daily data is only available for chemical concentration variables"))
    }

    df_chem <- chem_selected_series()
    if (nrow(df_chem) == 0) return(no_data_plot("No chemical data for this subbasin"))

    df_chem$stat_label <- factor(chem_stat_labels[df_chem$stat], levels = unname(chem_stat_labels))
    lbl <- prediction_variable_labels[[sub_variable]] %||% sub_variable

    ggplot(df_chem, aes(x = date, y = value, color = stat_label)) +
      geom_line(linewidth = 0.5) +
      scale_x_date(expand = c(0, 0)) +
      scale_color_ecomix() +
      labs(x = NULL, y = paste0(lbl, " [ng/L]"),
           caption = "Daily reconstructed concentration, 2018-2022.") +
      theme_ecomix()
  }) %>%
    bindCache(projection_plot_key(), sizePolicy = function(dims) dims)

  # "Download this selection" in the Hydro Explorer rail - the current
  # subbasin/variable extract, same shape as the Download page's output.
  # In Water body mode the rows are the picked water body's series, so the
  # file and its id column name the water body (they used to carry the Map's
  # subbasin, which those rows have nothing to do with).
  output$download_selection <- downloadHandler(
    filename = function() {
      var <- input$prediction_variable
      label <- if (is_chem_variable(var)) chem_key_from_variable(var) else var
      if (identical(input$de_entity_type, "waterbody")) {
        paste0("ecomix_waterbody_", rv_de_waterbody(), "_", label, ".csv")
      } else {
        paste0("ecomix_subbasin_", rv(), "_", label, ".csv")
      }
    },
    content = function(file) {
      req(hydro_location_ready())
      var <- input$prediction_variable
      if (is_chem_variable(var)) {
        df <- chem_selected_series() %>%
          dplyr::mutate(chemical = prediction_variable_labels[[var]] %||% chem_key_from_variable(var),
                        unit = "ng/L")
        df <- if (identical(input$de_entity_type, "waterbody")) {
          df %>% dplyr::mutate(waterbody_id = rv_de_waterbody()) %>%
            dplyr::select(waterbody_id, chemical, statistic = stat, date, value, unit)
        } else {
          df %>% dplyr::mutate(subbasin = rv()) %>%
            dplyr::select(subbasin, chemical, statistic = stat, date, value, unit)
        }
        write.csv(df, file, row.names = FALSE)
      } else {
        req(rv())
        df <- get_subbasin_tabular_download(var, rv())
        write.csv(df, file, row.names = FALSE)
      }
    }
  )


  ### NAVBAR 3 - SPATIAL DATASETS ###

  spatial_layer_data <- reactive({
    sub_variable <- input$prediction_variable_spatial[1]

    # Chemicals have no scenario/period - the map shows one time-averaged
    # (whole 2018-2022 record) value per subbasin (or, for modelled water
    # body chemicals, per water body) for the chosen statistic. The result is
    # shaped to match ds_map_input's columns (hype_variable, subbasin,
    # value, unit, prediction_percentile) so the rendering and
    # context-strip code below needs no branching of its own. The water body
    # branch has no "subbasin" concept, so it carries a placeholder NA
    # subbasin column purely so the context-strip's existing
    # filter(subbasin == rv()) calls don't error - they simply find no match
    # and read "-", since this page has no water-body-selection context strip.
    if (is_wbchem_variable(sub_variable)) {
      chem_key <- wbchem_key_from_variable(sub_variable)
      stat <- input$prediction_stat_spatial_chem %||% "median"
      get_wb_chem_spatial_summary(chem_key, stat) %>%
        dplyr::mutate(hype_variable = sub_variable,
                      unit = "ng/L", prediction_percentile = stat, subbasin = NA_integer_)
    } else if (is_chem_variable(sub_variable)) {
      chem_key <- chem_key_from_variable(sub_variable)
      stat <- input$prediction_stat_spatial_chem %||% "median"
      get_chem_spatial_summary(chem_key, stat) %>%
        dplyr::mutate(hype_variable = sub_variable,
                      unit = "ng/L", prediction_percentile = stat)
    } else {
      sub_period <- strsplit(input$prediction_period_spatial %||% "SSP585|2070-2079", "\\|")[[1]]
      sub_percentile <- as.numeric(input$prediction_percentile_spatial %||% "99")

      get_map_input_slice(sub_variable, sub_period[1], sub_period[2], sub_percentile)
    }
  })

  # The map is drawn once per basis (spatial_basis_of(): HYPE subbasins,
  # chemical subbasins or water bodies) and then only recoloured when the
  # variable, period, conditions or statistic change within it (the
  # observer below), instead of being rebuilt every time (~1.3-1.8 s and
  # 3.5 MB), which also put the tiles back to OSM and reset the view. So it
  # reads the slice, the tile choice and the view in isolate(): they are
  # only where it starts from, also when a new basis redraws it.
  sp_basis <- reactiveVal()

  output$prediction_map <- renderLeaflet({
    basis <- sp_basis()
    req(basis)
    df <- isolate(spatial_layer_data())
    center <- isolate(input$prediction_map_center)
    zoom <- isolate(input$prediction_map_zoom)
    map <- leaflet() %>% add_basemap_tiles(isolate(input$sp_basemap) %||% "osm")
    map <- if (is.null(center) || is.null(zoom)) {
      setView(map, lng = -1.16, lat = 53.75, zoom = 8.5)
    } else {
      setView(map, lng = center$lng, lat = center$lat, zoom = zoom)
    }
    add_spatial_values(map, basis, df)
  })

  # A new slice: when its basis is the one on the map, recolour the map in
  # place (update_spatial_values()); when the basis changes, record it,
  # which redraws the map above with this slice already, so there is
  # nothing to update in that flush. sp_basis is a reactiveVal so that it
  # only invalidates the map when the basis really changes. A slice that
  # fails to load leaves the map as it is (an error in an observer would end
  # the session); the context strip below shows the error.
  observeEvent(tryCatch(spatial_layer_data(), error = function(e) NULL), {
    df <- spatial_layer_data()
    basis <- spatial_basis_of(input$prediction_variable_spatial[1])
    if (!identical(basis, sp_basis())) {
      sp_basis(basis)
      return()
    }
    leafletProxy("prediction_map") %>% update_spatial_values(basis, df)
  })

  observeEvent(input$sp_basemap, {
    leafletProxy("prediction_map") %>%
      clearGroup("basetile") %>%
      add_basemap_tiles(input$sp_basemap)
  }, ignoreInit = TRUE)

  output$ctx_value_sp <- renderText({
    if (is.null(rv())) return("\u2014")
    df <- spatial_layer_data() %>% filter(subbasin == rv())
    if (nrow(df) == 0) return("no value")
    paste0(round(df$value[1], 1), " ", pretty_unit(df$unit[1] %||% ""))
  })

  output$ctx_rank_sp <- renderText({
    df <- spatial_layer_data() %>% arrange(desc(value))
    if (is.null(rv()) || nrow(df) == 0) return("\u2014")
    i <- which(df$subbasin == rv())
    if (length(i) == 0) return("\u2014")
    paste0(i[1], " of ", nrow(df))
  })

  output$ctx_range_sp <- renderText({
    df <- spatial_layer_data()
    if (nrow(df) == 0) return("\u2014")
    paste0(round(min(df$value, na.rm = TRUE), 1), " \u2013 ",
           round(max(df$value, na.rm = TRUE), 1))
  })


  ### NAVBAR 4 - DATA DOWNLOADER ###

  # Default the spatial layer to "Water bodies" once a modelled water-body
  # variable is picked, since that's the only layer with matching data.
  observeEvent(input$dl_variable, {
    if (is_wbchem_variable(input$dl_variable)) {
      updateSelectInput(session, "dl_spatial_layer", selected = "waterbodies")
    }
  }, ignoreInit = TRUE)

  observeEvent(input$dl_data_type, {
    if (input$dl_data_type == "tabular") {
      updateSelectInput(
        session,
        inputId = "dl_format",
        choices = c("CSV" = "csv", "XLSX" = "xlsx", "Parquet" = "parquet"),
        selected = "csv"
      )
    } else {
      updateSelectInput(
        session,
        inputId = "dl_format",
        choices = spatial_download_formats,
        selected = "gpkg"
      )
    }
  }, ignoreInit = TRUE)

  # All 336 modelled water bodies' rows for a wbchem variable - its tabular
  # download and the water bodies layer.
  downloader_wbchem_data <- reactive({
    chem_key <- wbchem_key_from_variable(input$dl_variable)
    stat <- input$dl_chem_stat %||% "median"
    get_wb_chem_all_waterbodies_monthly(chem_key, stat)
  })

  # Per-subbasin and catchment summaries of every subbasin's rows for a
  # subbasin variable - the subbasins and catchment layers.
  downloader_summary <- reactive({
    req(input$dl_variable)
    if (is_chem_variable(input$dl_variable)) {
      chem_key <- chem_key_from_variable(input$dl_variable)
      stat <- input$dl_chem_stat %||% "median"
      get_chem_download_summary(chem_key, stat)
    } else {
      get_tabular_download_summary(input$dl_variable)
    }
  })

  downloader_tabular_data <- reactive({
    # This page has no water-body picker, so a wbchem tabular download is
    # always all 336 modelled water bodies - unlike subbasin variables, which
    # are read for the currently rv()-selected subbasin only.
    if (is_wbchem_variable(input$dl_variable)) {
      return(downloader_wbchem_data())
    }
    req(rv())
    req(input$dl_variable)
    if (is_chem_variable(input$dl_variable)) {
      chem_key <- chem_key_from_variable(input$dl_variable)
      stat <- input$dl_chem_stat %||% "median"
      get_chem_subbasin_daily(chem_key, stat, rv())
    } else {
      get_subbasin_tabular_download(input$dl_variable, rv())
    }
  })

  downloader_spatial_data <- reactive({
    # "Water bodies" layer only makes sense with a wbchem variable and vice
    # versa (wbchem data has no subbasin/catchment mapping) - any mismatch
    # returns an empty result rather than erroring on a missing column.
    if (identical(input$dl_spatial_layer, "waterbodies") != is_wbchem_variable(input$dl_variable)) {
      return(subbasin_shp %>% filter(FALSE) %>%
               mutate(spatial_layer = input$dl_spatial_layer, variable = input$dl_variable))
    }

    if (identical(input$dl_spatial_layer, "waterbodies")) {
      tab_data <- downloader_wbchem_data()
      tab_summary_by_wb <- tab_data %>%
        group_by(waterbody_id) %>%
        summarise(
          n_records = n(),
          value_mean = round(mean(value, na.rm = TRUE), 3),
          value_min = round(min(value, na.rm = TRUE), 3),
          value_max = round(max(value, na.rm = TRUE), 3),
          unit = dplyr::first(unit),
          .groups = "drop"
        )
      return(
        full_res_layer("waterbodies") %>%
          left_join(tab_summary_by_wb, by = c("water_body" = "waterbody_id")) %>%
          mutate(spatial_layer = input$dl_spatial_layer, variable = input$dl_variable)
      )
    }

    tab_summaries <- downloader_summary()

    if (input$dl_spatial_layer == "catchment") {
      tab_summary <- tab_summaries$catchment
      if (is.null(tab_summary)) {
        return(full_res_layer("catchments") %>%
                 mutate(spatial_layer = input$dl_spatial_layer,
                        variable = input$dl_variable,
                        n_records = 0,
                        value_mean = NA_real_,
                        value_min = NA_real_,
                        value_max = NA_real_,
                        unit = NA_character_))
      }

      return(full_res_layer("catchments") %>%
               mutate(spatial_layer = input$dl_spatial_layer,
                      variable = input$dl_variable,
                      n_records = tab_summary$n_records,
                      value_mean = round(tab_summary$value_mean, 3),
                      value_min = round(tab_summary$value_min, 3),
                      value_max = round(tab_summary$value_max, 3),
                      unit = tab_summary$unit))
    }

    full_res_layer("subbasins") %>%
      mutate(Id = as.integer(Id)) %>%
      left_join(tab_summaries$by_subbasin, by = c("Id" = "subbasin")) %>%
      mutate(spatial_layer = input$dl_spatial_layer,
             variable = input$dl_variable)
  })

  # Context strip stats for the downloader
  downloader_preview_rows <- reactive({
    if (identical(input$dl_data_type, "tabular")) {
      if (!is_wbchem_variable(input$dl_variable) && is.null(rv())) return(0)
      nrow(downloader_tabular_data())
    } else {
      nrow(spatial_rows_with_values(downloader_spatial_data()))
    }
  })

  output$ctx_rows_dl <- renderText({ format(downloader_preview_rows(), big.mark = ",") })

  output$ctx_combos_dl <- renderText({
    if (!identical(input$dl_data_type, "tabular") || is.null(rv())) return("\u2014")
    df <- downloader_tabular_data()
    # Chemical downloads are a single daily series (no scenario/period).
    if (nrow(df) == 0 || !all(c("scenario", "period") %in% names(df))) return("\u2014")
    paste0(dplyr::n_distinct(df$scenario), " \u00d7 ", dplyr::n_distinct(df$period))
  })

  output$ctx_size_dl <- renderText({
    n <- downloader_preview_rows()
    kb <- max(round(n * 48 / 1024), 1)
    paste0(format(kb, big.mark = ","), " kB ", toupper(input$dl_format %||% "csv"))
  })

  output$dl_preview_note <- renderText({
    n <- downloader_preview_rows()
    if (n == 0) return("nothing selected yet")
    paste0("first 100 of ", format(n, big.mark = ","), " rows")
  })

  output$data_table <- DT::renderDataTable({
    if (input$dl_data_type == "tabular") {
      if (!is_wbchem_variable(input$dl_variable) && is.null(rv())) {
        return(DT::datatable(data.frame(Message = "Select a subbasin on the Map tab to build a download."),
                             rownames = FALSE, options = list(dom = "t")))
      }
      df_download <- downloader_tabular_data()
      if (nrow(df_download) == 0) {
        return(DT::datatable(data.frame(Message = "No downloader data available for this subbasin/variable."),
                             rownames = FALSE, options = list(dom = "t")))
      }
      return(DT::datatable(df_download, rownames = FALSE,
                           options = list(dom = "tp", pageLength = 100, scrollY = "100%",
                                          scrollCollapse = TRUE)))
    }

    df_spatial <- downloader_spatial_data()
    if (nrow(df_spatial) == 0) {
      return(DT::datatable(data.frame(Message = "No spatial data available for this subbasin."),
                           rownames = FALSE, options = list(dom = "t")))
    }

    df_spatial_tbl <- sf::st_drop_geometry(spatial_rows_with_values(df_spatial))

    if (nrow(df_spatial_tbl) == 0) {
      return(DT::datatable(data.frame(Message = "No spatial rows with values are available for this selection."),
                           rownames = FALSE, options = list(dom = "t")))
    }

    DT::datatable(df_spatial_tbl, rownames = FALSE,
                  options = list(dom = "tp", pageLength = 100, scrollY = "100%",
                                 scrollCollapse = TRUE))
  })

  output$download_data <- downloadHandler(
    filename = function() {
      ext <- switch(input$dl_format,
                    "shp" = "zip",
                    "geoparquet" = "parquet",
                    input$dl_format)
      prefix <- if (input$dl_data_type == "spatial") "ecomix_spatial" else "ecomix_tabular"
      layer_suffix <- if (input$dl_data_type == "spatial") paste0("_", input$dl_spatial_layer) else ""
      id_suffix <- if (input$dl_data_type == "spatial") {
        if (input$dl_spatial_layer == "subbasins") "all_subbasins"
        else if (input$dl_spatial_layer == "waterbodies") "all_waterbodies"
        else "catchment"
      } else if (is_wbchem_variable(input$dl_variable)) {
        "all_waterbodies"
      } else {
        paste0("subbasin_", rv())
      }
      var_label <- if (is_wbchem_variable(input$dl_variable)) wbchem_key_from_variable(input$dl_variable)
        else if (is_chem_variable(input$dl_variable)) chem_key_from_variable(input$dl_variable)
        else input$dl_variable
      paste0(prefix, layer_suffix, "_", id_suffix, "_", var_label, ".", ext)
    },
    content = function(file) {
      if (input$dl_data_type == "tabular") {
        df_download <- downloader_tabular_data()
        if (nrow(df_download) == 0) {
          stop("No tabular data available for the selected subbasin/variable.")
        }

        if (input$dl_format == "csv") {
          write.csv(df_download, file, row.names = FALSE)
        } else if (input$dl_format == "xlsx") {
          if (!requireNamespace("writexl", quietly = TRUE)) {
            stop("Package 'writexl' is required for XLSX downloads. Install with install.packages('writexl').")
          }
          writexl::write_xlsx(df_download, path = file)
        } else {
          arrow::write_parquet(df_download, sink = file)
        }
      } else {
        df_spatial <- spatial_rows_with_values(downloader_spatial_data())
        if (nrow(df_spatial) == 0) {
          stop("No spatial data with values available for the selected subbasin.")
        }

        # Export all spatial outputs in British National Grid.
        df_spatial_export <- sf::st_transform(df_spatial, 27700)

        if (input$dl_format == "shp") {
          tmp_dir <- tempfile("ecomix_shp_")
          dir.create(tmp_dir)
          zip_tmp <- tempfile(fileext = ".zip")
          # Shiny only removes `file`; in a long-running process every
          # shapefile download would otherwise leave these behind in tempdir().
          on.exit(unlink(c(tmp_dir, zip_tmp), recursive = TRUE, force = TRUE), add = TRUE)
          shp_path <- file.path(tmp_dir, "ecomix_spatial.shp")
          sf::st_write(df_spatial_export, dsn = shp_path, driver = "ESRI Shapefile", quiet = TRUE, delete_layer = TRUE)
          shp_files <- list.files(tmp_dir, full.names = TRUE)
          # -j stores bare file names (not the temp directory path), -X drops
          # extra file attributes, -q keeps zip quiet in the server log.
          utils::zip(zipfile = zip_tmp, files = shp_files, flags = "-j9Xq")
          file.copy(zip_tmp, file, overwrite = TRUE)
        } else if (input$dl_format == "gpkg") {
          sf::st_write(df_spatial_export, dsn = file, driver = "GPKG", quiet = TRUE, delete_dsn = TRUE)
        } else {
          sf::st_write(df_spatial_export, dsn = file, driver = "Parquet", quiet = TRUE, delete_dsn = TRUE)
        }
      }
    }
  )

}


### 3. Execution
app <- shinyApp(ui = ui, server = server)

app
