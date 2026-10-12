## Definition of widgets

# Widget to select one or more scenarios.
#
# Defaults to Baseline + the same future SSP the Spatial Datasets tab
# defaults to (SSP585) - Baseline alone has no data for any future period,
# so a Baseline-only default made picking a period in the sidebar look
# like it did nothing (nothing plots for a future period until a future
# scenario is also selected).
scenario_default <- intersect(c("Baseline", "SSP585"), available_scenarios)
if (length(scenario_default) == 0) scenario_default <- available_scenarios[1]

widget_scenario <-  selectizeInput(
  inputId =  "scenario",
  label = "Scenarios",
  choices = available_scenarios,
  selected = scenario_default,
  multiple = TRUE)

# Climate variable (drop down menu - only one selection)
widget_climate_variable <- selectInput(
  inputId = "climate_variable",
  label = "Climate variable",
  choices = c("Precipitation", "Temperature"),
  selected = "Precipitation")

# Temporal resolution
widget_climate_resolution <- selectInput(
  inputId = "climate_resolution",
  label = "Resolution",
  choices = c("Monthly" = "monthly", "Yearly" = "annual"),
  selected = "Monthly")

# Observational variables - populated once a subbasin is selected
widget_observed_variable <- selectizeInput(
  inputId = "observation_variable",
  label = "Observed variable",
  choices = NULL,
  multiple = FALSE
)

# HYPE output variable.
#
# The Hydro Explorer now has ONE control rail rather than a sidebar per plot
# card, so its three projection tabs share a single input - the four
# duplicated ids the old nested-sidebar layout needed are gone. Spatial
# Datasets is a separate nav_panel rendered into the DOM at the same time, so
# it still needs its own id.
hype_variable_choices <- c(
  "Discharge" = "discharge",
  "Water Temperature" = "water_temperature",
  "Susp. Sediments" = "SS",
  "Inorganic Nitrogen" = "IN",
  "Soluble Phosphorus" = "SP",
  "Particulate Phosphorus" = "PP",
  "Dissolved Organic Carbon" = "DOC"
)

# Water quality variables whose projection time slices start a year into
# each decade (2071_2079 vs discharge's 2070_2079) - the first simulated year
# of each slice is spin-up, which the yearly series drops to match.
hype_wq_variables <- c("SS", "IN", "SP", "PP", "DOC")

# Grouped so the Model variable dropdown lists the hydrology/climate
# variables first, with the chemical concentration variables in their own
# group underneath. A flat label lookup (keyed by the widget value, working
# across both groups) is kept alongside for renderers that just need "what's
# the display name of the currently selected variable".
prediction_variable_choices <- list(
  "Hydrology & climate" = hype_variable_choices,
  "Chemical concentrations (ng/L)" = chem_variable_choices
)
prediction_variable_labels <- setNames(
  c(names(hype_variable_choices), names(chem_variable_choices), names(wbchem_variable_choices)),
  c(unname(hype_variable_choices), unname(chem_variable_choices), unname(wbchem_variable_choices))
)

# Spatial Datasets and Download also offer the modelled-by-waterbody
# chemicals as a variable choice; Hydro Explorer does not (it switches basis
# via its own Subbasin/Water body mode toggle instead, reusing the plain
# chem_variable_choices ids either way - see NAVBAR 2 in the server).
prediction_variable_choices_ext <- list(
  "Hydrology & climate" = hype_variable_choices,
  "Chemical concentrations · subbasin (daily, ng/L)" = chem_variable_choices,
  "Chemical concentrations · water body (monthly, ng/L)" = wbchem_variable_choices
)

widget_prediction_variable <- selectInput(
  inputId = "prediction_variable",
  label = "Model variable",
  choices = prediction_variable_choices,
  selected = "discharge")

widget_prediction_variable_spatial <- selectInput(
  inputId = "prediction_variable_spatial",
  label = "Model variable",
  choices = prediction_variable_choices_ext,
  selected = "discharge")

widget_spatial_period <- selectInput(
  inputId = "prediction_period_spatial",
  label = "Period",
  choices = spatial_period_choices,
  selected = if ("SSP585|2070-2079" %in% spatial_period_choices) "SSP585|2070-2079" else spatial_period_choices[1])

widget_spatial_percentile <- selectInput(
  inputId = "prediction_percentile_spatial",
  label = "Conditions",
  choices = spatial_percentile_choices,
  selected = "99")

# Chemicals have no scenario/period dimension - the Spatial Datasets map
# shows a single time-averaged (whole 2018-2022 record) value per subbasin,
# chosen by statistic instead.
widget_spatial_chem_stat <- selectInput(
  inputId = "prediction_stat_spatial_chem",
  label = "Statistic",
  choices = chem_stat_choices,
  selected = "median")

# Output conditions - i.e. prediction percentile
widget_prediction_percentile <- selectInput(
  inputId = "prediction_percentile",
  label = "Conditions",
  choices = hype_pct_choices,
  selected = hype_pct_default,
  multiple = TRUE)

# Prediction period
widget_prediction_period <- selectInput(
  inputId = "prediction_period",
  label = "Projection periods",
  choices = prediction_period_choices,
  selected = utils::tail(prediction_period_choices, 1),
  multiple = TRUE)

# Plot type (Absolute or relative change)
widget_plot_type <- radioButtons(
  inputId = "plot_type",
  label = "Values shown as",
  choices = c("Absolute", "Relative"),
  selected = "Absolute",
  inline = TRUE)

widget_download_variable <- selectInput(
  inputId = "dl_variable",
  label = "Variable",
  choices = list(
    "Hydrology & climate" = c("Precipitation" = "precip",
                              "Temperature" = "temp",
                              hype_variable_choices),
    "Chemical concentrations · subbasin (daily, ng/L)" = chem_variable_choices,
    "Chemical concentrations · water body (monthly, ng/L)" = wbchem_variable_choices
  ),
  selected = "precip")

# Chemicals have three summary statistics (mean/median/90th) rather than the
# monthly p50 the HYPE downloads use - only shown once a chemical variable
# is picked (see the conditionalPanel around this in the Download UI).
widget_download_chem_stat <- selectInput(
  inputId = "dl_chem_stat",
  label = "Statistic",
  choices = chem_stat_choices,
  selected = "median")

widget_download_data_type <- selectInput(
  inputId = "dl_data_type",
  label = "Data type",
  choices = c("Tabular" = "tabular", "Spatial" = "spatial"),
  selected = "tabular"
)

widget_download_spatial_layer <- selectInput(
  inputId = "dl_spatial_layer",
  label = "Spatial layer",
  choices = c("Subbasins" = "subbasins", "Catchment" = "catchment", "Water bodies" = "waterbodies"),
  selected = "subbasins"
)

widget_download_format <- selectInput(
  inputId = "dl_format",
  label = "Format",
  choices = c("CSV" = "csv", "XLSX" = "xlsx", "Parquet" = "parquet"),
  selected = "csv"
)
