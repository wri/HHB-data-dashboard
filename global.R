# ============================================================================
# GLOBAL
# ============================================================================
# Runs once when the app starts: loads packages and the prepared data, and
# defines the lists/helpers that ui.R and server.R share.
# ============================================================================

suppressMessages({
  library(shiny)
  library(bslib)     # page layout (sidebar, cards, tabs)
  library(leaflet)   # interactive maps
  library(sf)        # map data
  library(dplyr)     # data wrangling
  library(ggplot2)   # charts and small maps
  library(scales)    # number formatting
})

# ---- Load the prepared data -------------------------------------------

# Built by prepare_data.R - run that first if this file is missing.
app_data <- readRDS(file.path("data", "app_data.rds"))

curves_df <- app_data$curves   # temperature-mortality curves (see archive/curves/README.md)

# Maps use latitude/longitude (EPSG:4326); the source data uses another
# coordinate system, so convert once here.
health_sf  <- st_transform(app_data$health, 4326)   # health polygons (one row per polygon x age group)

# Social inclusion index (PC1g) is a 1-5 category. HIGH IS GOOD: 5 = most
# inclusive (lowest risk), 1 = least inclusive (highest risk).
deprivation_labels <- c("1-Very low", "2-Low", "3-Medium", "4-High", "5-Very high")
health_sf$deprivation_group <- factor(health_sf$deprivation_group, levels = 1:5, labels = deprivation_labels)
barrios_sf <- st_transform(app_data$barrios, 4326)  # barrio boundaries (outlines only)

cities <- app_data$cities
age_groups_by_city <- app_data$age_groups_by_city

# Age group dropdown choices for a city: "All" first (the default), then the
# others. The names are what the user sees; the values match the data.
age_group_choices <- function(city_name) {
  groups <- age_groups_by_city$age_group[age_groups_by_city$city == city_name]
  groups <- c(intersect("all", groups), setdiff(groups, "all"))
  setNames(groups, ifelse(groups == "all", "All", groups))
}

# "all" -> "All" for display (other age groups are shown as they are).
age_group_label <- function(group) ifelse(group == "all", "All", group)


# ---- Variables ---------------------------------------------------------
# "Label shown to the user" = "column name in the data".

# Every health-data variable; each becomes a layer in the Data Explorer.
layer_vars <- list(
  "Excess death fraction (EDF)" = "edf",
  "EDF disparity" = "edf_disparity",
  "EDF from temperature increase" = "edf_dT",
  "EDF from vulnerability increase" = "edf_dE",
  "Mean temperature" = "mean_temp",
  "Relative risk at 99th percentile heat" = "rr99",
  "Population density" = "population_density",
  "Informal settlement" = "perc_informal",
  "Impervious surface" = "perc_imperv",
  "Tree cover" = "perc_tree",
  "Social inclusion index" = "deprivation_group"
)

# Highlight colour for the hovered / selected barrio and the selected curve.
# Sky blue is a hue the magma map scale never uses, so it can't be mistaken
# for data. On the maps it sits on a thin near-black casing: the blue stands
# out on the dark end of the scale, the casing on the pale-yellow end and on
# the light basemaps.
highlight_color  <- "#56b4e9"   # sky blue
highlight_casing <- "#111111"   # near-black

# Draw a highlight outline (thin casing + blue line) for the given barrios.
add_highlight_outline <- function(map, data, group, pane = "overlayPane", weight = 2) {
  map |>
    addPolygons(data = data, group = group, 
                fill = FALSE, color = highlight_casing, weight = weight + 1.5,
                opacity = 1, options = pathOptions(interactive = FALSE, pane = pane)) |>
    addPolygons(data = data, group = group, 
                fill = FALSE, color = highlight_color, weight = weight,
                opacity = 1, options = pathOptions(interactive = FALSE, pane = pane))
}

# EDF columns are stored as fractions (0.05); we always DISPLAY them as percent (5%).
edf_vars <- c("edf", "edf_disparity", "edf_dT", "edf_dE")

# Format a value for display, with its unit next to the number.
format_value <- function(variable, x) {
  if (variable %in% edf_vars) return(paste0(format(round(x * 100, 1), nsmall = 1), "%"))
  if (variable == "population_density") return(paste0(round(x), "/ha"))   # whole people
  unit <- switch(variable,
    mean_temp = "°C",
    perc_imperv = , perc_tree = , perc_informal = "%",
    population_density = "/ha",
    "")
  paste0(signif(x, 3), unit)
}

# Category (not number) columns need a different color scale.
categorical_vars <- c("deprivation_group")

# ---- Risk direction and colours ---------------------------------------
# Every map is about drivers of heat-related mortality, so the HIGH-RISK end
# of every scale gets the darkest colour and sits at the TOP of the legend.
# For most variables a high value is high risk. Variables where a LOW value
# is high risk go in this list (their scale is flipped):
flipped_vars <- c("perc_tree",           # less tree cover = higher risk
                  "deprivation_group")   # lower social inclusion = higher risk

# Colour function for a variable (darkest = highest risk).
risk_colors <- function(variable, values = NULL) {
  if (variable %in% categorical_vars) {
    # Social inclusion index: level 1 (lowest inclusion) is the highest-risk
    # end, so it gets the darkest colour and "5-Very high" the lightest.
    colorFactor(viridisLite::magma(5), levels = deprivation_labels, na.color = "#cccccc")
  } else {
    # magma runs dark -> light as values increase, so reverse it unless flipped
    colorNumeric("magma", domain = values, reverse = !(variable %in% flipped_vars), na.color = "#cccccc")
  }
}

# Column name -> friendly label (e.g. "edf" -> "Excess death fraction (edf)").
var_label <- function(column_name) {
  label <- names(layer_vars)[match(column_name, layer_vars)]
  if (length(label) == 0 || is.na(label)) column_name else label
}
