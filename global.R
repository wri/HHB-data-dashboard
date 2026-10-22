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
  library(readxl)    # reads the text / translation spreadsheet
})

# ---- Text and translations ---------------------------------------------
# Every piece of text the user sees lives in data/translations.xlsx (sheet
# "text"), one row per text: key | where | english | portuguese | notes.
# The code asks for a text by its key, e.g. tr("tab_mapping").

# Language the app opens in. Users switch with the EN | PT links in the tab
# bar, which reload the page with ?lang=en or ?lang=pt.
default_language <- "en"
languages <- c(en = "english", pt = "portuguese")   # URL code -> spreadsheet column

texts <- read_excel(file.path("data", "translations.xlsx"), sheet = "text", col_types = "text")
texts <- texts[!is.na(texts$key) & texts$key != "", ]   # drop the section-heading rows

# The language asked for in the URL (?lang=pt), or the default.
get_language <- function(query_string) {
  lang <- parseQueryString(query_string %||% "")$lang
  if (!is.null(lang) && lang %in% names(languages)) lang else default_language
}

# Returns tr(), which looks up a text by key in the given language.
#   tr("detail_above", difference = "0.3")  ->  "0.3 above"
# A blank Portuguese cell falls back to English; a missing key shows the key
# itself (and a warning in the console) so typos are easy to spot.
make_tr <- function(lang) {
  english <- setNames(texts$english, texts$key)
  chosen  <- setNames(texts[[languages[[lang]]]], texts$key)
  chosen[is.na(chosen) | chosen == ""] <- english[is.na(chosen) | chosen == ""]
  function(key, ...) {
    text <- unname(chosen[key])
    if (length(text) != 1 || is.na(text)) {
      warning("No text for key '", key, "' in data/translations.xlsx", call. = FALSE)
      return(key)
    }
    # fill in {placeholders}
    values <- list(...)
    for (name in names(values)) text <- gsub(paste0("{", name, "}"), values[[name]], text, fixed = TRUE)
    text
  }
}

# Long text: a blank line in the spreadsheet cell starts a new paragraph.
text_paragraphs <- function(text) {
  paragraphs <- trimws(strsplit(text, "\\n\\s*\\n")[[1]])
  lapply(paragraphs[paragraphs != ""], tags$p)
}

# Numbers: 1,234.5 in English, 1.234,5 in Portuguese.
number_marks <- function(lang) {
  if (lang == "pt") list(decimal = ",", big = ".") else list(decimal = ".", big = ",")
}
# A fixed number of decimals.
format_num <- function(x, digits, lang) {
  marks <- number_marks(lang)
  formatC(round(x, digits), format = "f", digits = digits, big.mark = marks$big, decimal.mark = marks$decimal)
}
# 3 significant figures (e.g. 0.123, 12.3, 123).
format_signif <- function(x, lang) {
  marks <- number_marks(lang)
  formatC(signif(x, 3), format = "fg", digits = 3, big.mark = marks$big, decimal.mark = marks$decimal)
}

# ---- Load the prepared data -------------------------------------------

# Built by prepare_data.R - run that first if this file is missing.
app_data <- readRDS(file.path("data", "app_data.rds"))

curves_df <- app_data$curves   # temperature-mortality curves (see archive/curves/README.md)

# Maps use latitude/longitude (EPSG:4326); the source data uses another
# coordinate system, so convert once here.
health_sf  <- st_transform(app_data$health, 4326)   # health polygons (one row per polygon x age group)

# Social inclusion index (PC1g) is a 1-5 category. HIGH IS GOOD: 5 = most
# inclusive (lowest risk), 1 = least inclusive (highest risk).
# Stored as "1".."5"; the words shown ("1-Very low" ...) come from the
# spreadsheet (social_inclusion_1 ... social_inclusion_5).
deprivation_levels <- as.character(1:5)
health_sf$deprivation_group <- factor(health_sf$deprivation_group, levels = 1:5, labels = deprivation_levels)
deprivation_label <- function(level, tr) vapply(as.character(level), function(x) tr(paste0("social_inclusion_", x)), "", USE.NAMES = FALSE)
barrios_sf <- st_transform(app_data$barrios, 4326)  # barrio boundaries (outlines only)

cities <- app_data$cities
age_groups_by_city <- app_data$age_groups_by_city

# Age group dropdown choices for a city: "All" first (the default), then the
# others. The names are what the user sees; the values match the data.
age_group_choices <- function(city_name, tr) {
  groups <- age_groups_by_city$age_group[age_groups_by_city$city == city_name]
  groups <- c(intersect("all", groups), setdiff(groups, "all"))
  setNames(groups, age_group_label(groups, tr))
}

# "all" -> "All" for display (other age groups are shown as they are).
age_group_label <- function(group, tr) ifelse(group == "all", tr("age_all"), group)


# ---- Variables ---------------------------------------------------------
# Column names in the data. Each one's label is the spreadsheet text with
# key "var_<column name>" (see var_label() below).

# Every health-data variable; each becomes a layer in the Data Explorer.
layer_vars <- c("edf", "edf_disparity", "edf_dT", "edf_dE", "mean_temp", "rr99",
                "population_density", "perc_informal", "perc_imperv", "perc_tree",
                "deprivation_group")

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
format_value <- function(variable, x, lang) {
  if (variable %in% edf_vars) return(paste0(format_num(x * 100, 1, lang), "%"))
  if (variable == "population_density") return(paste0(format_num(x, 0, lang), "/ha"))   # whole people
  unit <- switch(variable,
    mean_temp = "°C",
    perc_imperv = , perc_tree = , perc_informal = "%",
    population_density = "/ha",
    "")
  paste0(format_signif(x, lang), unit)
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
    colorFactor(viridisLite::magma(5), levels = deprivation_levels, na.color = "#cccccc")
  } else {
    # magma runs dark -> light as values increase, so reverse it unless flipped
    colorNumeric("magma", domain = values, reverse = !(variable %in% flipped_vars), na.color = "#cccccc")
  }
}

# Column name -> friendly label (e.g. "edf" -> "Excess death fraction (EDF)").
var_label <- function(column_name, tr) tr(paste0("var_", column_name))
