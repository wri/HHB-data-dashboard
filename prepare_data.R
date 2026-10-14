# ============================================================================
# PREPARE DATA
# ============================================================================
# Reads the raw files in data/ and builds data/app_data.rds for the Shiny app.
# Run it whenever the raw data changes:
#     Rscript prepare_data.R      (from the Heat-Health-Data-Dashboard folder)
# (It lives in the project folder, not in an R/ folder, because Shiny runs
# everything in R/ automatically every time an app starts.)
#
# TWO SEPARATE MAP LAYERS - they are NOT merged or aggregated:
#   1. "health"  - the health/temperature polygons
#                  (neighborhood_temperature_citylimits.geojson).
#                  All health numbers stay on these polygons.
#   2. "barrios" - the neighborhood boundaries (barrios_*.geojson).
#                  Drawn on top of the health polygons as outlines only.
#
# The only link between them: each barrio is given the ID of the health
# polygon that covers the largest share of its area ("majority coverage").
# Clicking a barrio in the app shows that one health polygon's numbers -
# no health data is averaged onto barrios.
# ============================================================================

suppressMessages({
  library(sf)
  library(dplyr)
})

data_dir <- "data"


# ----------------------------------------------------------------------------
# STEP 1: Barrio boundaries
# ----------------------------------------------------------------------------

belo_horizonte_barrios <- st_read(file.path(data_dir, "barrios_belohorizonte.geojson"), quiet = TRUE) |>
  transmute(city = "Belo Horizonte", barrio_name = nm_bairro)

campinas_barrios <- st_read(file.path(data_dir, "barrios_campinas.geojson"), quiet = TRUE) |>
  transmute(city = "Campinas", barrio_name = nome)

barrios <- bind_rows(belo_horizonte_barrios, campinas_barrios) |>
  st_make_valid() |>
  mutate(barrio_id = row_number())


# ----------------------------------------------------------------------------
# STEP 2: Health polygons
# ----------------------------------------------------------------------------
# One row per (health polygon x age group). Belo Horizonte columns are
# prefixed "bh_" / "bh_bh_", Campinas "cam_" / "cam_cam_" - we rename them to
# shared names. To add a variable, add a line to BOTH blocks below.

raw_health <- st_read(file.path(data_dir, "neighborhood_temperature_citylimits.geojson"), quiet = TRUE)

bh_health <- raw_health |>
  filter(city == "Belo Horizonte") |>
  mutate(
    L2_5LOCALID = L2_5LOCALID,       # ID of the health polygon
    city = city,
    age_group = age_group,
    mean_temp = mean_temp,
    edf = edf,                       # excess death fraction attributable to heat
    edf_disparity = edf_disparity,
    edf_dT = edf_dT,                 # part of edf driven by temperature
    edf_dE = edf_dE,                 # part of edf driven by vulnerability
    rr99 = rr99,                     # relative risk on a 99th-percentile hot day
    population = bh_CNSPOP10,        # 2010 census population
    perc_informal = bh_bh_perc_informal,
    perc_tree = bh_bh_perc_tree,
    perc_imperv = bh_bh_perc_imperv,
    deprivation_group = bh_PC1g,     # social inclusion, 1 = very low ... 5 = very high (1 = highest risk)
    geometry = geometry,
    .keep = "none"
  )

cam_health <- raw_health |>
  filter(city == "Campinas") |>
  mutate(
    L2_5LOCALID = L2_5LOCALID,
    city = city,
    age_group = age_group,
    mean_temp = mean_temp,
    edf = edf,
    edf_disparity = edf_disparity,
    edf_dT = edf_dT,
    edf_dE = edf_dE,
    rr99 = rr99,
    population = cam_CNSPOP10,
    perc_informal = cam_cam_perc_informal,
    perc_tree = cam_cam_perc_tree,
    perc_imperv = cam_cam_perc_imperv,
    deprivation_group = cam_PC1g,
    geometry = geometry,
    .keep = "none"
  )

health <- bind_rows(bh_health, cam_health) |> st_make_valid()

# Population density = 2010 census population / polygon area, in people per
# hectare. The data is in a metric projection (UTM 23S), so st_area() is in
# square metres; 1 hectare = 10,000 m2.
health$area_ha <- as.numeric(st_area(health)) / 10000
health$population_density <- health$population / health$area_ha


# ----------------------------------------------------------------------------
# STEP 3: Majority-coverage health polygon for each barrio
# ----------------------------------------------------------------------------
# For every barrio: intersect it with the health polygons of the same city,
# measure how much of the barrio each health polygon covers, and keep the
# health polygon with the LARGEST overlapping area. (The same polygon shape
# appears once per age group, so we use one copy per polygon ID.)

unique_polygons <- health |>
  distinct(L2_5LOCALID, .keep_all = TRUE) |>
  select(L2_5LOCALID, city)

find_majority_health <- function(which_city) {
  barrios_here  <- barrios |> filter(city == which_city) |> select(barrio_id)
  polygons_here <- unique_polygons |> filter(city == which_city) |> select(L2_5LOCALID)

  barrios_here$barrio_area <- as.numeric(st_area(barrios_here))

  # pieces = every place a barrio overlaps a health polygon
  pieces <- suppressWarnings(st_intersection(barrios_here, polygons_here))
  pieces$overlap_area <- as.numeric(st_area(pieces))

  pieces |>
    st_drop_geometry() |>
    group_by(barrio_id) |>
    slice_max(overlap_area, n = 1, with_ties = FALSE) |>   # the largest overlap wins
    ungroup() |>
    transmute(
      barrio_id,
      majority_health_id = L2_5LOCALID,
      majority_health_share = overlap_area / barrio_area  # share of the barrio it covers
    )
}

majority_health <- bind_rows(
  find_majority_health("Belo Horizonte"),
  find_majority_health("Campinas")
)

barrios <- barrios |> left_join(majority_health, by = "barrio_id")


# ----------------------------------------------------------------------------
# STEP 4: Temperature-mortality curves
# ----------------------------------------------------------------------------
# One row per city x age group x health polygon x temperature (see
# archive/curves/README.md). L2_5LOCALID = "city" is the city-average curve.
# rr = relative risk vs the 75th percentile; rr_lci / rr_uci = 95% interval.
# curves.csv.gz is the curve table from the curve package
# (campinas_bh_nbhd_summer_curves.parquet) saved as a gzipped CSV, so it can
# be read without any extra packages.

curves <- read.csv(file.path(data_dir, "curves.csv.gz"), colClasses = c(L2_5LOCALID = "character")) |>
  # The curve file covers more neighborhoods than the health polygons on the
  # map; keep only the ones on the map, plus the city-average curve.
  filter(L2_5LOCALID %in% c("city", as.character(health$L2_5LOCALID)))


# ----------------------------------------------------------------------------
# STEP 5: Save
# ----------------------------------------------------------------------------

app_data <- list(
  health = health,
  barrios = barrios,
  curves = curves,
  cities = sort(unique(health$city)),
  age_groups_by_city = health |>
    st_drop_geometry() |>
    distinct(city, age_group) |>
    arrange(city, age_group)
)

saveRDS(app_data, file.path(data_dir, "app_data.rds"))

cat("Saved", nrow(health), "health rows and", nrow(barrios), "barrios to data/app_data.rds\n")
cat("Health polygons with a curve:", sum(unique(as.character(health$L2_5LOCALID)) %in% curves$L2_5LOCALID),
    "of", length(unique(health$L2_5LOCALID)), "\n")
cat("Barrios with no overlapping health polygon:", sum(is.na(barrios$majority_health_id)),
    "of", nrow(barrios), "\n")
