library(sf)
library(dplyr)
library(ggplot2)
library(here)
library(glue)

bh_barrios <- st_read(here("data", "barrios_belohorizonte.geojson"))
cp_barrios <- st_read(here("data", "barrios_campinas.geojson"))

bh_health <- st_read(here("data", "neighborhood_temperature_citylimits.geojson")) |> 
  filter(city == "Belo Horizonte") |> 
  mutate(bh_PC1g = as.factor(bh_PC1g),
         bh_bh_perc_imperv = bh_bh_perc_imperv / 100,
         bh_bh_perc_tree = bh_bh_perc_tree / 100)
cp_health <- st_read(here("data", "neighborhood_temperature_citylimits.geojson")) |> 
  filter(city == "Campinas")

ggplot(data = bh_health) +
  geom_sf(aes(fill = edf_dT)) +
  labs(fill = "EDF from\ntemperature\nincrease") +
  scale_fill_viridis_c(labels = scales::percent, option = "rocket", direction = -1) +
  theme_minimal()

ggplot(data = bh_health) +
  geom_sf(aes(fill = edf_dE)) +
  labs(fill = "EDF from\nexposure\nincrease") +
  scale_fill_viridis_c(labels = scales::percent, option = "rocket", direction = -1) +
  geom_sf(data = bh_barrios) +
  scale_fill_continuous(aes(fill = NA)) +
  theme_minimal()

ggplot() +
  geom_sf(data = bh_health, aes(fill = edf_dE), color = NA) +
  geom_sf(data = bh_barrios, fill = NA, color = "grey30", linewidth = 0.2) +
  scale_fill_viridis_c(labels = scales::percent, option = "rocket", direction = -1) +
  labs(fill = "EDF from\nexposure\nincrease") +
  theme_minimal()

# SVI
ggplot() +
  geom_sf(data = bh_health, aes(fill = bh_PC1g), color = NA) +
  geom_sf(data = bh_barrios, fill = NA, color = "grey30", linewidth = 0.2) +
  scale_fill_viridis_d(option = "rocket", direction = -1) +
  labs(fill = "SVI") +
  theme_minimal()

# Impervious
ggplot() +
  geom_sf(data = bh_health, aes(fill = bh_bh_perc_imperv), color = NA) +
  geom_sf(data = bh_barrios, fill = NA, color = "grey30", linewidth = 0.2) +
  scale_fill_gradient(low = "lightgreen", high = "grey", labels = scales::percent) +
  labs(fill = "Impervious\npercent") +
  theme_minimal()

# Tree
ggplot() +
  geom_sf(data = bh_health, aes(fill = bh_bh_perc_tree), color = NA) +
  geom_sf(data = bh_barrios, fill = NA, color = "grey30", linewidth = 0.2) +
  scale_fill_gradient(low = "lightgreen", high = "darkgreen", labels = scales::percent) +
  labs(fill = "Tree cover") +
  theme_minimal()

# Air temp
ggplot() +
  geom_sf(data = bh_health, aes(fill = mean_temp), color = NA) +
  geom_sf(data = bh_barrios, fill = NA, color = "grey30", linewidth = 0.2) +
  scale_fill_scico(palette = "lajolla", 
                      labels = label_number(suffix = "°C"), direction = -1) +
  labs(fill = "Mean air\ntemperature") +
  theme_minimal()

# Campinas
# cam_PC1g = SVI
# cam_cam_perc_imperv
# cam_cam_perc_tree

# BH
# bh_PC1g = BH SVI
# bh_bh_perc_imperv
# bh_bh_perc_tree

# age_group
# edf_dT
# edf_dE
# mean_temp

