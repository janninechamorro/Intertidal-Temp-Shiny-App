# Attach packages
library(shiny)
library(tidyverse)
library(shinythemes) 
library(dplyr)
library(ggplot2)
library(slickR)
library(here)
library(leaflet)
library(htmltools)
library(htmlwidgets)

# Import data
all_sites <- read.csv("InfoMicrosite_2016_mussel.csv")

#########################

# Subset West Coast sites
site_data = filter(all_sites, 
                   state_province == "California" 
                   | state_province == "Oregon" 
                   | state_province == "Washington")

# Subset unique West Coast sites
unique_site_data <- distinct(site_data, location, .keep_all = TRUE)


# Map color palette
#map_palette <- color


leaflet(options = leafletOptions(minZoom = 10))

# Create map
map1 <- leaflet(unique_site_data) %>% 
  addProviderTiles(providers$Esri.OceanBasemap,
                   providerTileOptions(detectRetina = T)) %>% 
  addMarkers(
    lng = unique_site_data$field_lon,
    lat = unique_site_data$field_lat,
    label = unique_site_data$location,
    labelOptions = labelOptions(direction = "bottom",
       style = list(
         "color" = "black",
         "font-family" = "helvetica",
         "font-style" = "italic",
         "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
         "font-size" = "12px",
         "border-color" = "rgba(0,0,0,0.5)"
       )),
    options = markerOptions(interactive = T, clickable = NULL),
    popup = paste(unique_site_data$location, ", ", 
                  unique_site_data$state_province, "<br>",
                  "(", unique_site_data$field_lat, "ºN, ", 
                  unique_site_data$field_lon, " ºW)", sep = ""),
    popupOptions = popupOptions(maxWidth = 300, 
      minWidth = 50, maxHeight = NULL,
      autoPan = T, keepInView = F, closeButton = T,
      style = list(
        "color" = "black",
        "font-family" = "Helvetica",
        "font-style" = "italic",
        "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
        "font-size" = "12px",
        "border-color" = "rgba(0,0,0,0.5)"
      ))
    ) %>% 
  addMiniMap(
    tiles = providers$Esri.OceanBasemap,
    toggleDisplay = T
    ) %>% 
  addEasyButton(easyButton(
    icon="fa-globe", title="Zoom level 1",
    onClick=JS("function(btn, map){ map.setZoom(1); }")))


map1




