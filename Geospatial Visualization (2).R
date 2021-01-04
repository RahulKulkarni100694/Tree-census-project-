library(leaflet)
library(dplyr)

setwd("~/OR568/Data") # your working directory
data <- read.csv("data.csv") # whatever your dataset is called

# split original dataset into Good/Fair/Poor datasets
Good <- dplyr::filter(data, health == "Good")
Fair <- dplyr::filter(data, health == "Fair")
Poor <- dplyr::filter(data, health == "Poor")

# interactive leaflet map with all Fair/Poor trees in NYC
leaflet() %>%
  setView(lng = -74, lat = 40.7, zoom = 11) %>%
  addTiles() %>%
  addCircleMarkers(data = Poor,
                   lng = ~longitude,
                   lat = ~latitude,
                   radius = 1,
                   color = "red") %>% 
  addCircleMarkers(data = Fair,
                   lng = ~longitude,
                   lat = ~latitude,
                   radius = 1,
                   color = "orange")