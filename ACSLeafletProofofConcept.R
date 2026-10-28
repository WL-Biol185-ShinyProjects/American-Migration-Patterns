library(dplyr)
library(tidyverse)
library(tigris)

library(shiny)
library(sf)
library(leaflet)

ACS_2019_state_new <- read_csv("ACS_2019_state_new.csv")
ACS_2020_state_new <- read_csv("ACS_2020_state_new.csv")
ACS_2021_state_new <- read_csv("ACS_2021_state_new.csv")
ACS_2022_state_new <- read_csv("ACS_2022_state_new.csv")
ACS_2023_state_new <- read_csv("ACS_2023_state_new.csv")
ACS_2024_state_new <- read_csv("ACS_2024_state_new.csv")

#Clean the names of important vars
ACS_2019_state_cleaned <- ACS_2019_state_new %>% mutate(year = 2019) 

ACS_2020_state_cleaned <- ACS_2020_state_new %>% mutate(year = 2020) 

ACS_2021_state_cleaned <- ACS_2021_state_new %>% mutate(year = 2021) 

ACS_2022_state_cleaned <- ACS_2022_state_new %>% mutate(year = 2022)

ACS_2023_state_cleaned <- ACS_2023_state_new %>% mutate(year = 2023)

ACS_2024_state_cleaned <- ACS_2024_state_new %>% mutate(year = 2024)

# bins <- c(0, 10, 20, 50, 100, 200, 500, 1000, Inf)
# pal <- colorBin("YlOrRd", domain = ACS_2024_state_cleaned$"Total Population", bins = bins)
# 
# labels <- sprintf(
#   "<strong>%s</strong><br/>%g people / mi<sup>2</sup>",
#   ACS_2024_state_cleaned$NAME, ACS_2024_state_cleaned$"Total Population"
# ) %>% lapply(htmltools::HTML)
# 
# leaflet(ACS_2024_state_cleaned) %>%
#   setView(-96, 37.8, 4) %>%
#   addProviderTiles("MapBox", options = providerTileOptions(
#     id = "mapbox.light",
#     accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
#   addPolygons(
#     fillColor = ~pal("Total Population"),
#     weight = 2,
#     opacity = 1,
#     color = "white",
#     dashArray = "3",
#     fillOpacity = 0.7,
#     highlightOptions = highlightOptions(
#       weight = 5,
#       color = "#666",
#       dashArray = "",
#       fillOpacity = 0.7,
#       bringToFront = TRUE),
#     label = labels,
#     labelOptions = labelOptions(
#       style = list("font-weight" = "normal", padding = "3px 8px"),
#       textsize = "15px",
#       direction = "auto")) %>%
#   addLegend(pal = pal, values = ~"Total Population", opacity = 0.7, title = NULL,
#             position = "bottomright")
# #> Warning in sf::st_is_longlat(x): bounding box has potentially an
# #> invalid value range for longlat data

#Claude fixed above code by joining the state geometry using sf package
library(tidyverse)
library(sf)
library(leaflet)
library(tigris)

options(tigris_use_cache = TRUE)

ACS_2024_state_new <- read_csv("ACS_2024_state_new.csv")

# 1. Get state boundaries and join your data onto them
states_sf <- states(cb = TRUE, year = 2023) %>%
  st_transform(4326)

ACS_2024_state_cleaned <- states_sf %>%
  inner_join(ACS_2024_state_new, by = "NAME")   # keeps only the 50 states + DC + PR from your CSV

# 2. Bins now match the data (total population, not people / mi^2)
bins <- c(0, 1e6, 2e6, 5e6, 10e6, 20e6, Inf)
pal <- colorBin("YlOrRd", domain = ACS_2024_state_cleaned$`Total Population`, bins = bins)

labels <- sprintf(
  "<strong>%s</strong><br/>%s people",
  ACS_2024_state_cleaned$NAME,
  format(ACS_2024_state_cleaned$`Total Population`, big.mark = ",")
) %>% lapply(htmltools::HTML)

leaflet(ACS_2024_state_cleaned) %>%
  setView(-96, 37.8, 4) %>%
  addProviderTiles("CartoDB.Positron") %>%   # see note below about Mapbox
  addPolygons(
    fillColor = ~pal(`Total Population`),
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    highlightOptions = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE),
    label = labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")) %>%
  addLegend(pal = pal, values = ~`Total Population`, opacity = 0.7,
            title = "Total Population", position = "bottomright",
            labFormat = labelFormat(big.mark = ","))

#Notes for project: poverty rate is not a straightforward calculation
#Do more research on IRS exemptions statistics