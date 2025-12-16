# Define score_colors (moved from global to avoid loading heavy data)
score_colors <- list(
  "0" = "#000000", "0.01" = "#0D5359", "0.001" = "#91CB3E",
  "0.1" = "#E41A1C", "0.2" = "#03045E", "0.3" = "#08A045",
  "0.4" = "#805D93", "0.5" = "#EC4E20", "0.6" = "#FEEA00",
  "0.7" = "#540b0E", "0.8" = "#F49FBC", "0.9" = "#791E94",
  "1" = "#BE3E82"
)

# Source only the map creation functions
source("R/create_individual_map.R", local = TRUE)
source("R/create_combined_map.R", local = TRUE) 
source("R/create_continuous_palette.R", local = TRUE)

library(leaflet)
library(here)
library(sf)
library(dplyr)
library(knitr)
library(htmltools)
library(htmlwidgets)

# Null coalescing operator
`%||%` <- function(x, y) if(is.null(x)) y else x