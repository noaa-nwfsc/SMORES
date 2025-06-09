# load packages
library(shiny)
library(leaflet)
library(shinydashboard)
library(shinycssloaders)
library(markdown)
library(tidyverse)
library(bslib)
library(shinyWidgets)
library(mapview)
library(sf)
library(yaml)
library(shinyscreenshot)
library(webshot)

theme <- bslib::bs_theme()

# Load datasets
canyon_data <- readRDS("data/canyon_scored.rds")
DSC_RH_data <- readRDS("data/DSC_RH_scored.rds")
surveys_fixed <- readRDS("data/Surveys_fixed_scored.rds")
grid_test <- readRDS("data/2km_grid_norcal.rds")

# Create a list of all datasets
habitat_layer <- list(
  "Canyon" = canyon_data,
  "DSC_RH" = DSC_RH_data,
  "Fixed Surveys" = surveys_fixed
)

species_layer <- list(
  "whale1", "whale2"
)

bird_layer <- list (
  "bird1", "bird2"
)

# Weight values
score_values <- c("0.1", "0.2", "0.3", "0.4", "0.5" ,"0.6", "0.7", "0.8", "0.9", "1")

# Add a null coalescing operator helper since R doesn't have one built-in
`%||%` <- function(x, y) if(is.null(x)) y else x

# Color palette for scores
score_colors <- list(
  "0.1" = "#E41A1C",  # red
  "0.2" = "#377EB8",  # blue
  "0.3" = "#4DAF4A",  # green
  "0.4" = "#984EA3",  # purple
  "0.5" = "#FF7F00",  # orange
  "0.6" = "#FFFF33",  # yellow
  "0.7" = "#A65628",  # brown
  "0.8" = "#F781BF",  # pink
  "0.9" = "#999999",  # grey
  "1" = "#000000"     # black
)
