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
library(markdown)

theme <- bslib::bs_theme()

#load custom functions 
source("R/modal.R")
source("R/get_valid_configs_for_tab.R")
source("R/create_individual_maps.R")
source("R/create_combined_maps.R")
source("R/data_timestamps.R")

# Get data timestamps information
timestamp_info <- get_data_timestamps()
data_timestamps <- timestamp_info$data_timestamps
most_recent_update <- timestamp_info$most_recent_update

# Load datasets
canyon_data <- readRDS("data/canyon_scored.rds")
DSC_RH_data <- readRDS("data/DSC_RH_scored.rds")
surveys_fixed <- readRDS("data/Surveys_fixed_scored.rds")
surveys_periodic <- readRDS("data/Surveys_periodic_scored.rds")
seeps <- readRDS("data/Seeps_scored.rds")
shlfbrk <- readRDS("data/ShlfBrk_scored.rds")


grid_test <- readRDS("data/2km_grid_norcal.rds")


## Natural Resources Submodel
# Create a list of all datasets
habitat_layer <- list(
  "Canyon" = canyon_data,
  "DSC_RH" = DSC_RH_data,
  "Seeps" = seeps, 
  "Shelf Break" = shlfbrk
)

species_layer <- list(
  "whale1", "whale2"
)

bird_layer <- list (
  "bird1", "bird2"
)

##Fisheries Submodel

##Industry & Operations Submodel
industry_layer <- list(
  "Fixed Surveys" = surveys_fixed,
  "Periodic Surveys" = surveys_periodic
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
