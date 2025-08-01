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
library(rmarkdown)
library(viridis)
library(knitr)

theme <- bslib::bs_theme()

#load custom functions 
source("R/modal.R")
source("R/get_valid_configs_for_tab.R")
source("R/create_individual_map.R")
source("R/create_combined_map.R")
source("R/data_timestamps.R")
source("R/generate_habitat_sidebar.R")
source("R/generate_species_sidebar.R")
source("R/generate_surveys_sidebar.R")
source("R/create_maps_container.R")
source("R/filtered_data_timestamps.R")
source("R/check_submodel_status.R")
source("R/sidebar_helpers.R")
source("R/submodel_configurations.R")
source("R/generate_overall_model_sidebar.R")
source("R/generate_natural_resources_combined_sidebar.R")
source("R/create_combined_submodel_map.R")
source("R/generate_cables_sidebar.R")
source("R/create_aoi_cropped_map.R")
source("R/create_overall_combined_model.R")
source("R/submodel_status_display.R")
source("R/map_generation_status.R")
source("R/apply_calculation_method.R")
source("R/make_combined_map_dataset.R")
source("R/generate_combined_map_for_method.R")
source("R/calculate_geometric_mean_combined.R")
source("R/calculate_lowest_combined.R")
source("R/calculate_product_combined.R")
source("R/generate_submodel_component_report.R")

# Get data timestamps information
timestamp_info <- get_data_timestamps()
data_timestamps <- timestamp_info$data_timestamps
most_recent_update <- timestamp_info$most_recent_update

# Load datasets
#WEA
AOI <- readRDS("data/WEA.rds")
## Natural Resources Layers
### Habitat Layers
canyon <- readRDS("data/canyon_scored.rds")
DSC_RH <- readRDS("data/DSC_RH_scored.rds")
DSC_RH_z_membership <- readRDS("data/DSC_RH_z_membership_scored.rds")
surveys_fixed <- readRDS("data/Surveys_fixed_scored.rds")
surveys_periodic <- readRDS("data/Surveys_periodic_scored.rds")
seeps <- readRDS("data/Seeps_scored.rds")
shlfbrk <- readRDS("data/ShlfBrk_scored.rds")
efhca <- readRDS("data/efhca_scored.rds")
efhca_700 <- readRDS("data/efhca_700_scored.rds")
HAPCaoi <- readRDS("data/HAPCaoi_scored.rds")
HAPCreef <- readRDS("data/HAPCreef_scored.rds")
### Species Layers
killer_whale <- readRDS("data/killer_whale_scored.rds")
leatherback_turtle <- readRDS("data/leatherback_turtle_scored.rds")
humpback_whale <- readRDS("data/humpback_whale_scored.rds")
blue_whale <- readRDS("data/blue_whale_scored.rds")
### Birds Layers
## Industry & Operations Layers
###Survey Layers
surveys_fixed <- readRDS("data/Surveys_fixed_scored.rds")
surveys_periodic <- readRDS("data/Surveys_periodic_scored.rds")
###
submarine_cable <- readRDS("data/submarine_cable_scored.rds")
submarine_cable_500m <- readRDS("data/submarine_cable_500m_scored.rds")
submarine_cable_501_1000m <- readRDS("data/submarine_cable_501_1000m_scored.rds")
grid_test <- readRDS("data/2km_grid_norcal.rds")


## Natural Resources Submodel
# Create a list of all datasets
habitat_layer <- list(
  "Canyon" = canyon,
  "Deep Sea Coral Robust High Suitability" = DSC_RH,
  "Seeps" = seeps, 
  "Shelf Break" = shlfbrk, 
  "EFHCA" = efhca,
  "EFHCA 700 fathom" = efhca_700, 
  "HAPC AOI" = HAPCaoi,
  "HAPC Rocky Reef" = HAPCreef
)

species_layer <- list(
  "ESA Critical Habitat for Southern Resident Killer Whales" = killer_whale,
  "ESA Critical Habitat for Leatherback Sea Turtles" = leatherback_turtle,
  "ESA Critical Habitat for Humpback Whales - Mexico and Central DPS" = humpback_whale,
  "Biologically Important Area - Blue Whale" = blue_whale
)

bird_layer <- list (
  "bird1", "bird2"
)

##Fisheries Submodel

##Industry & Operations Submodel
surveys_layers <- list(
  "Fixed Surveys" = surveys_fixed,
  "Periodic Surveys" = surveys_periodic
)
submarine_cables_layer <- list(
  "Submarine Cables" = submarine_cable
)

# Weight values
score_values <- c("0.01", "0.1", "0.2", "0.3", "0.4", "0.5" ,"0.6", "0.7", "0.8", "0.9", "1")

# Weight values with z-membership involved
score_values_z_membership <- c("Z Membership" ,"0.01", "0.1", "0.2", "0.3", "0.4", "0.5" ,"0.6", "0.7", "0.8", "0.9", "1")

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

# Function to create continuous color palette for Z Membership
create_z_membership_palette <- function(data) {
  if(is.null(data) || !"Score.Z_Membership" %in% names(data)) {
    return(colorNumeric("viridis", domain = c(0, 1)))
  }
  
  z_values <- data$Score.Z_Membership[!is.na(data$Score.Z_Membership)]
  if(length(z_values) == 0) {
    return(colorNumeric("viridis", domain = c(0, 1)))
  }
  
  return(colorNumeric("viridis", domain = range(z_values, na.rm = TRUE)))
}