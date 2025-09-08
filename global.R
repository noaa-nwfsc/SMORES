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
library(rmarkdown)
library(viridis)
library(knitr)
library(fontawesome)

# Code to add _brand.yml theming
theme <- bslib::bs_theme()

# Load custom functions 
## Sidebar functions
source("R/generate_habitat_sidebar.R")
source("R/generate_species_sidebar.R")
source("R/generate_fisheries_sidebar.R")
source("R/generate_trawl_fisheries_sidebar.R")
source("R/generate_surveys_sidebar.R")
source("R/generate_cables_sidebar.R")
source("R/generate_full_model_sidebar.R")
source("R/generate_natural_resources_combined_sidebar.R")
source("R/sidebar_helpers.R")

## Mapping and Aesthetics Functions
source("R/modal.R")
source("R/get_valid_configs_for_tab.R")
source("R/create_individual_map.R")
source("R/create_combined_map.R")
source("R/filter_by_score.R")
source("R/calculate_submodel_geometric_mean.R")
source("R/create_continuous_palette.R")
source("R/create_maps_container.R")
source("R/check_submodel_status.R")
source("R/submodel_configurations.R")
source("R/create_combined_submodel_map.R")
source("R/create_aoi_cropped_map.R")
source("R/create_full_model_map.R")
source("R/submodel_status_display.R")
source("R/apply_calculation_method.R")
source("R/make_combined_map_dataset.R")
source("R/generate_combined_map_for_method.R")
source("R/calculate_geometric_mean_combined.R")
source("R/calculate_lowest_combined.R")
source("R/calculate_product_combined.R")

## Reporting Functions
source("R/generate_submodel_component_report.R")
source("R/generate_submodel_combined_report.R")
source("R/generate_full_model_report.R")

## Data Functions
source("R/data_timestamps.R")
source("R/filtered_data_timestamps.R")


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
efhca <- readRDS("data/EFHCA_scored.rds")
efhca_700 <- readRDS("data/EFHCA_700_scored.rds")
HAPCaoi <- readRDS("data/HAPCaoi_scored.rds")
HAPCreef <- readRDS("data/HAPCreef_scored.rds")

### Species Layers
killer_whale <- readRDS("data/killer_whale_scored.rds")
leatherback_turtle <- readRDS("data/leatherback_turtle_scored.rds")
humpback_whale <- readRDS("data/humpback_whale_scored.rds")
blue_whale <- readRDS("data/blue_whale_scored.rds")

## Fisheries Layers 
ASH <- readRDS("data/ASH_scored.rds")
ASH_ranked_importance <- readRDS("data/ASH_Ranked_Importance_scored.rds")
SSH <- readRDS("data/SSH_scored.rds")
SSH_ranked_importance <- readRDS("data/SSH_Ranked_Importance_scored.rds")
GFBT <- readRDS("data/GFBT_scored.rds")
GFBT_ranked_importance <- readRDS("data/GFBT_Ranked_Importance_scored.rds")
GFP <- readRDS("data/GFP_scored.rds")
GFP_ranked_importance <- readRDS("data/GFP_Ranked_Importance_scored.rds")
GFLL <- readRDS("data/GFLL_scored.rds")
GFLL_ranked_importance <- readRDS("data/GFLL_Ranked_Importance_scored.rds")
PS <- readRDS("data/PS_scored.rds")
PS_ranked_importance <- readRDS("data/PS_Ranked_Importance_scored.rds")
CRAB <- readRDS("data/CRAB_scored.rds")
CRAB_ranked_importance <- readRDS("data/CRAB_Ranked_Importance_scored.rds")
ALCO <- readRDS("data/ALCO_scored.rds")
ALCO_ranked_importance <- readRDS("data/ALCO_Ranked_Importance_scored.rds")
ALCH <- readRDS("data/ALCH_scored.rds")
ALCH_ranked_importance <- readRDS("data/ALCH_Ranked_Importance_scored.rds")

## Trawl Fisheries
trawl_fisheries <- readRDS("data/trawl_fisheries_scored.rds")

## Industry & Operations Layers

### Survey Layers
surveys_fixed <- readRDS("data/Surveys_fixed_scored.rds")
surveys_periodic <- readRDS("data/Surveys_periodic_scored.rds")

### Submarine Cable Layers 
submarine_cable <- readRDS("data/submarine_cable_scored.rds")

## Grid
grid_test <- readRDS("data/2km_grid_norcal.rds")


## Natural Resources Submodel
# Create a list of all datasets
habitat_layer <- list(
  "Canyon" = canyon,
  "Deep Sea Coral Robust High Suitability" = DSC_RH,
  "Seeps" = seeps, 
  "Shelf Break" = shlfbrk, 
  "EFHCA" = efhca,
  "EFHCA 700 fathoms" = efhca_700, 
  "HAPC AOI" = HAPCaoi,
  "HAPC Rocky Reef" = HAPCreef
)

species_layer <- list(
  "ESA Critical Habitat for Southern Resident Killer Whales" = killer_whale,
  "ESA Critical Habitat for Leatherback Sea Turtles" = leatherback_turtle,
  "ESA Critical Habitat for Humpback Whales - Mexico and Central DPS" = humpback_whale,
  "Biologically Important Area - Blue Whale" = blue_whale
)

##Fisheries Submodel
fisheries_layer <- list(
  "At-Sea Hake Mid-Water Trawl" = ASH,
  "Shoreside Hake Mid-Water Trawl" = SSH,
  "Groundfish Bottom Trawl" = GFBT,
  "Groundfish Pot Gear" = GFP,
  "Groundfish Long Line Gear" = GFLL,
  "Pink Shrimp Trawl" = PS,
  "Dungeness Crab" = CRAB,
  "Commercial Troll/Hook and Line Albacore" = ALCO,
  "Charter Vessel Albacore Troll/Hook and Line" = ALCH
)

trawl_fisheries_layer <- list(
  "Trawl Fisheries @ 75%" = trawl_fisheries
)

## Industry & Operations Submodel
surveys_layer <- list(
  "Fixed Surveys" = surveys_fixed,
  "Periodic Surveys" = surveys_periodic
)
submarine_cables_layer <- list(
  "Submarine Cables" = submarine_cable
)

# Weight values
score_values <- c("0","0.001", "0.01", "0.1", "0.2", "0.3", "0.4", "0.5" ,"0.6", "0.7", "0.8", "0.9", "1")

# Weight values with z-membership involved
score_values_z_membership <- c("Z Membership", "0", "0.001", "0.01", "0.1", "0.2", "0.3", "0.4", "0.5" ,"0.6", "0.7", "0.8", "0.9", "1")

# Weight values with fisheries 
score_values_ranked_importance <- c("Ranked Importance", "0", "0.001", "0.01")

# Weight Values for trawl fisheries
score_values_trawl_fisheries <- c("0.001")

# Add a null coalescing operator helper since R doesn't have one built-in
`%||%` <- function(x, y) if(is.null(x)) y else x

# Color palette for scores
score_colors <- list(
  "0" = "#000000",     # black
  "0.01" =  "#0D5359", #turquoise   
  "0.001" = "#91CB3E", # lime
  "0.1" = "#E41A1C",  # red
  "0.2" = "#03045E",  # blue
  "0.3" = "#08A045",  # green
  "0.4" = "#805D93",  # purple
  "0.5" = "#EC4E20",  # orange
  "0.6" = "#FEEA00",  # yellow
  "0.7" = "#540b0E",  # brown
  "0.8" = "#F49FBC",  # pink
  "0.9" = "#791E94",  # bright purple 
  "1" = "#BE3E82" # berry
)
