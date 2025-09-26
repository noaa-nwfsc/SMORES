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
library(arrow)
library(sfarrow)
library(digest)

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
source("R/preprocess_spatial_data.R")

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

# Load datasets with automatic preprocessing
#WEA
AOI <- readRDS_preprocessed("data/WEA.parquet", "AOI")

## Natural Resources Layers

### Habitat Layers
canyon <- readRDS_preprocessed("data/canyon_scored_full.parquet", "Canyon")
DSC_RH <- readRDS_preprocessed("data/DSC_RH_scored_full.parquet", "DSC_RH")
DSC_RH_z_membership <- readRDS_preprocessed("data/DSC_RH_z_membership_scored_full.parquet", "DSC_RH_z_membership")
seeps <- readRDS_preprocessed("data/Seeps_scored_full.parquet", "Seeps")
shlfbrk <- readRDS_preprocessed("data/ShlfBrk_scored_full.parquet", "Shelf_Break")
efhca <- readRDS_preprocessed("data/EFHCA_scored_full.parquet", "EFHCA")
efhca_700 <- readRDS_preprocessed("data/EFHCA_700_scored_full.parquet", "EFHCA_700")
HAPCaoi <- readRDS_preprocessed("data/HAPCaoi_scored_full.parquet", "HAPC_AOI")
HAPCreef <- readRDS_preprocessed("data/HAPCreef_scored_full.parquet", "HAPC_Rocky_Reef")

### Species Layers
killer_whale <- readRDS_preprocessed("data/killer_whale_scored_full.parquet", "Killer_Whale")
leatherback_turtle <- readRDS_preprocessed("data/leatherback_turtle_scored_full.parquet", "Leatherback_Turtle")
humpback_whale <- readRDS_preprocessed("data/humpback_whale_scored_full.parquet", "Humpback_Whale")
blue_whale <- readRDS_preprocessed("data/blue_whale_scored_full.parquet", "Blue_Whale")

## Fisheries Layers 
ASH <- readRDS_preprocessed("data/ASH_scored_full.parquet", "ASH")
ASH_ranked_importance <- readRDS_preprocessed("data/ASH_Ranked_Importance_scored_full.parquet", "ASH_ranked_importance")
SSH <- readRDS_preprocessed("data/SSH_scored_full.parquet", "SSH")
SSH_ranked_importance <- readRDS_preprocessed("data/SSH_Ranked_Importance_scored_full.parquet", "SSH_ranked_importance")
GFBT <- readRDS_preprocessed("data/GFBT_scored_full.parquet", "GFBT")
GFBT_ranked_importance <- readRDS_preprocessed("data/GFBT_Ranked_Importance_scored_full.parquet", "GFBT_ranked_importance")
GFP <- readRDS_preprocessed("data/GFP_scored_full.parquet", "GFP")
GFP_ranked_importance <- readRDS_preprocessed("data/GFP_Ranked_Importance_scored_full.parquet", "GFP_ranked_importance")
GFLL <- readRDS_preprocessed("data/GFLL_scored_full.parquet", "GFLL")
GFLL_ranked_importance <- readRDS_preprocessed("data/GFLL_Ranked_Importance_scored_full.parquet", "GFLL_ranked_importance")
PS <- readRDS_preprocessed("data/PS_scored_full.parquet", "PS")
PS_ranked_importance <- readRDS_preprocessed("data/PS_Ranked_Importance_scored_full.parquet", "PS_ranked_importance")
CRAB <- readRDS_preprocessed("data/CRAB_scored_full.parquet", "CRAB")
CRAB_ranked_importance <- readRDS_preprocessed("data/CRAB_Ranked_Importance_scored_full.parquet", "CRAB_ranked_importance")
ALCO <- readRDS_preprocessed("data/ALCO_scored_full.parquet", "ALCO")
ALCO_ranked_importance <- readRDS_preprocessed("data/ALCO_Ranked_Importance_scored_full.parquet", "ALCO_ranked_importance")
ALCH <- readRDS_preprocessed("data/ALCH_scored_full.parquet", "ALCH")
ALCH_ranked_importance <- readRDS_preprocessed("data/ALCH_Ranked_Importance_scored_full.parquet", "ALCH_ranked_importance")

## Trawl Fisheries
trawl_fisheries <- readRDS_preprocessed("data/trawl_fisheries_scored_full.parquet", "Trawl_Fisheries")

## Industry & Operations Layers

### Survey Layers
surveys_fixed <- readRDS_preprocessed("data/Surveys_fixed_scored_full.parquet", "Surveys_Fixed")
surveys_periodic <- readRDS_preprocessed("data/Surveys_periodic_scored_full.parquet", "Surveys_Periodic")

### Submarine Cable Layers 
submarine_cable <- readRDS_preprocessed("data/submarine_cable_scored_full.parquet", "Submarine_Cable")

## Grid
grid_test <- readRDS_preprocessed("data/2km_grid_full.parquet", "Grid")

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
