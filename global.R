# load packages
library(shiny)
library(leaflet)
library(shinydashboard)
library(shinycssloaders)
library(markdown)
library(tidyverse)
library(bslib)
library(brand.yml)
library(shinyWidgets)
library(sf)
library(yaml)
library(rmarkdown)
library(viridis)
library(knitr)
library(fontawesome)
library(arrow)
library(sfarrow)
library(RhpcBLASctl)

# set number of CPU cores to be used explicitly
blas_set_num_threads(1)
omp_set_num_threads(1)

# Code to add _brand.yml theming, note having brand.yml package loaded is required
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
source("R/calculate_geometric_mean_full.R")
source("R/submodel_status_display.R")
source("R/apply_calculation_method.R")
source("R/generate_combined_map_for_method.R")
source("R/calculate_geometric_mean_combined.R")
source("R/calculate_lowest_combined.R")
source("R/calculate_product_combined.R")
source("R/preprocess_spatial_data.R")
source("R/crop_data_to_aoi.R")
source("R/make_combined_map_from_cached_data.R")

## Reporting Functions
source("R/generate_submodel_component_report.R")
source("R/generate_submodel_combined_report.R")
source("R/generate_full_model_report.R")
source("R/determine_component_type.R")

## Data Functions
source("R/data_timestamps.R")
source("R/filtered_data_timestamps.R")

# Get data timestamps information
timestamp_info <- get_data_timestamps()
data_timestamps <- timestamp_info$data_timestamps
most_recent_update <- timestamp_info$most_recent_update

# Load datasets with automatic preprocessing
# WEA's and OCS Planning Areas as of 12/22
AOI <- readRDS_preprocessed("data/WEA_OCS.parquet", "AOI")

## Natural Resources Submodel
# Create a list of all datasets
habitat_layer <- list(
  "Canyon" = "canyon_scored_full.parquet",
  "Deep Sea Coral Robust High Suitability" = "DSC_RH_scored_full.parquet",
  "Seeps" = "Seeps_scored_full.parquet",
  "Shelf Break" = "ShlfBrk_scored_full.parquet",
  "EFHCA" = "EFHCA_scored_full.parquet",
  "EFHCA 700 fathoms" = "EFHCA_700_scored_full.parquet",
  "HAPC AOI" = "HAPCaoi_scored_full.parquet",
  "HAPC Rocky Reef" = "HAPCreef_scored_full.parquet"
)

species_layer <- list(
  "ESA Critical Habitat for Southern Resident Killer Whales" = "killer_whale_scored_full.parquet",
  "ESA Critical Habitat for Leatherback Sea Turtles" = "leatherback_turtle_scored_full.parquet",
  "ESA Critical Habitat for Humpback Whales - Mexico and Central DPS" = "humpback_whale_scored_full.parquet",
  "Biologically Important Area - Blue Whale" = "blue_whale_scored_full.parquet"
)

##Fisheries Submodel
fisheries_layer <- list(
  "At-Sea Hake Mid-Water Trawl" = "ASH_scored_full.parquet",
  "Shoreside Hake Mid-Water Trawl" = "SSH_scored_full.parquet",
  "Groundfish Bottom Trawl" = "GFBT_scored_full.parquet",
  "Groundfish Pot Gear" = "GFP_scored_full.parquet",
  "Groundfish Long Line Gear" = "GFLL_scored_full.parquet",
  "Pink Shrimp Trawl" = "PS_scored_full.parquet",
  "Dungeness Crab" = "CRAB_scored_full.parquet",
  "Commercial Troll/Hook and Line Albacore" = "ALCO_scored_full.parquet",
  "Charter Vessel Albacore Troll/Hook and Line" = "ALCH_scored_full.parquet"
)

trawl_fisheries_layer <- list(
  "Trawl Fisheries @ 75%" = "trawl_fisheries_scored_full.parquet"
)

## Industry & Operations Submodel
surveys_layer <- list(
  "Fixed Surveys" = "Surveys_fixed_scored_full.parquet",
  "Periodic Surveys" = "Surveys_periodic_scored_full.parquet"
)
submarine_cables_layer <- list(
  "Submarine Cables" = "submarine_cable_scored_full.parquet"
)

resolution_for_aoi <- c(
  "all" = "5km",
  "Brookings" = "2km",
  "Coos Bay" = "2km",
  "Morro Bay" = "2km",
  "Humboldt" = "2km",
  "Southern California" = "5km",
  "Central California" = "5km",
  "Northern California" = "5km",
  "Washington/Oregon" = "5km"
)

# Weight values
score_values <- c(
  "0",
  "0.001",
  "0.01",
  "0.1",
  "0.2",
  "0.3",
  "0.4",
  "0.5",
  "0.6",
  "0.7",
  "0.8",
  "0.9",
  "1"
)

# Weight values with z-membership involved
score_values_z_membership <- c(
  "Z Membership",
  "0",
  "0.001",
  "0.01",
  "0.1",
  "0.2",
  "0.3",
  "0.4",
  "0.5",
  "0.6",
  "0.7",
  "0.8",
  "0.9",
  "1"
)

# Weight values with fisheries
score_values_ranked_importance <- c("Ranked Importance", "0", "0.001", "0.01")

# Weight Values for trawl fisheries
score_values_trawl_fisheries <- c("0.001")

# Add a null coalescing operator helper since R doesn't have one built-in
`%||%` <- function(x, y) if (is.null(x)) y else x

# Color palette for scores
score_colors <- list(
  "0" = "#000000", # black
  "0.01" = "#0D5359", #turquoise
  "0.001" = "#91CB3E", # lime
  "0.1" = "#E41A1C", # red
  "0.2" = "#03045E", # blue
  "0.3" = "#08A045", # green
  "0.4" = "#805D93", # purple
  "0.5" = "#EC4E20", # orange
  "0.6" = "#FEEA00", # yellow
  "0.7" = "#540b0E", # brown
  "0.8" = "#F49FBC", # pink
  "0.9" = "#791E94", # bright purple
  "1" = "#BE3E82" # berry
)
