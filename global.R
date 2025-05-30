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

# Load datasets
canyon_data <- readRDS("data\\canyon_scored.rds")
DSC_RH_data <- readRDS("data\\DSC_RH_scored.rds")
surveys_fixed <- readRDS("data\\Surveys_fixed_scored.rds")

full_data <- readRDS("data\\full_data.rds")

# Create a list of all datasets
layer <- list(
  "Canyon" = canyon_data,
  "DSC_RH" = DSC_RH_data,
  "Fixed Surveys" = surveys_fixed
)

# Define possible weight values
score_values <- c("0.1", "0.2", "0.3", "0.4", "0.5" ,"0.6", "0.7", "0.8", "0.9", "1")

# Add a null coalescing operator helper since R doesn't have one built-in
`%||%` <- function(x, y) if(is.null(x)) y else x
