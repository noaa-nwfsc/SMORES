# load packages
library(shiny)
library(leaflet)
library(shinydashboard)
library(shinycssloaders)
library(markdown)
library(tidyverse)
library(bslib)
library(shinyWidgets)

# # Load all datasets
canyon_data <- readRDS("data\\canyon_scored.rds")
DSC_data <- readRDS("data\\DSC_RH_scored.rds")
surveys_fixed <- readRDS("data\\Surveys_fixed_scored.rds")

# Create a list of all datasets for easy access
layer <- list(
  "Canyon" = canyon_data,
  "DSC" = DSC_data,
  "Fixed Surveys" = surveys_fixed
)

# Define possible weight values
weight_values <- c("0.1", "0.2", "0.3", "0.4", "0.5" ,"0.6", "0.7", "0.8", "0.9", "1")



