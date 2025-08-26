generate_full_model_report <- function(
    input,
    combined_maps_data,
    filtered_aoi_data,
    data_timestamps,
    file
) {
  
  # Show spinner modal
  show_spinner_modal("Generating Full Model Report", 
                     "Please wait while the Full Model report is being generated...")
  
  # Initialize component_layer_details at the beginning
  component_layer_details <- list()
  
  # Define filter_by_score function locally since it's not available in global environment
  filter_by_score <- function(df, selected_score) {
    if(is.null(df) || is.null(selected_score) || selected_score == "None") {
      return(df)
    }
    
    # Special handling for Z Membership selection
    if(selected_score == "Z Membership") {
      # For Z Membership, return all data since it's already the correct dataset
      # and we want to show the continuous Z membership values
      return(df)
    }
    
    # Find the score column for each selected layer
    score_cols <- names(df)[grep("^Score\\.", names(df))]
    
    if(length(score_cols) == 0) {
      warning("No score columns found in the dataset")
      return(df)
    }
    
    # Create a filter condition for the selected score
    rows_to_keep <- rep(FALSE, nrow(df))
    
    for(col in score_cols) {
      # Convert both to character for comparison to handle numeric vs character issues
      rows_to_keep <- rows_to_keep | (as.character(df[[col]]) == as.character(selected_score))
    }
    
    # Return the filtered dataframe
    filtered_df <- df[rows_to_keep, ]
    
    return(filtered_df)
  }
  
  # Get which submodels are enabled and their weights
  nr_enabled <- input$enableNaturalResources %||% FALSE
  fisheries_enabled <- input$enableFisheries %||% FALSE
  industry_enabled <- input$enableIndustryOperations %||% FALSE
  
  # Get weights (store original values before normalization)
  natural_resources_weight <- input$weightNaturalResources %||% 0
  fisheries_weight <- input$weightFisheries %||% 0
  industry_weight <- input$weightIndustryOperations %||% 0
  
  # Build submodels_used list and original weights
  submodels_used <- list()
  submodel_weights_original <- list()  # Store original weights
  submodel_weights_normalized <- list()  # Store normalized weights
  
  if(nr_enabled && natural_resources_weight > 0 && 
     !is.null(combined_maps_data$natural_resources_combined_submodel)) {
    submodels_used[["natural_resources"]] <- TRUE
    submodel_weights_original[["natural_resources"]] <- natural_resources_weight
  }
  
  if(fisheries_enabled && fisheries_weight > 0 && 
     !is.null(combined_maps_data$fisheries_combined_submodel)) {
    submodels_used[["fisheries"]] <- TRUE
    submodel_weights_original[["fisheries"]] <- fisheries_weight
  }
  
  if(industry_enabled && industry_weight > 0 && 
     !is.null(combined_maps_data$industry_operations_combined_submodel)) {
    submodels_used[["industry_operations"]] <- TRUE
    submodel_weights_original[["industry_operations"]] <- industry_weight
  }
  
  # Normalize weights to sum to 1
  total_weight <- sum(unlist(submodel_weights_original))
  if(total_weight > 0) {
    submodel_weights_normalized <- lapply(submodel_weights_original, function(w) w / total_weight)
  } else {
    submodel_weights_normalized <- submodel_weights_original
  }
  
  # Get Natural Resources components if enabled
  natural_resources_components <- NULL
  if(nr_enabled && !is.null(combined_maps_data$natural_resources_combined_submodel)) {
    natural_resources_components <- list()
    
    # Check which NR components were used
    include_habitat <- input$includeHabitat %||% FALSE
    include_species <- input$includeSpecies %||% FALSE
    
    if(include_habitat) {
      habitat_method <- input$habitatCalculationMethod %||% "geometric_mean"
      natural_resources_components[["Habitat"]] <- "Included"
      # Store method separately for layer details
      if(is.null(component_layer_details[["natural_resources"]])) {
        component_layer_details[["natural_resources"]] <- list()
      }
      component_layer_details[["natural_resources"]][["Habitat"]] <- list(method = habitat_method)
    }
    
    if(include_species) {
      species_method <- input$speciesCalculationMethod %||% "geometric_mean"
      natural_resources_components[["Species"]] <- "Included"
      # Store method separately for layer details
      if(is.null(component_layer_details[["natural_resources"]])) {
        component_layer_details[["natural_resources"]] <- list()
      }
      component_layer_details[["natural_resources"]][["Species"]] <- list(method = species_method)
    }
  }
  
  # Get Fisheries components if enabled
  fisheries_components <- NULL
  if(fisheries_enabled && !is.null(combined_maps_data$fisheries_combined_submodel)) {
    fisheries_components <- list()
    
    # Check which IO components were used
    include_fisheries <- input$includeFisheries %||% FALSE
    include_trawl <- input$includeTrawl %||% FALSE
    
    if(include_fisheries) {
      fisheries_method <- input$fisheriesCalculationMethod %||% "geometric_mean"
      fisheries_components[["Fisheries"]] <- "Included"
      # Store method separately for layer details
      if(is.null(component_layer_details[["fisheries"]])) {
        component_layer_details[["fisheries"]] <- list()
      }
      component_layer_details[["fisheries"]][["Fisheries"]] <- list(method = fisheries_method)
    }
    
    if(include_trawl) {
      trawl_method <- input$trawlCalculationMethod %||% "geometric_mean"
      fisheries_components[["Trawl Fisheries"]] <- "Included"
      # Store method separately for layer details
      if(is.null(component_layer_details[["fisheries"]])) {
        component_layer_details[["fisheries"]] <- list()
      }
      component_layer_details[["fisheries"]][["Trawl Fisheries"]] <- list(method = fisheries_method)
    }
  }
  
  # Get Industry & Operations components if enabled
  industry_operations_components <- NULL
  if(industry_enabled && !is.null(combined_maps_data$industry_operations_combined_submodel)) {
    industry_operations_components <- list()
    
    # Check which IO components were used
    include_surveys <- input$includeSurveys %||% FALSE
    include_cables <- input$includeCables %||% FALSE
    
    if(include_surveys) {
      surveys_method <- input$surveysCalculationMethod %||% "geometric_mean"
      industry_operations_components[["Scientific Surveys"]] <- "Included"
      # Store method separately for layer details
      if(is.null(component_layer_details[["industry_operations"]])) {
        component_layer_details[["industry_operations"]] <- list()
      }
      component_layer_details[["industry_operations"]][["Scientific Surveys"]] <- list(method = surveys_method)
    }
    
    if(include_cables) {
      cables_method <- input$cablesCalculationMethod %||% "geometric_mean"
      industry_operations_components[["Submarine Cables"]] <- "Included"
      # Store method separately for layer details
      if(is.null(component_layer_details[["industry_operations"]])) {
        component_layer_details[["industry_operations"]] <- list()
      }
      component_layer_details[["industry_operations"]][["Submarine Cables"]] <- list(method = cables_method)
    }
  }
  
  # Build comprehensive component layer details with actual layer configurations
  # Natural Resources layer details
  if(!is.null(natural_resources_components)) {
    if(is.null(component_layer_details[["natural_resources"]])) {
      component_layer_details[["natural_resources"]] <- list()
    }
    
    if("Habitat" %in% names(natural_resources_components)) {
      habitat_configs <- get_valid_configs_for_tab(input, "habitat", habitat_layer, score_colors, filter_by_score)
      if(length(habitat_configs) > 0) {
        component_layer_details[["natural_resources"]][["Habitat"]] <- list(
          method = input$habitatCalculationMethod %||% "geometric_mean",
          layers = lapply(habitat_configs, function(config) {
            list(
              layer_name = config$layer %||% "Unknown",
              score_used = config$score %||% "Unknown"
            )
          })
        )
      }
    }
    
    if("Species" %in% names(natural_resources_components)) {
      species_configs <- get_valid_configs_for_tab(input, "species", species_layer, score_colors, filter_by_score)
      if(length(species_configs) > 0) {
        component_layer_details[["natural_resources"]][["Species"]] <- list(
          method = input$speciesCalculationMethod %||% "geometric_mean",
          layers = lapply(species_configs, function(config) {
            list(
              layer_name = config$layer %||% "Unknown",
              score_used = config$score %||% "Unknown"
            )
          })
        )
      }
    }
  }
  
  # Fisheries layer details
  if(!is.null(fisheries_components)) {
    if(is.null(component_layer_details[["fisheries"]])) {
      component_layer_details[["fisheries"]] <- list()
    }
    
    if("Fisheries" %in% names(fisheries_components)) {
      fisheries_configs <- get_valid_configs_for_tab(input, "fisheries", fisheries_layer, score_colors, filter_by_score)
      if(length(fisheries_configs) > 0) {
        component_layer_details[["fisheries"]][["Fisheries"]] <- list(
          method = input$fisheriesCalculationMethod %||% "geometric_mean",
          layers = lapply(fisheries_configs, function(config) {
            list(
              layer_name = config$layer %||% "Unknown",
              score_used = config$score %||% "Unknown"
            )
          })
        )
      }
    }
    
    if("Trawl Fisheries" %in% names(fisheries_components)) {
      trawl_configs <- get_valid_configs_for_tab(input, "trawl", trawl_fisheries_layer, score_colors, filter_by_score)
      if(length(trawl_configs) > 0) {
        component_layer_details[["fisheries"]][["Trawl Fisheries"]] <- list(
          method = input$trawlCalculationMethod %||% "geometric_mean",
          layers = lapply(trawl_configs, function(config) {
            list(
              layer_name = config$layer %||% "Unknown",
              score_used = config$score %||% "Unknown"
            )
          })
        )
      }
    }
  }
  
  # Industry & Operations layer details
  if(!is.null(industry_operations_components)) {
    if(is.null(component_layer_details[["industry_operations"]])) {
      component_layer_details[["industry_operations"]] <- list()
    }
    
    if("Scientific Surveys" %in% names(industry_operations_components)) {
      surveys_configs <- get_valid_configs_for_tab(input, "surveys", surveys_layer, score_colors, filter_by_score)
      if(length(surveys_configs) > 0) {
        component_layer_details[["industry_operations"]][["Scientific Surveys"]] <- list(
          method = input$surveysCalculationMethod %||% "geometric_mean",
          layers = lapply(surveys_configs, function(config) {
            list(
              layer_name = config$layer %||% "Unknown",
              score_used = config$score %||% "Unknown"
            )
          })
        )
      }
    }
    
    if("Submarine Cables" %in% names(industry_operations_components)) {
      cables_configs <- get_valid_configs_for_tab(input, "cables", submarine_cables_layer, score_colors, filter_by_score)
      if(length(cables_configs) > 0) {
        component_layer_details[["industry_operations"]][["Submarine Cables"]] <- list(
          method = input$cablesCalculationMethod %||% "geometric_mean",
          layers = lapply(cables_configs, function(config) {
            list(
              layer_name = config$layer %||% "Unknown",
              score_used = config$score %||% "Unknown"
            )
          })
        )
      }
    }
  }
  
  # Get filtered timestamp information for the full model
  all_configs <- list()
  timestamp_info <- get_filtered_timestamp_data(all_configs, "full_model")
  
  # Get filtered AOI data for the report
  aoi_data <- filtered_aoi_data()
  
  # Get the overall model data and maps
  overall_combined_data <- combined_maps_data$overall_combined_model
  overall_combined_map <- combined_maps_data$overall_combined_map
  overall_aoi_cropped_map <- combined_maps_data$overall_combined_map_cropped
  overall_aoi_cropped_normalized_map <- combined_maps_data$overall_combined_map_cropped_normalized
  
  # Render the full model report
  rmarkdown::render(
    input = "Full_Model_Report_Template.Rmd", 
    output_file = file,
    params = list(
      submodels_used = submodels_used,
      submodel_weights_original = submodel_weights_original,
      submodel_weights_normalized = submodel_weights_normalized, 
      weight_natural_resources = natural_resources_weight,
      weight_fisheries = fisheries_weight,
      weight_industry_operations = industry_weight,
      natural_resources_components = natural_resources_components,
      industry_operations_components = industry_operations_components,
      fisheries_components = fisheries_components,
      component_layer_details = component_layer_details,
      overall_combined_data = overall_combined_data,
      overall_combined_map = overall_combined_map,
      overall_aoi_cropped_map = overall_aoi_cropped_map,
      overall_aoi_cropped_normalized_map = overall_aoi_cropped_normalized_map,
      data_timestamps = data_timestamps,
      aoi_data = aoi_data
    ),
    envir = new.env(parent = globalenv())
  )
  # Remove modal
  removeModal()
}