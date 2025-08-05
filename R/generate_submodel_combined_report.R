generate_submodel_combined_report <- function(
    submodel_type,  # "natural_resources" or "industry_operations"
    input,
    combined_maps_data,
    filtered_aoi_data,
    file
) {
  
  # Show spinner modal
  show_spinner_modal("Generating Combined Submodel Report", 
                     paste("Please wait while the", submodel_type, "Combined Submodel report is being generated..."))
  
  # Set submodel-specific variables
  if(submodel_type == "natural_resources") {
    submodel_display_name <- "Natural Resources"
    habitat_input <- input$includeHabitat %||% FALSE
    species_input <- input$includeSpecies %||% FALSE
    birds_input <- input$includeBirds %||% FALSE
    
    habitat_method <- input$habitatCalculationMethod %||% "geometric_mean"
    species_method <- input$speciesCalculationMethod %||% "geometric_mean"
    
    combined_data <- combined_maps_data$natural_resources_combined_submodel
    combined_map <- combined_maps_data$natural_resources_combined_map
    aoi_cropped_map <- combined_maps_data$natural_resources_combined_map_cropped
    aoi_cropped_normalized_map <- combined_maps_data$natural_resources_combined_map_cropped_normalized
    
  } else if(submodel_type == "industry_operations") {
    submodel_display_name <- "Industry & Operations"
    surveys_input <- input$includeSurveys %||% FALSE
    cables_input <- input$includeCables %||% FALSE
    
    surveys_method <- input$surveysCalculationMethod %||% "geometric_mean"
    cables_method <- input$cablesCalculationMethod %||% "geometric_mean"
    
    combined_data <- combined_maps_data$industry_operations_combined_submodel
    combined_map <- combined_maps_data$industry_operations_combined_map
    aoi_cropped_map <- combined_maps_data$industry_operations_combined_map_cropped
    aoi_cropped_normalized_map <- combined_maps_data$industry_operations_combined_map_cropped_normalized
    
  } else {
    stop("Invalid submodel_type. Use 'natural_resources' or 'industry_operations'")
  }
  
  # Get component selections for the report
  selected_components <- c()
  component_methods <- c()
  component_layer_details <- list()
  component_data_summary <- list()
  
  if(submodel_type == "natural_resources") {
    # Natural Resources components
    if(habitat_input) {
      selected_components <- c(selected_components, "Habitat")
      component_methods <- c(component_methods, habitat_method)
      
      # Get habitat layer details with scores
      habitat_configs <- get_valid_configs_for_tab(input, "habitat", habitat_layer, score_colors, filter_by_score)
      if(length(habitat_configs) > 0) {
        component_layer_details[["Habitat"]] <- list(
          method = habitat_method,
          layers = lapply(habitat_configs, function(config) {
            list(
              layer_name = config$layer %||% "Unknown",
              score_used = config$score %||% "Unknown"
            )
          })
        )
      }
      
      # Add data summary
      habitat_data <- switch(habitat_method,
                             "geometric_mean" = combined_maps_data$habitat_geo,
                             "lowest" = combined_maps_data$habitat_lowest,
                             "product" = combined_maps_data$habitat_product,
                             combined_maps_data$habitat_geo)
      
      if(!is.null(habitat_data)) {
        component_data_summary[["Habitat"]] <- list(
          data_points = nrow(habitat_data),
          description = "Habitat suitability analysis for marine ecosystems"
        )
      }
    }
    
    if(species_input) {
      selected_components <- c(selected_components, "Species")
      component_methods <- c(component_methods, species_method)
      
      # Get species layer details with scores
      species_configs <- get_valid_configs_for_tab(input, "species", species_layer, score_colors, filter_by_score)
      if(length(species_configs) > 0) {
        component_layer_details[["Species"]] <- list(
          method = species_method,
          layers = lapply(species_configs, function(config) {
            list(
              layer_name = config$layer %||% "Unknown",
              score_used = config$score %||% "Unknown"
            )
          })
        )
      }
      
      # Add data summary
      species_data <- switch(species_method,
                             "geometric_mean" = combined_maps_data$species_geo,
                             "lowest" = combined_maps_data$species_lowest,
                             "product" = combined_maps_data$species_product,
                             combined_maps_data$species_geo)
      
      if(!is.null(species_data)) {
        component_data_summary[["Species"]] <- list(
          data_points = nrow(species_data),
          description = "Critical habitat analysis for protected species"
        )
      }
    }
    
  } else if(submodel_type == "industry_operations") {
    # Industry & Operations components
    if(surveys_input) {
      selected_components <- c(selected_components, "Scientific Surveys")
      component_methods <- c(component_methods, surveys_method)
      
      # Get surveys layer details with scores
      surveys_configs <- get_valid_configs_for_tab(input, "surveys", surveys_layer, score_colors, filter_by_score)
      if(length(surveys_configs) > 0) {
        component_layer_details[["Scientific Surveys"]] <- list(
          method = surveys_method,
          layers = lapply(surveys_configs, function(config) {
            list(
              layer_name = config$layer %||% "Unknown",
              score_used = config$score %||% "Unknown"
            )
          })
        )
      }
      
      # Add data summary
      surveys_data <- switch(surveys_method,
                             "geometric_mean" = combined_maps_data$surveys_geo,
                             "lowest" = combined_maps_data$surveys_lowest,
                             "product" = combined_maps_data$surveys_product,
                             combined_maps_data$surveys_geo)
      
      if(!is.null(surveys_data)) {
        component_data_summary[["Scientific Surveys"]] <- list(
          data_points = nrow(surveys_data),
          description = "Scientific survey activities and their spatial impact assessment"
        )
      }
    }
    
    if(cables_input) {
      selected_components <- c(selected_components, "Submarine Cables")
      component_methods <- c(component_methods, cables_method)
      
      # Get cables layer details with scores
      cables_configs <- get_valid_configs_for_tab(input, "cables", submarine_cables_layer, score_colors, filter_by_score)
      if(length(cables_configs) > 0) {
        component_layer_details[["Submarine Cables"]] <- list(
          method = cables_method,
          layers = lapply(cables_configs, function(config) {
            list(
              layer_name = config$layer %||% "Unknown",
              score_used = config$score %||% "Unknown"
            )
          })
        )
      }
      
      # Add data summary
      cables_data <- switch(cables_method,
                            "geometric_mean" = combined_maps_data$cables_geo,
                            "lowest" = combined_maps_data$cables_lowest,
                            "product" = combined_maps_data$cables_product,
                            combined_maps_data$cables_geo)
      
      if(!is.null(cables_data)) {
        component_data_summary[["Submarine Cables"]] <- list(
          data_points = nrow(cables_data),
          description = "Submarine cable infrastructure and associated buffer zones"
        )
      }
    }
  }
  
  # Get filtered timestamp information for the combined submodel
  all_configs <- list()
  timestamp_info <- get_filtered_timestamp_data(all_configs, "combined")
  
  # Get filtered AOI data for the report
  aoi_data <- filtered_aoi_data()
  
  # Render the combined submodel report
  rmarkdown::render(
    input = "Submodel_Combined_Report_Template.Rmd", 
    output_file = file,
    params = list(
      submodel_name = submodel_display_name,
      selected_components = selected_components,
      component_methods = component_methods,
      combined_data = combined_data,
      combined_map_title = paste(submodel_display_name, "Combined Submodel"),
      data_timestamps = timestamp_info,
      component_data_summary = component_data_summary,
      aoi_data = aoi_data,
      component_layer_details = component_layer_details,
      combined_map = combined_map,
      aoi_cropped_map = aoi_cropped_map,
      aoi_cropped_normalized_map = aoi_cropped_normalized_map
    ),
    envir = new.env(parent = globalenv())
  )
  
  # Remove modal
  removeModal()
}