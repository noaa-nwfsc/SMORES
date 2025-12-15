generate_submodel_component_report <- function(
    component_type,
    submodel_type,
    valid_configs,
    individual_processed_data,
    combined_data_extracted,
    input,
    filtered_aoi_data,
    file
) {
  # Component configuration - COMPLETE VERSION
  component_config <- list(
    habitat = list(
      display_name = "Habitat",
      tab_name = "Natural Resources",
      methods_input = "habitatCalculationMethods",
      combined_title = "Combined Habitat Maps",
      modal_message = "Please wait while the Habitat Component report is being generated..."
    ),
    species = list(
      display_name = "Species",
      tab_name = "Natural Resources", 
      methods_input = "speciesCalculationMethods",
      combined_title = "Combined Species Maps",
      modal_message = "Please wait while the Species Component report is being generated..."
    ),
    fisheries = list(
      display_name = "Fisheries",
      tab_name = "Fisheries",
      methods_input = "fisheriesCalculationMethods", 
      combined_title = "Combined Fisheries Maps",
      modal_message = "Please wait while the Fisheries Component report is being generated..."
    ),
    trawl = list(
      display_name = "Trawl Fisheries",
      tab_name = "Fisheries",
      methods_input = "trawlCalculationMethods",
      combined_title = "Combined Trawl Maps", 
      modal_message = "Please wait while the Trawl Component report is being generated..."
    ),
    surveys = list(
      display_name = "Surveys",
      tab_name = "Industry & Operations",
      methods_input = "surveysCalculationMethods",
      combined_title = "Combined Surveys Maps",
      modal_message = "Please wait while the Surveys Component report is being generated..."
    ),
    cables = list(
      display_name = "Submarine Cables", 
      tab_name = "Industry & Operations",
      methods_input = "cablesCalculationMethods",
      combined_title = "Combined Cables Maps",
      modal_message = "Please wait while the Cables Component report is being generated..."
    )
  )
  
  # Get component configuration
  config <- component_config[[component_type]]
  if(is.null(config)) {
    stop("Invalid component_type specified")
  }
  
  # Show modal with spinner
  show_spinner_modal("Generating Report", config$modal_message)
  
  # Get filtered timestamp information
  timestamp_info <- get_filtered_timestamp_data(valid_configs, component_type)
  
  # Get AOI data
  aoi_data <- filtered_aoi_data()
  
  # Pre-generate individual map widgets
  individual_map_objects <- list()
 
  if(!is.null(individual_processed_data)) {
    for(config_key in names(individual_processed_data)) {
      processed_item <- individual_processed_data[[config_key]]
      if(!is.null(processed_item$data)) {
        # Create the map and save as widget
        processed_config <- processed_item$config
        processed_config$data <- processed_item$data
        processed_config$color <- score_colors[[processed_item$score]] %||% "#E41A1C"
        
        individual_map_objects[[config_key]] <- list(
          layer = processed_item$layer,
          score = processed_item$score,
          score_column = processed_item$score_column,
          config = processed_config
        )
      }
    }
  }
  
  # Pre-generate combined map widgets
  selected_methods <- input[[config$methods_input]] %||% character(0)
  combined_data_list <- list()
  
  #Set method key naming convention
  method_keys <- c(geometric_mean = "geo", lowest = "lowest", product = "product")
  
  
  # Process combined data and create widget files
  if(component_type == "habitat") {
    for(method in names(method_keys)) {
      method_key <- paste0("habitat_", method_keys[method])
      if(method %in% selected_methods && !is.null(combined_data_extracted[[method_key]])) {
        combined_data_list[[method]] <- combined_data_extracted[[method_key]]
      }
    }
  }
  # species component
  else if(component_type == "species") {
    for(method in names(method_keys)) {
      method_key <- paste0("species_", method_keys[method])
      if(method %in% selected_methods && !is.null(combined_data_extracted[[method_key]])) {
        combined_data_list[[method]] <- combined_data_extracted[[method_key]]
      }
    }
  }
  
  # fisheries component
  else if(component_type == "fisheries") {
    for(method in names(method_keys)) {
      method_key <- paste0("fisheries_", method_keys[method])
      if(method %in% selected_methods && !is.null(combined_data_extracted[[method_key]])) {
        combined_data_list[[method]] <- combined_data_extracted[[method_key]]
      }
    }
  }
  # trawl component
  else if(component_type == "trawl") {
    for(method in names(method_keys)) {
      method_key <- paste0("trawl_", method_keys[method])
      if(method %in% selected_methods && !is.null(combined_data_extracted[[method_key]])) {
        combined_data_list[[method]] <- combined_data_extracted[[method_key]]
      }
    }
  }
  # surveys component
  else if(component_type == "surveys") {
    for(method in names(method_keys)) {
      method_key <- paste0("surveys_", method_keys[method])
      if(method %in% selected_methods && !is.null(combined_data_extracted[[method_key]])) {
        combined_data_list[[method]] <- combined_data_extracted[[method_key]]
      }
    }
  }
  # cables component
  else if(component_type == "cables") {
    for(method in names(method_keys)) {
      method_key <- paste0("cables_", method_keys[method])
      if(method %in% selected_methods && !is.null(combined_data_extracted[[method_key]])) {
        combined_data_list[[method]] <- combined_data_extracted[[method_key]]
      }
    }
  }
  
  # Render call for report
  rmarkdown::render(
      input = TEMP_REPORT_PATH,  # Use the global temporary path
      output_file = file,
    params = list(
      map_configs = valid_configs,
      individual_map_objects = individual_map_objects,
      combined_data_list = combined_data_list,
      selected_methods = selected_methods,
      tab_name = config$tab_name,
      combined_map_title = config$combined_title,
      data_timestamps = timestamp_info,
      component_name = config$display_name,
      aoi_data = aoi_data
    ),
    envir = new.env(parent = globalenv())
  )

  # Remove the modal when done
  removeModal()
}