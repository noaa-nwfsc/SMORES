get_valid_configs_for_tab <- function(input, current_tab, layer_data, score_colors, filter_by_score) {
  valid_configs <- list()
  index <- 1
  
  if(current_tab == "habitat") {
    # Special handling for habitat tab which uses different input naming pattern
    habitat_layers <- names(layer_data)
    
    for(layer_name in habitat_layers) {
      # Create consistent IDs
      layer_id <- gsub(" ", "_", layer_name)
      layer_id <- gsub("[^A-Za-z0-9_]", "", layer_id)
      
      enable_input_id <- paste0("EnableHabitatLayer_", layer_id)
      score_input_id <- paste0("HabitatScorePicker_", layer_id)
      
      # Check if this layer is enabled
      is_enabled <- !is.null(input[[enable_input_id]]) && input[[enable_input_id]]
      
      if(is_enabled) {
        score_value <- input[[score_input_id]]
        
        if(!is.null(score_value) && score_value != "None") {
          
          # Special handling for DSC layer with Z Membership
          if(layer_name == "Deep Sea Coral Robust High Suitability") {
            if(score_value == "Z Membership") {
              # Use the Z membership dataset and create continuous color palette
              filtered_data <- filter_by_score(DSC_RH_z_membership, score_value)
              score_color <- "continuous"  # Flag for continuous coloring
              color_palette <- create_z_membership_palette(filtered_data)
            } else {
              # Use the regular scored dataset
              filtered_data <- filter_by_score(layer_data[[layer_name]], score_value)
              score_color <- score_colors[[score_value]]
              color_palette <- NULL
            }
          } else {
            # For all other layers, use regular processing
            filtered_data <- filter_by_score(layer_data[[layer_name]], score_value)
            score_color <- score_colors[[score_value]]
            color_palette <- NULL
          }
          
          # Add to valid configs
          valid_configs[[length(valid_configs) + 1]] <- list(
            index = index,
            layer = layer_name,
            score = score_value,
            color = score_color,
            color_palette = color_palette,  # Add this for continuous coloring
            data = filtered_data
          )
          
          index <- index + 1
        }
      }
    }
  } else if(current_tab == "species") {
    # Special handling for industry tab which uses different input naming pattern
    species_layers <- names(layer_data)
    
    for(layer_name in species_layers) {
      # Create consistent IDs (same as in generate_industry_sidebar.R)
      layer_id <- gsub(" ", "_", layer_name)
      layer_id <- gsub("[^A-Za-z0-9_]", "", layer_id)
      
      enable_input_id <- paste0("EnableSpeciesLayer_", layer_id)
      score_input_id <- paste0("SpeciesScorePicker_", layer_id)
      
      # Check if this layer is enabled
      is_enabled <- !is.null(input[[enable_input_id]]) && input[[enable_input_id]]
      
      if(is_enabled) {
        score_value <- input[[score_input_id]]
        
        if(!is.null(score_value) && score_value != "None") {
          # Get score color
          score_color <- score_colors[[score_value]]
          
          # Filter data by score
          filtered_data <- filter_by_score(layer_data[[layer_name]], score_value)
          
          # Add to valid configs
          valid_configs[[length(valid_configs) + 1]] <- list(
            index = index,
            layer = layer_name,
            score = score_value,
            color = score_color,
            data = filtered_data
          )
          
          index <- index + 1
        }
      }
    }
  } else if(current_tab == "surveys") {
    # Special handling for industry tab which uses different input naming pattern
    surveys_layers <- names(layer_data)
    
    for(layer_name in surveys_layers) {
      # Create consistent IDs (same as in generate_industry_sidebar.R)
      layer_id <- gsub(" ", "_", layer_name)
      layer_id <- gsub("[^A-Za-z0-9_]", "", layer_id)
      
      enable_input_id <- paste0("EnableSurveysLayer_", layer_id)
      score_input_id <- paste0("SurveysScorePicker_", layer_id)
      
      # Check if this layer is enabled
      is_enabled <- !is.null(input[[enable_input_id]]) && input[[enable_input_id]]
      
      if(is_enabled) {
        score_value <- input[[score_input_id]]
        
        if(!is.null(score_value) && score_value != "None") {
          # Get score color
          score_color <- score_colors[[score_value]]
          
          # Filter data by score
          filtered_data <- filter_by_score(layer_data[[layer_name]], score_value)
          
          # Add to valid configs
          valid_configs[[length(valid_configs) + 1]] <- list(
            index = index,
            layer = layer_name,
            score = score_value,
            color = score_color,
            data = filtered_data
          )
          
          index <- index + 1
        }
      }
    }
  } else if(current_tab == "cables") {
    # Special handling for cables tab which uses different input naming pattern
    cable_layers <- names(layer_data)
    
    for(layer_name in cable_layers) {
      # Create consistent IDs
      layer_id <- gsub(" ", "_", layer_name)
      layer_id <- gsub("[^A-Za-z0-9_]", "", layer_id)
      
      enable_input_id <- paste0("EnableCablesLayer_", layer_id)
      score_input_id <- paste0("CablesScorePicker_", layer_id)
      
      # Check if this layer is enabled
      is_enabled <- !is.null(input[[enable_input_id]]) && input[[enable_input_id]]
      
      if(is_enabled) {
        score_value <- input[[score_input_id]]
        
        if(!is.null(score_value) && score_value != "None") {
          # Get score color
          score_color <- score_colors[[score_value]]
          
          # Filter data by score
          if(layer_name %in% names(layer_data)) {
            filtered_data <- filter_by_score(layer_data[[layer_name]], score_value)
            
            # Add to valid configs
            valid_configs[[length(valid_configs) + 1]] <- list(
              index = index,
              layer = layer_name,
              score = score_value,
              color = score_color,
              data = filtered_data
            )
            
            index <- index + 1
          }
        }
      }
    }
  } else {
    # Handling for other tabs ( birds)
    # Set the prefix based on the tab
    prefix <- switch(current_tab,
                     "birds" = "Bird",
                     "")
    
    if(prefix != "") {
      for(i in 1:6) {
        # Check if this configuration is enabled and has valid selections
        enable_input <- input[[paste0("Enable", prefix, "Map", i)]]
        layer_input <- input[[paste0(prefix, "LayerPicker", i)]]
        score_input <- input[[paste0(prefix, "ScorePicker", i)]]
        
        # Only process if all required inputs exist and are valid
        if(!is.null(enable_input) && enable_input &&
           !is.null(layer_input) && layer_input != "None" &&
           !is.null(score_input) && score_input != "None" &&
           layer_input %in% names(layer_data)) {
          
          # Get score color
          score_color <- score_colors[[score_input]]
          
          # Filter data by score
          filtered_data <- filter_by_score(layer_data[[layer_input]], score_input)
          
          # Add to valid configs
          valid_configs[[length(valid_configs) + 1]] <- list(
            index = index,
            layer = layer_input,
            score = score_input,
            color = score_color,
            data = filtered_data
          )
          
          index <- index + 1
        }
      }
    }
  }
  
  return(valid_configs)
}