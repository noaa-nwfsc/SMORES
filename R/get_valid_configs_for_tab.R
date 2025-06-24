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
      
      enable_input_id <- paste0("EnableLayer_", layer_id)
      score_input_id <- paste0("ScorePicker_", layer_id)
      
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
  } else if(current_tab == "industry_operations") {
    # Special handling for industry tab which uses different input naming pattern
    industry_layers <- names(layer_data)
    
    for(layer_name in industry_layers) {
      # Create consistent IDs (same as in generate_industry_sidebar.R)
      layer_id <- gsub(" ", "_", layer_name)
      layer_id <- gsub("[^A-Za-z0-9_]", "", layer_id)
      
      enable_input_id <- paste0("EnableIndustryLayer_", layer_id)
      score_input_id <- paste0("IndustryScorePicker_", layer_id)
      
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
  } else {
    # Handling for other tabs (species, birds)
    # Set the prefix based on the tab
    prefix <- switch(current_tab,
                     "species" = "Species",
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