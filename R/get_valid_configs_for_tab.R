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
              color_palette <- create_continuous_palette(filtered_data, "z_membership")
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
  } else if(current_tab == "fisheries") {
    # Special handling for fisheries tab
    fisheries_layers <- names(layer_data)
    
    for(layer_name in fisheries_layers) {
      layer_id <- gsub(" ", "_", layer_name)
      layer_id <- gsub("[^A-Za-z0-9_]", "", layer_id)
      
      enable_input_id <- paste0("EnableFisheriesLayer_", layer_id)
      score_input_id <- paste0("FisheriesScorePicker_", layer_id)
      
      # Check if this layer is enabled
      is_enabled <- !is.null(input[[enable_input_id]]) && input[[enable_input_id]]
      
      if(is_enabled) {
        score_value <- input[[score_input_id]]
        
        if(!is.null(score_value) && score_value != "None") {
          
          # Handle conditional dataset selection for fisheries layers
          if(score_value == "Ranked Importance") {
            # Use the ranked importance dataset
            dataset_to_use <- switch(layer_name,
                                     "At-Sea Hake Mid-Water Trawl" = ASH_ranked_importance,
                                     "Shoreside Hake Mid-Water Trawl" = SSH_ranked_importance,
                                     "Groundfish Bottom Trawl" = GFBT_ranked_importance,
                                     "Groundfish Pot Gear" = GFP_ranked_importance,
                                     "Groundfish Long Line Gear" = GFLL_ranked_importance,
                                     "Pink Shrimp Trawl" = PS_ranked_importance,
                                     "Dungeness Crab" = CRAB_ranked_importance,
                                     "Commercial Troll/Hook and Line Albacore" = ALCO_ranked_importance,
                                     "Charter Vessel Albacore Troll/Hook and Line" = ALCH_ranked_importance,
                                     layer_data[[layer_name]]  # fallback to original if no match
            )
            
            # Use continuous coloring for ranked importance
            filtered_data <- dataset_to_use
            score_color <- "continuous"  # Flag for continuous coloring
            color_palette <- create_continuous_palette(filtered_data, score_type = "ranked_importance", layer_name = "ranked_importance")
          } else {
            # For discrete scores (0, 0.01, 0.001), use the regular dataset
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
  } else if(current_tab == "trawl") {
    trawl_fisheries_layers <- names(layer_data)
    
    for(layer_name in trawl_fisheries_layers) {
      # Create consistent IDs
      layer_id <- gsub(" ", "_", layer_name)
      layer_id <- gsub("[^A-Za-z0-9_]", "", layer_id)
      
      enable_input_id <- paste0("EnableTrawlLayer_", layer_id)
      score_input_id <- paste0("TrawlScorePicker_", layer_id)
      
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
  }
  
  return(valid_configs)
}