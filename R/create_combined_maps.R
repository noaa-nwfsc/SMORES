# Calculate geometric mean across all score columns  
calculate_geometric_mean <- function(combined_data) {
  # Find all score columns
  score_cols <- names(combined_data)[grep("^Score\\.", names(combined_data))]
  
  if(length(score_cols) > 0) {
    # Convert to numeric and calculate row-wise geometric mean
    score_matrix <- as.matrix(combined_data[score_cols])
    score_matrix <- apply(score_matrix, 2, as.numeric)
    
    combined_data$Geo_mean <- apply(score_matrix, 1, function(x) {
      if(all(is.na(x))) return(NA)
      # Handle case where all values are the same
      if(length(unique(x[!is.na(x)])) <= 1) {
        return(x[!is.na(x)][1])  # Return the single unique value
      }
      exp(mean(log(x), na.rm = TRUE))
    })
    
    # Filter out rows where geometric mean is NA or 0
    combined_data <- combined_data[!is.na(combined_data$Geo_mean) & combined_data$Geo_mean > 0, ]
  }
  
  return(combined_data)
}

# Calculate lowest value across all score columns
calculate_lowest_value <- function(combined_data) {
  # Find all score columns (excluding geometry and ID columns)
  score_cols <- names(combined_data)[grep("^Score\\.", names(combined_data))]
  
  if(length(score_cols) > 0) {
    # Convert to numeric and calculate row-wise minimum
    score_matrix <- as.matrix(combined_data[score_cols])
    score_matrix <- apply(score_matrix, 2, as.numeric)
    combined_data$Lowest_value <- apply(score_matrix, 1, function(x) {
      if(all(is.na(x))) return(NA)
      min(x, na.rm = TRUE)
    })
  }
  
  return(combined_data)
}

# Calculate product across all score columns
calculate_product <- function(combined_data) {
  # Find all score columns
  score_cols <- names(combined_data)[grep("^Score\\.", names(combined_data))]
  
  if(length(score_cols) > 0) {
    # Convert to numeric and calculate row-wise product
    score_matrix <- as.matrix(combined_data[score_cols])
    score_matrix <- apply(score_matrix, 2, as.numeric)
    combined_data$Product_value <- apply(score_matrix, 1, function(x) {
      if(all(is.na(x))) return(NA)
      prod(x, na.rm = TRUE)
    })
  }
  
  return(combined_data)
}

generate_combined_maps_all_methods <- function(valid_configs, dataset_mapping, base_grid = grid_test, 
                                               selected_methods = c("geometric_mean"), 
                                               map_type = "Combined",
                                               wea_data_reactive = NULL) {
  
  # Initialize result structure for all methods
  results <- list()
  
  if(length(valid_configs) == 0) {
    # No valid configurations - return empty maps for all methods
    for(method in selected_methods) {
      results[[method]] <- list(
        combined_data = NULL,
        map = leaflet() %>%
          addProviderTiles("Esri.OceanBasemap") %>%
          addControl("Please configure at least one map to generate a combined map.", position = "topright")
      )
    }
    return(results)
  }
  
  # Use base_grid as the starting point for combining data
  base_combined_data <- base_grid
  
  # For each valid configuration, extract the data and join with the base grid
  for(config in valid_configs) {
    layer_name <- config$layer
    score_value <- config$score
    
    # Get dataset info from mapping
    dataset_info <- dataset_mapping[[layer_name]]
    if(is.null(dataset_info)) {
      next  # Skip if layer name doesn't match any mapping
    }
    
    dataset <- dataset_info$data
    score_column <- dataset_info$score_column
    
    # Filter for the selected score value and prepare for joining
    temp_data <- dataset %>%
      filter(.data[[score_column]] == score_value) %>%
      st_drop_geometry() %>%
      select(CellID_2km, !!score_column)
    
    # Convert the score column to numeric (explicit conversion)
    temp_data[[score_column]] <- as.numeric(temp_data[[score_column]])
    
    # Join with the combined data
    base_combined_data <- left_join(base_combined_data, temp_data, by = "CellID_2km")
  }
  
  # Now generate separate results for each selected method
  for(method in selected_methods) {
    # Make a copy of the base combined data for this method
    combined_data <- base_combined_data
    
    # Apply the specific calculation method and set method-specific variables
    if(method == "geometric_mean") {
      combined_data <- calculate_geometric_mean(combined_data)
      score_column <- "Geo_mean"
      popup_prefix <- "Geometric Mean Score:"
      map_title <- paste("Combined", map_type, "Score - Geometric Mean")
    } else if(method == "lowest") {
      combined_data <- calculate_lowest_value(combined_data)
      score_column <- "Lowest_value"
      popup_prefix <- "Lowest Value Score:"
      map_title <- paste("Combined", map_type, "Score - Lowest Value")
    } else if(method == "product") {
      combined_data <- calculate_product(combined_data)
      score_column <- "Product_value"
      popup_prefix <- "Product Score:"
      map_title <- paste("Combined", map_type, "Score - Product")
    } else {
      # Default to geometric mean if unknown method
      combined_data <- calculate_geometric_mean(combined_data)
      score_column <- "Geo_mean"
      popup_prefix <- "Geometric Mean Score:"
      map_title <- paste("Combined", map_type, "Score - Geometric Mean")
    }
    
    # Check if we have valid score data
    if(!is.null(score_column) && score_column %in% names(combined_data) && 
       any(!is.na(combined_data[[score_column]]))) {
      
      # Make sure geometry is set properly for leaflet
      combined_data <- st_transform(combined_data, '+proj=longlat +datum=WGS84')
      
      # Get the bounding box of the data to set initial view
      bbox <- st_bbox(combined_data)
      min_lng <- as.numeric(bbox["xmin"])
      min_lat <- as.numeric(bbox["ymin"])  
      max_lng <- as.numeric(bbox["xmax"])
      max_lat <- as.numeric(bbox["ymax"])
      
      # Get the range of score values for the specific calculation method
      score_values <- combined_data[[score_column]][!is.na(combined_data[[score_column]])]
      min_val <- min(score_values, na.rm = TRUE)
      max_val <- max(score_values, na.rm = TRUE)
      
      # Create popup text column directly in the data using the method-specific prefix
      combined_data$popup_display <- paste(popup_prefix, round(combined_data[[score_column]], 2))
      
      # Check if all values are the same (constant values)
      if(length(score_values) == 0 || min_val == max_val) {
        # When all values are the same, create a single-color palette
        single_color <- viridis::viridis(1, begin = 0.5, end = 0.5)
        
        # Create the map with constant color
        map <- leaflet() %>%
          addProviderTiles("Esri.OceanBasemap",
                           options = providerTileOptions(variant = "Ocean/World_Ocean_Base")) %>%
          addProviderTiles("Esri.OceanBasemap",
                           options = providerTileOptions(variant = "Ocean/World_Ocean_Reference")) %>%
          addPolygons(
            data = combined_data, 
            color = "#33333300",
            weight = 1, 
            fillColor = single_color, 
            fillOpacity = 1,
            popup = ~popup_display,
            group = "Combined Data"
          ) %>%
          addLegend(
            position = "bottomright",
            colors = single_color,
            labels = paste("Score:", round(min_val, 2)),
            title = map_title,  # Use the method-specific title
            opacity = 1
          ) %>%
          fitBounds(lng1 = min_lng, lat1 = min_lat, 
                    lng2 = max_lng, lat2 = max_lat)
      } else {
        # Normal case with varying values - use continuous palette
        pal <- colorNumeric("viridis", 
                            domain = range(score_values, na.rm = TRUE), 
                            na.color = "transparent")
        
        # Add a fillColor column directly to the data to avoid leaflet formula issues
        combined_data$fill_color <- pal(combined_data[[score_column]])
        
        # Create the map
        map <- leaflet() %>%
          addProviderTiles("Esri.OceanBasemap",
                           options = providerTileOptions(variant = "Ocean/World_Ocean_Base")) %>%
          addProviderTiles("Esri.OceanBasemap",
                           options = providerTileOptions(variant = "Ocean/World_Ocean_Reference")) %>%
          addPolygons(
            data = combined_data, 
            color = "#33333300",
            weight = 1, 
            fillColor = ~fill_color, 
            fillOpacity = 1,
            popup = ~popup_display,
            group = "Combined Data"
          ) %>%
          addLegend(
            position = "bottomright",
            pal = pal,
            values = combined_data[[score_column]],
            title = map_title,  # Use the method-specific title
            opacity = 1
          ) %>%
          fitBounds(lng1 = min_lng, lat1 = min_lat, 
                    lng2 = max_lng, lat2 = max_lat)
      }
    } else {
      # No score data available
      map <- leaflet() %>%
        addProviderTiles("Esri.OceanBasemap") %>%
        addControl("No score data available for the selected layers.", position = "topright")
    }
    
    # Add WEA data to the map if available
    if(!is.null(wea_data_reactive)) {
      tryCatch({
        wea_data <- wea_data_reactive()
        if(!is.null(wea_data) && nrow(wea_data) > 0) {
          # Transform WEA data if needed
          if(!st_is_longlat(wea_data)) {
            wea_data <- st_transform(wea_data, 4326)
          }
          wea_data <- st_zm(wea_data)
          
          map <- map %>%
            addPolygons(
              data = wea_data,
              fillColor = "transparent",
              color = "red",
              weight = 3,
              fillOpacity = 0,
              popup = ~paste("Area:", Area_Name),
              group = "WEA Area"
            ) %>%
            addLayersControl(
              overlayGroups = c("Combined Data", "WEA Area"),
              options = layersControlOptions(collapsed = FALSE)
            )
        }
      }, error = function(e) {
        # If WEA data fails, continue without it
        message("Could not add WEA data to combined map: ", e$message)
      })
    } else if(exists("WEA")) {
      # Fall back to global WEA data if no reactive provided
      tryCatch({
        wea_data <- WEA
        if(!is.null(wea_data) && nrow(wea_data) > 0) {
          # Transform WEA data if needed
          if(!st_is_longlat(wea_data)) {
            wea_data <- st_transform(wea_data, 4326)
          }
          wea_data <- st_zm(wea_data)
          
          map <- map %>%
            addPolygons(
              data = wea_data,
              fillColor = "transparent",
              color = "red",
              weight = 3,
              fillOpacity = 0,
              popup = ~paste("Area:", Area_Name),
              group = "WEA Area"
            ) %>%
            addLayersControl(
              overlayGroups = c("Combined Data", "WEA Area"),
              options = layersControlOptions(collapsed = FALSE)
            )
        }
      }, error = function(e) {
        # If global WEA data fails, continue without it
        message("Could not add global WEA data to combined map: ", e$message)
      })
    }
    
    # Store the result for this method
    results[[method]] <- list(
      combined_data = combined_data,
      map = map
    )
  }
  
  return(results)
}

# Keep the original function for backward compatibility
generate_combined_map <- function(valid_configs, dataset_mapping, base_grid = grid_test, 
                                  map_title = "Combined Map", 
                                  calculation_method = "geometric_mean",
                                  wea_data_reactive = NULL) {
  
  # Use the new function to generate a single method
  results <- generate_combined_maps_all_methods(
    valid_configs = valid_configs,
    dataset_mapping = dataset_mapping,
    base_grid = base_grid,
    selected_methods = c(calculation_method),
    wea_data_reactive = wea_data_reactive
  )
  
  # Return the result for the requested method
  return(results[[calculation_method]])
}