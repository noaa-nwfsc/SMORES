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
      # Handle case where all values are the same
      if(length(unique(x[!is.na(x)])) <= 1) {
        return(x[!is.na(x)][1])  # Return the single unique value
      }
      
      min(x, na.rm = TRUE)
    })
    
    # Filter out rows where geometric mean is NA or 0
    combined_data <- combined_data[!is.na(combined_data$Lowest_value) & combined_data$Lowest_value > 0, ]
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
      
      # Handle case where all values are the same
      if(length(unique(x[!is.na(x)])) <= 1) {
        return(x[!is.na(x)][1])  # Return the single unique value
      }
      
      prod(x, na.rm = TRUE)
    })
    
    # Filter out rows where geometric mean is NA or 0
    combined_data <- combined_data[!is.na(combined_data$Product_value) & combined_data$Product_value > 0, ]
  }
  
  return(combined_data)
}

generate_combined_maps_all_methods <- function(valid_configs, dataset_mapping, base_grid = grid_test,
                                               selected_methods = c("geometric_mean"),
                                               map_type = "Combined",
                                               aoi_data_reactive = NULL) {
  
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
      
      # Calculate map bounds - prioritize AOI bounds if available
      map_bounds <- NULL
      if(!is.null(aoi_data_reactive)) {
        tryCatch({
          aoi_data_temp <- aoi_data_reactive()
          if(!is.null(aoi_data_temp) && nrow(aoi_data_temp) > 0) {
            # Transform to WGS84 if needed
            if(!st_is_longlat(aoi_data_temp)) {
              aoi_data_temp <- st_transform(aoi_data_temp, 4326)
            }
            aoi_data_temp <- st_zm(aoi_data_temp)
            
            # Use AOI bounds for centering the map
            bbox <- st_bbox(aoi_data_temp)
            map_bounds <- list(
              lng1 = bbox[["xmin"]], lat1 = bbox[["ymin"]],
              lng2 = bbox[["xmax"]], lat2 = bbox[["ymax"]]
            )
          }
        }, error = function(e) {
          # If AOI data fails, continue without bounds
          message("Could not get AOI bounds: ", e$message)
        })
      } else if(exists("AOI")) {
        # Fall back to global AOI data if no reactive provided
        tryCatch({
          aoi_data_temp <- AOI
          if(!is.null(aoi_data_temp) && nrow(aoi_data_temp) > 0) {
            # Transform to WGS84 if needed
            if(!st_is_longlat(aoi_data_temp)) {
              aoi_data_temp <- st_transform(aoi_data_temp, 4326)
            }
            aoi_data_temp <- st_zm(aoi_data_temp)
            
            # Use AOI bounds for centering the map
            bbox <- st_bbox(aoi_data_temp)
            map_bounds <- list(
              lng1 = bbox[["xmin"]], lat1 = bbox[["ymin"]],
              lng2 = bbox[["xmax"]], lat2 = bbox[["ymax"]]
            )
          }
        }, error = function(e) {
          # If global AOI data fails, continue without bounds
          message("Could not get global AOI bounds: ", e$message)
        })
      }
      
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
          )
        
        # set map view based on bounds or default
        if(!is.null(map_bounds)) {
          map <- map %>%
            fitBounds(
              lng1 = map_bounds$lng1, lat1 = map_bounds$lat1,
              lng2 = map_bounds$lng2, lat2 = map_bounds$lat2,
              options = list(padding = c(20, 20))
            )
        } else {
          # Use data bounds as fallback
          bbox <- st_bbox(combined_data)
          map <- map %>%
            fitBounds(lng1 = bbox[["xmin"]], lat1 = bbox[["ymin"]],
                      lng2 = bbox[["xmax"]], lat2 = bbox[["ymax"]])
        }
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
            title = map_title,
            opacity = 1
          )
        
        # Set map view based on bounds or default
        if(!is.null(map_bounds)) {
          map <- map %>%
            fitBounds(
              lng1 = map_bounds$lng1, lat1 = map_bounds$lat1,
              lng2 = map_bounds$lng2, lat2 = map_bounds$lat2,
              options = list(padding = c(20, 20))
            )
        } else {
          # Use data bounds as fallback
          bbox <- st_bbox(combined_data)
          map <- map %>%
            fitBounds(lng1 = bbox[["xmin"]], lat1 = bbox[["ymin"]],
                      lng2 = bbox[["xmax"]], lat2 = bbox[["ymax"]])
        }
      }
      
      # Add WEA data to the map if available
      if(!is.null(aoi_data_reactive)) {
        tryCatch({
          aoi_data <- aoi_data_reactive()
          if(!is.null(aoi_data) && nrow(aoi_data) > 0) {
            # Transform WEA data if needed
            if(!st_is_longlat(aoi_data)) {
              aoi_data <- st_transform(aoi_data, 4326)
            }
            aoi_data <- st_zm(aoi_data)
            
            map <- map %>%
              addPolygons(
                data = aoi_data,
                fillColor = "transparent",
                color = "red",
                weight = 3,
                fillOpacity = 0,
                popup = ~paste("Area:", Area_Name),
                group = "AOI Area"
              ) %>%
              addLayersControl(
                overlayGroups = c("Combined Data", "AOI Area"),
                options = layersControlOptions(collapsed = FALSE)
              )
          }
        }, error = function(e) {
          # If WEA data fails, continue without it
          message("Could not add AOI data to combined map: ", e$message)
        })
      } else if(exists("AOI")) {
        # Fall back to global AOI data if no reactive provided
        tryCatch({
          aoi_data <- AOI
          if(!is.null(aoi_data) && nrow(aoi_data) > 0) {
            # Transform WEA data if needed
            if(!st_is_longlat(aoi_data)) {
              aoi_data <- st_transform(aoi_data, 4326)
            }
            aoi_data <- st_zm(aoi_data)
            
            map <- map %>%
              addPolygons(
                data = aoi_data,
                fillColor = "transparent",
                color = "red",
                weight = 3,
                fillOpacity = 0,
                popup = ~paste("Area:", Area_Name),
                group = "AOI Area"
              ) %>%
              addLayersControl(
                overlayGroups = c("Combined Data", "AOI Area"),
                options = layersControlOptions(collapsed = FALSE)
              )
          }
        }, error = function(e) {
          # If global WEA data fails, continue without it
          message("Could not add global AOI data to combined map: ", e$message)
        })
      }
      
      # Store the result for this method
      results[[method]] <- list(
        combined_data = combined_data,
        map = map
      )
    } else {
      # No score data available
      map <- leaflet() %>%
        addProviderTiles("Esri.OceanBasemap") %>%
        addControl("No score data available for the selected layers.", position = "topright")
      
      # Store the empty map for this method
      results[[method]] <- list(
        combined_data = NULL,
        map = map
      )
    }
  } 
  
  return(results) 
}
