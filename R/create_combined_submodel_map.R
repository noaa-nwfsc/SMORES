calculate_submodel_geometric_mean <- function(combined_data) {
  # Find all score columns - updated pattern to match both dots and underscores
  score_cols <- names(combined_data)[grep("^Score[._]", names(combined_data))]
  
  if(length(score_cols) > 0) {
    # Convert to numeric and calculate row-wise geometric mean
    score_matrix <- as.matrix(combined_data[score_cols])
    score_matrix <- apply(score_matrix, 2, as.numeric)
    
    combined_data$Geo_mean <- apply(score_matrix, 1, function(x) {
      if(all(is.na(x))) return(NA)
      # Filter out zero values for geometric mean calculation
      valid_values <- x[!is.na(x) & x > 0]
      if(length(valid_values) == 0) return(NA)
      
      # Handle case where all values are the same
      if(length(unique(valid_values)) <= 1) {
        return(valid_values[1])  # Return the single unique value
      }
      exp(mean(log(valid_values), na.rm = TRUE))
    })
    
    # Filter out rows where geometric mean is NA or 0
    combined_data <- combined_data[!is.na(combined_data$Geo_mean) & combined_data$Geo_mean > 0, ]
  } else {
    cat("WARNING: No score columns found. Looking for columns with pattern '^Score[._]'\n")
    cat("Available columns:", paste(names(combined_data), collapse = ", "), "\n")
  }
  
  return(combined_data)
}

create_combined_submodel_map <- function(component_data_list, base_grid = grid_test, aoi_data_reactive = NULL) {
  tryCatch({
    
    # Start with base grid
    combined_data <- base_grid
    
    # Extract the calculated values from each component
    for(component_name in names(component_data_list)) {
      
      component_data <- component_data_list[[component_name]]
      
      if(is.null(component_data)) {
        next
      }
      
      # Find the calculated score column
      score_cols <- names(component_data)[names(component_data) %in% c("Geo_mean", "Lowest_value", "Product_value")]
      
      if(length(score_cols) > 0) {
        score_col <- score_cols[1]
        new_col_name <- paste0("Score_", component_name)
        
        # Extract data and join
        temp_data <- component_data %>%
          st_drop_geometry() %>%
          select(CellID_2km, !!score_col) %>%
          rename(!!new_col_name := !!score_col)
        
        combined_data <- left_join(combined_data, temp_data, by = "CellID_2km")
      }
    }
    
    # Calculate geometric mean
    combined_data <- calculate_submodel_geometric_mean(combined_data)
    
    # Transform to WGS84
    combined_data <- st_transform(combined_data, 4326)
    
    # Filter valid data
    map_data <- combined_data[!is.na(combined_data$Geo_mean), ]
    
    if(nrow(map_data) == 0) {
      # No valid data
      map <- leaflet() %>%
        addProviderTiles("Esri.OceanBasemap") %>%
        setView(lng = -124, lat = 38, zoom = 7) %>%
        addControl("No valid data for combined submodel", position = "topright")
      
      return(list(
        combined_data = combined_data,
        map = map
      ))
    }
    
    # Get score values for color palette
    score_values <- map_data$Geo_mean
    min_val <- min(score_values, na.rm = TRUE)
    max_val <- max(score_values, na.rm = TRUE)
    
    # Store the full data range for consistent coloring
    full_data_range <- list(min = min_val, max = max_val)
    
    # Create popup text
    map_data$popup_display <- paste("Combined Natural Resources Score:", 
                                    round(map_data$Geo_mean, 2))
    
    # Calculate map bounds - prioritize AOI bounds if available
    map_bounds <- NULL
    # Initialize aoi_data to NULL
    aoi_data <- NULL
    if(!is.null(aoi_data_reactive)) {
      aoi_data <- aoi_data_reactive()
    }
    
    # Set map bounds if we have AOI data
    if(!is.null(aoi_data) && nrow(aoi_data) > 0) {
      tryCatch({
        # Transform to WGS84 if needed
        if(!st_is_longlat(aoi_data)) {
          aoi_data <- st_transform(aoi_data, 4326)
        }
        aoi_data <- st_zm(aoi_data)
        
        # Use AOI bounds for centering the map
        bbox <- st_bbox(aoi_data)
        map_bounds <- list(
          lng1 = bbox[["xmin"]], lat1 = bbox[["ymin"]],
          lng2 = bbox[["xmax"]], lat2 = bbox[["ymax"]]
        )
      }, error = function(e) {
        message("Could not get AOI bounds: ", e$message)
      })
    }
    
    # Initialize the map
    map <- leaflet(map_data) %>%
      addProviderTiles("Esri.OceanBasemap")
    
    # Set map view based on bounds or default
    if(!is.null(map_bounds)) {
      map <- map %>%
        fitBounds(
          lng1 = map_bounds$lng1, lat1 = map_bounds$lat1,
          lng2 = map_bounds$lng2, lat2 = map_bounds$lat2,
          options = list(padding = c(20, 20))  # Add some padding around the bounds
        )
    } else {
      map <- map %>%
        setView(lng = -124, lat = 38, zoom = 7)  # Default view
    }
    
    # Handle color palette - check if we have variation in values
    if(abs(min_val - max_val) < 1e-10) {
      # All values are the same - use single color
      map <- map %>%
        addPolygons(
          weight = 1,
          color = "#333333",
          fillColor = "#440154",  # Dark purple
          fillOpacity = 0.7,
          popup = ~popup_display,
          highlightOptions = highlightOptions(
            weight = 2,
            color = "#666",
            fillOpacity = 0.9,
            bringToFront = TRUE
          )
        ) %>%
        addControl(
          paste("All areas have the same score:", format(min_val, digits = 3)),
          position = "bottomright"
        )
    } else {
      # We have variation - create proper color palette
      
      # Expand the range slightly to ensure all values are included
      range_buffer <- (max_val - min_val) * 0.01
      pal_domain <- c(min_val - range_buffer, max_val + range_buffer)
      
      # Create color palette
      pal <- colorNumeric(
        palette = "viridis",
        domain = pal_domain,
        na.color = "transparent"
      )
      
      # Test the palette with actual values
      test_colors <- pal(c(min_val, max_val))
      
      
      # Add polygons with color mapping
      map <- map %>%
        addPolygons(
          weight = 1,
          color = "#333333",
          fillColor = ~pal(Geo_mean),
          fillOpacity = 0.7,
          popup = ~popup_display,
          highlightOptions = highlightOptions(
            weight = 2,
            color = "#666",
            fillOpacity = 0.9,
            bringToFront = TRUE
          )
        ) %>%
        addLegend(
          position = "bottomright",
          pal = pal,
          values = ~Geo_mean,
          title = "Combined Natural Resources Score",
          opacity = 1
        )
    }
    
    # Add AOI data to the map if available
    if(!is.null(aoi_data) && nrow(aoi_data) > 0) {
      map <- map %>%
        addPolygons(
          data = aoi_data,
          fillColor = "transparent",
          color = "red",
          weight = 3,
          fillOpacity = 0,
          group = "AOI Boundaries",
          options = pathOptions(
            interactive = FALSE  # Disable popup for AOI polygon
          ) 
        )
    }
    
    # Add layers control
    if(!is.null(aoi_data) && nrow(aoi_data) > 0) {
      map <- map %>%
        addLayersControl(
          overlayGroups = c("Combined Data", "AOI Area"),
          options = layersControlOptions(collapsed = FALSE)
        )
    }
    
    return(list(
      combined_data = combined_data,
      map = map,
      full_data_range = full_data_range
    ))
    
  }, error = function(e) {
    cat("ERROR in create_combined_submodel:", e$message, "\n")
    
    # Return error map
    error_map <- leaflet() %>%
      addProviderTiles("Esri.OceanBasemap") %>%
      setView(lng = -124, lat = 38, zoom = 7) %>%
      addControl(paste("Error generating combined submodel:", e$message), position = "topright")
    
    return(list(
      combined_data = combined_data,
      map = error_map
    ))
  })
}