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
    cat("WARNING: No score columns found. Looking for columns with pattern '^Score[._]'\\n")
    cat("Available columns:", paste(names(combined_data), collapse = ", "), "\\n")
  }
  
  return(combined_data)
}

create_combined_submodel <- function(component_data_list, base_grid = grid_test) {
  tryCatch({
    cat("DEBUG: create_combined_submodel called with", length(component_data_list), "components\n")
    
    # Start with base grid
    combined_data <- base_grid
    cat("DEBUG: Base grid has", nrow(combined_data), "rows\n")
    
    # Extract the calculated values from each component
    for(component_name in names(component_data_list)) {
      cat("DEBUG: Processing component:", component_name, "\n")
      
      component_data <- component_data_list[[component_name]]
      
      if(is.null(component_data)) {
        cat("WARNING: Component data is null for:", component_name, "\n")
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
    
    cat("DEBUG: Score range:", min_val, "to", max_val, "\n")
    
    # Create popup text
    map_data$popup_display <- paste("Combined Natural Resources Score:", 
                                    format(map_data$Geo_mean, digits = 3))
    
    # Initialize the map
    map <- leaflet(map_data) %>%
      addProviderTiles("Esri.OceanBasemap") %>%
      setView(lng = -124, lat = 38, zoom = 7)
    
    # Handle color palette - check if we have variation in values
    if(abs(min_val - max_val) < 1e-10) {
      cat("DEBUG: All values identical, using single color\n")
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
      cat("DEBUG: Creating color palette with variation\n")
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
      cat("DEBUG: Color palette test - min color:", test_colors[1], "max color:", test_colors[2], "\n")
      
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
    
    cat("DEBUG: Map created successfully\n")
    
    return(list(
      combined_data = combined_data,
      map = map
    ))
    
  }, error = function(e) {
    cat("ERROR in create_combined_submodel:", e$message, "\n")
    
    # Return a basic error map
    error_map <- leaflet() %>%
      addProviderTiles("Esri.OceanBasemap") %>%
      setView(lng = -124, lat = 38, zoom = 7) %>%
      addControl(paste("Error:", e$message), position = "topright")
    
    return(list(
      combined_data = NULL,
      map = error_map
    ))
  })
}