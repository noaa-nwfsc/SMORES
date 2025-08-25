create_combined_submodel_map <- function(component_data_list, base_grid = grid_test, aoi_data_reactive = NULL, submodel_type = NULL) {
  tryCatch({
    
    cat("DEBUG: Starting create_combined_submodel_map\n")
    cat("DEBUG: Number of components:", length(component_data_list), "\n")
    cat("DEBUG: Component names:", paste(names(component_data_list), collapse = ", "), "\n")
    cat("DEBUG: Submodel type:", ifelse(is.null(submodel_type), "NULL", submodel_type), "\n")
    
    # Get AOI data - handle both reactive and direct data
    aoi_data <- NULL
    if(!is.null(aoi_data_reactive)) {
      if(is.function(aoi_data_reactive)) {
        aoi_data <- aoi_data_reactive()
      } else {
        aoi_data <- aoi_data_reactive
      }
    } else if(exists("AOI")) {
      aoi_data <- AOI
    }
    
    # Transform AOI data to WGS84 if available and needed
    if(!is.null(aoi_data) && nrow(aoi_data) > 0) {
      if(!st_is_longlat(aoi_data)) {
        aoi_data <- st_transform(aoi_data, 4326)
      }
      aoi_data <- st_zm(aoi_data)
    }
    
    # Calculate map bounds based on AOI if available
    map_bounds <- NULL
    if(!is.null(aoi_data) && nrow(aoi_data) > 0) {
      bbox <- st_bbox(aoi_data)
      map_bounds <- list(
        lng1 = bbox[["xmin"]], lat1 = bbox[["ymin"]],
        lng2 = bbox[["xmax"]], lat2 = bbox[["ymax"]]
      )
    }
    
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
    cat("DEBUG: About to call calculate_submodel_geometric_mean\n")
    cat("DEBUG: Combined data dimensions before geometric mean:", dim(combined_data), "\n")
    cat("DEBUG: Combined data columns before geometric mean:", paste(names(combined_data), collapse = ", "), "\n")
    
    # Check for any non-numeric columns that might cause issues
    non_numeric_cols <- sapply(combined_data, function(x) !is.numeric(x) && !inherits(x, c("sfc", "sf")))
    if(any(non_numeric_cols)) {
      cat("DEBUG: Non-numeric columns found:", paste(names(combined_data)[non_numeric_cols], collapse = ", "), "\n")
    }
    
    combined_data <- calculate_submodel_geometric_mean(combined_data, submodel_type = submodel_type)
    
    cat("DEBUG: Returned from calculate_submodel_geometric_mean\n")
    cat("DEBUG: Combined data dimensions after geometric mean:", dim(combined_data), "\n")
    
    # Transform to WGS84
    cat("DEBUG: About to transform to WGS84\n")
    combined_data <- st_transform(combined_data, 4326)
    cat("DEBUG: Successfully transformed to WGS84\n")
    
    # Filter valid data
    cat("DEBUG: About to filter valid data\n")
    cat("DEBUG: Geo_mean column exists:", "Geo_mean" %in% names(combined_data), "\n")
    if("Geo_mean" %in% names(combined_data)) {
      cat("DEBUG: Geo_mean NAs before filtering:", sum(is.na(combined_data$Geo_mean)), "\n")
      cat("DEBUG: Geo_mean range before filtering:", range(combined_data$Geo_mean, na.rm = TRUE), "\n")
    }
    
    map_data <- combined_data[!is.na(combined_data$Geo_mean), ]
    cat("DEBUG: Map data dimensions after filtering:", dim(map_data), "\n")
    
    # Initialize base map
    map <- leaflet() %>%
      addProviderTiles("Esri.OceanBasemap")
    
    if(nrow(map_data) == 0) {
      # No valid data
      map <- map %>%
        addControl("No valid data for combined submodel", position = "topright")
    } else {
      # Get score values for color palette
      cat("DEBUG: Getting score values for color palette\n")
      score_values <- map_data$Geo_mean
      cat("DEBUG: Score values class:", class(score_values), "\n")
      cat("DEBUG: Score values length:", length(score_values), "\n")
      cat("DEBUG: Score values sample:", paste(head(score_values, 10), collapse = ", "), "\n")
      
      # Ensure score_values is numeric and handle any problematic values
      score_values <- as.numeric(score_values)
      score_values <- score_values[!is.na(score_values) & is.finite(score_values)]
      
      cat("DEBUG: After cleaning - score values length:", length(score_values), "\n")
      cat("DEBUG: After cleaning - sample values:", paste(head(score_values, 10), collapse = ", "), "\n")
      
      if(length(score_values) == 0) {
        cat("ERROR: No valid numeric score values found\n")
        return(list(
          combined_data = combined_data,
          map = leaflet() %>% addProviderTiles("Esri.OceanBasemap") %>%
            addControl("No valid score values for mapping", position = "topright")
        ))
      }
      
      min_val <- min(score_values)
      max_val <- max(score_values)
      cat("DEBUG: Min value:", min_val, "\n")
      cat("DEBUG: Max value:", max_val, "\n")
      
      # Store the full data range for consistent coloring
      full_data_range <- list(min = min_val, max = max_val)
      
      # Create popup text with proper formatting for small values
      map_data$popup_display <- paste0("Combined Submodel Score:", 
                                       ifelse(map_data$Geo_mean < 0.01,
                                              format(map_data$Geo_mean, scientific = FALSE, digits = 3),
                                              round(map_data$Geo_mean, 3)))
      
      # Handle color palette - check if we have variation in values
      if(abs(min_val - max_val) < 1e-10) {
        # All values are the same - use single color
        map <- map %>%
          addPolygons(
            data = map_data,
            weight = 1,
            color = "#333333",
            fillColor = "#440154",  # Dark purple
            fillOpacity = 0.7,
            popup = ~popup_display,
            group = "Combined Submodel",
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
        
        cat("DEBUG: About to create colorNumeric palette\n")
        cat("DEBUG: pal_domain:", paste(pal_domain, collapse = ", "), "\n")
        cat("DEBUG: min_val:", min_val, "max_val:", max_val, "range_buffer:", range_buffer, "\n")
        cat("DEBUG: Sample of map_data$Geo_mean for palette:", paste(head(map_data$Geo_mean, 10), collapse = ", "), "\n")
        
        # Create color palette
        cat("DEBUG: Creating colorNumeric palette\n")
        pal <- colorNumeric(
          palette = "viridis",
          domain = pal_domain,
          na.color = "transparent"
        )
        cat("DEBUG: colorNumeric palette created successfully\n")
        
        # Test the palette function with sample values
        cat("DEBUG: Testing palette function\n")
        test_colors <- tryCatch({
          pal(head(map_data$Geo_mean, 5))
        }, error = function(e) {
          cat("ERROR in palette function:", e$message, "\n")
          return(NULL)
        })
        cat("DEBUG: Test colors:", paste(test_colors, collapse = ", "), "\n")
        
        # Add polygons with color mapping
        cat("DEBUG: About to add polygons with color mapping\n")
        
        # Ensure Geo_mean values are properly prepared for color mapping
        geo_mean_values <- as.numeric(map_data$Geo_mean)
        cat("DEBUG: Geo_mean values for polygon colors - class:", class(geo_mean_values), "\n")
        cat("DEBUG: Geo_mean values for polygon colors - range:", range(geo_mean_values, na.rm = TRUE), "\n")
        
        # Add polygons with explicit color mapping to avoid non-numeric argument errors
        map <- tryCatch({
          map %>%
            addPolygons(
              data = map_data,
              weight = 1,
              color = "#333333",
              fillColor = pal(geo_mean_values),  # Apply palette to explicit numeric values
              fillOpacity = 0.7,
              popup = ~popup_display,
              group = "Combined Submodel",
              highlightOptions = highlightOptions(
                weight = 2,
                color = "#666",
                fillOpacity = 0.9,
                bringToFront = TRUE
              )
            )
        }, error = function(e) {
          cat("ERROR in addPolygons:", e$message, "\n")
          # Fallback to single color if color mapping fails
          map %>%
            addPolygons(
              data = map_data,
              weight = 1,
              color = "#333333",
              fillColor = "#440154",  # Single purple color
              fillOpacity = 0.7,
              popup = ~popup_display,
              group = "Combined Submodel",
              highlightOptions = highlightOptions(
                weight = 2,
                color = "#666",
                fillOpacity = 0.9,
                bringToFront = TRUE
              )
            ) %>%
            addControl("Color mapping failed - using single color", position = "topright")
        })
        
        cat("DEBUG: Successfully added polygons\n")
          # ) %>%
          # addLegend(
          #   position = "bottomright",
          #   pal = pal,
          #   values = map_data$Geo_mean,
          #   title = "Combined Submodel Score",
          #   opacity = 1,
          #   labFormat = labelFormat(
          #     digits = 3,
          #     transform = function(x) {
          #       ifelse(x < 0.01, 
          #              format(x, scientific = FALSE, digits = 3),
          #              round(x, 3))
          #     }
          #   )
          # )
      }
      
      # If no AOI bounds available, calculate from data
      if(is.null(map_bounds)) {
        bbox <- st_bbox(map_data)
        map_bounds <- list(
          lng1 = bbox[["xmin"]], lat1 = bbox[["ymin"]],
          lng2 = bbox[["xmax"]], lat2 = bbox[["ymax"]]
        )
      }
    }
    
    # Add AOI boundary outline after main data so it appears on top
    if(!is.null(aoi_data) && nrow(aoi_data) > 0) {
      map <- map %>%
        addPolygons(
          data = aoi_data,
          fillColor = "transparent",
          color = "red",
          weight = 3,
          fillOpacity = 0,
          group = "AOI Boundary",
          options = pathOptions(
            interactive = FALSE
          )
        ) %>%
        addLayersControl(
          overlayGroups = c("Combined Submodel", "AOI Boundary"),
          options = layersControlOptions(collapsed = FALSE)
        )
    }
    
    # Set map view bounds once at the end
    if(!is.null(map_bounds)) {
      map <- map %>%
        fitBounds(
          lng1 = map_bounds$lng1, lat1 = map_bounds$lat1,
          lng2 = map_bounds$lng2, lat2 = map_bounds$lat2,
          options = list(padding = c(20, 20))
        )
    }
    
    return(list(
      combined_data = combined_data,
      map = map,
      full_data_range = if(exists("full_data_range")) full_data_range else NULL
    ))
    
  }, error = function(e) {
    cat("ERROR in create_combined_submodel:", e$message, "\n")
    
    # Return error map
    error_map <- leaflet() %>%
      addProviderTiles("Esri.OceanBasemap") %>%
      addControl(paste("Error generating combined submodel:", e$message), position = "topright")
    
    return(list(
      combined_data = if(exists("combined_data")) combined_data else NULL,
      map = error_map
    ))
  })
}