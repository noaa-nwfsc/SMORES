create_combined_submodel_map <- function(component_data_list, base_grid = grid_test, aoi_data_reactive = NULL, submodel_type = NULL) {
  tryCatch({
    
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
    combined_data <- calculate_submodel_geometric_mean(combined_data, submodel_type = submodel_type)
    
    # Transform to WGS84
    combined_data <- st_transform(combined_data, 4326)
    
    # Filter valid data
    map_data <- combined_data[!is.na(combined_data$Geo_mean), ]
    
    # Initialize base map
    map <- leaflet() %>%
      addProviderTiles("Esri.OceanBasemap")
    
    if(nrow(map_data) == 0) {
      # No valid data
      map <- map %>%
        addControl("No valid data for combined submodel", position = "topright")
    } else {
      # Get score values for color palette
      score_values <- map_data$Geo_mean
      
      # Ensure score_values is numeric and handle any problematic values
      score_values <- as.numeric(score_values)
      score_values <- score_values[!is.na(score_values) & is.finite(score_values)]
      
      if(length(score_values) == 0) {
        return(list(
          combined_data = combined_data,
          map = leaflet() %>% addProviderTiles("Esri.OceanBasemap") %>%
            addControl("No valid score values for mapping", position = "topright")
        ))
      }
      
      min_val <- min(score_values)
      max_val <- max(score_values)
      
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
        
        # Create color palette
        pal <- colorNumeric(
          palette = "viridis",
          domain = pal_domain,
          na.color = "transparent"
        )
        
        # Ensure Geo_mean values are properly prepared for color mapping
        geo_mean_values <- as.numeric(map_data$Geo_mean)
        
        # Add polygons with explicit color mapping to avoid non-numeric argument errors
        map <- tryCatch({
          map %>%
            addPolygons(
              data = map_data,
              weight = 1,
              color = "#333333",
              fillColor = pal(geo_mean_values), 
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