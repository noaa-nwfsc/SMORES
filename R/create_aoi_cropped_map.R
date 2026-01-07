# create cropped map with consistent color scale
create_aoi_cropped_map <- function(combined_data, aoi_data_reactive = NULL, map_title = "AOI-Cropped Combined Map", full_data_range = NULL, aoi_bounds = NULL) {
  tryCatch({
    
    # Get AOI data
    aoi_data <- NULL
    if(!is.null(aoi_data_reactive)) {
      aoi_data <- aoi_data_reactive()
    }
    
    # Calculate map bounds based on AOI if available
    map_bounds <- aoi_bounds
    
    # Check if "All Areas" is selected (assuming this means no cropping needed)
    if(nrow(aoi_data) > 1) {
      return(leaflet() %>%
               addProviderTiles("Esri.OceanBasemap") %>%
               setView(lng = -124, lat = 38, zoom = 7) %>%
               addControl("Select a specific WEA area to view cropped data", position = "topright"))
    }
    
    # Remove Z & M dimensions
    combined_data <- st_zm(combined_data)
    aoi_data <- st_zm(aoi_data)
    
    # Crop the combined data to the WEA boundary using intersection
    cropped_data <- st_intersection(combined_data, aoi_data)
    
    # Check if we have any data after cropping
    if(nrow(cropped_data) == 0) {
      return(leaflet() %>%
               addProviderTiles("Esri.OceanBasemap") %>%
               setView(lng = -124, lat = 38, zoom = 7) %>%
               addControl("No data intersects with selected AOI", position = "topright"))
    }
    
    # Filter for valid data
    map_data <- cropped_data[!is.na(cropped_data$Geo_mean), ]
    
    if(nrow(map_data) == 0) {
      return(leaflet() %>%
               addProviderTiles("Esri.OceanBasemap") %>%
               setView(lng = -124, lat = 38, zoom = 7) %>%
               addControl("No valid data in selected AOI", position = "topright"))
    }
    
    # Use full data range for consistent coloring, or calculate from cropped data if not provided
    if(!is.null(full_data_range)) {
      min_val <- full_data_range$min
      max_val <- full_data_range$max
    } else {
      # Fall back to calculating from the full combined_data if full_data_range not provided
      full_score_values <- combined_data$Geo_mean[!is.na(combined_data$Geo_mean)]
      if(length(full_score_values) > 0) {
        min_val <- min(full_score_values, na.rm = TRUE)
        max_val <- max(full_score_values, na.rm = TRUE)
      } else {
        # Last resort: use cropped data range
        score_values <- map_data$Geo_mean
        min_val <- min(score_values, na.rm = TRUE)
        max_val <- max(score_values, na.rm = TRUE)
      }
    }
    
    # Create popup text
    map_data$popup_display <- paste("Combined Score:", round(map_data$Geo_mean, 2))
    
 
    
    # Initialize the map with view centered on cropped area
    map <- leaflet(map_data) %>%
      addProviderTiles("Esri.OceanBasemap") 
    
    # Get the actual cropped data values to check for uniformity
    cropped_score_values <- map_data$Geo_mean
    cropped_min <- min(cropped_score_values, na.rm = TRUE)
    cropped_max <- max(cropped_score_values, na.rm = TRUE)
    
    # Use full data range for palette domain, but check cropped data for uniformity
    if(!is.null(full_data_range)) {
      min_val <- full_data_range$min
      max_val <- full_data_range$max
    } else {
      # Fall back to calculating from the full combined_data
      full_score_values <- combined_data$Geo_mean[!is.na(combined_data$Geo_mean)]
      min_val <- min(full_score_values, na.rm = TRUE)
      max_val <- max(full_score_values, na.rm = TRUE)
    }
    
    # Check if CROPPED values are all the same (not full range)
    if(abs(cropped_min - cropped_max) < 1e-10) {
      # All CROPPED values are the same - use single color logic
      pal <- colorNumeric(
        palette = "viridis",
        domain = c(min_val, max_val),  # Still use full range for consistent coloring
        na.color = "transparent"
      )
      
      single_color <- pal(cropped_min)  # Color for the single cropped value
      
      map <- map %>%
        addPolygons(
          weight = 1,
          color = "#333333",
          fillColor = single_color,  # Use the palette-determined color
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
        ) %>%
        addLegend(
          position = "bottomright",
          colors = single_color,
          labels = format(min_val, digits = 3),
          title = paste(map_title, "Score"),
          opacity = 1
        )
    } else {
      # Create color palette using the same domain as the full map
      range_buffer <- (max_val - min_val) * 0.01
      pal_domain <- c(min_val - range_buffer, max_val + range_buffer)
      
      pal <- colorNumeric(
        palette = "viridis",
        domain = pal_domain,
        na.color = "transparent"
      )
      
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
          title = paste(map_title, "Score"),
          opacity = 1
        )
    }
    
    # Add WEA boundary outline
    map <- map %>%
      addPolygons(
        data = aoi_data,
        fillColor = "transparent",
        color = "red",
        weight = 3,
        fillOpacity = 0,
        group= "AOI Boundary",
        options = pathOptions(
          interactive = FALSE
        )
      ) %>%
      addLayersControl(
        overlayGroups = c("Combined Data", "AOI Boundary"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })

# Set map view bounds
if(!is.null(map_bounds)) {
  tryCatch({
    map <- map %>%
      fitBounds(
        lng1 = map_bounds$lng1, lat1 = map_bounds$lat1,
        lng2 = map_bounds$lng2, lat2 = map_bounds$lat2,
        options = list(padding = c(10, 10))
      )
  }, error = function(e) {
    # Error setting bounds
  })
}
    
    return(map)
    
}