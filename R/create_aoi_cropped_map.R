# create cropped map with consistent color scale
create_aoi_cropped_map <- function(combined_data, aoi_data_reactive = NULL, map_title = "AOI-Cropped Combined Map", full_data_range = NULL) {
  tryCatch({
    
    # Get AOI data
    aoi_data <- NULL
    if(!is.null(aoi_data_reactive)) {
      aoi_data <- aoi_data_reactive()
    }
    
    # If no specific AOI selected or aoi_data is empty, return message map
    if(is.null(aoi_data) || nrow(aoi_data) == 0) {
      return(leaflet() %>%
               addProviderTiles("Esri.OceanBasemap") %>%
               setView(lng = -124, lat = 38, zoom = 7) %>%
               addControl("Select a WEA area to view cropped data", position = "center"))
    }
    
    # Check if "All Areas" is selected (assuming this means no cropping needed)
    if(nrow(aoi_data) > 1) {
      return(leaflet() %>%
               addProviderTiles("Esri.OceanBasemap") %>%
               setView(lng = -124, lat = 38, zoom = 7) %>%
               addControl("Select a specific WEA area to view cropped data", position = "center"))
    }
    
    # Ensure both datasets are in the same CRS
    if(!st_is_longlat(combined_data)) {
      combined_data <- st_transform(combined_data, 4326)
    }
    if(!st_is_longlat(aoi_data)) {
      aoi_data <- st_transform(aoi_data, 4326)
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
               addControl("No data intersects with selected AOI", position = "center"))
    }
    
    # Filter for valid data
    map_data <- cropped_data[!is.na(cropped_data$Geo_mean), ]
    
    if(nrow(map_data) == 0) {
      return(leaflet() %>%
               addProviderTiles("Esri.OceanBasemap") %>%
               setView(lng = -124, lat = 38, zoom = 7) %>%
               addControl("No valid data in selected AOI", position = "center"))
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
    
    # Get bounds for the cropped area to set appropriate view
    bbox <- st_bbox(map_data)
    center_lng <- mean(c(bbox[1], bbox[3]))
    center_lat <- mean(c(bbox[2], bbox[4]))
    
    # Initialize the map with view centered on cropped area
    map <- leaflet(map_data) %>%
      addProviderTiles("Esri.OceanBasemap") %>%
      setView(lng = center_lng, lat = center_lat, zoom = 10)
    
    # Handle color palette - use the same logic as the full map
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
    
    return(map)
    
  }, error = function(e) {
    cat("ERROR in create_aoi_cropped_map:", e$message, "\n")
    
    # Return error map
    return(leaflet() %>%
             addProviderTiles("Esri.OceanBasemap") %>%
             setView(lng = -124, lat = 38, zoom = 7) %>%
             addControl(paste("Error creating cropped map:", e$message), position = "topright"))
  })
}