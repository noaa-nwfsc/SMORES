# Function to create normalized (0-1 scale) cropped map with its own color scheme
create_aoi_cropped_normalized_map <- function(combined_data, aoi_data_reactive = NULL, map_title = "AOI-Cropped Normalized Map") {
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
               addControl("Select a WEA area to view normalized cropped data", position = "center"))
    }
    
    # Check if "All Areas" is selected (assuming this means no cropping needed)
    if(nrow(aoi_data) > 1) {
      return(leaflet() %>%
               addProviderTiles("Esri.OceanBasemap") %>%
               setView(lng = -124, lat = 38, zoom = 7) %>%
               addControl("Select a specific WEA area to view normalized cropped data", position = "topright"))
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
    
    # Get the min and max values from the CROPPED data only for normalization
    score_values <- map_data$Geo_mean
    cropped_min_val <- min(score_values, na.rm = TRUE)
    cropped_max_val <- max(score_values, na.rm = TRUE)
    
    # Normalize the cropped data to 0-1 scale using only the cropped data range
    if(abs(cropped_max_val - cropped_min_val) < 1e-10) {
      # If all values in the cropped dataset are the same, set normalized values to 0.5
      map_data$Normalized_Score <- 0.5
    } else {
      # Normalize using cropped data range: (value - cropped_min) / (cropped_max - cropped_min)
      map_data$Normalized_Score <- (map_data$Geo_mean - cropped_min_val) / (cropped_max_val - cropped_min_val)
    }
    
    # Create popup text with both original and normalized values
    map_data$popup_display <- paste0(
      "Combined Normalized Score (0-1): ", round(map_data$Normalized_Score, 2)
    )
    
    # Get bounds for the cropped area to set appropriate view
    bbox <- st_bbox(map_data)
    center_lng <- mean(c(bbox[1], bbox[3]))
    center_lat <- mean(c(bbox[2], bbox[4]))
    
    # Initialize the map with view centered on cropped area
    map <- leaflet(map_data) %>%
      addProviderTiles("Esri.OceanBasemap") %>%
      setView(lng = center_lng, lat = center_lat, zoom = 10)
    
    # Get normalized score values for color palette (always 0-1 range)
    normalized_scores <- map_data$Normalized_Score
    norm_min_val <- min(normalized_scores, na.rm = TRUE)
    norm_max_val <- max(normalized_scores, na.rm = TRUE)
    
    # Handle color palette for normalized data - using a different color scheme
    if(abs(norm_min_val - norm_max_val) < 1e-10) {
      # All normalized values are the same - use single color (different from main maps)
      map <- map %>%
        addPolygons(
          weight = 1,
          color = "#333333",
          fillColor = "#d62728",  # Red color for single value
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
          paste("All areas have the same normalized score:", format(norm_min_val, digits = 3)),
          position = "bottomright"
        )
    } else {
      # Create color palette for normalized values using a different color scheme
      # Using "plasma" instead of "viridis" to distinguish from other maps
      pal <- colorNumeric(
        palette = "plasma",  # Different color scheme: purple-pink-yellow
        domain = c(0, 1),  # Always use 0-1 domain for normalized data
        na.color = "transparent"
      )
      
      # Add polygons with color mapping using normalized scores
      map <- map %>%
        addPolygons(
          weight = 1,
          color = "#333333",
          fillColor = ~pal(Normalized_Score),
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
          values = ~Normalized_Score,
          title = "Normalized Score<br>(0-1 Scale)",
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
        group = "AOI Boundary",
        options = pathOptions(
          interactive = FALSE
        )
      ) %>%
      addLayersControl(
        overlayGroups = c("Normalized Data", "AOI Boundary"),
        options = layersControlOptions(collapsed = FALSE)
      )
    
    return(map)
    
  }, error = function(e) {
    cat("ERROR in create_aoi_cropped_normalized_map:", e$message, "\n")
    
    # Return error map
    return(leaflet() %>%
             addProviderTiles("Esri.OceanBasemap") %>%
             setView(lng = -124, lat = 38, zoom = 7) %>%
             addControl(paste("Error creating normalized cropped map:", e$message), position = "topright"))
  })
}