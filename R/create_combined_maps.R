#' Generate Combined Map from Layer Configurations
#'
#' This function creates a combined map by merging multiple layer configurations
#' and calculating their geometric mean.
#'
#' @param valid_configs List of valid map configurations
#' @param dataset_mapping A named list mapping layer names to datasets and score columns
#' @param base_grid The base spatial grid to use for combining data (default: grid_test)
#' @param map_title Title for the legend (default: "Combined Geometric Mean")
#' @return A list containing the combined data and the leaflet map
#'
generate_combined_map <- function(valid_configs, dataset_mapping, base_grid = grid_test, 
                                  map_title = "Combined Geometric Mean") {
  
  # Initialize result structure
  result <- list(
    combined_data = NULL,
    map = NULL
  )
  
  if(length(valid_configs) == 0) {
    # No valid configurations - return empty map with message
    result$map <- leaflet() %>%
      addProviderTiles("Esri.OceanBasemap") %>%
      addControl("Please configure at least one map to generate a combined map.", position = "topright")
    
    return(result)
  }
  
  # Use base_grid as the starting point for combining data
  combined_data <- base_grid
  
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
    combined_data <- left_join(combined_data, temp_data, by = "CellID_2km")
  }
  
  # Calculate geometric mean 
  combined_data <- calculate_geometric_mean(combined_data)
  
  # Check if we have valid geometric mean data
  if("Geo_mean" %in% names(combined_data) && 
     any(!is.na(combined_data$Geo_mean))) {
    
    # Make sure geometry is set properly for leaflet
    combined_data <- st_transform(combined_data, '+proj=longlat +datum=WGS84')
    
    # Get the bounding box of the data to set initial view
    # Convert named vector to individual numeric values to avoid JSON serialization issues
    bbox <- st_bbox(combined_data)
    min_lng <- as.numeric(bbox["xmin"])
    min_lat <- as.numeric(bbox["ymin"])  
    max_lng <- as.numeric(bbox["xmax"])
    max_lat <- as.numeric(bbox["ymax"])
    
    # Get the range of geometric mean values
    geo_mean_values <- combined_data$Geo_mean[!is.na(combined_data$Geo_mean)]
    min_val <- min(geo_mean_values, na.rm = TRUE)
    max_val <- max(geo_mean_values, na.rm = TRUE)
    
    # Check if all values are the same (constant values)
    if(min_val == max_val) {
      # When all values are the same, create a single-color palette
      # Use a middle color from viridis palette
      single_color <- viridis::viridis(1, begin = 0.5, end = 0.5)
      
      # Create the map with constant color
      result$map <- leaflet() %>%
        addProviderTiles("Esri.OceanBasemap",
                         options = providerTileOptions(variant = "Ocean/World_Ocean_Base")) %>%
        addProviderTiles("Esri.OceanBasemap",
                         options = providerTileOptions(variant = "Ocean/World_Ocean_Reference")) %>%
        addPolygons(
          data = combined_data, 
          color = "#33333300", # adding 00 at the end makes this color transparent
          weight = 1, 
          fillColor = single_color, 
          fillOpacity = 1,
          popup = ~paste("Geometric Mean Score:", round(Geo_mean, 2))
        ) %>%
        addLegend(
          position = "bottomright",
          colors = single_color,
          labels = paste("Score:", round(min_val, 2)),
          title = map_title,
          opacity = 1
        ) %>%
        fitBounds(lng1 = min_lng, lat1 = min_lat, 
                  lng2 = max_lng, lat2 = max_lat)
    } else {
      # Normal case with varying values - use continuous palette
      # Create color palette for the geometric mean
      pal <- colorNumeric("viridis", 
                          domain = c(min_val, max_val), 
                          na.color = "transparent")
      
      # Create the map
      result$map <- leaflet() %>%
        addProviderTiles("Esri.OceanBasemap",
                         options = providerTileOptions(variant = "Ocean/World_Ocean_Base")) %>%
        addProviderTiles("Esri.OceanBasemap",
                         options = providerTileOptions(variant = "Ocean/World_Ocean_Reference")) %>%
        addPolygons(
          data = combined_data, 
          color = "#33333300", # adding 00 at the end makes this color transparent
          weight = 1, 
          fillColor = ~pal(Geo_mean), 
          fillOpacity = 1,
          popup = ~paste("Geometric Mean Score:", round(Geo_mean, 2))
        ) %>%
        addLegend(
          position = "bottomright",
          pal = pal,
          values = combined_data$Geo_mean,
          title = map_title,
          opacity = 1
        ) %>%
        fitBounds(lng1 = min_lng, lat1 = min_lat, 
                  lng2 = max_lng, lat2 = max_lat)
    }
  } else {
    # No score data available
    result$map <- leaflet() %>%
      addProviderTiles("Esri.OceanBasemap") %>%
      addControl("No score data available for the selected layers.", position = "topright")
  }
  
  # Store the combined data in the result
  result$combined_data <- combined_data
  
  return(result)
}