#' Generate Overall Combined Model from Multiple Submodels
#'
#' This function creates an overall combined model by applying weights to 
#' submodel results and calculating their weighted geometric mean.
#'
#' @param submodels List of submodel spatial data
#' @param weights List of weights corresponding to each submodel
#' @param base_grid The base spatial grid to use for combining data
#' @return A list containing the combined data and the leaflet map
#'
create_overall_combined_model <- function(submodels, weights, base_grid = grid_test) {
  
  # Initialize result structure
  result <- list(
    combined_data = NULL,
    map = NULL
  )
  
  if(length(submodels) == 0) {
    # No submodels - return empty map with message
    result$map <- leaflet() %>%
      addProviderTiles("Esri.OceanBasemap") %>%
      addControl("No submodels available for overall model generation.", position = "topright")
    
    return(result)
  }
  
  # Normalize weights to sum to 1
  total_weight <- sum(unlist(weights))
  if(total_weight > 0) {
    normalized_weights <- lapply(weights, function(w) w / total_weight)
  } else {
    # If all weights are 0, assign equal weights
    normalized_weights <- lapply(weights, function(w) 1 / length(weights))
  }
  
  # Start with base grid
  combined_data <- base_grid
  
  # Apply weights and combine submodels
  weighted_columns <- c()
  
  for(i in seq_along(submodels)) {
    submodel_name <- names(submodels)[i]
    submodel_data <- submodels[[i]]
    weight <- normalized_weights[[i]]
    
    # Check if submodel has Geo_mean column
    if(!"Geo_mean" %in% names(submodel_data)) {
      warning(paste("Submodel", submodel_name, "does not have Geo_mean column. Skipping."))
      next
    }
    
    # Create weighted column name
    weighted_col_name <- paste0("Weighted_", submodel_name)
    
    # Extract geometric mean and apply weight
    temp_data <- submodel_data %>%
      st_drop_geometry() %>%
      select(CellID_2km, Geo_mean) %>%
      mutate(!!weighted_col_name := Geo_mean^weight) %>%
      select(CellID_2km, !!weighted_col_name)
    
    # Join with combined data
    combined_data <- left_join(combined_data, temp_data, by = "CellID_2km")
    
    # Track weighted columns for final calculation
    weighted_columns <- c(weighted_columns, weighted_col_name)
  }
  
  # Calculate overall geometric mean from weighted components
  if(length(weighted_columns) > 0) {
    combined_data <- combined_data %>%
      rowwise() %>%
      mutate(
        Overall_Geo_mean = if(all(is.na(c_across(all_of(weighted_columns))))) {
          NA_real_
        } else {
          # Calculate geometric mean of weighted values
          non_na_values <- c_across(all_of(weighted_columns))[!is.na(c_across(all_of(weighted_columns)))]
          if(length(non_na_values) > 0) {
            exp(mean(log(non_na_values[non_na_values > 0]), na.rm = TRUE))
          } else {
            NA_real_
          }
        }
      ) %>%
      ungroup()
  }
  
  # Create the map
  if("Overall_Geo_mean" %in% names(combined_data) && 
     any(!is.na(combined_data$Overall_Geo_mean))) {
    
    # Transform for leaflet
    combined_data <- st_transform(combined_data, '+proj=longlat +datum=WGS84')
    
    # Get bounding box
    bbox <- st_bbox(combined_data)
    min_lng <- as.numeric(bbox["xmin"])
    min_lat <- as.numeric(bbox["ymin"])  
    max_lng <- as.numeric(bbox["xmax"])
    max_lat <- as.numeric(bbox["ymax"])
    
    # Get range of values
    overall_values <- combined_data$Overall_Geo_mean[!is.na(combined_data$Overall_Geo_mean)]
    min_val <- min(overall_values, na.rm = TRUE)
    max_val <- max(overall_values, na.rm = TRUE)
    
    # Create map with color palette
    if(min_val == max_val) {
      # Single color for constant values
      single_color <- viridis::viridis(1, begin = 0.5, end = 0.5)
      
      result$map <- leaflet() %>%
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
          popup = ~paste("Overall Model Score:", round(Overall_Geo_mean, 2))
        ) %>%
        addLegend(
          position = "bottomright",
          colors = single_color,
          labels = paste("Score:", round(min_val, 2)),
          title = "Overall Model Score",
          opacity = 1
        ) %>%
        fitBounds(lng1 = min_lng, lat1 = min_lat, 
                  lng2 = max_lng, lat2 = max_lat)
    } else {
      # Color palette for varying values
      pal <- colorNumeric("viridis", 
                          domain = c(min_val, max_val), 
                          na.color = "transparent")
      
      result$map <- leaflet() %>%
        addProviderTiles("Esri.OceanBasemap",
                         options = providerTileOptions(variant = "Ocean/World_Ocean_Base")) %>%
        addProviderTiles("Esri.OceanBasemap",
                         options = providerTileOptions(variant = "Ocean/World_Ocean_Reference")) %>%
        addPolygons(
          data = combined_data, 
          color = "#33333300",
          weight = 1, 
          fillColor = ~pal(Overall_Geo_mean), 
          fillOpacity = 1,
          popup = ~paste("Overall Model Score:", round(Overall_Geo_mean, 2))
        ) %>%
        addLegend(
          position = "bottomright",
          pal = pal,
          values = combined_data$Overall_Geo_mean,
          title = "Overall Model Score",
          opacity = 1
        ) %>%
        fitBounds(lng1 = min_lng, lat1 = min_lat, 
                  lng2 = max_lng, lat2 = max_lat)
    }
  } else {
    # No valid data
    result$map <- leaflet() %>%
      addProviderTiles("Esri.OceanBasemap") %>%
      addControl("No valid score data available for overall model.", position = "topright")
  }
  
  # Store combined data
  result$combined_data <- combined_data
  
  return(result)
}