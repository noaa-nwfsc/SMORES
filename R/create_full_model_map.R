create_full_model_map <- function(combined_data, aoi_data_reactive = NULL, aoi_bounds = NULL) {
  
  # Initialize result structure
  result <- list(map = NULL)
  aoi_data <- NULL
  
  # Attempt to resolve AOI data 
  if(!is.null(aoi_data_reactive)) {
    aoi_data <- tryCatch(aoi_data_reactive(), error = function(e) {
      if(exists("AOI")) get("AOI") else NULL
    })
  } else if(exists("AOI")) {
    aoi_data <- get("AOI")
  }
  
  if(!"Overall_Geo_mean" %in% names(combined_data) ||
     !any(!is.na(combined_data$Overall_Geo_mean))) {
    # No valid data for mapping
    result$map <- leaflet::leaflet() %>%
      leaflet::addProviderTiles("Esri.OceanBasemap") %>%
      leaflet::addControl("No valid score data available for full model.", position = "topright")
    
    return(result$map)
  }
  
  # Transform data for leaflet
  combined_data <- combined_data %>%
    sf::st_transform('+proj=longlat +datum=WGS84')
  
  # Calculate map bounds based on AOI if available
  map_bounds <- aoi_bounds
  
  # Get range of values for palette
  full_values <- combined_data$Overall_Geo_mean[!is.na(combined_data$Overall_Geo_mean)]
  min_val <- min(full_values, na.rm = TRUE)
  max_val <- max(full_values, na.rm = TRUE)
  
  # Create popup text
  combined_data$popup_display <- paste0("Offshore Wind Energy Suitability Score: ",
                                        ifelse(combined_data$Overall_Geo_mean < 0.01,
                                               format(combined_data$Overall_Geo_mean, scientific = FALSE, digits = 3),
                                               round(combined_data$Overall_Geo_mean, 3)))
  
  # --- Create Map ---
  m <- leaflet::leaflet() %>%
    leaflet::addProviderTiles("Esri.OceanBasemap",
                              options = leaflet::providerTileOptions("Esri.OceanBasemap")) 
  
  if(min_val == max_val) {
    # Single color for constant values
    single_color <- viridis::viridis(1, begin = 0.5, end = 0.5)
    
    m <- m %>%
      leaflet::addPolygons(
        data = combined_data,
        color = "#33333300",
        weight = 1,
        fillColor = single_color,
        fillOpacity = 1,
        popup = ~popup_display,
        group = "Full Model Data"
      ) %>%
      leaflet::addLegend(
        position = "bottomright",
        colors = single_color,
        labels = paste("Score:", round(min_val, 2)),
        title = "Full Model Offshore Wind Energy Suitability Score",
        opacity = 1
      )
  } else {
    # Color palette for varying values
    pal <- leaflet::colorNumeric("viridis",
                                 domain = c(min_val, max_val),
                                 na.color = "transparent")
    
    m <- m %>%
      leaflet::addPolygons(
        data = combined_data,
        color = "#33333300",
        weight = 1,
        fillColor = ~pal(Overall_Geo_mean),
        fillOpacity = 1,
        popup = ~popup_display,
        group = "Full Model Data"
      ) %>%
      leaflet::addLegend(
        position = "bottomright",
        pal = pal,
        values = combined_data$Overall_Geo_mean,
        title = "Full Model Offshore Wind Energy Suitability Score:",
        opacity = 1
      )
  }
  
  # Set map view based on bounds
  if(!is.null(map_bounds)) {
    m <- m %>%
      fitBounds(
        lng1 = map_bounds$lng1, lat1 = map_bounds$lat1,
        lng2 = map_bounds$lng2, lat2 = map_bounds$lat2,
        options = list(padding = c(10, 10))
      )
  }
  
  # Add AOI polygon if available
  if(!is.null(aoi_data) && nrow(aoi_data) > 0) {
    m <- m %>%
      leaflet::addPolygons(
        data = aoi_data,
        fillColor = "transparent",
        color = "red",
        weight = 3,
        fillOpacity = 0,
        group = "AOI Boundaries",
        options = leaflet::pathOptions(interactive = FALSE)
      ) %>%
      leaflet::addLayersControl(
        overlayGroups = c("Full Model Data", "AOI Boundaries"),
        options = leaflet::layersControlOptions(collapsed = FALSE)
      )
  }
  
  return(m)
}