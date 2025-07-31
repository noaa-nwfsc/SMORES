create_individual_map <- function(config, aoi_data = NULL) {
  
  # Get AOI data - handle both direct data and global AOI fallback
  if(is.null(aoi_data) && exists("AOI")) {
    aoi_data <- AOI
  }
  
  # Transform AOI data to WGS84 if available and needed
  if(!is.null(aoi_data) && nrow(aoi_data) > 0) {
    if(!st_is_longlat(aoi_data)) {
      aoi_data <- st_transform(aoi_data, 4326)
    }
    aoi_data <- st_zm(aoi_data)
  }
  
  # Calculate map bounds based on AOI if available, otherwise use data bounds
  map_bounds <- NULL
  if(!is.null(aoi_data) && nrow(aoi_data) > 0) {
    # Use AOI bounds for centering the map
    bbox <- st_bbox(aoi_data)
    map_bounds <- list(
      lng1 = bbox[["xmin"]], lat1 = bbox[["ymin"]],
      lng2 = bbox[["xmax"]], lat2 = bbox[["ymax"]]
    )
  } else if(!is.null(config$data) && nrow(config$data) > 0) {
    # Fallback to data bounds if no AOI
    data_transformed <- config$data
    if(!st_is_longlat(data_transformed)) {
      data_transformed <- st_transform(data_transformed, 4326)
    }
    bbox <- st_bbox(data_transformed)
    map_bounds <- list(
      lng1 = bbox[["xmin"]], lat1 = bbox[["ymin"]],
      lng2 = bbox[["xmax"]], lat2 = bbox[["ymax"]]
    )
  }
  
  # Ensure we have data to display
  if(is.null(config$data) || nrow(config$data) == 0) {
    base_map <- leaflet() %>%
      addProviderTiles("Esri.OceanBasemap",
                       options = providerTileOptions(variant = "Ocean/World_Ocean_Base")) %>%
      addProviderTiles("Esri.OceanBasemap",
                       options = providerTileOptions(variant = "Ocean/World_Ocean_Reference")) %>%
      addControl("No data matching selected score", position = "topright")
    
    # Set map view based on bounds
    if(!is.null(map_bounds)) {
      base_map <- base_map %>%
        fitBounds(
          lng1 = map_bounds$lng1, lat1 = map_bounds$lat1,
          lng2 = map_bounds$lng2, lat2 = map_bounds$lat2,
          options = list(padding = c(20, 20))  # Add some padding around the bounds
        )
    }
    
    # Add AOI polygon - always include it
    if(!is.null(aoi_data) && nrow(aoi_data) > 0) {
      base_map <- base_map %>%
        addPolygons(
          data = aoi_data,
          fillColor = "transparent",
          color = "red",
          weight = 3,
          fillOpacity = 0,
          group = "AOI Area"
        )
    }
    
    return(base_map)
  }
  
  # Create the map with legend
  map <- leaflet() %>%
    addProviderTiles("Esri.OceanBasemap",
                     options = providerTileOptions(variant = "Ocean/World_Ocean_Base")) %>%
    addProviderTiles("Esri.OceanBasemap",
                     options = providerTileOptions(variant = "Ocean/World_Ocean_Reference")) 
  
  # Handle different coloring types
  if(!is.null(config$score) && config$score == "Z Membership" && 
     !is.null(config$color_palette)) {
    # Handle continuous Z Membership coloring
    map <- map %>%
      addPolygons(
        data = config$data, 
        color = "#33333300",  # transparent border
        weight = 1,            
        fillColor = ~config$color_palette(Score.Z_Membership),
        fillOpacity = 0.7,
        popup = ~paste("Z Membership:", round(Score.Z_Membership, 3)),
        group = "Data Layer"
      ) %>%
      # Add continuous legend for Z Membership
      addLegend(
        position = "bottomright",
        pal = config$color_palette,
        values = config$data$Score.Z_Membership,
        opacity = 0.7,
        title = paste(config$layer, "<br>Z Membership")
      )
  } else {
    # Handle discrete score coloring
    map <- map %>%
      addPolygons(
        data = config$data, 
        color = "#33333300",  # transparent border
        weight = 1,            
        fillColor = config$color,
        fillOpacity = 0.7,
        popup = ~paste("Score:", config$score),
        group = "Data Layer"
      ) %>%
      # Add discrete legend
      addLegend(
        position = "bottomright",
        colors = config$color,
        labels = paste("Score:", config$score),
        opacity = 0.7,
        title = config$layer
      )
  }
  
  # Set map view based on bounds (prioritize AOI bounds)
  if(!is.null(map_bounds)) {
    map <- map %>%
      fitBounds(
        lng1 = map_bounds$lng1, lat1 = map_bounds$lat1,
        lng2 = map_bounds$lng2, lat2 = map_bounds$lat2,
        options = list(padding = c(20, 20))  # Add some padding around the bounds
      )
  }
  
  # Add AOI polygon - always include it
  if(!is.null(aoi_data) && nrow(aoi_data) > 0) {
    map <- map %>%
      addPolygons(
        data = aoi_data,
        fillColor = "transparent",
        color = "red",
        weight = 3,
        fillOpacity = 0,
        group = "AOI Area"
      ) %>%
      addLayersControl(
        overlayGroups = c("Data Layer", "AOI Area"),
        options = layersControlOptions(collapsed = FALSE)
      )
  }
  
  return(map)
}