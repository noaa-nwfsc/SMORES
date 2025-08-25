create_combined_map <- function(combined_data, map_title, method, aoi_data = NULL) {
  
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
  
  # Calculate map bounds based on AOI if available
  map_bounds <- NULL
  if(!is.null(aoi_data) && nrow(aoi_data) > 0) {
    bbox <- st_bbox(aoi_data)
    map_bounds <- list(
      lng1 = bbox[["xmin"]], lat1 = bbox[["ymin"]],
      lng2 = bbox[["xmax"]], lat2 = bbox[["ymax"]]
    )
  }
  
  # Determine score column and popup prefix based on method
  if(method == "geometric_mean") {
    score_column <- "Geo_mean"
    popup_prefix <- "Geometric Mean Score:"
  } else if(method == "lowest") {
    score_column <- "Lowest_value"
    popup_prefix <- "Lowest Value Score:"
  } else if(method == "product") {
    score_column <- "Product_value"
    popup_prefix <- "Product Score:"
  } else {
    # Default fallback
    score_column <- "Geo_mean"
    popup_prefix <- "Geometric Mean Score:"
  }
  
  # Initialize base map
  map <- leaflet() %>%
    addProviderTiles("Esri.OceanBasemap") %>%
    addProviderTiles("Esri.OceanBasemap",
                     options = providerTileOptions(variant = "Ocean/World_Ocean_Reference"))
  
  # Check if we have valid data and score column
  if(is.null(combined_data) || nrow(combined_data) == 0 || 
     !score_column %in% names(combined_data)) {
    map <- map %>%
      addControl("No data available for combined map", position = "topright")
  } else {
    # Make sure geometry is set properly for leaflet
    combined_data <- st_transform(combined_data, '+proj=longlat +datum=WGS84')
    
    # Get the range of score values - preserve precision for small values
    score_values <- combined_data[[score_column]][!is.na(combined_data[[score_column]])]
    
    if(length(score_values) == 0) {
      # No valid score data
      map <- map %>%
        addControl("No valid score data available", position = "topright")
    } else {
      min_val <- min(score_values, na.rm = TRUE)
      max_val <- max(score_values, na.rm = TRUE)
      
      # Create popup text with proper formatting for small values
      combined_data$popup_display <- paste(popup_prefix, 
                                           ifelse(combined_data[[score_column]] < 0.01,
                                                  format(combined_data[[score_column]], scientific = FALSE, digits = 3),
                                                  round(combined_data[[score_column]], 3)))
      
      # Handle coloring based on whether values are constant or varying
      if(abs(min_val - max_val) < .Machine$double.eps * 100) {
        # Constant values (within machine precision) - single color
        single_color <- viridis::viridis(1, begin = 0.7, end = 0.7)
        
        map <- map %>%
          addPolygons(
            data = combined_data,
            color = "#33333300",
            weight = 1,
            fillColor = single_color,
            fillOpacity = 1,
            popup = ~popup_display,
            group = "Combined Data"
          ) %>%
          addLegend(
            position = "bottomright",
            colors = single_color,
            labels = paste("Score:", 
                           ifelse(min_val < 0.01,
                                  format(min_val, scientific = FALSE, digits = 3),
                                  round(min_val, 3))),
            title = map_title,
            opacity = 1
          )
      } else {
        # Varying values - continuous palette with proper domain handling
        # Ensure the domain captures the full range including small values
        palette_domain <- c(min_val, max_val)
        
        # Create color palette with explicit domain
        pal <- colorNumeric("viridis",
                            domain = palette_domain,
                            na.color = "transparent")
        
        combined_data$fill_color <- pal(combined_data[[score_column]])
        
        map <- map %>%
          addPolygons(
            data = combined_data,
            color = "#33333300",
            weight = 1,
            fillColor = ~fill_color,
            fillOpacity = 1,
            popup = ~popup_display,
            group = "Combined Data"
          ) %>%
          addLegend(
            position = "bottomright",
            pal = pal,
            values = combined_data[[score_column]],
            title = map_title,
            opacity = 1,
            # Custom label formatting for small values
            labFormat = labelFormat(
              digits = 3,
              transform = function(x) {
                ifelse(x < 0.01, 
                       format(x, scientific = FALSE, digits = 3),
                       round(x, 3))
              }
            )
          )
      }
      
      # If no AOI bounds available, calculate from data
      if(is.null(map_bounds)) {
        bbox <- st_bbox(combined_data)
        map_bounds <- list(
          lng1 = bbox[["xmin"]], lat1 = bbox[["ymin"]],
          lng2 = bbox[["xmax"]], lat2 = bbox[["ymax"]]
        )
      }
    }
  }
  
  # Add AOI polygon after combined data so it appears on top
  if(!is.null(aoi_data) && nrow(aoi_data) > 0) {
    map <- map %>%
      addPolygons(
        data = aoi_data,
        fillColor = "transparent",
        color = "red",
        weight = 3,
        fillOpacity = 0,
        group = "AOI Area",
        options = pathOptions(
          interactive = FALSE
        )
      ) %>%
      addLayersControl(
        overlayGroups = c("Combined Data", "AOI Area"),
        options = layersControlOptions(collapsed = FALSE)
      )
  }
  
  # Set map view bounds
  if(!is.null(map_bounds)) {
    map <- map %>%
      fitBounds(
        lng1 = map_bounds$lng1, lat1 = map_bounds$lat1,
        lng2 = map_bounds$lng2, lat2 = map_bounds$lat2,
        options = list(padding = c(20, 20))
      )
  }
  
  return(map)
}