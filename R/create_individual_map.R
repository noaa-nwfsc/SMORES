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
          popup = ~paste("AOI Area:", if("Area_Name" %in% names(aoi_data)) Area_Name else "Selected Area"),
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
  
  # Add data layer FIRST
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
        popup = ~paste("Cell Score:", round(Score.Z_Membership, 3)),
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
  } else if(!is.null(config$score) && config$score == "Ranked Importance" && 
            !is.null(config$color_palette)) {
    # Handle continuous Ranked Importance coloring for fisheries
    # Determine the score column name based on the layer
    score_column <- switch(config$layer,
                           "At-Sea Hake Mid-Water Trawl" = "Score.ASH_Ranked_Importance",
                           "Shoreside Hake Mid-Water Trawl" = "Score.SSH_Ranked_Importance",
                           "Groundfish Bottom Trawl" = "Score.GFBT_Ranked_Importance",
                           "Groundfish Pot Gear" = "Score.GFP_Ranked_Importance",
                           "Groundfish Long Line Gear" = "Score.GFLL_Ranked_Importance",
                           "Pink Shrimp Trawl" = "Score.PS_Ranked_Importance",
                           "Dungeness Crab" = "Score.CRAB_Ranked_Importance",
                           "Commercial Troll/Hook and Line Albacore" = "Score.ALCO_Ranked_Importance",
                           "Charter Vessel Albacore Troll/Hook and Line" = "Score.ALCH_Ranked_Importance",
                           NULL  # fallback
    )
    
    if(!is.null(score_column) && score_column %in% names(config$data)) {
      # Create fillColor formula dynamically
      fill_color_formula <- paste0("~config$color_palette(", score_column, ")")
      popup_formula <- paste0("~paste('Cell Score: Ranked Importance:', round(", score_column, ", 3))")
      
      map <- map %>%
        addPolygons(
          data = config$data, 
          color = "#33333300",  # transparent border
          weight = 1,            
          fillColor = config$color_palette(config$data[[score_column]]),
          fillOpacity = 0.7,
          popup = ~paste("Cell Score:", round(get(score_column), 3)),
          group = "Data Layer"
        ) %>%
        # Add continuous legend for Ranked Importance
        addLegend(
          position = "bottomright",
          pal = config$color_palette,
          values = config$data[[score_column]],
          opacity = 0.7,
          title = paste(config$layer, "<br>Ranked Importance")
        )
    }
  } else {
    # Handle discrete score coloring
    map <- map %>%
      addPolygons(
        data = config$data, 
        color = "#33333300",  # transparent border
        weight = 1,            
        fillColor = config$color,
        fillOpacity = 0.7,
        popup = ~paste("Cell Score:", config$score),
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
  
  # Add AOI polygon AFTER data layer with smart interactivity
  if(!is.null(aoi_data) && nrow(aoi_data) > 0) {
    map <- map %>%
      addPolygons(
        data = aoi_data,
        fillColor = "transparent",
        color = "red",
        weight = 3,
        fillOpacity = 0,  # Completely transparent fill
        popup = ~paste("AOI Area:", if("Area_Name" %in% names(aoi_data)) Area_Name else "Selected Area"),
        group = "AOI Area",
        options = pathOptions(
          interactive = FALSE
        )
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
  
  # Add layers control
  if(!is.null(aoi_data) && nrow(aoi_data) > 0) {
    map <- map %>%
      addLayersControl(
        overlayGroups = c("Data Layer", "AOI Area"),
        options = layersControlOptions(collapsed = FALSE)
      )
  }
  
  return(map)
}