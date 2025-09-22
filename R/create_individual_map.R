create_individual_map <- function(config, aoi_data = NULL) {
  
  # Get AOI data - handle both direct data and global AOI fallback
  if(is.null(aoi_data) && exists("AOI")) {
    aoi_data <- AOI
  }
  
  # Calculate map bounds based on AOI if available
  map_bounds <- NULL
  if(!is.null(aoi_data) && nrow(aoi_data) > 0) {
    tryCatch({
      bbox <- st_bbox(aoi_data)
      map_bounds <- list(
        lng1 = bbox[["xmin"]], lat1 = bbox[["ymin"]],
        lng2 = bbox[["xmax"]], lat2 = bbox[["ymax"]]
      )
    }, error = function(e) {
      # Error calculating AOI bounds - fall back to data bounds
      if(!is.null(config$data) && nrow(config$data) > 0) {
        bbox <- get_bbox_fast(config$data)
        if(!is.null(bbox)) {
          map_bounds <<- list(
            lng1 = bbox[["xmin"]], lat1 = bbox[["ymin"]],
            lng2 = bbox[["xmax"]], lat2 = bbox[["ymax"]]
          )
        }
      }
    })
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
      tryCatch({
        base_map <- base_map %>%
          fitBounds(
            lng1 = map_bounds$lng1, lat1 = map_bounds$lat1,
            lng2 = map_bounds$lng2, lat2 = map_bounds$lat2,
            options = list(padding = c(20, 20))
          )
      }, error = function(e) {
        # Error setting bounds
      })
    }
    
    # Add AOI polygon
    if(!is.null(aoi_data) && nrow(aoi_data) > 0) {
      tryCatch({
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
      }, error = function(e) {
        # Error adding AOI polygon
      })
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
        color = "#33333300",
        weight = 1,            
        fillColor = ~config$color_palette(Score.Z_Membership),
        fillOpacity = 0.7,
        popup = ~paste("Cell Score:", round(Score.Z_Membership, 3)),
        group = "Data Layer"
      ) %>%
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
                           NULL)
    
    if(!is.null(score_column) && score_column %in% names(config$data)) {
      map <- map %>%
        addPolygons(
          data = config$data, 
          color = "#33333300",
          weight = 1,            
          fillColor = config$color_palette(config$data[[score_column]]),
          fillOpacity = 0.7,
          popup = ~paste("Cell Score:", round(get(score_column), 3)),
          group = "Data Layer"
        ) %>%
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
        color = "#33333300",
        weight = 1,            
        fillColor = config$color,
        fillOpacity = 0.7,
        popup = ~paste("Cell Score:", config$score),
        group = "Data Layer"
      ) %>%
      addLegend(
        position = "bottomright",
        colors = config$color,
        labels = paste("Score:", config$score),
        opacity = 0.7,
        title = config$layer
      )
  }
  
  # If no AOI bounds available, calculate from data (fallback - using preprocessed data)
  if(is.null(map_bounds) && !is.null(config$data) && nrow(config$data) > 0) {
    tryCatch({
      bbox <- get_bbox_fast(config$data)
      if(!is.null(bbox)) {
        map_bounds <- list(
          lng1 = bbox[["xmin"]], lat1 = bbox[["ymin"]],
          lng2 = bbox[["xmax"]], lat2 = bbox[["ymax"]]
        )
      }
    }, error = function(e) {
      # Error calculating data bounds
    })
  }
  
  # Add AOI polygon AFTER data layer
  if(!is.null(aoi_data) && nrow(aoi_data) > 0) {
    tryCatch({
      map <- map %>%
        addPolygons(
          data = aoi_data,
          fillColor = "transparent",
          color = "red",
          weight = 3,
          fillOpacity = 0,
          popup = ~paste("AOI Area:", if("Area_Name" %in% names(aoi_data)) Area_Name else "Selected Area"),
          group = "AOI Area",
          options = pathOptions(
            interactive = FALSE
          )
        ) %>%
        addLayersControl(
          overlayGroups = c("Data Layer", "AOI Area"),
          options = layersControlOptions(collapsed = FALSE)
        )
    }, error = function(e) {
      # Error adding AOI polygon
    })
  }
  
  # Set map view bounds (same approach as combined map)
  if(!is.null(map_bounds)) {
    tryCatch({
      map <- map %>%
        fitBounds(
          lng1 = map_bounds$lng1, lat1 = map_bounds$lat1,
          lng2 = map_bounds$lng2, lat2 = map_bounds$lat2,
          options = list(padding = c(20, 20))
        )
    }, error = function(e) {
      # Error setting bounds
    })
  }
  
  return(map)
}