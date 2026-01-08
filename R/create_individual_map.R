create_individual_map <- function(config, aoi_data = NULL, aoi_bounds = NULL) {
  
  # Get AOI data
  if(is.null(aoi_data) && exists("AOI")) {
    aoi_data <- AOI
  }
  
  # Calculate map bounds based on AOI if available
  map_bounds <- aoi_bounds
  
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
        popup = ~paste("Z-Membership Cell Score:", round(Score.Z_Membership, 3)),
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
          popup = ~paste("Ranked Importance Cell Score:", round(get(score_column), 3)),
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
        fillColor = ~{
          # Find the score column and color based on actual values
          score_cols <- names(config$data)[grep("^Score\\.", names(config$data))]
          if(length(score_cols) > 0) {
            actual_score <- get(score_cols[1])
            ifelse(as.character(actual_score) == as.character(config$score), 
                   config$color,     # User-selected score gets the configured color
                   "#CCCCCC")        # Unselected cells (value 1) get gray
          } else {
            config$color
          }
        },
        fillOpacity = 0.7,
        popup = ~{
          # Find the score column and get the actual value for each cell
          score_cols <- names(config$data)[grep("^Score\\.", names(config$data))]
          if(length(score_cols) > 0) {
            actual_score <- get(score_cols[1])  # Get the first score column value
            paste("Offshore Wind Energy Suitability Cell Score:", actual_score)
          } else {
            paste("Offshore Wind Energy Suitability Cell Score:", config$score)
          }
        },
        group = "Data Layer"
      ) %>%
      addLegend(
        position = "bottomright",
        colors = c(config$color, "#CCCCCC"),  # Add gray for the 1's
        labels = c(paste("Selected Score for Offshore Wind Energy Suitability:", config$score), "Unselected (1)"),
        opacity = 0.7,
        title = config$layer
      )
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
  
  # Set map view based on bounds
  if(!is.null(map_bounds)) {
    map <- map %>%
      fitBounds(
        lng1 = map_bounds$lng1, lat1 = map_bounds$lat1,
        lng2 = map_bounds$lng2, lat2 = map_bounds$lat2,
        options = list(padding = c(10, 10))
      )
  }
  
  return(map)
}