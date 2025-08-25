create_combined_map <- function(combined_data, map_title, method, aoi_data = NULL) {
  
  # Get AOI data - handle both direct data and global AOI fallback
  if(is.null(aoi_data) && exists("AOI")) {
    aoi_data <- AOI
  } else if(!is.null(aoi_data)) {
    # AOI data provided
  } else {
    # No AOI data available
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
    tryCatch({
      bbox <- st_bbox(aoi_data)
      map_bounds <- list(
        lng1 = bbox[["xmin"]], lat1 = bbox[["ymin"]],
        lng2 = bbox[["xmax"]], lat2 = bbox[["ymax"]]
      )
    }, error = function(e) {
      # Error calculating AOI bounds
    })
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
  if(is.null(combined_data) || nrow(combined_data) == 0) {
    map <- map %>%
      addControl("No data available for combined map", position = "topright")
  } else if(!score_column %in% names(combined_data)) {
    map <- map %>%
      addControl("No data available for combined map", position = "topright")
  } else {
    
    # Make sure geometry is set properly for leaflet
    tryCatch({
      combined_data <- st_transform(combined_data, '+proj=longlat +datum=WGS84')
    }, error = function(e) {
      # Error transforming combined data
    })
    
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
      tryCatch({
        combined_data$popup_display <- paste(popup_prefix, 
                                             ifelse(combined_data[[score_column]] < 0.01,
                                                    format(combined_data[[score_column]], scientific = FALSE, digits = 3),
                                                    round(combined_data[[score_column]], 3)))
      }, error = function(e) {
        # Error creating popup display
      })
      
      # Handle coloring based on whether values are constant or varying
      if(abs(min_val - max_val) < .Machine$double.eps * 100) {
        # Constant values (within machine precision) - single color
        single_color <- viridis::viridis(1, begin = 0.7, end = 0.7)
        
        tryCatch({
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
        }, error = function(e) {
          # Error adding single color polygons
        })
      } else {
        # Varying values - continuous palette with proper domain handling
        tryCatch({
          # Ensure all score values are numeric and handle zeros properly
          score_column_data <- as.numeric(combined_data[[score_column]])
          
          # Verify we have valid numeric data
          if(all(is.na(score_column_data))) {
            map <- map %>%
              addControl("No valid score data available", position = "topright")
          } else {
            # Clean the data - remove any problematic values
            clean_values <- score_column_data[!is.na(score_column_data) & is.finite(score_column_data)]
            
            if(length(clean_values) == 0) {
              map <- map %>%
                addControl("No valid score data available", position = "topright")
            } else {
              # Create color palette with the clean range
              pal <- colorNumeric("viridis",
                                  domain = range(clean_values),
                                  na.color = "transparent")
              
              # Apply colors directly to avoid formula processing issues
              fill_colors <- pal(score_column_data)
              
              # Replace any remaining NA colors with transparent
              fill_colors[is.na(fill_colors)] <- "transparent"
              
              map <- map %>%
                addPolygons(
                  data = combined_data,
                  color = "#33333300",
                  weight = 1,
                  fillColor = fill_colors,  # Use direct color vector instead of formula
                  fillOpacity = 1,
                  popup = combined_data$popup_display,  # Use direct column instead of formula
                  group = "Combined Data"
                ) %>%
                addLegend(
                  position = "bottomright",
                  pal = pal,
                  values = clean_values,  # Use clean values for legend
                  title = map_title,
                  opacity = 1
                )
            }
          }
        }, error = function(e) {
          # Fallback to single color if continuous fails
          single_color <- viridis::viridis(1, begin = 0.5, end = 0.5)
          map <- map %>%
            addPolygons(
              data = combined_data,
              color = "#33333300",
              weight = 1,
              fillColor = single_color,
              fillOpacity = 1,
              popup = combined_data$popup_display,
              group = "Combined Data"
            )
        })
      }
      
      # If no AOI bounds available, calculate from data
      if(is.null(map_bounds)) {
        tryCatch({
          bbox <- st_bbox(combined_data)
          map_bounds <- list(
            lng1 = bbox[["xmin"]], lat1 = bbox[["ymin"]],
            lng2 = bbox[["xmax"]], lat2 = bbox[["ymax"]]
          )
        }, error = function(e) {
          # Error calculating data bounds
        })
      }
    }
  }
  
  # Add AOI polygon after combined data so it appears on top
  if(!is.null(aoi_data) && nrow(aoi_data) > 0) {
    tryCatch({
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
    }, error = function(e) {
      # Error adding AOI overlay
    })
  }
  
  # Set map view bounds
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