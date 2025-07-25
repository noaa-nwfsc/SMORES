#' Create individual maps for each configuration
#'
#' This function creates individual Leaflet maps for each valid configuration
#' and assigns them to the corresponding output elements.
#'
#' @param configs A list of valid map configurations
#' @param output The Shiny output object
#' @param namespace A string prefix to use for map IDs (default: NULL)
#' @param aoi_data_reactive A reactive expression that returns AOI data (optional)
#'
#' @return NULL (creates maps as a side effect)
#'
#' @examples
#' # Get valid configurations and create maps
#' valid_configs <- get_valid_configs()
#' create_individual_maps(valid_configs, output, "naturalresources")
create_individual_maps <- function(configs, output, namespace = NULL, aoi_data_reactive = NULL) {
  # Early return if no configs
  if(length(configs) == 0) {
    return(invisible(NULL))
  }
  
  # For each configuration, create a map
  for(config in configs) {
    local({
      local_config <- config
      # Create namespaced map ID if namespace is provided
      map_id <- if (!is.null(namespace)) {
        paste0(namespace, "_map_", local_config$index)
      } else {
        paste0("map_", local_config$index)
      }
      
      output[[map_id]] <- renderLeaflet({
        # Get AOI data - use the reactive if provided, otherwise use global AOI
        aoi_data <- NULL
        if(!is.null(aoi_data_reactive)) {
          tryCatch({
            aoi_data <- aoi_data_reactive()
          }, error = function(e) {
            # If reactive fails, try to get AOI directly
            if(exists("AOI")) {
              aoi_data <- AOI
            }
          })
        } else if(exists("AOI")) {
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
        } else if(!is.null(local_config$data) && nrow(local_config$data) > 0) {
          # Fallback to data bounds if no AOI
          data_transformed <- local_config$data
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
        if(is.null(local_config$data) || nrow(local_config$data) == 0) {
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
                popup = ~paste("Area:", Area_Name),
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
                           options = providerTileOptions(variant = "Ocean/World_Ocean_Reference")) %>%
          addPolygons(
            data = local_config$data, 
            color = "#33333300",  # transparent border
            weight = 1,            
            fillColor = local_config$color,
            fillOpacity = 0.7,
            popup = ~paste("Score:", local_config$score),
            group = "Data Layer"
          ) %>%
          # Add a legend
          addLegend(
            position = "bottomright",
            colors = local_config$color,
            labels = paste("Score:", local_config$score),
            opacity = 0.7,
            title = local_config$layer
          )
        
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
              popup = ~paste("Area:", Area_Name),
              group = "AOI Area"
            ) %>%
            addLayersControl(
              overlayGroups = c("Data Layer", "AOI Area"),
              options = layersControlOptions(collapsed = FALSE)
            )
        }
        
        return(map)
      })
    })
  }
  
  return(invisible(NULL))
}