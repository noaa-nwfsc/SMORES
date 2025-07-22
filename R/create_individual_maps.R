#' Create individual maps for each configuration
#'
#' This function creates individual Leaflet maps for each valid configuration
#' and assigns them to the corresponding output elements.
#'
#' @param configs A list of valid map configurations
#' @param output The Shiny output object
#' @param namespace A string prefix to use for map IDs (default: NULL)
#' @param wea_data_reactive A reactive expression that returns WEA data (optional)
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
        # Get WEA data - use the reactive if provided, otherwise use global WEA
        wea_data <- NULL
        if(!is.null(aoi_data_reactive)) {
          tryCatch({
            aoi_data <- aoi_data_reactive()
          }, error = function(e) {
            # If reactive fails, try to get WEA directly
            if(exists("AOI")) {
              aoi_data <- AOI
            }
          })
        } else if(exists("AOI")) {
          aoi_data <- AOI
        }
        
        # Ensure we have data to display
        if(is.null(local_config$data) || nrow(local_config$data) == 0) {
          base_map <- leaflet() %>%
            addProviderTiles("Esri.OceanBasemap",
                             options = providerTileOptions(variant = "Ocean/World_Ocean_Base")) %>%
            addProviderTiles("Esri.OceanBasemap",
                             options = providerTileOptions(variant = "Ocean/World_Ocean_Reference")) %>%
            addControl("No data matching selected score", position = "topright")
          
          # Add WEA polygon - always include it
          if(!is.null(aoi_data) && nrow(aoi_data) > 0) {
            # Transform WEA data if needed
            if(!st_is_longlat(aoi_data)) {
              aoi_data <- st_transform(aoi_data, 4326)
            }
            aoi_data <- st_zm(aoi_data)
            
            base_map <- base_map %>%
              addPolygons(
                data = aoi_data,
                fillColor = "transparent",
                color = "red",
                weight = 3,
                fillOpacity = 0,
                popup = ~paste("Area:", Area_Name),
                group = "WEA Area"
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
        
        # Add WEA polygon - always include it
        if(!is.null(aoi_data) && nrow(aoi_data) > 0) {
          # Transform WEA data if needed
          if(!st_is_longlat(aoi_data)) {
            aoi_data <- st_transform(aoi_data, 4326)
          }
          aoi_data <- st_zm(aoi_data)
          
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