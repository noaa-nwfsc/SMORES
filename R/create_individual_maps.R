#' Create individual maps for each configuration
#'
#' This function creates individual Leaflet maps for each valid configuration
#' and assigns them to the corresponding output elements.
#'
#' @param configs A list of valid map configurations
#' @param output The Shiny output object
#' @param namespace A string prefix to use for map IDs (default: NULL)
#'
#' @return NULL (creates maps as a side effect)
#'
#' @examples
#' # Get valid configurations and create maps
#' valid_configs <- get_valid_configs()
#' create_individual_maps(valid_configs, output, "naturalresources")
create_individual_maps <- function(configs, output, namespace = NULL) {
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
        # Ensure we have data to display
        if(is.null(local_config$data) || nrow(local_config$data) == 0) {
          return(leaflet() %>%
                   addProviderTiles("Esri.OceanBasemap",
                                    options = providerTileOptions(variant = "Ocean/World_Ocean_Base")) %>%
                   addProviderTiles("Esri.OceanBasemap",
                                    options = providerTileOptions(variant = "Ocean/World_Ocean_Reference")) %>%
                   addControl("No data matching selected score", position = "topright"))
        }
        
        # Create the map with legend
        leaflet() %>%
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
            popup = ~paste("Score:", local_config$score)
          ) %>%
          # Add a legend
          addLegend(
            position = "bottomright",
            colors = local_config$color,
            labels = paste("Score:", local_config$score),
            opacity = 0.7,
            title = local_config$layer
          )
      })
    })
  }
  
  return(invisible(NULL))
}