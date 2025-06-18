#' Get valid map configurations for a specific tab
#'
#' This function examines all map configurations for a specified tab and returns
#' a list of valid configurations with their associated data, colors, and other properties.
#'
#' @param input The Shiny input object containing user selections
#' @param tab_name The name of the tab to get configurations for (e.g., "habitat", "species", "birds", "industry_operations")
#' @param layer_data A list of data layers available for the specified tab
#' @param score_colors A list mapping score values to colors
#' @param filter_func The function to use for filtering data by score
#'
#' @return A list of valid map configurations
#'
#' @examples
#' # Get valid configurations for the habitat tab
#' habitat_configs <- get_valid_configs_for_tab(input, "habitat", habitat_layer, score_colors, filter_by_score)
get_valid_configs_for_tab <- function(input, tab_name, layer_data, score_colors, filter_func) {
  valid_configs <- list()
  
  # Set the prefix based on the tab name
  prefix <- switch(tab_name,
                   "habitat" = "Habitat",
                   "species" = "Species",
                   "birds" = "Bird",
                   "industry_operations" = "Industry",
                   "")
  
  if(prefix == "" || is.null(layer_data)) return(valid_configs) # Empty list if invalid
  
  for(i in 1:6) {
    enable_input <- input[[paste0("Enable", prefix, "Map", i)]]
    if(!is.null(enable_input) && enable_input) {
      layer_name <- input[[paste0(prefix, "LayerPicker", i)]]
      score_name <- input[[paste0(prefix, "ScorePicker", i)]]
      
      if(!is.null(layer_name) && layer_name != "None" && 
         !is.null(score_name) && score_name != "None" &&
         layer_name %in% names(layer_data)) {
        
        dataset <- layer_data[[layer_name]]
        filtered_data <- filter_func(dataset, score_name)
        
        # Get the color for this score
        color <- score_colors[[score_name]]
        
        valid_configs[[length(valid_configs) + 1]] <- list(
          index = i,
          layer = layer_name,
          score = score_name,
          data = filtered_data,
          color = color
        )
      }
    }
  }
  
  return(valid_configs)
}