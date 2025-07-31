# Function to generate combined maps using the restructured modular approach
generate_combined_map_for_method <- function(valid_configs, dataset_mapping, method, 
                                             map_type = "Combined", aoi_data = NULL,
                                             base_grid = grid_test) {
  
  # Error handling wrapper
  tryCatch({
    
    # Step 1: Prepare the combined dataset using make_combined_map_dataset
    base_combined_data <- make_combined_map_dataset(
      valid_configs = valid_configs,
      dataset_mapping = dataset_mapping, 
      base_grid = base_grid
    )
    
    if(is.null(base_combined_data)) {
      # Return empty result if data preparation failed
      return(list(
        combined_data = NULL,
        map = leaflet() %>%
          addProviderTiles("Esri.OceanBasemap") %>%
          addControl("Data preparation failed.", position = "topright")
      ))
    }
    
    # Step 2: Apply calculation method using apply_calculation_method
    method_result <- apply_calculation_method(
      combined_data = base_combined_data,
      method = method,
      map_type = map_type
    )
    
    if(is.null(method_result$data)) {
      # Return empty result if calculation failed
      return(list(
        combined_data = NULL,
        map = leaflet() %>%
          addProviderTiles("Esri.OceanBasemap") %>%
          addControl(paste("Calculation failed for", method, "method."), position = "topright")
      ))
    }
    
    # Step 3: Create the map using create_combined_map
    combined_map <- create_combined_map(
      combined_data = method_result$data,
      map_title = method_result$title,
      method = method,
      aoi_data = aoi_data
    )
    
    # Return the successful result
    return(list(
      combined_data = method_result$data,
      map = combined_map
    ))
    
  }, error = function(e) {
    # Error handler 
    return(list(
      combined_data = NULL,
      map = leaflet() %>%
        addProviderTiles("Esri.OceanBasemap") %>%
        addControl(paste("Error generating", method, "map:", e$message), position = "topright")
    ))
  })
}