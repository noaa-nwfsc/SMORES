# Function to generate combined maps using the restructured modular approach
generate_combined_map_for_method <- function(valid_configs, dataset_mapping, method, 
                                             map_type = "Combined", aoi_data = NULL,
                                             base_grid = grid_test) {
  
  # Error handling wrapper
  tryCatch({    
    # Step 0: Crop datasets to AOI BEFORE processing (NEW)
    if(!is.null(aoi_data) && nrow(aoi_data) > 0) {
      # Create a cropped version of the dataset mapping
      cropped_dataset_mapping <- dataset_mapping
      
      for(layer_name in names(dataset_mapping)) {
        dataset_info <- dataset_mapping[[layer_name]]
        
        if(is.function(dataset_info$data)) {
          # For function-based datasets, create a wrapper that returns cropped data
          original_data_fn <- dataset_info$data
          cropped_dataset_mapping[[layer_name]]$data <- function(score) {
            full_data <- original_data_fn(score)
            return(crop_data_to_aoi(full_data, aoi_data))
          }
        } else {
          # For static datasets, crop once
          cropped_dataset_mapping[[layer_name]]$data <- crop_data_to_aoi(dataset_info$data, aoi_data)
        }
      }
      
      # Use cropped dataset mapping for the rest of the process
      dataset_mapping <- cropped_dataset_mapping
    }
    
    # Step 1: Prepare the combined dataset using make_combined_map_dataset
    base_combined_data <- make_combined_map_dataset(
      valid_configs = valid_configs,
      dataset_mapping = dataset_mapping,  # Now using potentially cropped data
      base_grid = base_grid,
      aoi_data = aoi_data
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