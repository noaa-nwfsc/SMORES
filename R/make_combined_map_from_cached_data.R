# Function to create combined maps from cached individual data
make_combined_map_from_cached_data <- function(valid_configs, cached_data, method, base_grid = grid_test, aoi_data = NULL) {
  
  # Initialize with base grid
  if(!is.null(aoi_data) && nrow(aoi_data) > 0) {
    base_combined_data <- crop_data_to_aoi(base_grid, aoi_data)
  } else {
    base_combined_data <- base_grid
  }
  
  # Process each valid configuration using cached data
  score_columns_added <- c()
  for(config in valid_configs) {
    config_key <- paste(config$layer, config$score, config$index, sep = "_")
    
    # Get cached processed data
    cached_config <- cached_data[[config_key]]
    
    if(!is.null(cached_config) && !is.null(cached_config$data)) {
      
      # Use the already processed and cropped data
      temp_data <- cached_config$data
      score_col <- cached_config$score_column
      
      if(!is.null(score_col) && score_col %in% names(temp_data)) {
 
        
        # Keep only necessary columns for joining
        temp_data_for_join <- temp_data %>%
          select(CellID_2km, all_of(score_col))
        
        # Join with combined dataset
        base_combined_data <- base_combined_data %>%
          left_join(st_drop_geometry(temp_data_for_join), by = "CellID_2km")

        score_columns_added <- c(score_columns_added, score_col)
      } else {
        cat("WARNING: Score column", score_col, "not found in cached data for", config$layer, "\n")
        if(!is.null(temp_data)) {
          cat("Available columns:", paste(names(temp_data), collapse = ", "), "\n")
        }
      }
    } else {
      cat("WARNING: No cached data found for config:", config_key, "\n")
    }
  }
  
  # Verify we have score columns for the calculation
  if(length(score_columns_added) == 0) {
    cat("ERROR: No score columns were successfully added to the combined data\n")
    return(NULL)
  }
  
  return(base_combined_data)
}