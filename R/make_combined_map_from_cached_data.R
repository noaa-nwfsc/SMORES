# Function to create combined maps from cached individual data
make_combined_map_from_cached_data <- function(valid_configs, cached_data, method, base_grid = grid_test, aoi_data = NULL) {
  
  # Initialize with base grid
  if(!is.null(aoi_data) && nrow(aoi_data) > 0) {
    # Crop the base grid to the AOI first
    base_sf <- crop_data_to_aoi(base_grid, aoi_data)
  } else {
    base_sf <- base_grid
  }
  
  # Separate Geometry from Data
  base_df <- sf::st_drop_geometry(base_sf)
  
  # Keep the geometry to re-attach at the very end
  base_geom <- sf::st_geometry(base_sf)
  
  score_columns_added <- c()
  
  # Iterate and Join
  for(config in valid_configs) {
    config_key <- paste(config$layer, config$score, config$index, sep = "_")
    
    # Get cached processed data
    cached_config <- cached_data[[config_key]]
    
    if(!is.null(cached_config) && !is.null(cached_config$data)) {
      
      temp_data <- cached_config$data
      score_col <- cached_config$score_column
      
      if(!is.null(score_col) && score_col %in% names(temp_data)) {
        
        # Create a lightweight lookup table (ID + Score only)
        temp_df_clean <- sf::st_drop_geometry(temp_data)[, c("CellID_2km", score_col)]
        
        # Join on standard dataframe
        base_df <- dplyr::left_join(base_df, temp_df_clean, by = "CellID_2km")
        
        score_columns_added <- c(score_columns_added, score_col)
        
      } else {
        cat("WARNING: Score column", score_col, "not found in cached data for", config$layer, "\n")
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
  
  # Re-attach Geometry
  base_combined_sf <- sf::st_as_sf(base_df, geometry = base_geom)
  
  return(base_combined_sf)
}