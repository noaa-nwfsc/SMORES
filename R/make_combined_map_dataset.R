# Function to prepare and combine datasets based on valid configurations
make_combined_map_dataset <- function(valid_configs, dataset_mapping, base_grid = grid_test, aoi_data = NULL) {
  cat("=== MAKE_COMBINED_MAP_DATASET DEBUG ===\n")
  cat("Number of valid configs:", length(valid_configs), "\n")
  
  # Initialize with base grid and crop it to AOI first
  cat("Cropping base grid...\n")
  
  if(!is.null(aoi_data) && nrow(aoi_data) > 0) {
    base_combined_data <- crop_data_to_aoi(base_grid, aoi_data)
  } else {
    base_combined_data <- base_grid
  }
  
  cat("Base grid after cropping - rows:", nrow(base_combined_data), "\n")
  
  # Process each configuration
  for(i in seq_along(valid_configs)) {
    config <- valid_configs[[i]]
    
    cat("\n--- Processing config", i, ":", config$layer, "with score:", config$score, "---\n")
    
    # Get dataset info from mapping
    dataset_info <- dataset_mapping[[config$layer]]
    
    if(is.null(dataset_info)) {
      cat("WARNING: No dataset mapping found for layer:", config$layer, "\n")
      next
    }
    
    # Get the data (handling function-based data)
    if(is.function(dataset_info$data)) {
      layer_data <- dataset_info$data(config$score)
    } else {
      layer_data <- dataset_info$data
    }
    
    cat("Dataset rows before cropping:", nrow(layer_data), "\n")
    
    # Get score column
    if(is.function(dataset_info$score_column)) {
      score_col <- dataset_info$score_column(config$score)
    } else {
      score_col <- dataset_info$score_column
    }
    
    cat("Score column:", score_col, "\n")
    
    # Crop the dataset to AOI (if Step 0 in generate_combined_map_for_method already cropped it, this is redundant but safe)
    cat("Cropping dataset to AOI...\n")
    if(!is.null(aoi_data) && nrow(aoi_data) > 0) {
      layer_data <- crop_data_to_aoi(layer_data, aoi_data)
    }
    
    cat("Dataset rows after cropping:", nrow(layer_data), "\n")
    
    # Apply filter_by_score with the CROPPED base grid
    cat("Applying filter_by_score...\n")
    temp_data <- filter_by_score(layer_data, config$score, base_combined_data, config$layer)
    
    cat("Dataset rows after filtering:", nrow(temp_data), "\n")
    
    # Debug: Check the score column data
    if(score_col %in% names(temp_data)) {
      cat("temp_data rows:", nrow(temp_data), "\n")
      cat("temp_data columns:", paste(names(temp_data), collapse = ", "), "\n")
      
      # Debug score column
      score_column_data <- temp_data[[score_col]]
      cat("Score column type before conversion:", class(score_column_data), "\n")
      cat("Score column is.list():", is.list(score_column_data), "\n")
      
      # Show sample values
      if(length(score_column_data) > 0) {
        sample_values <- head(score_column_data, 5)
        cat("Sample values:", paste(sample_values, collapse = ", "), "\n")
      }
      
      # Ensure score column is numeric
      if(is.list(score_column_data)) {
        # If it's a list, try to unlist it
        temp_data[[score_col]] <- as.numeric(unlist(score_column_data))
      } else {
        temp_data[[score_col]] <- as.numeric(score_column_data)
      }
      
      # Debug after conversion
      score_column_data_after <- temp_data[[score_col]]
      cat("Score column type AFTER conversion:", class(score_column_data_after), "\n")
      cat("Score column is.list() AFTER conversion:", is.list(score_column_data_after), "\n")
    } else {
      cat("WARNING: Score column", score_col, "not found in temp_data\n")
      cat("Available columns:", paste(names(temp_data), collapse = ", "), "\n")
      next
    }
    
    # Join with the combined dataset
    cat("Joining with combined data...\n")
    cat("base_combined_data rows before join:", nrow(base_combined_data), "\n")
    
    # Keep only necessary columns for joining
    temp_data_for_join <- temp_data %>%
      select(CellID_2km, all_of(score_col))
    
    # Perform the join
    base_combined_data <- base_combined_data %>%
      left_join(st_drop_geometry(temp_data_for_join), by = "CellID_2km")
    
    cat("base_combined_data rows after join:", nrow(base_combined_data), "\n")
    cat("base_combined_data columns after join:", paste(names(base_combined_data), collapse = ", "), "\n")
    
    # Check for list columns
    list_columns <- sapply(base_combined_data, is.list)
    if(any(list_columns)) {
      cat("WARNING: List columns in combined data:", paste(names(base_combined_data)[list_columns], collapse = ", "), "\n")
    }
  }
  
  cat("Final combined data rows:", nrow(base_combined_data), "\n")
  cat("Final combined data columns:", paste(names(base_combined_data), collapse = ", "), "\n")
  
  return(base_combined_data)
}