# Function to prepare and combine datasets based on valid configurations
make_combined_map_dataset <- function(valid_configs, dataset_mapping, base_grid = grid_test) {
  
  if(length(valid_configs) == 0) {
    return(NULL)
  }
  
  # Use base_grid as the starting point for combining data
  base_combined_data <- base_grid
  
  # For each valid configuration, extract the data and join with the base grid
  for(config in valid_configs) {
    layer_name <- config$layer
    score_value <- config$score
    
    # Get dataset info from mapping
    dataset_info <- dataset_mapping[[layer_name]]
    if(is.null(dataset_info)) {
      next  # Skip if layer name doesn't match any mapping
    }
    
    # Handle dynamic dataset selection for layers with multiple score types
    if(is.function(dataset_info$data)) {
      dataset <- dataset_info$data(score_value)
      score_column <- dataset_info$score_column(score_value)
    } else {
      dataset <- dataset_info$data
      score_column <- dataset_info$score_column
    }
    
    # Handle different score types
    if(score_value == "Ranked Importance") {
      # For ranked importance, use the complete dataset that includes 1's from filter_by_score
      # This ensures we get both the ranked importance values AND 1's for unselected cells
      temp_data <- dataset %>%
        st_drop_geometry() %>%
        select(CellID_2km, !!score_column)
    } else if(score_value == "Z Membership") {
      # Special handling for Z Membership - don't filter, use all data
      temp_data <- dataset %>%
        st_drop_geometry() %>%
        select(CellID_2km, !!score_column)
    } else if(layer_name == "Trawl Fisheries @ 75%" && score_value == "0.001") {
      # Special handling for trawl fisheries - apply score to all non-NA spatial coverage
      temp_data <- dataset %>%
        st_drop_geometry() %>%
        select(CellID_2km, !!score_column) %>%
        # For trawl fisheries, any non-NA value in the score column should be replaced with 0.001
        mutate(!!score_column := ifelse(!is.na(.data[[score_column]]) & .data[[score_column]] != 0, 
                                        as.numeric(score_value), 
                                        NA_real_))
    } else {
      # For discrete scores, use the complete dataset that already includes 1's from filter_by_score
      # Don't filter again - the dataset is already processed with selected scores + 1's for unselected cells
      temp_data <- dataset %>%
        st_drop_geometry() %>%
        select(CellID_2km, !!score_column)
    }
    
    # Convert the score column to numeric (explicit conversion)
    temp_data[[score_column]] <- as.numeric(temp_data[[score_column]])
    
    # Join with the combined data
    base_combined_data <- left_join(base_combined_data, temp_data, by = "CellID_2km")
  }
  
  return(base_combined_data)
}