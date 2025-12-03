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
    
    # Apply filter_by_score with base_grid to get complete dataset including 1s for unselected cells
    filtered_dataset <- filter_by_score(dataset, score_value, base_grid, layer_name)
    
    # Handle different score types
    if(score_value == "Ranked Importance") {
      # Use the filtered dataset that now includes 1's for unselected cells
      temp_data <- filtered_dataset %>%
        st_drop_geometry() %>%
        select(CellID_2km, !!score_column)
    } else if(score_value == "Z Membership") {
      # Special handling for Z Membership - use filtered dataset
      temp_data <- filtered_dataset %>%
        st_drop_geometry() %>%
        select(CellID_2km, !!score_column)
    } else if(layer_name == "Trawl Fisheries @ 75%" && score_value == "0.001") {
      # Special handling for trawl fisheries
      temp_data <- filtered_dataset %>%
        st_drop_geometry() %>%
        select(CellID_2km, !!score_column) %>%
        mutate(!!score_column := ifelse(!is.na(.data[[score_column]]) & .data[[score_column]] != 0, 
                                        as.numeric(score_value), 
                                        NA_real_))
    } else {
      # For discrete scores, use the filtered dataset that includes 1's for unselected cells
      temp_data <- filtered_dataset %>%
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