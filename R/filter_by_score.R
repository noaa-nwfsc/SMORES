# filter dataframes by score 
filter_by_score <- function(df, selected_score, base_grid = NULL, layer_name = NULL) {
  
  if(is.null(df) || is.null(selected_score) || selected_score == "None") {
    return(df)
  }
  
  # Define layers not included - CHECK THIS FIRST before any processing
  excluded_layers <- c("Trawl Fisheries @ 75%")
  
  # Return unchanged data immediately for excluded layers
  if(!is.null(layer_name) && layer_name %in% excluded_layers) {
    return(df)
  }
  
  # Special handling for Z Membership selection
  if(selected_score == "Z Membership") {
    # For Z Membership, return all data since it's already the correct dataset
    # and we want to show the continuous Z membership values
    return(df)
  }
  
  # Find the score column for each selected layer
  score_cols <- names(df)[grep("^Score\\.", names(df))]
  
  if(length(score_cols) == 0) {
    warning("No score columns found in the dataset")
    return(df)
  }
  
  # Special handling for Ranked Importance selection
  if(selected_score == "Ranked Importance") {
    # For Ranked Importance, we want to keep all the data (no filtering)
    # but still do the grid completion to fill NAs with 1s
    filtered_df <- df  # Keep all rows
    
  } else {
    # Original filtering logic for discrete scores
    
    # Create a filter condition for selected score
    rows_to_keep <- rep(FALSE, nrow(df))
    
    for(col in score_cols) {
      col_values <- as.character(df[[col]])
      matches <- col_values == as.character(selected_score)
      matches[is.na(matches)] <- FALSE  # Handle NAs
      rows_to_keep <- rows_to_keep | matches
    }
    
    # Get the filtered dataframe (selected cells only)
    filtered_df <- df[rows_to_keep, ]
  }
  
  # If base_grid and layer_name are provided, add 1s to unselected cells
  # Note: This section will not execute for excluded layers since they return early
  if(!is.null(base_grid) && !is.null(layer_name)) {
    
    # Left join the filtered data to the base grid
    if("CellID_2km" %in% names(filtered_df) && "CellID_2km" %in% names(base_grid)) {
      
      # Join the filtered data to the complete grid using regular data frames
      complete_data <- base_grid %>%
        st_drop_geometry() %>%  # Remove geometry for the join
        left_join(filtered_df %>% st_drop_geometry(), by = "CellID_2km")
      
      # Add back the geometry from the base_grid
      complete_data <- base_grid %>%
        select(CellID_2km) %>%  # Keep only ID and geometry
        left_join(complete_data, by = "CellID_2km")
      
      # For each score column, fill NA values (unselected cells) with 1
      for(col in score_cols) {
        if(col %in% names(complete_data)) {
          complete_data[[col]][is.na(complete_data[[col]])] <- 1
        }
      }
      
      # Return the complete dataset
      return(complete_data)
    }
  }
  
  # Return the filtered dataframe (original behavior when no grid completion is requested)
  return(filtered_df)
}