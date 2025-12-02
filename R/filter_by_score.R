# filter dataframes by score 
filter_by_score <- function(df, selected_score, base_grid = NULL, layer_name = NULL) {
  if(is.null(df) || is.null(selected_score) || selected_score == "None") {
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
  
  # Create a filter condition for selected score
  rows_to_keep <- rep(FALSE, nrow(df))
  
  for(col in score_cols) {
    rows_to_keep <- rows_to_keep | (as.character(df[[col]]) == as.character(selected_score))
  }
  
  # Get the filtered dataframe (selected cells only)
  filtered_df <- df[rows_to_keep, ]
  
  # If base_grid and layer_name are provided, add 1s to unselected cells
  if(!is.null(base_grid) && !is.null(layer_name)) {
    
    # Define layers not included
    excluded_layers <- c("Deep Sea Coral Robust High Suitability", "Trawl Fisheries @ 75%")
    
    # Skip transformation for excluded layers
    if(!layer_name %in% excluded_layers) {
      
      # Left join the filtered data to the base grid
      # This assumes both datasets have GRID_ID as the joining column
      if("CellID_2km" %in% names(filtered_df) && "CellID_2km" %in% names(base_grid)) {
        
        # Join the filtered data to the complete grid using regular data frames
        complete_data <- base_grid_df %>%
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
  }
  
  # Return the filtered dataframe (original behavior when no grid completion is requested)
  return(filtered_df)
}