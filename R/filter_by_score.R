# filter dataframes by score 
filter_by_score <- function(df, selected_score, base_grid = NULL, layer_name = NULL) {
  
  # 1. Quick Checks
  if(is.null(df) || is.null(selected_score) || selected_score == "None") {
    return(df)
  }
  
  # 2. Excluded Layers (Trawl Fisheries)
  # These layers skip the standard score filtering because we want to keep all their values
  # (Preventing the "filling of 1s" that happens when we drop rows).
  excluded_layers <- c("Trawl Fisheries @ 75%")
  
  if(!is.null(layer_name) && layer_name %in% excluded_layers) {
    
    # --- NEW: CROP TO AOI ---
    # We use the base_grid (which is the AOI) to filter the rows of the Trawl data.
    
    if(!is.null(base_grid) && "CellID_2km" %in% names(base_grid) && "CellID_2km" %in% names(df)) {
      
      # Get the list of Cell IDs that exist in the AOI
      valid_ids <- base_grid$CellID_2km
      
      # Filter the Trawl data to keep ONLY rows that are in the AOI
      # This performs the crop without changing the scores
      if("sf" %in% class(df)) {
        df <- df[df$CellID_2km %in% valid_ids, ]
      } else {
        df <- df[df$CellID_2km %in% valid_ids, ]
      }
    }
    
    # Return the cropped, but UNFILTERED data.
    # This preserves real zeros and scores, satisfying "should not have 1s filled in".
    return(df)
  }
  
  # 3. Z Membership (Return all data)
  if(selected_score == "Z Membership") {
    return(df)
  }
  
  # 4. Ranked Importance (Return all data)
  if(selected_score == "Ranked Importance") {
    return(df)
  }
  
  # 5. Standard Filtering Logic
  # Find score columns
  score_cols <- names(df)[grep("^Score\\.", names(df))]
  
  if(length(score_cols) == 0) {
    warning("No score columns found in the dataset")
    return(df)
  }
  
  # Create filter condition
  rows_to_keep <- rep(FALSE, nrow(df))
  
  for(col in score_cols) {
    col_values <- as.character(df[[col]])
    matches <- col_values == as.character(selected_score)
    matches[is.na(matches)] <- FALSE
    rows_to_keep <- rows_to_keep | matches
  }
  
  # 6. CRITICAL OPTIMIZATION: Return Sparse Data
  # We return ONLY the matching rows. 
  # We do NOT join to the base_grid or fill with 1s here.
  # This keeps the dataframe tiny and fast.
  filtered_df <- df[rows_to_keep, ]
  
  return(filtered_df)
}