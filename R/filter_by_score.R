# filter dataframes by score 
filter_by_score <- function(df, selected_score, base_grid = NULL, layer_name = NULL) {
  
  # 1. Quick Checks
  if(is.null(df) || is.null(selected_score) || selected_score == "None") {
    return(df)
  }
  
  # 2. Excluded Layers (Return immediately)
  excluded_layers <- c("Trawl Fisheries @ 75%")
  if(!is.null(layer_name) && layer_name %in% excluded_layers) {
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