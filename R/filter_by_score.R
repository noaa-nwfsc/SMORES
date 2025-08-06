# filter dataframes by score 
filter_by_score <- function(df, selected_score) {
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
  
  # Create a filter condition for the selected score
  rows_to_keep <- rep(FALSE, nrow(df))
  
  for(col in score_cols) {
    # Convert both to character for comparison to handle numeric vs character issues
    rows_to_keep <- rows_to_keep | (as.character(df[[col]]) == as.character(selected_score))
  }
  
  # Return the filtered dataframe
  filtered_df <- df[rows_to_keep, ]
  
  return(filtered_df)
}