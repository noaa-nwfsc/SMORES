# Calculate product across all score columns
calculate_product_value <- function(combined_data) {
  
  # Find all score columns
  score_cols <- names(combined_data)[grep("^Score\\.", names(combined_data))]
  
  if(length(score_cols) > 0) {
    
    # Extract score values
    if("sf" %in% class(combined_data)) {
      score_df <- sf::st_drop_geometry(combined_data)[, score_cols, drop = FALSE]
    } else {
      score_df <- combined_data[, score_cols, drop = FALSE]
    }
    
    # Initialize a result vector with 1s (The identity value for multiplication)
    # Rows that have no data start and end as 1
    n_rows <- nrow(score_df)
    result_vector <- rep(1, n_rows)
    
    # Loop over columns
    for(col in names(score_df)) {
      vals <- score_df[[col]]
      
      # Force to numeric
      if(is.list(vals)) vals <- as.numeric(unlist(vals))
      if(!is.numeric(vals)) vals <- as.numeric(vals)
      
      # Treat NAs as 1's
      vals[is.na(vals)] <- 1
      
      # Vectorized multiplication of the whole column at once
      result_vector <- result_vector * vals
    }
    
    # Assign the result back to the main dataset
    combined_data$Product_value <- result_vector
    
  }
  
  return(combined_data)
}