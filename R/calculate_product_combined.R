# Calculate product across all score columns
calculate_product_value <- function(combined_data) {
  
  # Find all score columns (excluding geometry and ID columns)
  score_cols <- names(combined_data)[grep("^Score\\.", names(combined_data))]
  
  if(length(score_cols) > 0) {
    
    # Extract score values (Drop geometry for speed)
    if("sf" %in% class(combined_data)) {
      score_df <- sf::st_drop_geometry(combined_data)[, score_cols, drop = FALSE]
    } else {
      score_df <- combined_data[, score_cols, drop = FALSE]
    }
    
    # --- VECTORIZED CALCULATION (Column-wise) ---
    
    # Initialize a result vector with 1s (The identity value for multiplication)
    # If we started with 0, everything would become 0.
    n_rows <- nrow(score_df)
    result_vector <- rep(1, n_rows)
    
    # Identify rows that are completely empty (All NAs)
    # We want these to be NA at the end, not 1.
    # rowSums is highly optimized C-code
    has_data_mask <- rowSums(!is.na(score_df)) > 0
    
    # Loop over COLUMNS (Fast, because there are few columns)
    for(col in names(score_df)) {
      vals <- score_df[[col]]
      
      # Ensure numeric (handle potential list-columns or characters)
      if(is.list(vals)) vals <- as.numeric(unlist(vals))
      if(!is.numeric(vals)) vals <- as.numeric(vals)
      
      # Handle NAs: Treat them as 1 so they don't affect the product
      # (e.g., 0.5 * NA becomes 0.5 * 1 = 0.5)
      vals[is.na(vals)] <- 1
      
      # Vectorized multiplication of the whole column at once
      result_vector <- result_vector * vals
    }
    
    # If a row had NO data (all NAs), set the result to NA (instead of the initialized 1)
    result_vector[!has_data_mask] <- NA
    
    # Assign the result back to the main dataset
    combined_data$Product_value <- result_vector
    
    # Filter out rows where the result is NA
    combined_data <- combined_data[!is.na(combined_data$Product_value), ]
  }
  
  return(combined_data)
}