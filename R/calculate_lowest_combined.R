# Calculate lowest value across all score columns
calculate_lowest_value <- function(combined_data) {
  
  # Find all score columns
  score_cols <- names(combined_data)[grep("^Score\\.", names(combined_data))]
  
  if(length(score_cols) > 0) {
    
    # Extract score values without geometry
    if("sf" %in% class(combined_data)) {
      score_df <- sf::st_drop_geometry(combined_data)[, score_cols, drop = FALSE]
    } else {
      score_df <- combined_data[, score_cols, drop = FALSE]
    }
    
    # --- VECTORIZED CALCULATION (Instant) ---
    
    # We iterate through columns to ensure they are numeric and fill NAs with 1.
    # This treats missing data as 1
    score_df[] <- lapply(score_df, function(x) {
      # Handle potential list-columns from parquet/sf logic
      if(is.list(x)) x <- unlist(x)
      
      # Ensure numeric
      x <- as.numeric(x)
      
      # Replace NA with 1
      x[is.na(x)] <- 1
      return(x)
    })
    
    # Execute pmin efficiently
    # pmin compares Index 1 of Col A vs Index 1 of Col B, etc.
    combined_data$Lowest_value <- do.call(pmin, score_df)
  }
  
  return(combined_data)
}