# Calculate lowest value across all score columns
calculate_lowest_value <- function(combined_data) {
  
  # Find all score columns (excluding geometry and ID columns)
  score_cols <- names(combined_data)[grep("^Score\\.", names(combined_data))]
  
  if(length(score_cols) > 0) {
    
    # 1. Extract just the score values (Drop geometry for speed)
    if("sf" %in% class(combined_data)) {
      score_df <- sf::st_drop_geometry(combined_data)[, score_cols, drop = FALSE]
    } else {
      score_df <- combined_data[, score_cols, drop = FALSE]
    }
    
    # --- VECTORIZED CALCULATION (Instant) ---
    
    # We use do.call(pmin, ...) to pass all columns as arguments to pmin at once.
    # pmin compares Index 1 of Col A vs Index 1 of Col B, etc.
    
    # Prepare arguments list from the dataframe columns
    args_list <- as.list(score_df)
    
    # Add na.rm = TRUE to ignore NAs during comparison
    args_list$na.rm <- TRUE
    
    # Execute pmin efficiently across all columns
    # This replaces the entire 'apply' loop
    combined_data$Lowest_value <- do.call(pmin, args_list)
    
    # Filter out rows where lowest value is NA (meaning all inputs were NA)
    combined_data <- combined_data[!is.na(combined_data$Lowest_value), ]
  }
  
  return(combined_data)
}