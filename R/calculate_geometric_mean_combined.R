calculate_geometric_mean <- function(combined_data) {
  
  score_cols <- names(combined_data)[grep("^Score\\.", names(combined_data))]
  
  if(length(score_cols) > 0) {
    
    # 1. Extract just the score values (Drop geometry for speed)
    if("sf" %in% class(combined_data)) {
      score_df <- sf::st_drop_geometry(combined_data)[, score_cols, drop = FALSE]
    } else {
      score_df <- combined_data[, score_cols, drop = FALSE]
    }
    
    # Convert to numeric matrix for math operations
    score_matrix <- as.matrix(score_df)
    mode(score_matrix) <- "numeric" # Ensure purely numeric type
    
    # --- VECTORIZED CALCULATION (Instant) ---
    
    # --- NEW: Treat NA as 1 (Neutral) ---
    # This replaces the old "Grid Completion" logic but is 100x faster
    score_matrix[is.na(score_matrix)] <- 1
    
    # 2. Identify Special Cases (Zeroes and Negatives)
    # Using rowSums is much faster than checking each row in a loop
    has_zeros <- rowSums(score_matrix == 0, na.rm = TRUE) > 0
    has_neg   <- rowSums(score_matrix < 0, na.rm = TRUE) > 0
    
    # 3. Calculate Logarithms
    # suppressWarnings hides warnings about log(0) producing -Inf
    log_matrix <- suppressWarnings(log(score_matrix))
    
    # Replace infinite/NaN values (from 0 or negative inputs) with NA
    # so they don't break the mean calculation
    log_matrix[is.infinite(log_matrix) | is.nan(log_matrix)] <- NA
    
    # 4. Calculate Mean of Logs (Vectorized)
    # This is the "heavy lifting," done instantly by rowMeans
    mean_log <- rowMeans(log_matrix, na.rm = TRUE)
    
    # 5. Exponentiate to get Geometric Mean
    geo_mean <- exp(mean_log)
    
    # 6. Re-apply Special Case Logic
    # If a row had a 0, the Geometric Mean is mathematically 0
    geo_mean[has_zeros] <- 0
    # If a row had a negative number, the Geometric Mean is undefined
    geo_mean[has_neg] <- NA
    
    # Assign result back to the main data
    combined_data$Geo_mean <- geo_mean
    
    # Filter out NAs (rows that couldn't be calculated)
    combined_data <- combined_data[!is.na(combined_data$Geo_mean), ]
    
  } else {
    cat("No score columns found - returning original data\n")
  }
  
  return(combined_data)
}