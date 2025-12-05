calculate_geometric_mean <- function(combined_data) {
  
  score_cols <- names(combined_data)[grep("^Score\\.", names(combined_data))]
  
  if(length(score_cols) > 0) {
    
    # IMPORTANT: For sf objects, we need to extract just the score values (not geometry) for calculation
    if("sf" %in% class(combined_data)) {
      # Extract just the score columns as a regular data frame for matrix operations
      score_values_df <- combined_data %>%
        sf::st_drop_geometry() %>%
        select(all_of(score_cols))
    } else {
      score_values_df <- combined_data[, score_cols, drop = FALSE]
    }
    
    # Convert to matrix with debugging
    score_matrix <- as.matrix(score_values_df)
    
    # Ensure all columns are numeric
    score_matrix <- apply(score_matrix, 2, function(col) {
      if(is.list(col)) {
        as.numeric(unlist(col))
      } else {
        as.numeric(col)
      }
    })
    
    # Ensure proper matrix structure for single column case
    if(length(score_cols) == 1) {
      score_matrix <- matrix(score_matrix, ncol = 1)
      colnames(score_matrix) <- score_cols
    }
    
    # Robust geometric mean function that handles 0 values
    robust_geomean <- function(x) {
      x <- x[!is.na(x)]  # Remove NAs
      
      if(length(x) == 0) {
        return(NA)
      }
      
      if(any(x < 0)) {
        return(NA)  # Geometric mean undefined for negative numbers
      }
      
      if(any(x == 0)) {
        return(0)  # If any value is 0, geometric mean is 0
      }
      
      if(length(unique(x)) == 1) {
        return(x[1])  # All values the same
      }
      
      # Check if all values are positive before log transformation
      if(!all(x > 0)) {
        return(NA)
      }
      
      # Safe geometric mean calculation for positive values only
      log_values <- log(x)
      
      if(any(is.infinite(log_values))) {
        return(NA)
      }
      
      mean_log <- mean(log_values)
      result <- exp(mean_log)
      
      return(result)
    }
    
    # Calculate geometric mean and add it to the spatial data
    combined_data$Geo_mean <- apply(score_matrix, 1, robust_geomean)
    
    # Keep all valid results including 0 - don't filter out 0 values
    original_rows <- nrow(combined_data)
    combined_data <- combined_data[!is.na(combined_data$Geo_mean), ]
    final_rows <- nrow(combined_data)
    
  } else {
    cat("No score columns found - returning original data\n")
  }

  return(combined_data)
}