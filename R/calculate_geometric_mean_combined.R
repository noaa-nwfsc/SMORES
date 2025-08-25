calculate_geometric_mean <- function(combined_data) {
  
  score_cols <- names(combined_data)[grep("^Score\\.", names(combined_data))]
  
  if(length(score_cols) > 0) {
    score_matrix <- as.matrix(combined_data[score_cols])
    score_matrix <- apply(score_matrix, 2, as.numeric)
    
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
    
    combined_data$Geo_mean <- apply(score_matrix, 1, robust_geomean)
    
    # Keep all valid results including 0 - don't filter out 0 values
    combined_data <- combined_data[!is.na(combined_data$Geo_mean), ]
    
  }
  
  return(combined_data)
}