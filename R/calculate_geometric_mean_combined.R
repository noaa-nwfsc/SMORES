# Calculate geometric mean across all score columnsfor combined map
calculate_geometric_mean <- function(combined_data) {
  # Find all score columns
  score_cols <- names(combined_data)[grep("^Score\\.", names(combined_data))]
  
  if(length(score_cols) > 0) {
    # Convert to numeric and calculate row-wise geometric mean
    score_matrix <- as.matrix(combined_data[score_cols])
    score_matrix <- apply(score_matrix, 2, as.numeric)
    
    combined_data$Geo_mean <- apply(score_matrix, 1, function(x) {
      if(all(is.na(x))) return(NA)
      # Handle case where all values are the same
      if(length(unique(x[!is.na(x)])) <= 1) {
        return(x[!is.na(x)][1])  # Return the single unique value
      }
      exp(mean(log(x), na.rm = TRUE))
    })
    
    # Filter out rows where geometric mean is NA or 0
    combined_data <- combined_data[!is.na(combined_data$Geo_mean) & combined_data$Geo_mean > 0, ]
  }
  
  return(combined_data)
}