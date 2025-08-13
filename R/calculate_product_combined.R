# Calculate product across all score columns
calculate_product <- function(combined_data) {
  # Find all score columns
  score_cols <- names(combined_data)[grep("^Score\\.", names(combined_data))]
  
  if(length(score_cols) > 0) {
    # Convert to numeric and calculate row-wise product
    score_matrix <- as.matrix(combined_data[score_cols])
    score_matrix <- apply(score_matrix, 2, as.numeric)
    combined_data$Product_value <- apply(score_matrix, 1, function(x) {
      if(all(is.na(x))) return(NA)
  
      prod(x, na.rm = TRUE)
    })
    
    # Filter out rows where geometric mean is NA or 0
    combined_data <- combined_data[!is.na(combined_data$Product_value), ]
  }
  
  return(combined_data)
}