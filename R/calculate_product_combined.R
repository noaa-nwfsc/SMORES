# Calculate product across all score columns
calculate_product_value <- function(combined_data) {
  
  # Find all score columns (excluding geometry and ID columns)
  score_cols <- names(combined_data)[grep("^Score\\.", names(combined_data))]
  
  if(length(score_cols) > 0) {
    
    # extract just the score values (not geometry) for calculation
    if("sf" %in% class(combined_data)) {
      # extract just the score columns as a regular data frame for matrix operations
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
     combined_data$Product_value <- apply(score_matrix, 1, function(x) {
      if(all(is.na(x))) return(NA)
  
      prod(x, na.rm = TRUE)
    })
    
    # Filter out rows where geometric mean is NA
    combined_data <- combined_data[!is.na(combined_data$Product_value), ]
  }
  
  return(combined_data)
}