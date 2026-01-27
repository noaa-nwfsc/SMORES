calculate_product_value <- function(combined_data) {
  
  score_cols <- names(combined_data)[grep("^Score\\.", names(combined_data))]
  
  if(length(score_cols) > 0) {
    
    if("sf" %in% class(combined_data)) {
      score_df <- sf::st_drop_geometry(combined_data)[, score_cols, drop = FALSE]
    } else {
      score_df <- combined_data[, score_cols, drop = FALSE]
    }
    
    # Identify rows that have at least SOME data
    all_na_mask <- rowSums(!is.na(score_df)) == 0
    
    # Vectorized Product
    n_rows <- nrow(score_df)
    result_vector <- rep(1, n_rows)
    
    for(col in names(score_df)) {
      vals <- score_df[[col]]
      if(is.list(vals)) vals <- as.numeric(unlist(vals))
      if(!is.numeric(vals)) vals <- as.numeric(vals)
      
      # Treat NA as 1 for Product Logic
      vals[is.na(vals)] <- 1
      result_vector <- result_vector * vals
    }
    
    combined_data$Product_value <- result_vector
  }
  
  return(combined_data)
}