calculate_lowest_value <- function(combined_data) {
  
  score_cols <- names(combined_data)[grep("^Score\\.", names(combined_data))]
  
  if(length(score_cols) > 0) {
    
    strict_cols <- attr(combined_data, "strict_na_cols")
    
    if("sf" %in% class(combined_data)) {
      score_df <- sf::st_drop_geometry(combined_data)[, score_cols, drop = FALSE]
    } else {
      score_df <- combined_data[, score_cols, drop = FALSE]
    }
    
    # Hybrid Imputation
    score_df[] <- lapply(names(score_df), function(col_name) {
      x <- score_df[[col_name]]
      if(is.list(x)) x <- unlist(x)
      x <- as.numeric(x)
      
      # Only fill NAs with 1 if it is NOT a strict column
      if (!col_name %in% strict_cols) {
        x[is.na(x)] <- 1
      }
      return(x)
    })
    
    # Use na.rm = TRUE to ignore the Strict NAs (masked layers)
    combined_data$Lowest_value <- do.call(pmin, c(score_df, list(na.rm = TRUE)))
    
    # Filter out rows that are entirely NA
    combined_data <- combined_data[!is.na(combined_data$Lowest_value) & !is.infinite(combined_data$Lowest_value), ]
  }
  
  return(combined_data)
}