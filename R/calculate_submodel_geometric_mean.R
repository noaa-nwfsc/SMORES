calculate_submodel_geometric_mean <- function(combined_data, submodel_type = NULL) {
  # Find all score columns - updated pattern to match both dots and underscores
  score_cols <- names(combined_data)[grep("^Score[._]", names(combined_data))]
  
  if(length(score_cols) > 0) {
    # Special handling for fisheries submodel
    if(!is.null(submodel_type) && submodel_type == "fisheries") {
      
      # Check if we have both fisheries and trawl fisheries components
      fisheries_cols <- score_cols[grep("fisheries", score_cols, ignore.case = TRUE)]
      trawl_cols <- score_cols[grep("trawl", score_cols, ignore.case = TRUE)]
      
      if(length(trawl_cols) > 0 && length(fisheries_cols) > 0) {
        # Trawl fisheries is present - use special logic
        
        # Get the trawl fisheries score
        trawl_score_col <- trawl_cols[1]  # Assume first trawl column
        
        # Get the general fisheries score  
        general_fisheries_col <- fisheries_cols[!fisheries_cols %in% trawl_cols][1]
        
        if(!is.na(general_fisheries_col)) {
          # Create the combined score:
          trawl_values <- as.numeric(combined_data[[trawl_score_col]])  # Use trawl score where trawl data exists and is not NA/0
          fisheries_values <- as.numeric(combined_data[[general_fisheries_col]]) # Use fisheries score everywhere else
          
          combined_data$Geo_mean <- ifelse(
            !is.na(trawl_values) & trawl_values > 0,
            trawl_values,  # Use trawl score where available
            fisheries_values  # Use fisheries score as background
          )
          
          # Remove rows where the final score is NA or 0
          combined_data <- combined_data[!is.na(combined_data$Geo_mean) & combined_data$Geo_mean > 0, ]
          
          return(combined_data)
        }
      }
    }
    
    # Standard geometric mean calculation for all other cases
    # Convert to numeric and calculate row-wise geometric mean
    score_matrix <- as.matrix(combined_data[score_cols])
    score_matrix <- apply(score_matrix, 2, as.numeric)
    
    combined_data$Geo_mean <- apply(score_matrix, 1, function(x) {
      if(all(is.na(x))) return(NA)
      # Filter out zero values for geometric mean calculation
      valid_values <- x[!is.na(x)]
      if(length(valid_values) == 0) return(NA)
      
      # Handle case where all values are the same
      if(length(unique(valid_values)) <= 1) {
        return(valid_values[1])  # Return the single unique value
      }
      exp(mean(log(valid_values), na.rm = TRUE))
    })
    
    # Filter out rows where geometric mean is NA or 0
    combined_data <- combined_data[!is.na(combined_data$Geo_mean), ]
    
  } else {
    cat("WARNING: No score columns found. Looking for columns with pattern '^Score[._]'\n")
    cat("Available columns:", paste(names(combined_data), collapse = ", "), "\n")
  }
  
  return(combined_data)
}