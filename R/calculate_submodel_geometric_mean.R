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
    
        # Get the trawl fisheries score
        trawl_score_col <- trawl_cols[1]  # Assume first trawl column
        
        # Get the general fisheries score  
        general_fisheries_col <- fisheries_cols[!fisheries_cols %in% trawl_cols][1]
  
        if(!is.na(general_fisheries_col)) {
          
          # Create the combined score:
          trawl_values <- combined_data[[trawl_score_col]]
          fisheries_values <- combined_data[[general_fisheries_col]]
      
          # Convert to numeric with error checking
          trawl_values_orig <- trawl_values
          fisheries_values_orig <- fisheries_values
          
          trawl_values <- as.numeric(trawl_values)
          fisheries_values <- as.numeric(fisheries_values)
          
          combined_data$Geo_mean <- ifelse(
            !is.na(trawl_values) & trawl_values > 0,
            trawl_values,  # Use trawl score where available
            fisheries_values  # Use fisheries score as background
          )
          
          # Keep rows with 0 values
          combined_data <- combined_data[!is.na(combined_data$Geo_mean), ]
          return(combined_data)
        } else {
          cat("WARNING: General fisheries column is NA, falling back to standard processing\n")
        }
      } else {
        cat("DEBUG: Not both trawl and fisheries columns found, using standard processing\n")
      }
    }
    
    # More robust filtering for numeric score columns - exclude geometry columns explicitly
    numeric_score_cols <- score_cols[sapply(score_cols, function(col) {
      # Check if column exists and is not a geometry column
      if(!col %in% names(combined_data)) return(FALSE)
      if(col == "Shape" || col == "geometry") return(FALSE)
      
      # Check if it's numeric or can be converted to numeric
      col_data <- combined_data[[col]]
      return(is.numeric(col_data) || 
               (!inherits(col_data, c("sfc", "sfc_POLYGON", "sfc_MULTIPOLYGON", "sfc_POINT", "sfc_MULTIPOINT", "sfc_LINESTRING", "sfc_MULTILINESTRING")) &&
                  !all(is.na(suppressWarnings(as.numeric(col_data))))))
    })]
    
    if(length(numeric_score_cols) == 0) {
      cat("ERROR: No numeric score columns found\n")
      return(combined_data)
    }
    
    # Extract numeric data from the valid score columns
    score_data <- combined_data %>%
      st_drop_geometry() %>%
      select(all_of(numeric_score_cols))
    
    # Convert to matrix
    score_matrix <- as.matrix(score_data)
    
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
    original_rows <- nrow(combined_data)
    combined_data <- combined_data[!is.na(combined_data$Geo_mean), ]
    final_rows <- nrow(combined_data)
    
  } else {
    cat("WARNING: No score columns found. Looking for columns with pattern '^Score[._]'\n")
  }
  return(combined_data)
}