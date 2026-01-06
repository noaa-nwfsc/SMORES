calculate_geometric_mean_full <- function(submodels, weights, base_grid) {
  
  # Weight Preparation
  if(length(submodels) == 0) {
    stop("No submodels provided for suitability calculation.")
  }
  
  # Store raw weights used in the model construction as a named vector for easy lookup
  weight_vector <- unlist(weights)
  
  # Calculate Weighted Components
  combined_data <- base_grid
  weighted_columns <- c()
  
  for(i in seq_along(submodels)) {
    submodel_name <- names(submodels)[i]
    submodel_data <- submodels[[i]]
    
    # Use the raw input weight directly
    weight <- weight_vector[[submodel_name]] 
    
    if(is.null(submodel_data) || !"Geo_mean" %in% names(submodel_data)) {
      warning(paste("Submodel", submodel_name, "is missing or lacks 'Geo_mean'. Skipping."))
      next
    }
    
    # Weighted component: C_i = x_i ^ W_i (W_i is the raw weight)
    weighted_col_name <- paste0("Weighted_", submodel_name)
    
    temp_data <- submodel_data %>%
      st_drop_geometry() %>%
      select(CellID_2km, Geo_mean) %>%
      mutate(
        !!weighted_col_name := case_when(
          is.na(Geo_mean) ~ NA_real_,
          Geo_mean == 0 ~ 0,
          Geo_mean > 0 ~ Geo_mean^weight,
          TRUE ~ NA_real_
        )
      ) %>%
      select(CellID_2km, !!weighted_col_name)
    
    # Join with combined data
    combined_data <- dplyr::left_join(combined_data, temp_data, by = "CellID_2km")
    
    weighted_columns <- c(weighted_columns, weighted_col_name)
  }
  
  # Calculate Overall Suitability (Weighted Geometric Mean)
  if(length(weighted_columns) > 0) {
    
    # For single submodel case, just copy the weighted values directly
    if(length(weighted_columns) == 1) {
      # Direct assignment for single submodel
      combined_data$Overall_Geo_mean <- combined_data[[weighted_columns[1]]]
      
    } else {
      # Multiple submodels - use weighted geometric mean
      # Get all weighted component columns as a matrix
      weight_matrix <- combined_data %>%
        st_drop_geometry() %>%
        select(all_of(weighted_columns)) %>%
        as.matrix()
      
      # Calculate geometric mean row by row
      combined_data$Overall_Geo_mean <- apply(weight_matrix, 1, function(row) {
        non_na_vals <- row[!is.na(row)]
        
        if(length(non_na_vals) == 0) {
          return(NA_real_)
        } else if(any(non_na_vals == 0)) {
          return(0)
        } else {
          # Get corresponding weights for non-NA values
          non_na_indices <- which(!is.na(row))
          contributing_cols <- weighted_columns[non_na_indices]
          contributing_submodel_names <- gsub("Weighted_", "", contributing_cols)
          participating_weights <- weight_vector[contributing_submodel_names]
          sum_weights <- sum(participating_weights, na.rm = TRUE)
          
          if(sum_weights > 0) {
            product_P <- prod(non_na_vals)
            if(is.finite(product_P) && product_P > 0) {
              result <- product_P^(1 / sum_weights)
              if(is.finite(result)) return(result) else return(NA_real_)
            }
          }
          return(NA_real_)
        }
      })
    }
  } else {
    # No valid submodels processed
    combined_data$Overall_Geo_mean <- NA_real_
  }
  
  # Return the combined data
  return(combined_data)
}