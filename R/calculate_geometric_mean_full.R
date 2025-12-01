calculate_geometric_mean_full <- function(submodels, weights, base_grid) {
  tryCatch({
    
    # --- 1. Weight Preparation ---
    if(length(submodels) == 0) {
      stop("No submodels provided for suitability calculation.")
    }
    
    # Store raw weights used in the model construction as a named vector for easy lookup
    weight_vector <- unlist(weights)
    
    # --- 2. Calculate Weighted Components ---
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
      
      # DEBUG: Check the input data
      cat("Processing submodel:", submodel_name, "with weight:", weight, "\n")
      geo_mean_values <- submodel_data$Geo_mean[!is.na(submodel_data$Geo_mean)]
      cat("Non-NA Geo_mean values in", submodel_name, ":", length(geo_mean_values), "\n")
      if(length(geo_mean_values) > 0) {
        cat("Geo_mean range:", min(geo_mean_values), "to", max(geo_mean_values), "\n")
      }
      
      # Weighted component: C_i = x_i ^ W_i (W_i is the raw weight)
      weighted_col_name <- paste0("Weighted_", submodel_name)
      
      temp_data <- submodel_data %>%
        sf::st_drop_geometry() %>%
        dplyr::select(CellID_2km, Geo_mean) %>%
        dplyr::mutate(
          !!weighted_col_name := case_when(
            is.na(Geo_mean) ~ NA_real_,
            Geo_mean == 0 ~ 0,
            Geo_mean > 0 ~ Geo_mean^weight,
            TRUE ~ NA_real_
          )
        ) %>%
        dplyr::select(CellID_2km, !!weighted_col_name)
      
      # DEBUG: Check the weighted values
      weighted_values <- temp_data[[weighted_col_name]][!is.na(temp_data[[weighted_col_name]])]
      cat("Non-NA weighted values in", weighted_col_name, ":", length(weighted_values), "\n")
      if(length(weighted_values) > 0) {
        cat("Weighted range:", min(weighted_values), "to", max(weighted_values), "\n")
      }
      
      # Join with combined data
      combined_data <- dplyr::left_join(combined_data, temp_data, by = "CellID_2km")
      
      weighted_columns <- c(weighted_columns, weighted_col_name)
    }
    
    cat("Total weighted columns:", length(weighted_columns), "\n")
    cat("Weighted columns:", paste(weighted_columns, collapse = ", "), "\n")
    
    # --- 3. Calculate Overall Suitability (Weighted Geometric Mean) ---
    if(length(weighted_columns) > 0) {
      
      cat("Starting Overall_Geo_mean calculation...\n")
      
      # For single submodel case, just copy the weighted values directly
      if(length(weighted_columns) == 1) {
        cat("Single submodel detected - using direct assignment\n")
        cat("Checking weighted column:", weighted_columns[1], "\n")
        
        # Check if the weighted column exists and has data
        if(weighted_columns[1] %in% names(combined_data)) {
          cat("Weighted column exists in data\n")
          weighted_col_values <- combined_data[[weighted_columns[1]]][!is.na(combined_data[[weighted_columns[1]]])]
          cat("Non-NA values in weighted column:", length(weighted_col_values), "\n")
          if(length(weighted_col_values) > 0) {
            cat("Range of weighted values:", min(weighted_col_values), "to", max(weighted_col_values), "\n")
          }
        } else {
          cat("ERROR: Weighted column not found in combined_data\n")
        }
        
        # Direct assignment for single submodel
        combined_data$Overall_Geo_mean <- combined_data[[weighted_columns[1]]]
        
        # Verify assignment worked
        final_check <- combined_data$Overall_Geo_mean[!is.na(combined_data$Overall_Geo_mean)]
        cat("After assignment - Non-NA Overall_Geo_mean values:", length(final_check), "\n")
        
      } else {
        # Multiple submodels - use proper weighted geometric mean
        cat("Multiple submodels detected - calculating weighted geometric mean\n")
        
        # Get all weighted component columns as a matrix
        weight_matrix <- combined_data %>%
          sf::st_drop_geometry() %>%
          dplyr::select(all_of(weighted_columns)) %>%
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
      
      # DEBUG: Check final results
      final_values <- combined_data$Overall_Geo_mean[!is.na(combined_data$Overall_Geo_mean)]
      cat("Final non-NA Overall_Geo_mean values:", length(final_values), "\n")
      if(length(final_values) > 0) {
        cat("Final range:", min(final_values), "to", max(final_values), "\n")
      } else {
        cat("ERROR: No final values - investigating...\n")
        cat("Combined data structure:\n")
        print(str(combined_data))
      }
      
    } else {
      # No valid submodels processed
      combined_data$Overall_Geo_mean <- NA_real_
      cat("No weighted columns created - setting all Overall_Geo_mean to NA\n")
    }
    
    # Return the combined data
    return(combined_data)
    
  }, error = function(e) {
    cat("ERROR in calculate_geometric_mean_full:", e$message, "\n")
    stop(paste("Error during suitability calculation:", e$message))
  })
}