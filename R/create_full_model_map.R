#' Generate Overall Combined Model from Multiple Submodels
#'
#' This function creates an overall combined model by applying weights to 
#' submodel results and calculating their weighted geometric mean.
#'
#' @param submodels List of submodel spatial data
#' @param weights List of weights corresponding to each submodel
#' @param base_grid The base spatial grid to use for combining data
#' @return A list containing the combined data and the leaflet map
#'
create_full_model_map <- function(submodels, weights, base_grid = grid_test, aoi_data_reactive = NULL) {
  tryCatch({
    
    # Console output for debugging
    cat("=== STARTING create_full_model_map ===\n")
    cat("Number of submodels received:", length(submodels), "\n")
    cat("Submodel names:", names(submodels), "\n")
    cat("Number of weights received:", length(weights), "\n")
    cat("Weight names:", names(weights), "\n")
    cat("Weight values:", unlist(weights), "\n")
    
    # Initialize result structure
    result <- list(
      combined_data = NULL,
      map = NULL
    )
    
    if(length(submodels) == 0) {
      # No submodels - return empty map with message
      cat("ERROR: No submodels provided!\n")
      result$map <- leaflet() %>%
        addProviderTiles("Esri.OceanBasemap") %>%
        addControl("No submodels available for overall model generation.", position = "topright")
      
      return(result)
    }
    
    # Normalize weights to sum to 1
    total_weight <- sum(unlist(weights))
    cat("Total weight before normalization:", total_weight, "\n")
    
    if(total_weight > 0) {
      normalized_weights <- lapply(names(weights), function(name) {
        weights[[name]] / total_weight
      })
      names(normalized_weights) <- names(weights)
    } else {
      # If all weights are 0, assign equal weights
      equal_weight <- 1 / length(weights)
      normalized_weights <- lapply(names(weights), function(name) equal_weight)
      names(normalized_weights) <- names(weights)
    }
    
    cat("Normalized weights:", unlist(normalized_weights), "\n")
    
    # Start with base grid
    combined_data <- base_grid
    cat("Base grid rows:", nrow(combined_data), "\n")
    
    # Apply weights and combine submodels
    weighted_columns <- c()
    
    for(i in seq_along(submodels)) {
      submodel_name <- names(submodels)[i]
      submodel_data <- submodels[[i]]
      weight <- normalized_weights[[i]]
      
      cat("Processing submodel", i, ":", submodel_name, "\n")
      cat("  Weight:", weight, "\n")
      cat("  Submodel data rows:", ifelse(is.null(submodel_data), "NULL", nrow(submodel_data)), "\n")
      
      # Check if submodel has Geo_mean column
      if(is.null(submodel_data)) {
        cat("  ERROR: Submodel data is NULL. Skipping.\n")
        next
      }
      
      if(!"Geo_mean" %in% names(submodel_data)) {
        cat("  ERROR: Submodel", submodel_name, "does not have Geo_mean column. Available columns:", names(submodel_data), "\n")
        warning(paste("Submodel", submodel_name, "does not have Geo_mean column. Skipping."))
        next
      }
      
      # Create weighted column name
      weighted_col_name <- paste0("Weighted_", submodel_name)
      cat("  Creating weighted column:", weighted_col_name, "\n")
      
      # Extract geometric mean and apply weight
      temp_data <- submodel_data %>%
        st_drop_geometry() %>%
        select(CellID_2km, Geo_mean) %>%
        mutate(!!weighted_col_name := Geo_mean^weight) %>%
        select(CellID_2km, !!weighted_col_name)
      
      cat("  Temp data rows:", nrow(temp_data), "\n")
      cat("  Non-NA values in weighted column:", sum(!is.na(temp_data[[weighted_col_name]])), "\n")
      
      # Join with combined data
      combined_data <- left_join(combined_data, temp_data, by = "CellID_2km")
      
      # Track weighted columns for final calculation
      weighted_columns <- c(weighted_columns, weighted_col_name)
      cat("  Successfully added weighted column. Total weighted columns so far:", length(weighted_columns), "\n")
    }
    
    cat("Final weighted columns:", weighted_columns, "\n")
    
    # Calculate overall geometric mean from weighted components
    if(length(weighted_columns) > 0) {
      cat("Calculating overall geometric mean using", length(weighted_columns), "weighted columns\n")
      
      combined_data <- combined_data %>%
        rowwise() %>%
        mutate(
          Overall_Geo_mean = if(all(is.na(c_across(all_of(weighted_columns))))) {
            NA_real_
          } else {
            # Calculate geometric mean of weighted values
            non_na_values <- c_across(all_of(weighted_columns))[!is.na(c_across(all_of(weighted_columns)))]
            if(length(non_na_values) > 0) {
              # Check if any value is exactly 0 - if so, result should be 0
              if(any(non_na_values == 0)) {
                0
              } else if(all(non_na_values > 0)) {
                # Calculate geometric mean only if all values are positive
                exp(mean(log(non_na_values), na.rm = TRUE))
              } else {
                # If we have negative values, return NA
                NA_real_
              }
            } else {
              NA_real_
            }
          }
        ) %>%
        ungroup()
      
      # Report results
      overall_values <- combined_data$Overall_Geo_mean[!is.na(combined_data$Overall_Geo_mean)]
      cat("Overall calculation complete:\n")
      cat("  Non-NA overall values:", length(overall_values), "\n")
      if(length(overall_values) > 0) {
        cat("  Min overall value:", min(overall_values), "\n")
        cat("  Max overall value:", max(overall_values), "\n")
        cat("  Mean overall value:", mean(overall_values), "\n")
      }
    } else {
      cat("ERROR: No weighted columns available for calculation!\n")
    }
    
    aoi_data <- NULL
    if(!is.null(aoi_data_reactive)) {
      tryCatch({
        aoi_data <- aoi_data_reactive()
      }, error = function(e) {
        # If reactive fails, try to get AOI directly
        if(exists("AOI")) {
          aoi_data <- AOI
        }
      })
    } else if(exists("AOI")) {
      aoi_data <- AOI
    }
    
    # Transform AOI data to WGS84 if available and needed
    if(!is.null(aoi_data) && nrow(aoi_data) > 0) {
      if(!st_is_longlat(aoi_data)) {
        aoi_data <- st_transform(aoi_data, 4326)
      }
      aoi_data <- st_zm(aoi_data)
    }
    
    # Create the map
    if("Overall_Geo_mean" %in% names(combined_data) && 
       any(!is.na(combined_data$Overall_Geo_mean))) {
      
      cat("Creating map with Overall_Geo_mean data\n")
      
      # Transform for leaflet
      combined_data <- st_transform(combined_data, '+proj=longlat +datum=WGS84')
      
      # Calculate map bounds - prioritize AOI bounds if available
      map_bounds <- NULL
      if(!is.null(aoi_data) && nrow(aoi_data) > 0) {
        # Use AOI bounds for centering the map
        bbox <- st_bbox(aoi_data)
        map_bounds <- list(
          lng1 = bbox[["xmin"]], lat1 = bbox[["ymin"]],
          lng2 = bbox[["xmax"]], lat2 = bbox[["ymax"]]
        )
      } else if(nrow(combined_data) > 0) {
        # Fallback to data bounds if no AOI
        bbox <- st_bbox(combined_data)
        map_bounds <- list(
          lng1 = as.numeric(bbox["xmin"]), lat1 = as.numeric(bbox["ymin"]),
          lng2 = as.numeric(bbox["xmax"]), lat2 = as.numeric(bbox["ymax"])
        )
      }
      
      # Get range of values
      overall_values <- combined_data$Overall_Geo_mean[!is.na(combined_data$Overall_Geo_mean)]
      min_val <- min(overall_values, na.rm = TRUE)
      max_val <- max(overall_values, na.rm = TRUE)
      
      # Create map with color palette
      if(min_val == max_val) {
        # Single color for constant values
        single_color <- viridis::viridis(1, begin = 0.5, end = 0.5)
        
        result$map <- leaflet() %>%
          addProviderTiles("Esri.OceanBasemap",
                           options = providerTileOptions(variant = "Ocean/World_Ocean_Base")) %>%
          addProviderTiles("Esri.OceanBasemap",
                           options = providerTileOptions(variant = "Ocean/World_Ocean_Reference")) %>%
          addPolygons(
            data = combined_data, 
            color = "#33333300",
            weight = 1, 
            fillColor = single_color, 
            fillOpacity = 1,
            popup = ~paste("Overall Model Score:", round(Overall_Geo_mean, 2)),
            group = "Overall Model Data"
          ) %>%
          addLegend(
            position = "bottomright",
            colors = single_color,
            labels = paste("Score:", round(min_val, 2)),
            title = "Overall Model Score",
            opacity = 1
          )
      } else {
        # Color palette for varying values
        pal <- colorNumeric("viridis", 
                            domain = c(min_val, max_val), 
                            na.color = "transparent")
        
        result$map <- leaflet() %>%
          addProviderTiles("Esri.OceanBasemap",
                           options = providerTileOptions(variant = "Ocean/World_Ocean_Base")) %>%
          addProviderTiles("Esri.OceanBasemap",
                           options = providerTileOptions(variant = "Ocean/World_Ocean_Reference")) %>%
          addPolygons(
            data = combined_data, 
            color = "#33333300",
            weight = 1, 
            fillColor = ~pal(Overall_Geo_mean), 
            fillOpacity = 1,
            popup = ~paste("Overall Model Score:", round(Overall_Geo_mean, 2)),
            group = "Overall Model Data"
          ) %>%
          addLegend(
            position = "bottomright",
            pal = pal,
            values = combined_data$Overall_Geo_mean,
            title = "Overall Model Score",
            opacity = 1
          )
      }
      
      # Set map view based on bounds
      if(!is.null(map_bounds)) {
        result$map <- result$map %>%
          fitBounds(
            lng1 = map_bounds$lng1, lat1 = map_bounds$lat1,
            lng2 = map_bounds$lng2, lat2 = map_bounds$lat2,
            options = list(padding = c(20, 20))  # Add some padding around the bounds
          )
      }
      
      # Add AOI polygon if available - always include it
      if(!is.null(aoi_data) && nrow(aoi_data) > 0) {
        result$map <- result$map %>%
          addPolygons(
            data = aoi_data,
            fillColor = "transparent",
            color = "red",
            weight = 3,
            fillOpacity = 0,
            group = "AOI Boundaries"
            # path = pathOptions(
            #   interactive = FALSE
            # )
          ) %>%
          addLayersControl(
            overlayGroups = c("Overall Model Data", "AOI Boundaries"),
            options = layersControlOptions(collapsed = FALSE)
          )
      }
      
    } else {
      # No valid data
      result$map <- leaflet() %>%
        addProviderTiles("Esri.OceanBasemap") %>%
        addControl("No valid score data available for overall model.", position = "topright")
    }
    
    # Store combined data
    result$combined_data <- combined_data
    
    return(list(
      combined_data = combined_data,
      map = result$map,
      full_data_range = list(min = min_val, max = max_val)
    ))
  }, error = function(e) {
    cat("ERROR in create_full_model_map:", e$message, "\n")
    
    # Return error map
    error_map <- leaflet() %>%
      addProviderTiles("Esri.OceanBasemap") %>%
      setView(lng = -124, lat = 38, zoom = 7) %>%
      addControl(paste("Error generating full model:", e$message), position = "topright")
    
    return(list(
      combined_data = NULL,
      map = error_map,
      full_data_range = NULL
    ))
  })
}