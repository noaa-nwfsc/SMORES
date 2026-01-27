# Function to create combined maps from cached individual data
make_combined_map_from_cached_data <- function(valid_configs, cached_data, method, base_grid = grid_test, aoi_data = NULL) {
  
  # Initialize with base grid
  if(!is.null(aoi_data) && nrow(aoi_data) > 0) {
    # Crop the base grid to the AOI first
    base_sf <- crop_data_to_aoi(base_grid, aoi_data)
  } else {
    base_sf <- base_grid
  }
  
  # Separate Geometry from Data
  base_df <- sf::st_drop_geometry(base_sf)
  
  # Keep the geometry to re-attach at the very end
  base_geom <- sf::st_geometry(base_sf)
  
  # Keep track of which columns we added
  score_columns_added <- c()
  
  # Flag to track if we need to apply the "Trawl Clip"
  trawl_col_name <- NULL
  
  # 2. Iterative Join
  for(config in valid_configs) {
    config_key <- paste(config$layer, config$score, config$index, sep = "_")
    
    # Retrieve data from cache
    cached_config <- cached_data[[config_key]]
    
    if(!is.null(cached_config) && !is.null(cached_config$data)) {
      temp_data <- cached_config$data
      score_col <- cached_config$score_column
      
      if(!is.null(score_col) && score_col %in% names(temp_data)) {
        
        # Prepare temp dataframe (ID and Score only)
        temp_df_clean <- sf::st_drop_geometry(temp_data)[, c("CellID_2km", score_col)]
        
        # Rename score column to unique ID to prevent collisions
        unique_col_name <- paste0("Score.", config_key)
        names(temp_df_clean)[2] <- unique_col_name
        
        # Join to base_df
        base_df <- dplyr::left_join(base_df, temp_df_clean, by = "CellID_2km")
        
        score_columns_added <- c(score_columns_added, unique_col_name)
        
        # CHECK: Is this the Trawl Layer?
        # If so, mark this column so we can filter by it later.
        if(grepl("Trawl Fisheries", config$layer, ignore.case = TRUE)) {
          trawl_col_name <- unique_col_name
        }
      }
    }
  }
  
  # 3. SPECIAL HANDLING: Trawl Fisheries Logic
  # The user requirement: "Trawl... should not have 1's filled in".
  # If Trawl is present, we interpret missing Trawl data as "Invalid/No Data" for that cell.
  # Instead of letting the NAs become 1s, we REMOVE the rows where Trawl is NA.
  
  if(!is.null(trawl_col_name) && trawl_col_name %in% names(base_df)) {
    
    # Identify rows where Trawl data exists (is not NA)
    valid_trawl_rows <- !is.na(base_df[[trawl_col_name]])
    
    # Filter the WHOLE combined map to only these rows
    # This effectively crops the map to the Trawl footprint and prevents "1-filling" outside it.
    base_df <- base_df[valid_trawl_rows, ]
    
    # Sync geometry to the new row count (Standard subsetting)
    # Note: We must subset the geometry using the SAME index or ID match
    # Since base_df was joined and order might be preserved, but best to be safe:
    if(nrow(base_df) > 0) {
      # We need to ensure we align with base_geom.
      # The safest way is to filter the geometry by the remaining IDs
      remaining_ids <- base_df$CellID_2km
      base_sf_subset <- base_sf[base_sf$CellID_2km %in% remaining_ids, ]
      base_geom <- sf::st_geometry(base_sf_subset)
      
      # Ensure base_df is sorted or aligned if necessary (usually ID match is enough)
      # But since we are about to st_as_sf, we need row alignment.
      # Let's re-merge geometry carefully:
      # Ideally, we just attach base_geom if the order didn't change.
      # But filtering changed the length.
    }
  }
  
  # 4. Re-attach Geometry
  # We use the filtered geometry from Step 3 if Trawl was present,
  # otherwise we use the original base_geom.
  
  if(nrow(base_df) == 0) {
    return(NULL) # Handle empty result
  }
  
  # If we filtered rows (Trawl logic), we must match geometry to data
  if(!is.null(trawl_col_name)) {
    # Re-join geometry based on ID to ensure perfect alignment
    # This is safer than assuming row order
    base_sf_final <- dplyr::inner_join(base_sf[, "CellID_2km"], base_df, by = "CellID_2km")
  } else {
    # Standard path (All rows kept, just simple attach)
    base_sf_final <- sf::st_as_sf(base_df, geometry = base_geom)
  }
  
  return(base_sf_final)
}