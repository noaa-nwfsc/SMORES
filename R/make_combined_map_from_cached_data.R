make_combined_map_from_cached_data <- function(
  valid_configs,
  cached_data,
  method,
  base_grid = grid_test,
  aoi_data = NULL
) {
  # 1. Setup Base Grid
  if (!is.null(aoi_data) && nrow(aoi_data) > 0) {
    base_sf <- crop_data_to_aoi(base_grid, aoi_data)
  } else {
    base_sf <- base_grid
  }

  base_df <- sf::st_drop_geometry(base_sf)
  base_geom <- sf::st_geometry(base_sf)

  score_columns_added <- c()
  strict_na_cols <- c()

  # 2. Iterative Join
  for (config in valid_configs) {
    config_key <- paste(config$layer, config$score, config$index, sep = "_")
    cached_config <- cached_data[[config_key]]

    if (!is.null(cached_config) && !is.null(cached_config$data)) {
      temp_data <- cached_config$data
      score_col <- cached_config$score_column

      if (!is.null(score_col) && score_col %in% names(temp_data)) {
        # Automatically find whether we are joining on CellID_2km or CellID_5km
        id_col <- grep(
          "^CellID",
          intersect(names(base_df), names(temp_data)),
          value = TRUE
        )[1]

        # ==========================================
        # DIAGNOSTIC PROBE: WHY ARE WE GETTING NAs?
        # ==========================================
        print(paste("=== JOIN DIAGNOSTIC:", config$layer, "==="))
        print(paste("1. Temp Data Rows:", nrow(temp_data)))
        print(paste("2. Detected ID Column:", id_col))

        if (is.na(id_col)) {
          print(
            "CRITICAL WARNING: No CellID column found in Species data. Tabular left_join will fail!"
          )
          print(paste(
            "Available Columns:",
            paste(names(temp_data), collapse = ", ")
          ))
        } else {
          matching_cells <- sum(temp_data[[id_col]] %in% base_df[[id_col]])
          print(paste(
            "3. Matching CellIDs found in base grid:",
            matching_cells
          ))
        }
        print("=======================================")

        # Subset dynamically using the detected ID column
        temp_df_clean <- sf::st_drop_geometry(temp_data)[, c(id_col, score_col)]

        # --- FIX: KEEP EXACT SHORT SPECIES NAMES & APPEND SCORE ---
        # e.g., "Score.killer_whale_0.1".
        # This prevents identical names if an admin loads two different scoring systems.
        unique_col_name <- paste0(score_col, "_", config$score)

        names(temp_df_clean)[2] <- unique_col_name

        # Dynamic Left Join (Keeps all data, creates NAs where missing)
        base_df <- dplyr::left_join(base_df, temp_df_clean, by = id_col)

        score_columns_added <- c(score_columns_added, unique_col_name)

        # IDENTIFY SPECIAL LAYERS
        is_trawl <- grepl("Trawl Fisheries", config$layer, ignore.case = TRUE)
        is_coral_z <- (config$layer ==
          "Deep Sea Coral Robust High Suitability" &&
          config$score == "Z Membership")

        if (is_trawl || is_coral_z) {
          strict_na_cols <- c(strict_na_cols, unique_col_name)
        }
      }
    }
  }

  # 3. Store the tag as an attribute
  base_sf_final <- sf::st_as_sf(base_df, geometry = base_geom)
  attr(base_sf_final, "strict_na_cols") <- strict_na_cols

  return(base_sf_final)
}
