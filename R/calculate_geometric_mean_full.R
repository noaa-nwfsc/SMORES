calculate_geometric_mean_full <- function(submodels, weights, base_grid) {
  if (length(submodels) == 0) {
    stop("No submodels provided for suitability calculation.")
  }

  # drop geometry to create base grid
  if ("sf" %in% class(active_base_grid_df())) {
    base_df <- sf::st_drop_geometry(active_base_grid_df())
    base_geom <- sf::st_geometry(active_base_grid_df())
  } else {
    base_df <- active_base_grid_df()
    base_geom <- NULL
  }

  # Start with just the ID column
  combined_df <- active_base_grid_df()[, "CellID", drop = FALSE]

  # Vectors to track valid columns and their weights
  data_col_names <- c()
  weight_lookup <- unlist(weights)
  ordered_weights <- c()

  for (submodel_name in names(submodels)) {
    submodel_data <- submodels[[submodel_name]]

    # Validation check
    if (is.null(submodel_data) || !"Geo_mean" %in% names(submodel_data)) {
      warning(paste(
        "Submodel",
        submodel_name,
        "is missing or lacks 'Geo_mean'. Skipping."
      ))
      next
    }

    # Extract just the ID and Score
    temp_df <- sf::st_drop_geometry(submodel_data)[, c("CellID", "Geo_mean")]

    # Rename to submodel name
    names(temp_df)[2] <- submodel_name

    # Fast Left Join
    combined_df <- dplyr::left_join(combined_df, temp_df, by = "CellID")

    # Track this column and its weight
    data_col_names <- c(data_col_names, submodel_name)
    ordered_weights <- c(ordered_weights, weight_lookup[[submodel_name]])
  }

  # Vectorized Weighted Geometric Mean Calculation
  if (length(data_col_names) > 0) {
    # Convert scores to matrix for math operations
    score_matrix <- as.matrix(combined_df[, data_col_names, drop = FALSE])
    mode(score_matrix) <- "numeric"

    # Create a matching matrix of weights (Rows = Cells, Cols = Layers)
    n_rows <- nrow(score_matrix)
    weight_matrix <- matrix(
      ordered_weights,
      nrow = n_rows,
      ncol = length(ordered_weights),
      byrow = TRUE
    )

    # Create a mask of where data actually exists
    is_valid <- !is.na(score_matrix)

    # If a cell has NA for a layer, the weight for that layer becomes 0
    weight_matrix[!is_valid] <- 0

    # Replace NA with 1 so the log() function doesn't crash
    score_matrix[!is_valid] <- 1

    # If any *valid* submodel has a score of 0, the result is 0
    has_zeros <- rowSums((score_matrix == 0) & is_valid, na.rm = TRUE) > 0

    # Step A: Numerator -> Sum of Weighted Logs
    # suppressWarnings handles log(0) producing -Inf (we fix 0s later)
    log_scores <- suppressWarnings(log(score_matrix))
    log_scores[is.infinite(log_scores)] <- 0 # Temp fix for 0s
    weighted_log_sum <- rowSums(weight_matrix * log_scores)

    # Step B: Denominator -> Sum of Valid Weights
    # This ensures that if a layer is missing, the weights of the *remaining* layers scale up.
    total_valid_weight <- rowSums(weight_matrix)

    # Step C: Final Calculation
    overall_score <- rep(NA_real_, n_rows)
    has_data <- total_valid_weight > 0

    overall_score[has_data] <- exp(
      weighted_log_sum[has_data] / total_valid_weight[has_data]
    )

    # Step D: Apply Zero Logic (0 overrides everything)
    overall_score[has_zeros] <- 0

    # Assign Result
    combined_df$Overall_Geo_mean <- overall_score
  } else {
    combined_df$Overall_Geo_mean <- NA_real_
  }

  # Re-attach Geometry
  if (!is.null(base_geom)) {
    result_sf <- sf::st_as_sf(combined_df, geometry = base_geom)
    return(result_sf)
  } else {
    return(combined_df)
  }
}
