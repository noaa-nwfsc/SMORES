calculate_submodel_geometric_mean <- function(
  combined_data,
  submodel_type = NULL
) {
  # Identify Score Columns
  score_cols <- names(combined_data)[grep("^Score[._]", names(combined_data))]

  if (length(score_cols) > 0) {
    # --- FISHERIES SPECIAL HANDLING ---
    if (!is.null(submodel_type) && submodel_type == "fisheries") {
      fisheries_cols <- score_cols[grep(
        "fisheries",
        score_cols,
        ignore.case = TRUE
      )]
      trawl_cols <- score_cols[grep("trawl", score_cols, ignore.case = TRUE)]

      if (length(trawl_cols) > 0 && length(fisheries_cols) > 0) {
        trawl_score_col <- trawl_cols[1]
        general_fisheries_col <- fisheries_cols[
          !fisheries_cols %in% trawl_cols
        ][1]

        if (!is.na(general_fisheries_col)) {
          t_vals <- as.numeric(combined_data[[trawl_score_col]])
          f_vals <- as.numeric(combined_data[[general_fisheries_col]])

          # Use Trawl if valid (>0), else use Fisheries
          # This logic assumes "valid" means not NA and > 0
          use_trawl <- !is.na(t_vals) & t_vals > 0

          # Initialize result with fisheries values
          final_vals <- f_vals
          # Overwrite with trawl values where applicable
          final_vals[use_trawl] <- t_vals[use_trawl]

          combined_data$Geo_mean <- final_vals
          combined_data <- combined_data[!is.na(combined_data$Geo_mean), ]
          return(combined_data)
        }
      }
    }

    # filter numeric columns
    if ("sf" %in% class(combined_data)) {
      score_df <- sf::st_drop_geometry(combined_data)[,
        score_cols,
        drop = FALSE
      ]
    } else {
      score_df <- combined_data[, score_cols, drop = FALSE]
    }

    # Convert to matrix for speed
    score_matrix <- as.matrix(score_df)
    mode(score_matrix) <- "numeric"

    if (ncol(score_matrix) == 0) {
      return(combined_data)
    }

    # If any non-NA value is 0, the result is 0.
    has_zeros <- rowSums(score_matrix == 0, na.rm = TRUE) > 0

    # Handle Negatives (Invalid)
    has_neg <- rowSums(score_matrix < 0, na.rm = TRUE) > 0

    # exp(mean(log(x)))
    log_matrix <- suppressWarnings(log(score_matrix))

    # Fix Infinite/NaN from 0s or negatives so rowMeans doesn't break
    log_matrix[is.infinite(log_matrix) | is.nan(log_matrix)] <- NA

    # Calculate Mean of Logs
    mean_log <- rowMeans(log_matrix, na.rm = TRUE)

    # Exponentiate
    geo_mean <- exp(mean_log)

    # Apply Special Cases
    geo_mean[has_zeros] <- 0
    geo_mean[has_neg] <- NA

    # Assign and Filter
    combined_data$Geo_mean <- geo_mean
  } else {
    cat("WARNING: No score columns found.\n")
  }

  return(combined_data)
}
