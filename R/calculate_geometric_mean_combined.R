calculate_geometric_mean <- function(combined_data) {
  score_cols <- names(combined_data)[grep("^Score\\.", names(combined_data))]

  if (length(score_cols) > 0) {
    # Get the list of strict columns we attached earlier
    strict_cols <- attr(combined_data, "strict_na_cols")

    if ("sf" %in% class(combined_data)) {
      score_df <- sf::st_drop_geometry(combined_data)[,
        score_cols,
        drop = FALSE
      ]
    } else {
      score_df <- combined_data[, score_cols, drop = FALSE]
    }

    score_matrix <- as.matrix(score_df)
    mode(score_matrix) <- "numeric"

    # Identify columns that are NOT strict (aka not trawl or coral z)
    std_cols_indices <- which(!colnames(score_matrix) %in% strict_cols)

    if (length(std_cols_indices) > 0) {
      # subset matrix to standard columns
      sub_mat <- score_matrix[, std_cols_indices, drop = FALSE]
      # Replace NA with 1
      sub_mat[is.na(sub_mat)] <- 1
      # Put it back
      score_matrix[, std_cols_indices] <- sub_mat
    }

    # Check for zeros (overrides NA)
    has_zeros <- rowSums(score_matrix == 0, na.rm = TRUE) > 0
    has_neg <- rowSums(score_matrix < 0, na.rm = TRUE) > 0

    log_matrix <- suppressWarnings(log(score_matrix))
    log_matrix[is.infinite(log_matrix) | is.nan(log_matrix)] <- NA

    # Calculate Mean of Logs (na.rm=TRUE ignores the Strict NAs)
    # This prevents Trawl=NA from diluting the score of the other layers
    mean_log <- rowMeans(log_matrix, na.rm = TRUE)

    geo_mean <- exp(mean_log)
    geo_mean[has_zeros] <- 0
    geo_mean[has_neg] <- NA

    # Assign result
    combined_data$Geo_mean <- geo_mean
  } else {
    cat("No score columns found\n")
  }

  return(combined_data)
}
