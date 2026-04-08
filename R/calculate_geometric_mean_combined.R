calculate_geometric_mean <- function(combined_data) {
  score_cols <- names(combined_data)[grep("^Score\\.", names(combined_data))]

  if (length(score_cols) > 0) {
    strict_cols <- attr(combined_data, "strict_na_cols")
    if (is.null(strict_cols)) {
      strict_cols <- character(0)
    }

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

    # 1. Identify Standard vs Strict (Mask) columns
    std_cols_indices <- which(!colnames(score_matrix) %in% strict_cols)

    # 2. Impute 1 ONLY for Standard columns
    if (length(std_cols_indices) > 0) {
      sub_mat <- score_matrix[, std_cols_indices, drop = FALSE]
      sub_mat[is.na(sub_mat)] <- 1
      score_matrix[, std_cols_indices] <- sub_mat
    }

    # 3. Detect completely empty rows (e.g., Only Trawl selected, and it's NA)
    all_na_mask <- rowSums(!is.na(score_matrix)) == 0

    # 4. Math execution
    has_zeros <- rowSums(score_matrix == 0, na.rm = TRUE) > 0
    has_neg <- rowSums(score_matrix < 0, na.rm = TRUE) > 0

    log_matrix <- suppressWarnings(log(score_matrix))
    log_matrix[is.infinite(log_matrix) | is.nan(log_matrix)] <- NA

    # Calculate Mean of Logs (na.rm=TRUE naturally ignores the Trawl NAs!)
    mean_log <- suppressWarnings(rowMeans(log_matrix, na.rm = TRUE))
    geo_mean <- exp(mean_log)

    # 5. strict NA cleanup (Fixes the Leaflet coloring bug!)
    geo_mean[has_zeros] <- 0
    geo_mean[has_neg] <- NA
    geo_mean[all_na_mask] <- NA # Force completely empty rows to NA
    geo_mean[is.nan(geo_mean)] <- NA # Convert NaN to strict NA

    combined_data$Geo_mean <- geo_mean
  } else {
    cat("No score columns found\n")
  }

  return(combined_data)
}
