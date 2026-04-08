calculate_lowest_value <- function(combined_data) {
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

    # 3. Detect completely empty rows
    all_na_mask <- rowSums(!is.na(score_matrix)) == 0

    # 4. Math execution (na.rm = TRUE ignores the Trawl NAs)
    lowest_vals <- suppressWarnings(
      do.call(pmin, c(as.data.frame(score_matrix), list(na.rm = TRUE)))
    )

    # 5. strict NA cleanup (Fixes Inf converting to colors in Leaflet)
    lowest_vals[all_na_mask] <- NA
    lowest_vals[is.infinite(lowest_vals)] <- NA

    combined_data$Lowest_value <- lowest_vals
  }

  return(combined_data)
}
