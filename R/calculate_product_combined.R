calculate_product_value <- function(combined_data) {
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

    # 4. Vectorized Product
    n_rows <- nrow(score_matrix)
    result_vector <- rep(1, n_rows)

    for (i in 1:ncol(score_matrix)) {
      vals <- score_matrix[, i]
      # Treat remaining NAs (from Trawl) as 1 so they don't break the multiplication math
      vals[is.na(vals)] <- 1
      result_vector <- result_vector * vals
    }

    # 5. strict NA cleanup (Forces non-trawled cells back to NA if only Trawl was selected)
    result_vector[all_na_mask] <- NA

    combined_data$Product_value <- result_vector
  }

  return(combined_data)
}
