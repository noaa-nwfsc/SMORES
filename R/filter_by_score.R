# filter dataframes by score
filter_by_score <- function(
  df,
  selected_score,
  base_grid = NULL,
  layer_name = NULL
) {
  # 1. Quick Checks
  if (is.null(df) || is.null(selected_score) || selected_score == "None") {
    return(df)
  }

  # 2. Excluded Layers (Trawl Fisheries)
  # These layers skip the standard score filtering because we want to keep all their values
  # (Preventing the "filling of 1s" that happens when we drop rows).
  excluded_layers <- c("Trawl Fisheries @ 75%")

  if (!is.null(layer_name) && layer_name %in% excluded_layers) {
    # Finds the matching Cell ID column automatically (CellID_2km or CellID_5km)
    if (!is.null(base_grid)) {
      id_col <- grep(
        "^CellID",
        intersect(names(base_grid), names(df)),
        value = TRUE
      )

      if (length(id_col) > 0) {
        target_col <- id_col[1]
        valid_ids <- base_grid[[target_col]]

        # Filter the Trawl data to keep ONLY rows that are in the AOI
        if ("sf" %in% class(df)) {
          df <- df[df[[target_col]] %in% valid_ids, ]
        } else {
          df <- df[df[[target_col]] %in% valid_ids, ]
        }
      }
    }
    return(df)
  }

  # 3. Z Membership (Return all data)
  if (selected_score == "Z Membership") {
    return(df)
  }

  # 4. Ranked Importance (Return all data)
  if (selected_score == "Ranked Importance") {
    return(df)
  }

  score_cols <- names(df)[grep("^Score\\.", names(df))]

  if (length(score_cols) == 0) {
    warning("No score columns found in the dataset")
    return(df)
  }

  rows_to_keep <- rep(FALSE, nrow(df))
  target_score_num <- suppressWarnings(as.numeric(selected_score))
  is_numeric_target <- !is.na(target_score_num)

  for (col in score_cols) {
    if (is_numeric_target) {
      # Safe mathematical comparison
      col_values <- suppressWarnings(as.numeric(df[[col]]))
      matches <- !is.na(col_values) & abs(col_values - target_score_num) < 1e-6
    } else {
      # Fallback for text
      col_values <- as.character(df[[col]])
      matches <- !is.na(col_values) & col_values == as.character(selected_score)
    }
    rows_to_keep <- rows_to_keep | matches
  }

  # 6. CRITICAL OPTIMIZATION: Return Sparse Data
  # We return ONLY the matching rows.
  # We do NOT join to the base_grid or fill with 1s here.
  # This keeps the dataframe tiny and fast.
  filtered_df <- df[rows_to_keep, ]

  return(filtered_df)
}
