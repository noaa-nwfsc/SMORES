get_valid_configs_for_tab <- function(
  input,
  layer_data,
  score_colors,
  input_prefix
) {
  valid_configs <- list()
  index <- 1

  # layer_data is just your named list from global.R (e.g., "Canyon" = "canyon.parquet")
  layer_names <- names(layer_data)

  for (layer_name in layer_names) {
    # Create consistent IDs (e.g., "Canyon")
    layer_id <- gsub(" ", "_", layer_name)
    layer_id <- gsub("[^A-Za-z0-9_]", "", layer_id)

    # Dynamically build the input IDs based on the prefix passed in
    # e.g., if input_prefix = "Habitat", this becomes "EnableHabitatLayer_Canyon"
    enable_input_id <- paste0("Enable", input_prefix, "Layer_", layer_id)
    score_input_id <- paste0(input_prefix, "ScorePicker_", layer_id)

    # Check if this layer is enabled in the UI
    is_enabled <- !is.null(input[[enable_input_id]]) && input[[enable_input_id]]

    if (is_enabled) {
      score_value <- input[[score_input_id]]

      if (!is.null(score_value) && score_value != "None") {
        # Determine score color metadata
        # (Handle the continuous edge cases for DSC and Fisheries)
        if (
          (layer_name == "Deep Sea Coral Robust High Suitability" &&
            score_value == "Z Membership") ||
            score_value == "Ranked Importance"
        ) {
          score_color <- "continuous"
        } else {
          score_color <- score_colors[[score_value]]
        }

        # Add to valid configs - data is NOW JUST THE FILENAME STRING
        valid_configs[[length(valid_configs) + 1]] <- list(
          index = index,
          layer = layer_name,
          score = score_value,
          color = score_color,
          data = layer_data[[layer_name]] # Passes the filename string
        )

        index <- index + 1
      }
    }
  }

  return(valid_configs)
}
