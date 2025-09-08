# Function to get filtered timestamp data for selected layers
get_filtered_timestamp_data <- function(valid_configs, layer_type = "habitat") {
  # First get all timestamp data
  timestamp_info <- tryCatch({
    source("R/data_timestamps.R")
    get_data_timestamps()
  }, error = function(e) {
    list(
      data_timestamps = data.frame(
        dataset_name = character(0),
        formatted_date = character(0)
      ),
      most_recent_update = "Information not available"
    )
  })
  
  # If no valid configs or no timestamp data, return empty
  if (length(valid_configs) == 0 || is.null(timestamp_info$data_timestamps)) {
    return(timestamp_info)
  }
  
  # Get the layer names from valid configs
  selected_layers <- sapply(valid_configs, function(x) x$layer)
  
  # Create mapping between layer names and dataset names
  if (layer_type == "habitat") {
    layer_to_dataset_mapping <- list(
      "Canyon" = "Canyon",
      "Deep Sea Coral Robust High Suitability" = "Deep Sea Coral Robust High Suitability",
      "Seeps" = "Seeps",
      "Shelf Break" = "Shelf Break",
      "EFHCA" = "EFHCA",
      "EFHCA 700 fathoms" = "EFHCA 700 fathoms",
      "HAPC AOI" = "HAPC AOI",
      "HAPC Rocky Reef" = "HAPC Rocky Reef"
    )
  } else if (layer_type == "species") {
    layer_to_dataset_mapping <- list(
      "Southern Resident Killer Whales" = "ESA Critical Habitat for Southern Resident Killer Whales",
      "Leatherback Sea Turtles" = "ESA Critical Habitat for Leatherback Sea Turtles",
      "Humpback Whale - Mexico and Central DPS" =  "ESA Critical Habitat for Humpback Whales - Mexico and Central DPS",
      "Biologically Important Area - Blue Whale" = "Biologically Important Area - Blue Whale"
    )
  } else {
    # For other layer types, use layer name as dataset name
    layer_to_dataset_mapping <- setNames(selected_layers, selected_layers)
  }
  
  # Get dataset names for selected layers
  selected_datasets <- sapply(selected_layers, function(layer) {
    layer_to_dataset_mapping[[layer]] %||% layer
  })
  
  # Filter timestamp data to only selected datasets
  timestamp_info$data_timestamps <- timestamp_info$data_timestamps %>%
    filter(dataset_name %in% selected_datasets)
  
  # Update most recent update to only reflect selected layers
  if (nrow(timestamp_info$data_timestamps) > 0) {
    selected_dates <- timestamp_info$data_timestamps$last_modified[!is.na(timestamp_info$data_timestamps$last_modified)]
    if (length(selected_dates) > 0) {
      timestamp_info$most_recent_update <- format(max(selected_dates), "%B %d, %Y at %H:%M:%S")
    }
  }
  
  return(timestamp_info)
}