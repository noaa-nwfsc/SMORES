get_data_timestamps <- function() {
  # Read the pre-computed metadata file you generated
  metadata_path <- "data/data_timestamps.rds"

  if (!file.exists(metadata_path)) {
    warning(
      "data_timestamps.rds not found. Run generate_data_timestamps.R locally."
    )
    return(list(
      data_timestamps = data.frame(),
      most_recent_update = "Unknown"
    ))
  }

  data_timestamps <- readRDS(metadata_path)

  # Calculate the global "Most recent data update" for the very top of the Data tab
  all_dates <- c(
    data_timestamps$last_modified_2km,
    data_timestamps$last_modified_5km
  )
  valid_dates <- all_dates[!is.na(all_dates)]

  if (length(valid_dates) > 0) {
    most_recent_update <- format(max(valid_dates), "%B %d, %Y at %H:%M:%S")
  } else {
    most_recent_update <- "Not Found"
  }

  return(list(
    data_timestamps = data_timestamps,
    most_recent_update = most_recent_update
  ))
}
