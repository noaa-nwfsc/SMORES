get_data_timestamps <- function(data_dir = "data") {
  # Define the data files to track
  data_files <- c(
    "canyon_scored.rds",
    "DSC_RH_scored.rds",
    "Surveys_fixed_scored.rds",
    "Surveys_periodic_scored.rds",
    "Seeps_scored.rds",
    "ShlfBrk_scored.rds", 
    "EFHCA_scored.rds",
    "EFHCA_700_scored.rds", 
    "HAPCaoi_scored.rds",
    "HAPCreef_scored.rds",
    "killer_whale_scored.rds",
    "leatherback_turtle_scored.rds",
    "humpback_whale_scored.rds"
  )
  
  # Create a data frame to store file information
  data_timestamps <- data.frame(
    filename = data_files,
    last_modified = as.POSIXct(NA),
    stringsAsFactors = FALSE
  )
  
  # Load datasets and track when they were last modified
  for (i in seq_along(data_files)) {
    file_path <- file.path(data_dir, data_files[i])
    if (file.exists(file_path)) {
      file_info <- file.info(file_path)
      data_timestamps$last_modified[i] <- file_info$mtime
    }
  }
  
  # Add human-readable names
  data_timestamps$dataset_name <- c(
    "Canyon Data", 
    "DSC_RH Data", 
    "Fixed Surveys", 
    "Periodic Surveys", 
    "Seeps Data", 
    "Shelf Break Data", 
    "EFHCA",
    "EFHCA 700 fathoms",
    "HAPC AOI",
    "HAPC Rocky Reef",
    "ESA Critical Habitat for Southern Resident Killer Whales",
    "ESA Critical Habitat for Leatherback Sea Turtles",
    "ESA Critical Habitat for Humpback Whales - Mexico and Central DPS"
  )
  
  # Format the timestamps for display
  data_timestamps$formatted_date <- format(data_timestamps$last_modified, "%B %d, %Y at %H:%M:%S")
  
  # Get the most recent update across all data files
  most_recent_update <- format(max(data_timestamps$last_modified, na.rm = TRUE), 
                               "%B %d, %Y at %H:%M:%S")
  
  # Return both the detailed timestamps and the most recent update
  return(list(
    data_timestamps = data_timestamps,
    most_recent_update = most_recent_update
  ))
}