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
    "humpback_whale_scored.rds", 
    "blue_whale_scored.rds",
    "ASH_scored.rds",
    "SSH_scored.rds",
    "GFBT_scored.rds",
    "GFP_scored.rds",
    "GFLL_scored.rds",
    "PS_scored.rds",
    "CRAB_scored.rds",
    "ALCO_scored.rds",
    "ALCH_scored.rds",
    "submarine_cable_scored.rds"
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
    "Canyon", 
    "Deep Sea Coral Robust High Suitability", 
    "Fixed Surveys", 
    "Periodic Surveys", 
    "Seeps", 
    "Shelf Break", 
    "EFHCA",
    "EFHCA 700 fathoms",
    "HAPC AOI",
    "HAPC Rocky Reef",
    "ESA Critical Habitat for Southern Resident Killer Whales",
    "ESA Critical Habitat for Leatherback Sea Turtles",
    "ESA Critical Habitat for Humpback Whales - Mexico and Central DPS", 
    "Biologically Important Area - Blue Whale",
    "At-Sea Hake Mid-Water Trawl",
    "Shoreside Hake Mid-Water Trawl",
    "Groundfish Bottom Trawl",
    "Groundfish Pot Gear",
    "Groundfish Long Line Gear",
    "Pink Shrimp Trawl",
    "Dungeness Crab",
    "Commercial Troll/Hook and Line Albacore",
    "Charter Vessel Albacore Troll/Hook and Line",
    "Submarine Cables"
  )
  
  # Add dataset descriptions (need to be in same order as earlier lists)
  data_timestamps$description <- c(
    "Submarine canyons",
    "Deep sea coral areas with robust high suitability",
    "Scientific survey locations with fixed monitoring stations",
    "Scientific survey areas with periodic monitoring schedules",
    "Methane Seeps",
    "Continental shelf break features important for marine habitat",
    "Essential Fish Habitat Conservation Areas",
    "Essential Fish Habitat Conservation Areas, 700 fathoms",
    "Habitat Areas of Particular Concern - Areas of Interest",
    "Habitat Areas of Particular Concern - Rocky Reef areas",
    "Critical habitat for endangered Southern Resident Killer Whales",
    "Critical habitat for endangered Leatherback Sea Turtles",
    "Critical habitat for Humpback Whales (Mexico and Central Distinct Population Segments)",
    "Biologically Important Area (Parent and Core) for Blue Whales",
    "At-Sea Hake Mid-Water Trawl",
    "Shoreside Hake Mid-Water Trawl",
    "Groundfish Bottom Trawl",
    "Groundfish Pot Gear",
    "Groundfish Long Line Gear",
    "Pink Shrimp Trawl",
    "Dungeness Crab",
    "Commercial Troll/Hook and Line Albacore",
    "Charter Vessel Albacore Troll/Hook and Line",
    "Submarine Cables"
  )
  
  # Add data type (need to be in same order as earlier lists)
  data_timestamps$data_type <- c(
    "Discrete",         # Canyon
    "Dicrete (*Continuous if z-membership is selected)",     # Deep Sea Coral Robust High Suitability
    "Discrete",         # Fixed Surveys
    "Discrete",         # Periodic Surveys
    "Discrete",         # Seeps
    "Discrete",         # Shelf Break
    "Discrete",         # EFHCA
    "Discrete",         # EFHCA 700 fathoms
    "Discrete",         # HAPC AOI
    "Discrete",         # HAPC Rocky Reef
    "Discrete",         # Killer Whales
    "Discrete",         # Leatherback Turtles
    "Discrete",         # Humpback Whales
    "Discrete",         # Blue Whale
    "Continuous",       # ASH
    "Continuous",       # SSH
    "Continuous",       # GFBT
    "Continuous",       # GFP
    "Continuous",       # GFLL
    "Continuous",       # PS
    "Continuous",       # CRAB
    "Continuous",       # ALCO
    "Continuous",       # ALCH
    "Discrete"          # Submarine Cables
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