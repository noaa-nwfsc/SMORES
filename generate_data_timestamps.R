data_files <- c(
  "Canyon_scored_full.parquet",
  "DSC_RH_scored_full.parquet",
  "Seeps_scored_full.parquet",
  "ShlfBrk_scored_full.parquet",
  "EFHCA_scored_full.parquet",
  "EFHCA_700_scored_full.parquet",
  "HAPCaoi_scored_full.parquet",
  "HAPCreef_scored_full.parquet",
  "killer_whale_scored_full.parquet",
  "Leatherback_turtle_scored_full.parquet",
  "Humpback_whale_scored_full.parquet",
  "Blue_whale_scored_full.parquet",
  "ASH_scored_full.parquet",
  "SSH_scored_full.parquet",
  "GFBT_scored_full.parquet",
  "GFP_scored_full.parquet",
  "GFLL_scored_full.parquet",
  "PS_scored_full.parquet",
  "CRAB_scored_full.parquet",
  "ALCO_scored_full.parquet",
  "ALCH_scored_full.parquet",
  "Trawl_fisheries_scored_full.parquet",
  "Surveys_fixed_scored_full.parquet",
  "Surveys_periodic_scored_full.parquet",
  "Submarine_cable_scored_full.parquet"
)

dataset_names <- c(
  "Canyon",
  "Deep Sea Coral Robust High Suitability",
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
  "Trawl Fisheries @ 75%",
  "Fixed Surveys",
  "Periodic Surveys",
  "Submarine Cables"
)

descriptions <- c(
  "Submarine canyons",
  "Deep sea coral areas with robust high suitability",
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
  "Grid cells contained within the top 75% of the ranked importance values for the four trawl fisheries",
  "Scientific survey locations with fixed monitoring stations",
  "Scientific survey areas with periodic monitoring schedules",
  "Submarine Cables"
)

data_types <- c(
  "Discrete",
  "Discrete (*Continuous if z-membership is selected)",
  "Discrete",
  "Discrete",
  "Discrete",
  "Discrete",
  "Discrete",
  "Discrete",
  "Discrete",
  "Discrete",
  "Discrete",
  "Discrete",
  "Continuous (*Discrete if static score is selected)",
  "Continuous (*Discrete if static score is selected)",
  "Continuous (*Discrete if static score is selected)",
  "Continuous (*Discrete if static score is selected)",
  "Continuous (*Discrete if static score is selected)",
  "Continuous (*Discrete if static score is selected)",
  "Continuous (*Discrete if static score is selected)",
  "Continuous (*Discrete if static score is selected)",
  "Continuous (*Discrete if static score is selected)",
  "Discrete",
  "Discrete",
  "Discrete",
  "Discrete"
)

df <- data.frame(
  filename = data_files,
  dataset_name = dataset_names,
  description = descriptions,
  data_type = data_types,
  stringsAsFactors = FALSE
)

# Grab 2km times
df$last_modified_2km <- as.POSIXct(
  sapply(df$filename, function(f) {
    p <- file.path("SMORES", "data", "2km", f)
    if (file.exists(p)) file.info(p)$mtime else NA
  }),
  origin = "1970-01-01"
)

# Grab 5km times
df$last_modified_5km <- as.POSIXct(
  sapply(df$filename, function(f) {
    p <- file.path("SMORES", "data", "5km", f)
    if (file.exists(p)) file.info(p)$mtime else NA
  }),
  origin = "1970-01-01"
)

# Format for the UI Table
df$formatted_date_2km <- ifelse(
  is.na(df$last_modified_2km),
  "Not Found",
  format(df$last_modified_2km, "%B %d, %Y")
)
df$formatted_date_5km <- ifelse(
  is.na(df$last_modified_5km),
  "Not Found",
  format(df$last_modified_5km, "%B %d, %Y")
)

# Save the lightweight metadata file
saveRDS(df, "C:\\GitHub\\SMORES\\data\\data_timestamps.rds")
cat("✅ Successfully generated data_timestamps.rds!\n")
