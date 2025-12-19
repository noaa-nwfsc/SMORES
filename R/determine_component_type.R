determine_component_type <- function(layer_name) {
  # Habitat layers
  habitat_layers <- c(
    "Canyon", "Deep Sea Coral Robust High Suitability", "Seeps", 
    "Shelf Break", "EFHCA", "EFHCA 700 fathoms", "HAPC AOI", "HAPC Rocky Reef"
  )
  
  # Species layers  
  species_layers <- c(
    "ESA Critical Habitat for Southern Resident Killer Whales",
    "ESA Critical Habitat for Leatherback Sea Turtles", 
    "ESA Critical Habitat for Humpback Whales - Mexico and Central DPS",
    "Biologically Important Area - Blue Whale"
  )
  
  # Fisheries layers
  fisheries_layers <- c(
    "At-Sea Hake Mid-Water Trawl", "Shoreside Hake Mid-Water Trawl",
    "Groundfish Bottom Trawl", "Groundfish Pot Gear", "Groundfish Long Line Gear",
    "Pink Shrimp Trawl", "Dungeness Crab", "Commercial Troll/Hook and Line Albacore",
    "Charter Vessel Albacore Troll/Hook and Line"
  )
  
  # Trawl layers
  trawl_layers <- c("Trawl Fisheries @ 75%")
  
  # Surveys layers
  surveys_layers <- c("Fixed Surveys", "Periodic Surveys")
  
  # Cables layers
  cables_layers <- c("Submarine Cables")
  
  # Return component type
  if (layer_name %in% habitat_layers) return("habitat")
  if (layer_name %in% species_layers) return("species")  
  if (layer_name %in% fisheries_layers) return("fisheries")
  if (layer_name %in% trawl_layers) return("trawl")
  if (layer_name %in% surveys_layers) return("surveys")
  if (layer_name %in% cables_layers) return("cables")
  
  return("unknown")
}