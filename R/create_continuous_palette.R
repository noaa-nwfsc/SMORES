# Function to create continuous color palette for continuous scoring (Z Membership and Ranked Importance)
create_continuous_palette <- function(data, score_type = "z_membership", layer_name = NULL) {
  # Determine the score column based on score type and layer name
  if(score_type == "ranked_importance" && !is.null(layer_name)) {
    # Use layer-specific column naming for ranked importance
    score_column <- switch(layer_name,
                           "At-Sea Hake Mid-Water Trawl" = "Score.ASH_Ranked_Importance",
                           "Shoreside Hake Mid-Water Trawl" = "Score.SSH_Ranked_Importance", 
                           "Groundfish Bottom Trawl" = "Score.GFBT_Ranked_Importance",
                           "Groundfish Pot Gear" = "Score.GFP_Ranked_Importance",
                           "Groundfish Long Line Gear" = "Score.GFLL_Ranked_Importance",
                           "Pink Shrimp Trawl" = "Score.PS_Ranked_Importance",
                           "Dungeness Crab" = "Score.CRAB_Ranked_Importance", 
                           "Commercial Troll/Hook and Line Albacore" = "Score.ALCO_Ranked_Importance",
                           "Charter Vessel Albacore Troll/Hook and Line" = "Score.ALCH_Ranked_Importance",
                           "Score.Ranked_Importance")  # fallback to generic
  } else {
    # For Z membership or other score types
    score_column <- switch(score_type,
                           "z_membership" = "Score.Z_Membership",
                           "ranked_importance" = "Score.Ranked_Importance",
                           "Score.Z_Membership")  # default fallback
  }
  
  if(is.null(data) || !score_column %in% names(data)) {
    return(colorNumeric("viridis", domain = c(0, 1)))
  }
  
  score_values <- data[[score_column]][!is.na(data[[score_column]])]
if(length(score_values) == 0) {
  return(colorNumeric("viridis", domain = c(0, 1)))
}

return(colorNumeric("viridis", domain = range(score_values, na.rm = TRUE)))
}