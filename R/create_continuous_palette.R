# Function to create continuous color palette for continuous scoring (Z Membership and Ranked Importance)
create_continuous_palette <- function(data, score_type = "z_membership") {
  # Determine the score column based on score type
  score_column <- switch(score_type,
                         "z_membership" = "Score.Z_Membership",
                         "ranked_importance" = "Score.Ranked_Importance",
                         "Score.Z_Membership")  # default fallback
  
  if(is.null(data) || !score_column %in% names(data)) {
    return(colorNumeric("viridis", domain = c(0, 1)))
  }
  
  score_values <- data[[score_column]][!is.na(data[[score_column]])]
  if(length(score_values) == 0) {
    return(colorNumeric("viridis", domain = c(0, 1)))
  }
  
  return(colorNumeric("viridis", domain = range(score_values, na.rm = TRUE)))
}