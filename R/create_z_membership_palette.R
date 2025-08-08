# Function to create continuous color palette for Z Membership
create_z_membership_palette <- function(data) {
  if(is.null(data) || !"Score.Z_Membership" %in% names(data)) {
    return(colorNumeric("viridis", domain = c(0, 1)))
  }
  
  z_values <- data$Score.Z_Membership[!is.na(data$Score.Z_Membership)]
  if(length(z_values) == 0) {
    return(colorNumeric("viridis", domain = c(0, 1)))
  }
  
  return(colorNumeric("viridis", domain = range(z_values, na.rm = TRUE)))
}