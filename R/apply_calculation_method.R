# Function to apply calculation method to combined dataset
apply_calculation_method <- function(combined_data, method, map_type = "Combined") {
  
  if(is.null(combined_data)) {
    return(list(
      data = NULL,
      score_column = NULL,
      title = paste("Combined", map_type, "Score"),
      popup_prefix = "Score:"
    ))
  }
  
  # Apply the specific calculation method
  if(method == "geometric_mean") {
    processed_data <- calculate_geometric_mean(combined_data)
    score_column <- "Geo_mean"
    popup_prefix <- "Geometric Mean Score:"
    map_title <- paste("Combined", map_type, "Score - Geometric Mean")
  } else if(method == "lowest") {
    processed_data <- calculate_lowest_value(combined_data)
    score_column <- "Lowest_value"
    popup_prefix <- "Lowest Value Score:"
    map_title <- paste("Combined", map_type, "Score - Lowest Value")
  } else if(method == "product") {
    processed_data <- calculate_product(combined_data)
    score_column <- "Product_value"
    popup_prefix <- "Product Score:"
    map_title <- paste("Combined", map_type, "Score - Product")
  } else {
    # Default fallback to geometric mean for unknown methods
    processed_data <- calculate_geometric_mean(combined_data)
    score_column <- "Geo_mean"
    popup_prefix <- "Geometric Mean Score:"
    map_title <- paste("Combined", map_type, "Score - Geometric Mean")
  }
  
  return(list(
    data = processed_data,
    score_column = score_column,
    title = map_title,
    popup_prefix = popup_prefix
  ))
}