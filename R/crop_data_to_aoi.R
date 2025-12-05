# Function to crop spatial data to Area of Interest
crop_data_to_aoi <- function(dataset, aoi_data, buffer_distance = 1000) {
  
  # Skip cropping for Trawl Fisheries data to avoid geometry issues
  if("Score.Trawl_Fisheries" %in% names(dataset)) {
    warning("Skipping cropping for Trawl Fisheries due to geometry issues")
    return(dataset)
  }
  
  # Return original data if no AOI selected or "all" areas
  if(is.null(aoi_data) || nrow(aoi_data) == 0) {
    
    return(dataset)
  }
  
  # Handle "All Areas" selection - no cropping
  if("Area_Name" %in% names(aoi_data) && 
     length(unique(aoi_data$Area_Name)) > 1) {
    
    return(dataset)
  }
  
  tryCatch({
    # Ensure same CRS
    if(st_crs(dataset) != st_crs(aoi_data)) {
      aoi_data <- st_transform(aoi_data, st_crs(dataset))
    }
    
    # Optional: Buffer AOI slightly to capture edge effects
    if(buffer_distance > 0) {
      aoi_buffered <- st_buffer(aoi_data, dist = buffer_distance)
    } else {
      aoi_buffered <- aoi_data
    }
    
    # Spatial intersection to crop data
    cropped_data <- st_intersection(dataset, aoi_buffered)
    
    
    # Handle edge cases
    if(nrow(cropped_data) == 0) {
      warning("No data found within AOI bounds")
      return(dataset[FALSE, ]) # Return empty dataset with same structure
    }
    
    return(cropped_data)
    
  }, error = function(e) {
    warning(paste("Error cropping data:", e$message))
    return(dataset) # Return original data on error
  })
}