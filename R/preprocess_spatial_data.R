#' Enhanced spatial data preprocessing function
#' 
#' @param data An sf object to preprocess
#' @param dataset_name Optional name for the dataset (used for logging)
#' @return Preprocessed spatial data
preprocess_spatial_data <- function(data, dataset_name = NULL) {
  if(is.null(data)) {
    return(NULL)
  }
  
  # Skip preprocessing if already done
  if(is_preprocessed(data)) {
    return(data)
  }
  
  tryCatch({
    # Transform to WGS84 if needed
    if(!st_is_longlat(data)) {
      data <- st_transform(data, 4326)
    }
    
    # Remove Z & M dimensions
    data <- st_zm(data)
    
    # Pre-calculate and store bounding box as attribute
    attr(data, "bbox_precalc") <- st_bbox(data)
    
    # Store CRS information for quick reference
    attr(data, "crs_checked") <- TRUE
    
    # Mark as preprocessed
    attr(data, "preprocessed") <- TRUE
    
    return(data)
    
  }, error = function(e) {
    return(data)  # Return original data if preprocessing fails
  })
}

#' Read parquet file with automatic spatial preprocessing
#' 
#' @param file File path to read from
#' @param dataset_name Optional name for the dataset (used for logging)
#' @return Preprocessed spatial data
readRDS_preprocessed <- function(file, dataset_name = NULL) {
  
  # Check if file exists
  if(!file.exists(file)) {
    return(NULL)
  }
  
  tryCatch({
    # Read spatial parquet file using sfarrow
    data <- sfarrow::st_read_parquet(file)
    
    # Preprocess the spatial data
    data <- preprocess_spatial_data(data, dataset_name)
    
    return(data)
    
  }, error = function(e) {
    return(NULL)
  })
}

#' Check if data has been preprocessed
#' 
#' @param data An sf object to check
#' @return Logical indicating if data has been preprocessed
is_preprocessed <- function(data) {
  if(is.null(data)) return(FALSE)
  
  preprocessed <- attr(data, "preprocessed")
  return(!is.null(preprocessed) && preprocessed == TRUE)
}