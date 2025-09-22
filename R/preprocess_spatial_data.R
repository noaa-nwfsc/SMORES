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
    attr(data, "preprocessed_timestamp") <- Sys.time()
    
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
    # Read parquet file - try sfarrow first for spatial parquet
    if(requireNamespace("sfarrow", quietly = TRUE)) {
      data <- sfarrow::st_read_parquet(file)
    } else {
      # Fallback to arrow if sfarrow is not available
      if(requireNamespace("arrow", quietly = TRUE)) {
        data <- arrow::read_parquet(file)
        # Convert to sf if it has geometry column
        if("geometry" %in% names(data) || any(grepl("geom", names(data), ignore.case = TRUE))) {
          data <- sf::st_as_sf(data)
        }
      } else {
        stop("Neither sfarrow nor arrow package is available for reading parquet files")
      }
    }
    
    # Check if it's spatial data and preprocess
    if(inherits(data, "sf")) {
      data <- preprocess_spatial_data(data, dataset_name)
    }
    
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

#' Get preprocessed bounding box
#' 
#' @param data An sf object that should have preprocessed bbox
#' @return The cached bounding box or calculated bbox if not cached
get_bbox_fast <- function(data) {
  if(is.null(data) || nrow(data) == 0) return(NULL)
  
  # Try to get cached bbox first
  cached_bbox <- attr(data, "bbox_precalc")
  if(!is.null(cached_bbox)) {
    return(cached_bbox)
  }
  
  # Fallback to calculation
  return(st_bbox(data))
}