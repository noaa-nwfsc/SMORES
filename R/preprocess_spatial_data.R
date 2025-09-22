#' Enhanced spatial data preprocessing function
#' 
#' @param data An sf object to preprocess
#' @param dataset_name Optional name for the dataset (used for logging)
#' @return Preprocessed spatial data
preprocess_spatial_data <- function(data, dataset_name = NULL) {
  if(is.null(data)) {
    cat(paste("✗ No data to process for:", dataset_name, "\n"))
    return(NULL)
  }
  
  # Skip preprocessing if already done
  if(is_preprocessed(data)) {
    cat(paste("↻ Already processed:", dataset_name, "\n"))
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
    
    cat(paste("✓ Processed:", dataset_name, "\n"))
    return(data)
    
  }, error = function(e) {
    cat(paste("✗ Error processing", dataset_name, ":", e$message, "\n"))
    return(data)  # Return original data if preprocessing fails
  })
}

#' Enhanced readRDS that includes automatic spatial preprocessing
#' 
#' @param file File path to read from
#' @param dataset_name Optional name for the dataset (used for logging)
#' @return Preprocessed spatial data
read_spatial_data <- function(file, dataset_name = NULL) {
  # Extract dataset name from filename if not provided
  if(is.null(dataset_name)) {
    dataset_name <- tools::file_path_sans_ext(basename(file))
  }
  
  # Check if file exists
  if(!file.exists(file)) {
    cat(paste("✗ File not found:", file, "\n"))
    return(NULL)
  }
  
  # Determine file type and read accordingly
  file_ext <- tolower(tools::file_ext(file))
  
  tryCatch({
    if(file_ext == "rds") {
      cat(paste("📁 Reading RDS file:", dataset_name, "\n"))
      data <- readRDS(file)
    } else if(file_ext == "parquet") {
      cat(paste("📁 Reading Parquet file:", dataset_name, "\n"))
      # Check if sfarrow is available for reading spatial parquet files
      if(requireNamespace("sfarrow", quietly = TRUE)) {
        # Try reading as spatial parquet first
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
    } else {
      stop(paste("Unsupported file format:", file_ext, ". Supported formats: rds, parquet"))
    }
    
    # Check if it's spatial data and preprocess
    if(inherits(data, "sf")) {
      data <- preprocess_spatial_data(data, dataset_name)
    } else {
      cat(paste("📊 Loaded (non-spatial):", dataset_name, "\n"))
    }
    
    return(data)
    
  }, error = function(e) {
    cat(paste("✗ Error reading", file, ":", e$message, "\n"))
    return(NULL)
  })
}

#' Wrapper function for backward compatibility
#' 
#' @param file File path to read from  
#' @param dataset_name Optional name for the dataset (used for logging)
#' @return Preprocessed spatial data
readRDS_preprocessed <- function(file, dataset_name = NULL) {
  read_spatial_data(file, dataset_name)
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

#' Utility function to check file format compatibility
#' 
#' @param file File path to check
#' @return Character string indicating the file type or "unsupported"
check_file_format <- function(file) {
  if(!file.exists(file)) return("not_found")
  
  file_ext <- tolower(tools::file_ext(file))
  
  if(file_ext %in% c("rds", "parquet")) {
    return(file_ext)
  } else {
    return("unsupported")
  }
}

#' Get file format statistics for a directory
#' 
#' @param dir_path Directory path to analyze
#' @return Data frame with file format counts
get_data_format_summary <- function(dir_path = "data") {
  if(!dir.exists(dir_path)) {
    cat(paste("Directory not found:", dir_path, "\n"))
    return(data.frame(format = character(0), count = integer(0)))
  }
  
  files <- list.files(dir_path, full.names = FALSE)
  extensions <- tolower(tools::file_ext(files))
  
  # Count file formats
  format_counts <- table(extensions)
  
  # Convert to data frame
  summary_df <- data.frame(
    format = names(format_counts),
    count = as.integer(format_counts),
    stringsAsFactors = FALSE
  )
  
  # Filter to relevant formats
  summary_df <- summary_df[summary_df$format %in% c("rds", "parquet", ""), ]
  summary_df$format[summary_df$format == ""] <- "no_extension"
  
  return(summary_df)
}