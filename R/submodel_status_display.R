submodel_status_display <- function(submodel_name, status_info) {
  # Create standardized status display for submodels
  alert_class <- if(status_info$available) "alert-success" else "alert-warning"
  
  content <- list(h6(paste(submodel_name, "Submodel")))
  
  if(status_info$available) {
    content <- c(content, list("✓ Available"))
    # Add component-specific status
    for(comp_name in names(status_info$components)) {
      if(status_info$components[[comp_name]]) {
        content <- c(content, list(div(paste("•", comp_name, "ready"))))
      }
    }
  } else {
    content <- c(content, list("⚠ Not ready - Generate component maps first"))
  }
  
  div(class = alert_class, content)
}