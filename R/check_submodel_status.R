# Function to generate submodel status UI
check_submodel_status <- function(submodel_type, combined_maps_data) {
  # Define submodel configurations
  submodel_configs <- list(
    "natural_resources" = list(
      name = "Natural Resources",
      components = list(
        list(key = "habitat", name = "Habitat", data_key = "habitat"),
        list(key = "species", name = "Species", data_key = "species"),
        list(key = "birds", name = "Birds", data_key = "birds")
      )
    ),
    "industry_operations" = list(
      name = "Industry & Operations", 
      components = list(
        list(key = "surveys", name = "Scientific Surveys", data_key = "surveys"),
        list(key = "misc", name = "Miscellaneous", data_key = "misc")
      )
    )
  )
  
  # Get the configuration for the specified submodel
  config <- submodel_configs[[submodel_type]]
  
  if(is.null(config)) {
    return(div(
      class = "alert alert-danger",
      icon("exclamation-triangle"), 
      "Invalid submodel type specified."
    ))
  }
  
  # Check which components have been generated
  components_ready <- sapply(config$components, function(comp) {
    !is.null(combined_maps_data[[comp$data_key]])
  })
  
  # If no components are ready, show warning
  if(!any(components_ready)) {
    return(div(
      class = "alert alert-warning",
      icon("exclamation-triangle"), 
      paste0("No ", tolower(config$name), " submodel components have been generated yet. Please go to each tab and generate the combined maps first.")
    ))
  }
  
  # Generate status items for each component
  status_items <- list()
  
  for(i in seq_along(config$components)) {
    comp <- config$components[[i]]
    is_ready <- components_ready[i]
    
    if(is_ready) {
      status_items <- c(status_items, list(
        div(icon("check-circle", class = "text-success"), paste(" ", comp$name, "submodel ready"))
      ))
    } else {
      status_items <- c(status_items, list(
        div(icon("times-circle", class = "text-danger"), paste(" ", comp$name, "submodel not ready"))
      ))
    }
  }
  
  return(div(
    class = "alert alert-info",
    h5(paste(config$name, "Submodel Status:")),
    status_items
  ))
}