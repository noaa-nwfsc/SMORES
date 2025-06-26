# Generic combined model sidebar generator
generate_combined_model_sidebar <- function(submodel_config) {
  if(is.null(submodel_config)) {
    return(div(class = "alert alert-danger", "Submodel configuration not provided"))
  }
  
  # Generate weight sliders based on configuration
  weight_sliders <- generate_weight_sliders(submodel_config$components)
  
  # Generate status display
  status_display <- generate_status_display(submodel_config$name, submodel_config$status_output_id)
  
  # Generate export section
  export_section <- generate_export_section(submodel_config$export_config)
  
  tagList(
    h4("Combined Model Settings"),
    hr(),
    h5("Submodel Status"),
    status_display,
    hr(),
    h5("Model Weights"),
    weight_sliders,
    # Weight validation
    htmlOutput("weightValidation"),
    hr(),
    # Button to generate the combined model
    actionButton(submodel_config$generate_button_id, "Generate Combined Model", 
                 class = "btn-primary btn-block"),
    # Export section
    export_section
  )
}

# Helper function to generate weight sliders
generate_weight_sliders <- function(components) {
  weight_sliders <- lapply(components, function(comp) {
    sliderInput(
      comp$weight_id, 
      comp$weight_label, 
      min = 0, 
      max = 1, 
      value = comp$default_weight, 
      step = 0.01
    )
  })
  
  return(weight_sliders)
}

# Helper function to generate status display
generate_status_display <- function(submodel_name, status_output_id) {
  htmlOutput(status_output_id)
}

# Helper function to generate export section
generate_export_section <- function(export_config) {
  tagList(
    hr(), 
    h4("Export"),
    downloadButton(
      export_config$button_id, 
      export_config$button_label,
      icon = icon("file-export"),
      class = "btn-info btn-block"
    )
  )
}
