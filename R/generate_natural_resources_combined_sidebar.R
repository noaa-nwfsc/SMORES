generate_natural_resources_combined_sidebar <- function(submodel_config, combined_maps_data) {
  
  # Check status of each component
  habitat_ready <- combined_maps_data$habitat_combined_map_generated %||% FALSE
  species_ready <- combined_maps_data$species_combined_map_generated %||% FALSE
  birds_ready <- FALSE  # Update when birds is implemented
  
  # Create status indicators
  habitat_status <- if(habitat_ready) {
    div(class = "alert alert-success", "✓ Habitat combined maps generated")
  } else {
    div(class = "alert alert-warning", "⚠ Generate habitat combined maps first")
  }
  
  species_status <- if(species_ready) {
    div(class = "alert alert-success", "✓ Species combined maps generated")
  } else {
    div(class = "alert alert-warning", "⚠ Generate species combined maps first")
  }
  
  birds_status <- if(birds_ready) {
    div(class = "alert alert-success", "✓ Birds combined maps generated")
  } else {
    div(class = "alert alert-warning", "⚠ Birds combined maps not yet implemented")
  }
  
  # At least one component must be ready to enable the submodel
  any_ready <- habitat_ready || species_ready || birds_ready
  
  tagList(
    h4("Natural Resources Submodel Configuration"),
    p("Configure which calculation methods to use for each component of the Natural Resources submodel."),
    
    # Component Status Section
    h5("Component Status"),
    habitat_status,
    species_status,
    birds_status,
    
    hr(),
    
    # Calculation Method Selection Section
    h5("Calculation Method Selection"),
    p("Select which calculation method to use for each component in the combined submodel:"),
    
    # Habitat Method Selection
    conditionalPanel(
      condition = "true", # Always show, but disable if not ready
      div(
        h6("Habitat Layers"),
        radioButtons("habitatSubmodelMethod",
                     label = NULL,
                     choices = list(
                       "Geometric Mean" = "geometric_mean",
                       "Lowest Value" = "lowest",
                       "Product" = "product"
                     ),
                     selected = "geometric_mean"),
        class = if(!habitat_ready) "text-muted" else ""
      )
    ),
    
    br(),
    
    # Species Method Selection
    conditionalPanel(
      condition = "true", # Always show, but disable if not ready
      div(
        h6("Species Layers"),
        radioButtons("speciesSubmodelMethod",
                     label = NULL,
                     choices = list(
                       "Geometric Mean" = "geometric_mean",
                       "Lowest Value" = "lowest",
                       "Product" = "product"
                     ),
                     selected = "geometric_mean"),
        class = if(!species_ready) "text-muted" else ""
      )
    ),
    
    br(),
    
    # Birds Method Selection (disabled until implemented)
    div(
      h6("Birds Layers"),
      radioButtons("birdsSubmodelMethod",
                   label = NULL,
                   choices = list(
                     "Geometric Mean" = "geometric_mean",
                     "Lowest Value" = "lowest", 
                     "Product" = "product"
                   ),
                   selected = "geometric_mean"),
      class = "text-muted"
    ),
    
    hr(),
    
    # Component Weights Section
    h5("Component Weights"),
    p("Set the relative importance of each component (weights will be normalized to sum to 1.0):"),
    
    # Habitat Weight
    conditionalPanel(
      condition = "true",
      numericInput("habitatWeight",
                   "Habitat Weight:",
                   value = 0.33,
                   min = 0,
                   max = 1,
                   step = 0.01)
    ),
    
    # Species Weight
    conditionalPanel(
      condition = "true",
      numericInput("speciesWeight", 
                   "Species Weight:",
                   value = 0.33,
                   min = 0,
                   max = 1,
                   step = 0.01)
    ),
    
    # Birds Weight (disabled until implemented)
    numericInput("birdsWeight",
                 "Birds Weight:",
                 value = 0.34,
                 min = 0,
                 max = 1,
                 step = 0.01),
    
    # Weight Validation Display
    div(id = "naturalResourcesWeightValidation"),
    
    hr(),
    
    # Generate Combined Model Section
    h5("Generate Combined Natural Resources Submodel"),
    
    conditionalPanel(
      condition = "true",
      if(any_ready) {
        actionButton("generateNaturalResourcesSubmodel",
                     "Generate Natural Resources Submodel",
                     class = "btn-primary btn-block")
      } else {
        div(
          class = "alert alert-info",
          "Generate at least one component's combined maps before creating the submodel."
        )
      }
    ),
    
    hr(),
    
    # Export Section
    h5("Export"),
    downloadButton("naturalResourcesSubmodelExport",
                   "Export Natural Resources Submodel",
                   icon = icon("file-export"),
                   class = "btn-info btn-block")
  )
}