generate_natural_resources_combined_sidebar <- function(submodel_config, combined_maps_data) {
  
  # Check status of each component
  habitat_ready <- combined_maps_data$habitat_combined_map_generated %||% FALSE
  species_ready <- combined_maps_data$species_combined_map_generated %||% FALSE
  birds_ready <- FALSE  # Update when birds is implemented
  
  # At least one component must be ready to enable the submodel
  any_ready <- habitat_ready || species_ready || birds_ready
  
  tagList(
    h4("Natural Resources Combined Submodel Configuration"),
    p("Select which components to include and their calculation methods for the combined Natural Resources submodel."),
    
    # Component Selection Section
    h5("Component Selection"),
    p("Select which components to include in the combined submodel:"),
    
    # Habitat Component Selection
    div(
      checkboxInput("includeHabitat",
                    label = div(
                      "Include Habitat Component",
                      if(habitat_ready) {
                        span(class = "text-success", " ✓ Ready")
                      } else {
                        span(class = "text-warning", " ⚠ Generate combined maps first")
                      }
                    ),
                    value = habitat_ready),
      
      # Habitat calculation method dropdown (only show if habitat is selected)
      conditionalPanel(
        condition = "input.includeHabitat == true",
        selectInput("habitatCalculationMethod",
                    "Habitat Calculation Method:",
                    choices = list(
                      "Geometric Mean" = "geometric_mean",
                      "Lowest Value" = "lowest",
                      "Product" = "product"
                    ),
                    selected = "geometric_mean"),
        style = "margin-left: 20px; margin-top: 10px;"
      )
    ),
    
    br(),
    
    # Species Component Selection
    div(
      checkboxInput("includeSpecies",
                    label = div(
                      "Include Species Component",
                      if(species_ready) {
                        span(class = "text-success", " ✓ Ready")
                      } else {
                        span(class = "text-warning", " ⚠ Generate combined maps first")
                      }
                    ),
                    value = species_ready),
      
      # Species calculation method dropdown (only show if species is selected)
      conditionalPanel(
        condition = "input.includeSpecies == true",
        selectInput("speciesCalculationMethod",
                    "Species Calculation Method:",
                    choices = list(
                      "Geometric Mean" = "geometric_mean",
                      "Lowest Value" = "lowest",
                      "Product" = "product"
                    ),
                    selected = "geometric_mean"),
        style = "margin-left: 20px; margin-top: 10px;"
      )
    ),
    
    br(),
    
    # Birds Component Selection (disabled until implemented)
    div(
      checkboxInput("includeBirds",
                    label = div(
                      "Include Birds Component",
                      span(class = "text-muted", " (Not yet implemented)")
                    ),
                    value = FALSE),
      
      # Birds calculation method dropdown (disabled)
      conditionalPanel(
        condition = "input.includeBirds == true",
        selectInput("birdsCalculationMethod",
                    "Birds Calculation Method:",
                    choices = list(
                      "Geometric Mean" = "geometric_mean",
                      "Lowest Value" = "lowest",
                      "Product" = "product"
                    ),
                    selected = "geometric_mean"),
        style = "margin-left: 20px; margin-top: 10px;"
      ),
      class = "text-muted"
    ),
    
    hr(),
    
    # Validation Message
    div(id = "naturalResourcesCombinedValidation",
        uiOutput("naturalResourcesCombinedValidation")),
    
    # Generate Combined Submodel Button
    div(
      style = "margin-top: 15px;",
      conditionalPanel(
        condition = "input.includeHabitat == true || input.includeSpecies == true || input.includeBirds == true",
        actionButton("generateNaturalResourcesCombinedSubmodel",
                     "Generate Combined Natural Resources Submodel",
                     class = "btn-primary btn-block",
                     icon = icon("calculator"))
      )
    ),
    hr(),
    # Export Section
    h5("Export"),
    conditionalPanel(
      condition = "output.naturalResourcesCombinedMap",
      downloadButton("naturalResourcesCombinedExport",
                     "Export Combined Submodel",
                     icon = icon("file-export"),
                     class = "btn-info btn-block")
    )
  )
}