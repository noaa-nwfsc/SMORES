generate_natural_resources_combined_sidebar <- function(submodel_config, combined_maps_data) {
  
  # Check status of each component
  habitat_ready <- combined_maps_data$habitat_combined_map_generated %||% FALSE
  species_ready <- combined_maps_data$species_combined_map_generated %||% FALSE
  
  # At least one component must be ready to enable the submodel
  any_ready <- habitat_ready || species_ready 
  
  # Function to get available methods based on generated data
  get_available_methods <- function(component_type, combined_maps_data) {
    methods <- list()
    
    if(component_type == "habitat") {
      if(!is.null(combined_maps_data$habitat_geo)) {
        methods[["Geometric Mean"]] <- "geometric_mean"
      }
      if(!is.null(combined_maps_data$habitat_lowest)) {
        methods[["Lowest Value"]] <- "lowest"
      }
      if(!is.null(combined_maps_data$habitat_product)) {
        methods[["Product"]] <- "product"
      }
    } else if(component_type == "species") {
      if(!is.null(combined_maps_data$species_geo)) {
        methods[["Geometric Mean"]] <- "geometric_mean"
      }
      if(!is.null(combined_maps_data$species_lowest)) {
        methods[["Lowest Value"]] <- "lowest"
      }
      if(!is.null(combined_maps_data$species_product)) {
        methods[["Product"]] <- "product"
      }
    }
    
    return(methods)
  }
  
  # Get available methods for each component
  habitat_methods <- get_available_methods("habitat", combined_maps_data)
  species_methods <- get_available_methods("species", combined_maps_data)
  
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
      
      # Habitat calculation method dropdown (only show if habitat is selected and methods are available)
      conditionalPanel(
        condition = "input.includeHabitat == true",
        if(length(habitat_methods) > 0) {
          selectInput("habitatCalculationMethod",
                      "Habitat Calculation Method:",
                      choices = habitat_methods,
                      selected = if("geometric_mean" %in% habitat_methods) "geometric_mean" else habitat_methods[[1]])
        } else {
          div(class = "text-danger", "No calculation methods available. Generate habitat combined maps first.")
        },
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
      
      # Species calculation method dropdown (only show if species is selected and methods are available)
      conditionalPanel(
        condition = "input.includeSpecies == true",
        if(length(species_methods) > 0) {
          selectInput("speciesCalculationMethod",
                      "Species Calculation Method:",
                      choices = species_methods,
                      selected = if("geometric_mean" %in% species_methods) "geometric_mean" else species_methods[[1]])
        } else {
          div(class = "text-danger", "No calculation methods available. Generate species combined maps first.")
        },
        style = "margin-left: 20px; margin-top: 10px;"
      )
    ),
    
    br(),
    # Validation Message
    div(id = "naturalResourcesCombinedValidation",
        uiOutput("naturalResourcesCombinedValidation")),
    
    # Generate Combined Submodel Button
    div(
      style = "margin-top: 15px;",
      conditionalPanel(
        condition = "input.includeHabitat == true || input.includeSpecies == true",
        actionButton("generateNaturalResourcesCombinedSubmodel",
                     "Generate Combined Natural Resources Submodel",
                     class = "btn-primary btn-block",
                     icon = icon("calculator"))
      )
    ),
    hr(),
    # Export Section
    h5("Export"),
      downloadButton("naturalResourcesCombinedExport",
                     "Export Combined Submodel",
                     icon = icon("file-export"),
                     class = "btn-info btn-block")
  )
}