generate_natural_resources_combined_sidebar <- function() {
  tagList(
    h4("Natural Resources Combined Submodel Configuration"),
    p(
      "Select which components to include and their calculation methods for the combined Natural Resources submodel."
    ),

    # Component Selection Section
    h5("Component Selection"),
    p("Select which components to include in the combined submodel:"),

    # Habitat Component Selection
    div(
      checkboxInput(
        "includeHabitat",
        "Include Habitat Component",
        value = FALSE
      ),

      conditionalPanel(
        condition = "input.includeHabitat == true",
        selectInput(
          "habitatCalculationMethod",
          "Habitat Calculation Method:",
          choices = c(
            "Geometric Mean" = "geometric_mean",
            "Lowest Value" = "lowest",
            "Product" = "product"
          )
        ),
        style = "margin-left: 20px; margin-top: 10px;"
      )
    ),

    br(),

    # Species Component Selection
    div(
      checkboxInput(
        "includeSpecies",
        "Include Species Component",
        value = FALSE
      ),

      conditionalPanel(
        condition = "input.includeSpecies == true",
        selectInput(
          "speciesCalculationMethod",
          "Species Calculation Method:",
          choices = c(
            "Geometric Mean" = "geometric_mean",
            "Lowest Value" = "lowest",
            "Product" = "product"
          )
        ),
        style = "margin-left: 20px; margin-top: 10px;"
      )
    ),

    br(),

    # Validation Message
    div(
      id = "naturalResourcesCombinedValidation",
      uiOutput("naturalResourcesCombinedValidation")
    ),

    # Generate Combined Submodel Button
    div(
      style = "margin-top: 15px;",
      conditionalPanel(
        condition = "input.includeHabitat == true || input.includeSpecies == true",
        actionButton(
          "generateNaturalResourcesCombinedSubmodel",
          "Generate Combined Natural Resources Submodel Maps",
          class = "btn-primary btn-block",
          icon = icon("calculator")
        )
      )
    ),
    hr(),
    # Export Section
    h5("Export"),
    downloadButton(
      "naturalResourcesCombinedExport",
      "Export Natural Resources Combined Submodel Report",
      icon = icon("file-export"),
      class = "btn-info btn-block"
    )
  )
}
