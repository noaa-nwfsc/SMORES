generate_fisheries_combined_sidebar <- function() {
  tagList(
    h4("Fisheries Combined Submodel Configuration"),
    p(
      "Select which components to include and their calculation methods for the combined Fisheries submodel."
    ),

    # Component Selection Section
    h5("Component Selection"),
    p("Select which components to include in the combined submodel:"),

    # Fisheries Component Selection
    div(
      checkboxInput(
        "includeFisheries",
        "Include Fisheries Component",
        value = FALSE
      ),

      conditionalPanel(
        condition = "input.includeFisheries == true",
        selectInput(
          "fisheriesCalculationMethod",
          "Fisheries Calculation Method:",
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

    # Trawl Component Selection
    div(
      checkboxInput(
        "includeTrawl",
        "Include Trawl Fisheries Component",
        value = FALSE
      ),

      conditionalPanel(
        condition = "input.includeTrawl == true",
        selectInput(
          "trawlCalculationMethod",
          "Trawl Fisheries Calculation Method:",
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
      id = "fisheriesCombinedValidation",
      uiOutput("fisheriesCombinedValidation")
    ),

    # Generate Combined Submodel Button
    div(
      style = "margin-top: 15px;",
      conditionalPanel(
        condition = "input.includeFisheries == true || input.includeTrawl == true",
        actionButton(
          "generateFisheriesCombinedSubmodel",
          "Generate Combined Fisheries Submodel Maps",
          class = "btn-primary btn-block",
          icon = icon("calculator")
        )
      )
    ),
    hr(),
    # Export Section
    h5("Export"),
    downloadButton(
      "fisheriesCombinedExport",
      "Export Fisheries Combined Submodel Report",
      icon = icon("file-export"),
      class = "btn-info btn-block"
    )
  )
}
