generate_industry_operations_combined_sidebar <- function() {
  tagList(
    h4("Industry & Operations Combined Submodel Configuration"),
    p(
      "Select which components to include and their calculation methods for the combined Industry & Operations submodel."
    ),

    # Component Selection Section
    h5("Component Selection"),
    p("Select which components to include in the combined submodel:"),

    # Surveys Component Selection
    div(
      checkboxInput(
        "includeSurveys",
        "Include Scientific Surveys Component",
        value = FALSE
      ),

      conditionalPanel(
        condition = "input.includeSurveys == true",
        selectInput(
          "surveysCalculationMethod",
          "Scientific Surveys Calculation Method:",
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

    # Cables Component Selection
    div(
      checkboxInput(
        "includeCables",
        "Include Submarine Cables Component",
        value = FALSE
      ),

      conditionalPanel(
        condition = "input.includeCables == true",
        selectInput(
          "cablesCalculationMethod",
          "Submarine Cables Calculation Method:",
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
      id = "industryOperationsCombinedValidation",
      uiOutput("industryOperationsCombinedValidation")
    ),

    # Generate Combined Submodel Button
    div(
      style = "margin-top: 15px;",
      conditionalPanel(
        condition = "input.includeSurveys == true || input.includeCables == true",
        actionButton(
          "generateIndustryOperationsCombinedSubmodel",
          "Generate Combined Industry & Operations Submodel Maps",
          class = "btn-primary btn-block",
          icon = icon("calculator")
        )
      )
    ),
    hr(),
    # Export Section
    h5("Export"),
    downloadButton(
      "industryOperationsCombinedExport",
      "Export Industry & Operations Combined Submodel Report",
      icon = icon("file-export"),
      class = "btn-info btn-block"
    )
  )
}
