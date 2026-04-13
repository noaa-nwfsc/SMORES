generate_surveys_sidebar <- function(surveys_layers, score_values) {
  # Individual surveys tab logic
  # Create inputs for each survey layer
  layer_inputs <- lapply(surveys_layers, function(layer_name) {
    # Create consistent IDs
    layer_id <- gsub(" ", "_", layer_name)
    layer_id <- gsub("[^A-Za-z0-9_]", "", layer_id)

    tagList(
      hr(),
      h5(layer_name),
      checkboxInput(
        paste0("EnableSurveysLayer_", layer_id),
        paste("Include", layer_name),
        value = FALSE
      ),
      conditionalPanel(
        condition = paste0("input.EnableSurveysLayer_", layer_id, " == true"),
        pickerInput(
          paste0("SurveysScorePicker_", layer_id),
          paste("Select score for", layer_name),
          choices = c("None", score_values),
          selected = "None"
        )
      )
    )
  })

  # Return the complete sidebar UI for individual surveys tab
  tagList(
    h4("Surveys Map Settings"),
    p("Select which survey layers to include and their scores:"),

    layer_inputs,

    hr(),

    actionButton(
      "update_surveys_map_btn",
      "Generate Scientific Survey Maps",
      class = "btn-primary btn-block"
    ),
    hr(),

    h4("Calculation Methods"),
    checkboxGroupInput(
      "surveysCalculationMethods",
      "Select calculation methods to generate:",
      choices = list(
        "Geometric Mean" = "geometric_mean",
        "Lowest Value" = "lowest",
        "Product" = "product"
      ),
      selected = "geometric_mean"
    ),

    hr(),

    h4("Combined Map Settings"),
    actionButton(
      "generateCombinedSurveysMap",
      "Generate Combined Map(s)",
      class = "btn-primary btn-block"
    ),

    hr(),
    # Export button
    h4("Export"),
    downloadButton(
      "surveysExportRmd",
      "Export Scientific Surveys Component Report",
      icon = icon("file-export"),
      class = "btn-info btn-block"
    )
  )
}
