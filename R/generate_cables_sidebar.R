generate_cables_sidebar <- function(submarine_cables_layer, score_values) {
  # Individual cables tab logic
  # Create inputs for each cable layer - use names() to get the layer names
  layer_inputs <- lapply(submarine_cables_layer, function(layer_name) {
    # Create consistent IDs
    layer_id <- gsub(" ", "_", layer_name)
    layer_id <- gsub("[^A-Za-z0-9_]", "", layer_id)

    tagList(
      hr(),
      h5(layer_name),
      checkboxInput(
        paste0("EnableCablesLayer_", layer_id),
        paste("Include", layer_name),
        value = FALSE
      ),
      conditionalPanel(
        condition = paste0("input.EnableCablesLayer_", layer_id, " == true"),
        pickerInput(
          paste0("CablesScorePicker_", layer_id),
          paste("Select score for", layer_name),
          choices = c("None", score_values),
          selected = "None"
        )
      )
    )
  })

  # Return the complete sidebar UI for individual cables tab
  tagList(
    h4("Cables Map Settings"),
    p("Select which cable layers to include and their scores:"),

    layer_inputs,

    hr(),

    actionButton(
      "update_cables_map_btn",
      "Generate Submarine Cables Maps",
      class = "btn-primary btn-block"
    ),
    hr(),

    h4("Calculation Methods"),
    checkboxGroupInput(
      "cablesCalculationMethods",
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
      "generateCombinedCablesMap",
      "Generate Combined Map(s)",
      class = "btn-primary btn-block"
    ),

    hr(),

    # Export button
    h4("Export"),
    downloadButton(
      "cablesExportRmd",
      "Export Submarine Cables Component Report",
      icon = icon("file-export"),
      class = "btn-info btn-block"
    )
  )
}
