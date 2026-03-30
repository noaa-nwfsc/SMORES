generate_species_sidebar <- function(species_layers, score_values) {
  # Individual species tab logic
  # Create inputs for each species layer
  layer_inputs <- lapply(species_layers, function(layer_name) {
    # Create consistent IDs
    layer_id <- gsub(" ", "_", layer_name)
    layer_id <- gsub("[^A-Za-z0-9_]", "", layer_id)

    tagList(
      hr(),
      h5(layer_name),
      checkboxInput(
        paste0("EnableSpeciesLayer_", layer_id),
        paste("Include", layer_name),
        value = FALSE
      ),
      conditionalPanel(
        condition = paste0("input.EnableSpeciesLayer_", layer_id, " == true"),
        pickerInput(
          paste0("SpeciesScorePicker_", layer_id),
          paste("Select score for", layer_name),
          choices = c("None", score_values),
          selected = "None"
        )
      )
    )
  })

  # Return the complete sidebar UI for individual species tab
  tagList(
    h4("Species Map Settings"),
    p("Select which species layers to include and their scores:"),
    layer_inputs,

    hr(),

    actionButton(
      "update_species_map_btn",
      "Generate Species Maps",
      class = "btn-primary btn-block"
    ),
    hr(),

    h4("Calculation Methods"),
    checkboxGroupInput(
      "speciesCalculationMethods",
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
      "generateCombinedSpeciesMap",
      "Generate Combined Map(s)",
      class = "btn-primary btn-block"
    ),
    # Export button
    hr(),
    h4("Export"),
    downloadButton(
      "speciesExportRmd",
      "Export Species Component Report",
      icon = icon("file-export"),
      class = "btn-info btn-block"
    )
  )
}
