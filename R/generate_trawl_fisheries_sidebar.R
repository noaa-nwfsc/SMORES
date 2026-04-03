generate_trawl_fisheries_sidebar <- function(
  trawl_fisheries_layer,
  score_values_trawl_fisheries
) {
  # Individual trawl fisheries tab logic
  # Create inputs for each trawl fisheries layer - use names() to get the layer names
  layer_inputs <- lapply(names(trawl_fisheries_layer), function(layer_name) {
    # Create consistent IDs
    layer_id <- gsub(" ", "_", layer_name)
    layer_id <- gsub("[^A-Za-z0-9_]", "", layer_id)

    tagList(
      hr(),
      h5(layer_name),
      checkboxInput(
        paste0("EnableTrawlLayer_", layer_id),
        paste("Include", layer_name),
        value = FALSE
      ),
      conditionalPanel(
        condition = paste0("input.EnableTrawlLayer_", layer_id, " == true"),
        pickerInput(
          paste0("TrawlScorePicker_", layer_id),
          paste("Select score for", layer_name),
          choices = c("None", score_values_trawl_fisheries),
          selected = "None"
        )
      )
    )
  })

  # Return the complete sidebar UI for individual cables tab
  tagList(
    h4("Trawl Fisheries Map Settings"),
    p(
      "Select if you would like to include the trawl fisheries layer and its scores:"
    ),

    layer_inputs,

    hr(),

    actionButton(
      "update_trawl_map_btn",
      "Generate Trawl Fisheries Maps",
      class = "btn-primary btn-block"
    ),
    hr(),

    h4("Calculation Methods"),
    checkboxGroupInput(
      "trawlCalculationMethods",
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
      "generateCombinedTrawlMap",
      "Generate Combined Map(s)",
      class = "btn-primary btn-block"
    ),
    hr(),
    # Export button
    h4("Export"),
    downloadButton(
      "trawlExportRmd",
      "Export Trawl Fisheries Component Report",
      icon = icon("file-export"),
      class = "btn-info btn-block"
    )
  )
}
