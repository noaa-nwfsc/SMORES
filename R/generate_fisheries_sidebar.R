generate_fisheries_sidebar <- function(fisheries_layers, score_values, current_tab = NULL, submodel_config = NULL) {
  
  # Individual fisheries tab logic
  # Create inputs for each fisheries layer
  layer_inputs <- lapply(fisheries_layers, function(layer_name) { 
    # Create consistent IDs
    layer_id <- gsub(" ", "_", layer_name)
    layer_id <- gsub("[^A-Za-z0-9_]", "", layer_id)
    
    tagList(
      hr(),
      h5(layer_name),
      checkboxInput(paste0("EnableFisheriesLayer_", layer_id), 
                    paste("Include", layer_name), 
                    value = FALSE),
      conditionalPanel(
        condition = paste0("input.EnableFisheriesLayer_", layer_id, " == true"),
        pickerInput(
          paste0("FisheriesScorePicker_", layer_id),
          paste("Select score for", layer_name),
          choices = c("None", score_values),
          selected = "None"
        )
      )
    )
  })
  
  # Return the complete sidebar UI for fisheries tab
  tagList(
    h4("Fisheries Map Settings"), 
    p("Select which fisheries layers to include and their scores:"),
    layer_inputs,
    hr(),
    h4("Calculation Methods"),
    checkboxGroupInput("fisheriesCalculationMethods",
                       "Select calculation methods to generate:",
                       choices = list(
                         "Geometric Mean" = "geometric_mean",
                         "Lowest Value" = "lowest",
                         "Product" = "product"
                       ),
                       selected = "geometric_mean"),
    hr(),
    h4("Combined Map Settings"),
    actionButton("generateCombinedFisheriesMap", "Generate Combined Map(s)", 
                 class = "btn-primary btn-block"),
    # Export button
    hr(),
    h4("Export"),
    downloadButton("fisheriesExportRmd", "Export to R Markdown",
                   icon = icon("file-export"),
                   class = "btn-info btn-block")
  )
}