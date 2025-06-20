generate_habitat_sidebar <- function(habitat_layers, score_values) {
  # Create inputs for each habitat layer
  layer_inputs <- lapply(habitat_layers, function(layer_name) {
    # Create consistent IDs
    layer_id <- gsub(" ", "_", layer_name)
    layer_id <- gsub("[^A-Za-z0-9_]", "", layer_id)
    
    tagList(
      hr(),
      h5(layer_name),
      checkboxInput(paste0("EnableLayer_", layer_id), 
                    paste("Include", layer_name), 
                    value = FALSE),
      conditionalPanel(
        condition = paste0("input.EnableLayer_", layer_id, " == true"),
        pickerInput(
          paste0("ScorePicker_", layer_id),
          paste("Select score for", layer_name),
          choices = c("None", score_values),
          selected = "None"
        )
      )
    )
  })
  
  # Return the complete sidebar UI
  tagList(
    h4("Habitat Map Settings"),
    p("Select which habitat layers to include and their scores:"),
    layer_inputs,
    hr(),
    h4("Combined Map Settings"),
    actionButton("generateCombinedMap", "Generate Combined Map", 
                 class = "btn-primary btn-block"),
    # Export button
    hr(),
    h4("Export"),
    downloadButton("habitatExportRmd", "Export to R Markdown",
                   icon = icon("file-export"),
                   class = "btn-info btn-block")
  )
}