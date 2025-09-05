generate_cables_sidebar <- function(submarine_cables_layer, score_values, current_tab = NULL, submodel_config = NULL) {
  
  # If this is a combined model tab, delegate to the combined model sidebar
  if(!is.null(current_tab) && current_tab == "combined_model_industry_operations") {
    return(generate_combined_model_sidebar(submodel_config))
  }
  
  # Individual cables tab logic
  # Create inputs for each cable layer - use names() to get the layer names
  layer_inputs <- lapply(names(submarine_cables_layer), function(layer_name) {
    # Create consistent IDs
    layer_id <- gsub(" ", "_", layer_name)
    layer_id <- gsub("[^A-Za-z0-9_]", "", layer_id)
    
    tagList(
      hr(),
      h5(layer_name),
      checkboxInput(paste0("EnableCablesLayer_", layer_id), 
                    paste("Include", layer_name), 
                    value = FALSE),
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
    h4("Calculation Methods"),
    checkboxGroupInput("cablesCalculationMethods",
                       "Select calculation methods to generate:",
                       choices = list(
                         "Geometric Mean" = "geometric_mean",
                         "Lowest Value" = "lowest",
                         "Product" = "product"
                       ),
                       selected = "geometric_mean"),
    hr(),
    h4("Combined Map Settings"),
    actionButton("generateCombinedCablesMap", "Generate Combined Map(s)", 
                 class = "btn-primary btn-block"),
    # Export button
    hr(),
    h4("Export"),
    downloadButton("cablesExportRmd", "Export Submarine Cables Component Report",
                   icon = icon("file-export"),
                   class = "btn-info btn-block")
  )
}