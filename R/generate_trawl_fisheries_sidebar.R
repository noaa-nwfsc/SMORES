generate_trawl_fisheries_sidebar <- function(trawl_fisheries_layer, score_values_trawl_fisheries, current_tab = NULL, submodel_config = NULL) {
  
  # If this is a combined model tab, delegate to the combined model sidebar
  if(!is.null(current_tab) && current_tab == "combined_model_fisheries") {
    return(generate_combined_model_sidebar(submodel_config))
  }
  
  # Individual trawl fisheries tab logic
  # Create inputs for each trawl fisheries layer - use names() to get the layer names
  layer_inputs <- lapply(names(trawl_fisheries_layer), function(layer_name) {
    # Create consistent IDs
    layer_id <- gsub(" ", "_", layer_name)
    layer_id <- gsub("[^A-Za-z0-9_]", "", layer_id)
    
    tagList(
      hr(),
      h5(layer_name),
      checkboxInput(paste0("EnableTrawlLayer_", layer_id), 
                    paste("Include", layer_name), 
                    value = FALSE),
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
    p("Select if you would like to include the trawl fisheries layer and its scores:"),
    layer_inputs,
    hr(),
    h4("Calculation Methods"),
    checkboxGroupInput("trawlCalculationMethods",
                       "Select calculation methods to generate:",
                       choices = list(
                         "Geometric Mean" = "geometric_mean",
                         "Lowest Value" = "lowest",
                         "Product" = "product"
                       ),
                       selected = "geometric_mean"),
    hr(),
    h4("Combined Map Settings"),
    actionButton("generateCombinedTrawlMap", "Generate Combined Map", 
                 class = "btn-primary btn-block"),
    # Export button
    hr(),
    h4("Export"),
    downloadButton("trawlExportRmd", "Export to HTML File",
                   icon = icon("file-export"),
                   class = "btn-info btn-block")
  )
}