generate_species_sidebar <- function(species_layers, score_values, current_tab = NULL, submodel_config = NULL) {
  
  # If this is a combined model tab, delegate to the combined model sidebar
  if(!is.null(current_tab) && current_tab == "combined_model_natural_resources") {
    return(generate_combined_model_sidebar(submodel_config))
  }
  
  # Individual species tab logic
  # Create inputs for each species layer
  layer_inputs <- lapply(species_layers, function(layer_name) {
    # Create consistent IDs
    layer_id <- gsub(" ", "_", layer_name)
    layer_id <- gsub("[^A-Za-z0-9_]", "", layer_id)
    
    tagList(
      hr(),
      h5(layer_name),
      checkboxInput(paste0("EnableSpeciesLayer_", layer_id), 
                    paste("Include", layer_name), 
                    value = FALSE),
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
    hr(),
    h4("Calculation Methods"),
    checkboxGroupInput("speciesCalculationMethods",
                       "Select calculation methods to generate:",
                       choices = list(
                         "Geometric Mean" = "geometric_mean",
                         "Lowest Value" = "lowest",
                         "Product" = "product"
                       ),
                       selected = "geometric_mean"),
    hr(),
    h4("Combined Map Settings"),
    actionButton("generateCombinedSpeciesMap", "Generate Combined Map", 
                 class = "btn-primary btn-block"),
    # Export button
    hr(),
    h4("Export"),
    downloadButton("speciesExportRmd", "Export to R Markdown",
                   icon = icon("file-export"),
                   class = "btn-info btn-block")
  )
}