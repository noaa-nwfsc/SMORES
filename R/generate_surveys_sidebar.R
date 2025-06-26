generate_surveys_sidebar <- function(surveys_layers, score_values, current_tab = NULL, submodel_config = NULL) {
  
  # If this is a combined model tab, delegate to the combined model sidebar
  if(!is.null(current_tab) && current_tab == "combined_model_industry_operations") {
    return(generate_combined_model_sidebar(submodel_config))
  }
  
  # Individual species tab logic
  # Create inputs for each species layer
  layer_inputs <- lapply(surveys_layers, function(layer_name) {
    # Create consistent IDs
    layer_id <- gsub(" ", "_", layer_name)
    layer_id <- gsub("[^A-Za-z0-9_]", "", layer_id)
    
    tagList(
      hr(),
      h5(layer_name),
      checkboxInput(paste0("EnableSurveysLayer_", layer_id), 
                    paste("Include", layer_name), 
                    value = FALSE),
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
  
  # Return the complete sidebar UI for individual species tab
  tagList(
    h4("Scientific Surveys Map Settings"),
    p("Select which survey layers to include and their scores:"),
    layer_inputs,
    hr(),
    h4("Combined Map Settings"),
    actionButton("generateCombinedSurveysMap", "Generate Combined Map", 
                 class = "btn-primary btn-block"),
    # Export button
    hr(),
    h4("Export"),
    downloadButton("surveysExportRmd", "Export to R Markdown",
                   icon = icon("file-export"),
                   class = "btn-info btn-block")
  )
}