generate_industry_sidebar <- function(industry_layers, score_values){
  # create inputs for each industry layer
  layer_inputs <- lapply(industry_layers, function (layer_name){
    #create consistent ids
    layer_id <- gsub(" ", "_", layer_name)
    layer_id <- gsub("[^A-Za-z0-9_]", "", layer_id)
    
    tagList(
      hr(), 
      h5(layer_name), 
      checkboxInput(paste0("EnableIndustryLayer_", layer_id),
                    paste("Include", layer_name),
                    value = FALSE),
      conditionalPanel(
        condition = paste0("input.EnableIndustryLayer_", layer_id, "== true"), 
        pickerInput(
          paste0("IndustryScorePicker_", layer_id),
          paste("Select score for", layer_name),
          choices = c("None", score_values), 
          selected = "None"
        )
      )
    )
  })
  
  # Return the complete sidebar UI
  tagList(
    h4("Industry & Operations Map Settings"),
    p("Select which industry layers to include and their scores:"),
    layer_inputs, 
    hr(), 
    h4("Combined Map Settings"), 
    actionButton("generateIndustryMap", "Generate Combined Industry Map",
                 class = "btn-primary btn-block"),
    # Export Button
    hr(), 
    h4("Export"),
    downloadButton("industryExportRmd", "Export to R Markdown",
                   icon = icon("file-export"),
                   class = "btn-info btn-block")
  )
}