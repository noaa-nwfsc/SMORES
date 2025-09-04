generate_industry_operations_combined_sidebar <- function(submodel_config, combined_maps_data) {
  
  # Check status of each Industry & Operations component
  surveys_ready <- combined_maps_data$surveys_combined_map_generated %||% FALSE
  cables_ready <- combined_maps_data$cables_combined_map_generated %||% FALSE
  
  # At least one component must be ready to enable the submodel
  any_ready <- surveys_ready || cables_ready 
  
  # Function to get available methods based on generated data
  get_available_methods <- function(component_type, combined_maps_data) {
    methods <- list()
    
    if(component_type == "surveys") {
      if(!is.null(combined_maps_data$surveys_geo)) {
        methods[["Geometric Mean"]] <- "geometric_mean"
      }
      if(!is.null(combined_maps_data$surveys_lowest)) {
        methods[["Lowest Value"]] <- "lowest"
      }
      if(!is.null(combined_maps_data$surveys_product)) {
        methods[["Product"]] <- "product"
      }
    } else if(component_type == "cables") {
      if(!is.null(combined_maps_data$cables_geo)) {
        methods[["Geometric Mean"]] <- "geometric_mean"
      }
      if(!is.null(combined_maps_data$cables_lowest)) {
        methods[["Lowest Value"]] <- "lowest"
      }
      if(!is.null(combined_maps_data$cables_product)) {
        methods[["Product"]] <- "product"
      }
    }
    
    return(methods)
  }
  
  # Get available methods for each component
  surveys_methods <- get_available_methods("surveys", combined_maps_data)
  cables_methods <- get_available_methods("cables", combined_maps_data)
  
  tagList(
    h4("Industry & Operations Combined Submodel Configuration"),
    p("Select which components to include and their calculation methods for the combined Industry & Operations submodel."),
    
    # Component Selection Section
    h5("Component Selection"),
    p("Select which components to include in the combined submodel:"),
    
    # Habitat Component Selection
    div(
      checkboxInput("includeSurveys",
                    label = div(
                      "Include Scientific Surveys Component",
                      if(surveys_ready) {
                        span(class = "text-success", " ✓ Ready")
                      } else {
                        span(class = "text-warning", " ⚠ Generate combined maps first")
                      }
                    ),
                    value = surveys_ready),
      
      # Surveys calculation method dropdown (only show if surveys is selected and methods are available)
      conditionalPanel(
        condition = "input.includeSurveys == true",
        if(length(surveys_methods) > 0) {
          selectInput("surveysCalculationMethod",
                      "Scientific Surveys Calculation Method:",
                      choices = surveys_methods,
                      selected = if("geometric_mean" %in% surveys_methods) "geometric_mean" else surveys_methods[[1]])
        } else {
          div(class = "text-warning", "No calculation methods available. Generate Scientific Surveys combined maps first.")
        },
        style = "margin-left: 20px; margin-top: 10px;"
      )
    ),
    
    br(),
    
    # Submarine Cables Component Selection
    div(
      checkboxInput("includeCables",
                    label = div(
                      "Include Submarine Cable Component",
                      if(cables_ready) {
                        span(class = "text-success", " ✓ Ready")
                      } else {
                        span(class = "text-warning", " ⚠ Generate combined maps first")
                      }
                    ),
                    value = cables_ready),
      
      # Submarine Cable calculation method dropdown (only show if species is selected and methods are available)
      conditionalPanel(
        condition = "input.includeCables == true",
        if(length(cables_methods) > 0) {
          selectInput("cablesCalculationMethod",
                      "Submarine Cable Calculation Method:",
                      choices = cables_methods,
                      selected = if("geometric_mean" %in% cables_methods) "geometric_mean" else cables_methods[[1]])
        } else {
          div(class = "text-warning", "No calculation methods available. Generate Submarine Cables combined maps first.")
        },
        style = "margin-left: 20px; margin-top: 10px;"
      )
    ),
    
    br(),
    
    # Validation Message
    div(id = "industryOperationsCombinedValidation",
        uiOutput("industryOperationsCombinedValidation")),
    
    # Generate Combined Submodel Button
    div(
      style = "margin-top: 15px;",
      conditionalPanel(
        condition = "input.includeSurveys == true || input.includeCables == true",
        actionButton("generateIndustryOperationsCombinedSubmodel",
                     "Generate Combined Industry & Operations Submodel",
                     class = "btn-primary btn-block",
                     icon = icon("calculator"))
      )
    ),
    hr(),
    # Export Section
    h5("Export"),
      downloadButton("industryOperationsCombinedExport",
                     "Export Industry & Operations Combined Submodel Report",
                     icon = icon("file-export"),
                     class = "btn-info btn-block")
  )
}