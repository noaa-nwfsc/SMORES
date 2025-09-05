generate_fisheries_combined_sidebar <- function(submodel_config, combined_maps_data) {
  
  # Check status of each component
  fisheries_ready <- combined_maps_data$fisheries_combined_map_generated %||% FALSE
  trawl_ready <- combined_maps_data$trawl_combined_map_generated %||% FALSE
  
  # At least one component must be ready to enable the submodel
  any_ready <- fisheries_ready || trawl_ready 
  
  # Function to get available methods based on generated data
  get_available_methods <- function(component_type, combined_maps_data) {
    methods <- list()
    
    if(component_type == "fisheries") {
      if(!is.null(combined_maps_data$fisheries_geo)) {
        methods[["Geometric Mean"]] <- "geometric_mean"
      }
      if(!is.null(combined_maps_data$fisheries_lowest)) {
        methods[["Lowest Value"]] <- "lowest"
      }
      if(!is.null(combined_maps_data$fisheries_product)) {
        methods[["Product"]] <- "product"
      }
    } else if(component_type == "trawl") {
      if(!is.null(combined_maps_data$trawl_geo)) {
        methods[["Geometric Mean"]] <- "geometric_mean"
      }
      if(!is.null(combined_maps_data$trawl_lowest)) {
        methods[["Lowest Value"]] <- "lowest"
      }
      if(!is.null(combined_maps_data$trawl_product)) {
        methods[["Product"]] <- "product"
      }
    }
    
    return(methods)
  }
  
  # Get available methods for each component
  fisheries_methods <- get_available_methods("fisheries", combined_maps_data)
  trawl_methods <- get_available_methods("trawl", combined_maps_data)
  
  tagList(
    h4("Fisheries Combined Submodel Configuration"),
    p("Select which components to include and their calculation methods for the combined Fisheries submodel."),
    
    # Component Selection Section
    h5("Component Selection"),
    p("Select which components to include in the combined submodel:"),
    
    # Fisheries Component Selection
    div(
      checkboxInput("includeFisheries",
                    label = div(
                      "Include Fisheries Component",
                      if(fisheries_ready) {
                        span(class = "text-success", " ✓ Ready")
                      } else {
                        span(class = "text-warning", " ⚠ Generate combined maps first")
                      }
                    ),
                    value = fisheries_ready),
      
      # Fisheries calculation method dropdown (only show if fisheries is selected and methods are available)
      conditionalPanel(
        condition = "input.includeFisheries == true",
        if(length(fisheries_methods) > 0) {
          selectInput("fisheriesCalculationMethod",
                      "Fisheries Calculation Method:",
                      choices = fisheries_methods,
                      selected = if("geometric_mean" %in% fisheries_methods) "geometric_mean" else fisheries_methods[[1]])
        } else {
          div(class = "text-danger", "No calculation methods available. Generate fisheries combined maps first.")
        },
        style = "margin-left: 20px; margin-top: 10px;"
      )
    ),
    
    br(),
    
    # Trawl Component Selection
    div(
      checkboxInput("includeTrawl",
                    label = div(
                      "Include Trawl Fisheries Component",
                      if(trawl_ready) {
                        span(class = "text-success", " ✓ Ready")
                      } else {
                        span(class = "text-warning", " ⚠ Generate combined maps first")
                      }
                    ),
                    value = trawl_ready),
      
      # Trawl calculation method dropdown (only show if trawl is selected and methods are available)
      conditionalPanel(
        condition = "input.includeTrawl == true",
        if(length(trawl_methods) > 0) {
          selectInput("trawlCalculationMethod",
                      "Trawl Fisheries Calculation Method:",
                      choices = trawl_methods,
                      selected = if("geometric_mean" %in% trawl_methods) "geometric_mean" else trawl_methods[[1]])
        } else {
          div(class = "text-danger", "No calculation methods available. Generate trawl fisheries combined maps first.")
        },
        style = "margin-left: 20px; margin-top: 10px;"
      )
    ),
    
    br(),
    # Validation Message
    div(id = "fisheriesCombinedValidation",
        uiOutput("fisheriesCombinedValidation")),
    
    # Generate Combined Submodel Button
    div(
      style = "margin-top: 15px;",
      conditionalPanel(
        condition = "input.includeFisheries == true || input.includeTrawl == true",
        actionButton("generateFisheriesCombinedSubmodel",
                     "Generate Combined Fisheries Submodel Maps",
                     class = "btn-primary btn-block",
                     icon = icon("calculator"))
      )
    ),
    hr(),
    # Export Section
    h5("Export"),
    downloadButton("fisheriesCombinedExport",
                   "Export Fisheries Combined Submodel Report",
                   icon = icon("file-export"),
                   class = "btn-info btn-block")
  )
}