# Generate sidebar for overall model tab
generate_full_model_sidebar <- function() {
  tagList(
    h5("Submodel Availability"),
    htmlOutput("fullModelSubmodelStatus"),
    hr(),
    h4("Full Model Settings"),
    hr(),
    
    # Submodel Selection Section
    h5("Available Submodels"),
    div(
      class = "submodel-selection",
      checkboxInput(
        "enableNaturalResources", 
        "Natural Resources Submodel", 
        value = FALSE
      ),
      conditionalPanel(
        condition = "input.enableNaturalResources == true",
        sliderInput(
          "weightNaturalResources",
          "Weight for Natural Resources:",
          min = 0,
          max = 1,
          value = 0.33,
          step = 0.01
        )
      ),
      
      checkboxInput(
        "enableFisheries", 
        "Fisheries Submodel", 
        value = FALSE
      ),
      conditionalPanel(
        condition = "input.enableFisheries == true",
        sliderInput(
          "weightFisheries",
          "Weight for Fisheries:",
          min = 0,
          max = 1,
          value = 0.33,
          step = 0.01
        )
      ),
      
      checkboxInput(
        "enableIndustryOperations", 
        "Industry & Operations Submodel", 
        value = FALSE
      ),
      conditionalPanel(
        condition = "input.enableIndustryOperations == true",
        sliderInput(
          "weightIndustryOperations",
          "Weight for Industry & Operations:",
          min = 0,
          max = 1,
          value = 0.33,
          step = 0.01
        )
      )
    ),
    
    hr(),
    
    # Weight validation display
    h5("Weight Summary"),
    htmlOutput("fullWeightValidation"),
    hr(),
    
    # Generate final model button
    actionButton(
      "generateFullModel", 
      "Generate Full Model", 
      class = "btn-primary btn-block",
      icon = icon("calculator")
    ),
    
    hr(),
    
    # Export section
    h4("Export"),
    downloadButton(
      "fullModelExportRmd", 
      "Export Full Model Report",
      icon = icon("file-export"),
      class = "btn-info btn-block"
    )
  )
}