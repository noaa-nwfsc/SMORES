function(input, output, session) {
  
  # Create a reactive values object to store combined maps data
  combined_maps_data <- reactiveValues(
    habitat = NULL,
    species = NULL,
    birds = NULL,
    industry = NULL, 
    habitat_combined_map_generated = FALSE,
    industry_combined_map_generated = FALSE
  )
  
  # filter dataframes by score 
  filter_by_score <- function(df, selected_score) {
    if(is.null(df) || is.null(selected_score) || selected_score == "None") {
      return(df)
    }
    
    # Find the score column for each selected layer
    score_cols <- names(df)[grep("^Score\\.", names(df))]
    
    if(length(score_cols) == 0) {
      warning("No score columns found in the dataset")
      return(df)
    }
    
    # Create a filter condition for the selected score
    rows_to_keep <- rep(FALSE, nrow(df))
    
    for(col in score_cols) {
      rows_to_keep <- rows_to_keep | (df[[col]] == selected_score)
    }
    
    # Return the filtered dataframe
    filtered_df <- df[rows_to_keep, ]
    
    return(filtered_df)
  }
  
  # Helper function to check if a configuration is valid
  is_valid_config <- function(i) {
    # Determine which tab we're on
    current_tab <- input$dataTabs %||% "habitat"
    
    # Set the prefix based on the tab
    prefix <- switch(current_tab,
                     "habitat" = "Habitat",
                     "species" = "Species",
                     "birds" = "Bird",
                     "industry_operations" = "Industry",
                     "")
    
    if(prefix == "") return(FALSE) # Invalid tab
    
    # Get the input values with the appropriate prefix
    enable_input <- input[[paste0("Enable", prefix, "Map", i)]]
    layer_input <- input[[paste0(prefix, "LayerPicker", i)]]
    score_input <- input[[paste0(prefix, "ScorePicker", i)]]
    
    # Get the appropriate layer data based on the tab
    layer_data <- switch(current_tab,
                         "habitat" = habitat_layer,
                         "species" = species_layer,
                         "birds" = bird_layer,
                         "industry_operations" = industry_layer,
                         NULL)
    
    if(is.null(layer_data)) return(FALSE) # Invalid layer data
    
    # Check if configuration is valid
    !is.null(enable_input) && enable_input &&
      !is.null(layer_input) && layer_input != "None" &&
      !is.null(score_input) && score_input != "None" &&
      layer_input %in% names(layer_data)
  }
  
  # Reactive expression for Natural Resources tab valid configs
  natural_resources_valid_configs <- reactive({
    # Debug output
    cat("Natural Resources reactive called. Navbar:", input$navbar, "dataTabs:", input$dataTabs, "\n")
    
    # For Natural Resources, we check if we have dataTabs (which means we're on the Natural Resources section)
    # OR if navbar is explicitly set to Natural Resources
    is_natural_resources <- (!is.null(input$dataTabs) && 
                               input$dataTabs %in% c("habitat", "species", "birds", "combined_model")) ||
      (!is.null(input$navbar) && input$navbar == "Natural Resources Submodel")
    
    if(!is_natural_resources) {
      return(list())
    }
    
    # Default to habitat if dataTabs is not set
    current_tab <- input$dataTabs %||% "habitat"
    
    cat("Processing Natural Resources - dataTabs:", current_tab, "\n")
    
    # Set the layer data based on the current data tab
    layer_data <- switch(current_tab,
                         "habitat" = habitat_layer,
                         "species" = species_layer,
                         "birds" = bird_layer,
                         NULL)
    
    if(is.null(layer_data)) {
      cat("Layer data is NULL for tab:", current_tab, "\n")
      return(list())
    }
    
    # Use your existing function to get valid configurations
    configs <- get_valid_configs_for_tab(input, current_tab, layer_data, score_colors, filter_by_score)
    cat("Natural Resources configs count:", length(configs), "\n")
    
    return(configs)
  })
  
  # Reactive expression for Industry tab valid configs
  industry_valid_configs <- reactive({
    cat("Industry reactive called. Navbar:", input$navbar, "\n")
    
    # Check if we have any industry-related inputs (this means the sidebar has been rendered)
    industry_input_names <- names(input)[grepl("^EnableIndustryLayer_", names(input))]
    has_industry_inputs <- length(industry_input_names) > 0
    
    cat("Industry debug - has_industry_inputs:", has_industry_inputs, "\n")
    
    # Only process if we have industry inputs (meaning we're on the industry tab)
    if(!has_industry_inputs) {
      return(list())
    }
    
    cat("Processing Industry tab\n")
    
    # Use your existing function with the industry tab specifically
    configs <- get_valid_configs_for_tab(input, "industry_operations", industry_layer, score_colors, filter_by_score)
    cat("Industry configs count:", length(configs), "\n")
    return(configs)
  })
  
  # Natural Resources maps
  observe({
    # Remove the req() condition and let the reactive handle the logic
    valid_configs <- natural_resources_valid_configs()
    
    if(length(valid_configs) > 0) {
      cat("Creating", length(valid_configs), "Natural Resources maps\n")
      create_individual_maps(valid_configs, output, namespace = "naturalresources")
    }
  })
  
  # Industry maps
  observe({
    # Get valid configs without navbar condition check
    valid_configs <- industry_valid_configs()
    
    if(length(valid_configs) > 0) {
      cat("Creating", length(valid_configs), "Industry maps\n")
      create_individual_maps(valid_configs, output, namespace = "industry")
    }
  })
  
  # Dynamic sidebar content
  output$dynamicSidebar <- renderUI({
    current_tab <- input$dataTabs %||% "habitat"
    
    if(current_tab == "habitat") {
      # Get the layer names for habitat
      habitat_layers <- names(habitat_layer)
      
      #use function to make habitat sidebar
      generate_habitat_sidebar(habitat_layers, score_values)
      
    } else if (current_tab == "species") {
      map_inputs <- lapply(1:6, function(i) {
        tagList(
          hr(),
          h5(paste("Map", i, "Configuration")),
          checkboxInput(paste0("EnableSpeciesMap", i), paste("Enable Map", i), value = FALSE),
          conditionalPanel(
            condition = paste0("input.EnableSpeciesMap", i, " == true"),
            pickerInput(
              paste0("SpeciesLayerPicker", i),
              paste("Select Layer for Map", i),
              choices = c("None", names(species_layer)),
              selected = "None"
            ),
            pickerInput(
              paste0("SpeciesScorePicker", i),
              paste("Select score for Map", i),
              choices = c("None", score_values),
              selected = "None"
            )
          )
        )
      })
      
      # generate combined map
      tagList(
        h4("Species Map Settings"),
        map_inputs,
        hr(),
        h4("Combined Species Map Settings"),
        # helpText("The combined map will calculate the geometric mean"),
        actionButton("generateCombinedSpeciesMap", "Generate Combined Species Map", 
                     class = "btn-primary btn-block"),
        # Export button
        hr(), 
        h4("Export"),
        downloadButton("speciesExportRmd", "Export to R Markdown",
                       icon = icon("file-export"),
                       class = "btn-info btn-block")
      )
      
      # Bird tab sidebar content
    } else if (current_tab == "birds") {
      map_inputs <- lapply(1:6, function(i) {
        tagList(
          hr(),
          h5(paste("Map", i, "Configuration")),
          checkboxInput(paste0("EnableBirdMap", i), paste("Enable Map", i), value = FALSE),
          conditionalPanel(
            condition = paste0("input.EnableBirdMap", i, " == true"),
            pickerInput(
              paste0("BirdLayerPicker", i),
              paste("Select Layer for Map", i),
              choices = c("None", names(bird_layer)),
              selected = "None"
            ),
            pickerInput(
              paste0("BirdScorePicker", i),
              paste("Select score for Map", i),
              choices = c("None", score_values),
              selected = "None"
            )
          )
        )
      })
      
      # generate combined map
      tagList(
        h4("Bird Map Settings"),
        map_inputs,
        hr(),
        h4("Combined Bird Map Settings"),
        # helpText("The combined map will calculate the geometric mean"),
        actionButton("GenerateCombinedBirdMap", "Generate Combined Bird Map", 
                     class = "btn-primary btn-block"),
        # Export button
        hr(),
        h4("Export"),
        downloadButton("birdsExportRmd", "Export to R Markdown",
                       icon = icon("file-export"),
                       class = "btn-info btn-block")
      )
      
      # Combined Model Tab
    } else if(current_tab == "combined_model") {
      # Combined model tab sidebar content
      tagList(
        h4("Combined Model Settings"),
        hr(),
        h5("Submodel Status"),
        htmlOutput("combinedModelStatus"),
        hr(),
        h5("Model Weights"),
        
        # Sliders for model weights
        sliderInput("HabitatWeight", "Habitat Weight", 
                    min = 0, max = 1, value = 0.33, step = 0.01),
        sliderInput("SpeciesWeight", "Species Weight", 
                    min = 0, max = 1, value = 0.33, step = 0.01),
        sliderInput("BirdsWeight", "Birds Weight", 
                    min = 0, max = 1, value = 0.34, step = 0.01),
        
        # Ensure weights sum to 1
        htmlOutput("weightValidation"),
        hr(),
        # Button to generate the combined model
        actionButton("generateCombinedModel", "Generate Combined Model", 
                     class = "btn-primary btn-block"),
        # Export Button
        hr(), 
        h4("Export"),
        downloadButton("combinedModelExportRmd", "Export to R Markdown",
                       icon = icon("file-export"),
                       class = "btn-info btn-block")
      )
    }
  })
  
  # Multiple maps container for habitat
  output$multipleMapsContainer_habitat <- renderUI({
    valid_configs <- natural_resources_valid_configs()
    
    create_maps_container(
      configs = valid_configs,
      namespace = "naturalresources",
      combined_map_output_id = "combinedMap",
      combined_map_generated = combined_maps_data$habitat_combined_map_generated,
      combined_map_title = "Combined Map Result (Geometric Mean)"
    )
  })
  
  # Combined map logic
  observeEvent(input$generateCombinedMap, {
    # Show modal with spinner that covers the whole tab
    show_spinner_modal("Generating Combined Map", 
                       "Please wait while the combined map is being generated...")
    
    # Add a small delay to ensure the modal is visible before proceeding
    Sys.sleep(0.5)
    
    # Define dataset mapping for habitat tab
    habitat_dataset_mapping <- list(
      "Canyon" = list(data = canyon, score_column = "Score.Canyon"),
      "Deep Sea Coral Robust High Suitability" = list(data = DSC_RH, score_column = "Score.DSC_RH"),
      "Seeps" = list(data = seeps, score_column = "Score.Seeps"),
      "Shelf Break" = list(data = shlfbrk, score_column = "Score.ShlfBrk"),
      "EFHCA" = list(data = efhca, score_column = "Score.EFHCA"), 
      "EFHCA 700 fathom" = list(data = efhca_700, score_column = "Score.EFHCA.700"), 
      "HAPC AOI" = list(data = HAPCaoi, score_column = "Score.HAPC.AOI"), 
      "HAPC Rocky Reef" = list(data = HAPCreef, score_column = "Score.HAPC.Reef")
    )
    
    # Get valid configurations
    valid_configs <- natural_resources_valid_configs()
    
    # Generate the combined map
    result <- generate_combined_map(
      valid_configs = valid_configs,
      dataset_mapping = habitat_dataset_mapping,
      map_title = "Combined Habitat Score"
    )
    
    # Use the result
    output$combinedMap <- renderLeaflet(result$map)
    
    # store the combined habitat data for use in the combined model
    combined_maps_data$habitat <- result$combined_data
    
    # set flag to indicate combined map has been generated
    combined_maps_data$habitat_combined_map_generated <- TRUE
    
    # Remove modal spinner
    removeModal()
  })
  
  # Habitat/Natural Resources tab export
  output$habitatExportRmd <- downloadHandler(
    filename = function() {
      paste("Natural_Resources_Submodel_Report_", format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".html", sep = "")
    },
    content = function(file) {
      # Show modal with spinner
      show_spinner_modal("Generating Report", 
                         "Please wait while the Natural Resources report is being generated...")
      
      # Get valid configurations
      valid_configs <- natural_resources_valid_configs()
      
      # Make sure each valid_config has valid spatial data
      for(i in seq_along(valid_configs)) {
        # Ensure data is transformed to WGS84 for leaflet
        if(!is.null(valid_configs[[i]]$data) && inherits(valid_configs[[i]]$data, "sf")) {
          valid_configs[[i]]$data <- st_transform(valid_configs[[i]]$data, '+proj=longlat +datum=WGS84')
        }
      }
      
      # Get combined data if available
      combined_data <- NULL
      if(combined_maps_data$habitat_combined_map_generated) {
        combined_data <- combined_maps_data$habitat
        # Ensure combined data is also in WGS84
        if(!is.null(combined_data) && inherits(combined_data, "sf")) {
          combined_data <- st_transform(combined_data, '+proj=longlat +datum=WGS84')
        }
      }
      
      # Render the RMarkdown report
      rmarkdown::render(
        input = "Natural_Resources_Submodel.Rmd", 
        output_file = file,
        params = list(
          map_configs = valid_configs,
          combined_data = combined_data,
          tab_name = "Natural Resources",
          combined_map_title = "Combined Habitat Geometric Mean"
        ),
        envir = new.env(parent = globalenv())
      )
      
      # Remove the modal when done
      removeModal()
    }
  )
  
  # Dynamic sidebar content for Industry & Operations tab
  output$industryOperationsSidebar <- renderUI({
    # Get the layer names for industry
    industry_layers <- names(industry_layer)
    
    #use the generate industry sidebar function 
    generate_industry_sidebar(industry_layers, score_values)
    
  })
  
  # Industry combined map logic
  observeEvent(input$generateIndustryMap, {
    # Show modal with spinner
    show_spinner_modal("Generating Combined Map", 
                       "Please wait while the combined map is being generated...")
    
    # Add a small delay to ensure the modal is visible before proceeding
    Sys.sleep(0.5)
    
    # Define dataset mapping for industry tab
    industry_dataset_mapping <- list(
      "Fixed Surveys" = list(data = surveys_fixed, score_column = "Score.Surveys_fixed"),
      "Periodic Surveys" = list(data = surveys_periodic, score_column = "Score.Surveys_periodic")
    )
    
    # Get valid configurations
    valid_configs <- industry_valid_configs()
    
    # Generate the combined map
    result <- generate_combined_map(
      valid_configs = valid_configs,
      dataset_mapping = industry_dataset_mapping,
      map_title = "Combined Industry Score"
    )
    
    # Use the result
    output$industryMap <- renderLeaflet(result$map)
    
    # store the combined industry data for use in the combined model
    combined_maps_data$industry <- result$combined_data
    
    # set flag to indicate combined map has been clicked
    combined_maps_data$industry_combined_map_generated <- TRUE
    
    # Remove modal spinner
    removeModal()
  })
  
  # Industry map container
  output$industryMapContainer <- renderUI({
    valid_configs <- industry_valid_configs()
    
    create_maps_container(
      configs = valid_configs, 
      namespace = "industry",
      combined_map_output_id = "industryMap",
      combined_map_generated = combined_maps_data$industry_combined_map_generated,
      combined_map_title = "Combined Industry & Operations Map Result (Geometric Mean)"
    )
  })
  
  # Industry & Operations tab export
  output$industryExportRmd <- downloadHandler(
    filename = function() {
      paste("Industry_Operations_Submodel_Report_", format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".html", sep = "")
    },
    content = function(file) {
      # Show modal with spinner
      show_spinner_modal("Generating Report", 
                         "Please wait while the Industry & Operations report is being generated...")
      
      # Get valid configurations
      valid_configs <- industry_valid_configs()
      
      # Make sure each valid_config has valid spatial data
      for(i in seq_along(valid_configs)) {
        # Ensure data is transformed to WGS84 for leaflet
        if(!is.null(valid_configs[[i]]$data) && inherits(valid_configs[[i]]$data, "sf")) {
          valid_configs[[i]]$data <- st_transform(valid_configs[[i]]$data, '+proj=longlat +datum=WGS84')
        }
      }
      
      # Get combined data if available
      combined_data <- NULL
      if(combined_maps_data$industry_combined_map_generated) {
        combined_data <- combined_maps_data$industry
        # Ensure combined data is also in WGS84
        if(!is.null(combined_data) && inherits(combined_data, "sf")) {
          combined_data <- st_transform(combined_data, '+proj=longlat +datum=WGS84')
        }
      }
      
      # Render the RMarkdown report
      rmarkdown::render(
        input = "Industry_Operations_Submodel.Rmd", 
        output_file = file,
        params = list(
          map_configs = valid_configs,
          combined_data = combined_data,
          tab_name = "Industry & Operations",
          combined_map_title = "Combined Industry & Operations Geometric Mean"
        ),
        envir = new.env(parent = globalenv())
      )
      
      # Remove the modal when done
      removeModal()
    }
  )
   
  # Add a message to inform users when submodels aren't generated
  output$combinedModelStatus <- renderUI({
    # Check which submodels have been generated
    habitat_ready <- !is.null(combined_maps_data$habitat)
    species_ready <- !is.null(combined_maps_data$species)
    birds_ready <- !is.null(combined_maps_data$birds)
    
    if(!habitat_ready && !species_ready && !birds_ready) {
      div(
        class = "alert alert-warning",
        icon("exclamation-triangle"), 
        "No submodels have been generated yet. Please go to each tab (Habitat, Species, Birds) and generate the combined maps first."
      )
    } else {
      status_items <- list()
      
      # Add status for each submodel
      if(habitat_ready) {
        status_items <- c(status_items, list(
          div(icon("check-circle", class = "text-success"), " Habitat submodel ready")
        ))
      } else {
        status_items <- c(status_items, list(
          div(icon("times-circle", class = "text-danger"), " Habitat submodel not ready")
        ))
      }
      
      if(species_ready) {
        status_items <- c(status_items, list(
          div(icon("check-circle", class = "text-success"), " Species submodel ready")
        ))
      } else {
        status_items <- c(status_items, list(
          div(icon("times-circle", class = "text-danger"), " Species submodel not ready")
        ))
      }
      
      if(birds_ready) {
        status_items <- c(status_items, list(
          div(icon("check-circle", class = "text-success"), " Birds submodel ready")
        ))
      } else {
        status_items <- c(status_items, list(
          div(icon("times-circle", class = "text-danger"), " Birds submodel not ready")
        ))
      }
      
      div(
        class = "alert alert-info",
        h5("Submodel Status:"),
        status_items
      )
    }
  })
  
  # Data tab timestamp table
  output$data_timestamps_table <- renderTable({
    data_timestamps %>%
      select(dataset_name, formatted_date) %>%
      rename(
        "Dataset" = dataset_name,
        "Last Updated" = formatted_date
      )
  })
}