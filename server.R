function(input, output, session) {
  
  # Create a reactive values object to store combined maps data
  combined_maps_data <- reactiveValues(
    habitat = NULL,
    species = NULL,
    birds = NULL,
    industry = NULL, 
    habitat_combined_map_generated = FALSE,
    species_combined_map_generated = FALSE,
    surveys_combined_map_generated = FALSE
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
                     "surveys" = "Scientific Surveys",
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
                         "surveys" = surveys_layer,
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
  
    # For Natural Resources, we check if we have dataTabs (which means we're on the Natural Resources section)
    # OR if navbar is explicitly set to Natural Resources
    is_natural_resources <- (!is.null(input$dataTabs_natural_resources) && 
                               input$dataTabs_natural_resources %in% c("habitat", "species", "birds", "combined_model_natural_resources")) ||
      (!is.null(input$navbar) && input$navbar == "Natural Resources Submodel")
    
    if(!is_natural_resources) {
      return(list())
    }
    
    # Default to habitat if dataTabs is not set
    current_tab_natural_resources <- input$dataTabs_natural_resources %||% "habitat"
    
    # Set the layer data based on the current data tab
    layer_data <- switch(current_tab_natural_resources,
                         "habitat" = habitat_layer,
                         "species" = species_layer,
                         "birds" = bird_layer,
                         NULL)
    
    # Use your existing function to get valid configurations
    configs <- get_valid_configs_for_tab(input, current_tab_natural_resources, layer_data, score_colors, filter_by_score)

    return(configs)
  })
  
  # Reactive expression for Industry & Operations tab valid configs
  industry_operations_valid_configs <- reactive({
   
    # For Industry & Operations, we check if we have dataTabs (which means we're on the Natural Resources section)
    # OR if navbar is explicitly set to Industry & Operations
    is_industry_operations <- (!is.null(input$dataTabs_industry_operations) && 
                               input$dataTabs_industry_operations %in% c("surveys", "misc", "combined_model_industry_operations")) ||
      (!is.null(input$navbar) && input$navbar == "Industry & Operations Submodel")
    
    if(!is_industry_operations) {
      return(list())
    }
    
    # Default to surveys if dataTabs is not set
    current_tab_industry_operations <- input$dataTabs_industry_operations %||% "surveys"
    
    # Set the layer data based on the current data tab
    layer_data <- switch(current_tab_industry_operations,
                         "surveys" = surveys_layer,
                        # "misc" = misc_layer,
                         NULL)
    
    # Use your existing function to get valid configurations
    configs <- get_valid_configs_for_tab(input, current_tab_industry_operations, layer_data, score_colors, filter_by_score)
   
    return(configs)
  })
  
  # Natural Resources maps
  observe({
    # Remove the req() condition and let the reactive handle the logic
    valid_configs <- natural_resources_valid_configs()
    
    if(length(valid_configs) > 0) {
      create_individual_maps(valid_configs, output, namespace = "naturalresources")
    }
  })
  
  # Industry maps
  observe({
    # Get valid configs without navbar condition check
    valid_configs <- industry_operations_valid_configs()
    
    if(length(valid_configs) > 0) {
      create_individual_maps(valid_configs, output, namespace = "industryoperations")
    }
  })
  
  # Dynamic sidebar content
  output$dynamicSidebar_natural_resources <- renderUI({
    current_tab_natural_resources <- input$dataTabs_natural_resources %||% "habitat"
    
    if (current_tab_natural_resources == "habitat") {
      # Get the layer names for habitat
      habitat_layers <- names(habitat_layer)
      natural_resources_config <- get_natural_resources_config()
      
      #use function to make habitat sidebar
      generate_habitat_sidebar(
        habitat_layers, 
        score_values, 
        current_tab = current_tab_natural_resources,
        submodel_config = natural_resources_config
      )
      
    } else if (current_tab_natural_resources == "species") {
      # Get the layer names for species
      species_layers <- names(species_layer)
      natural_resources_config <- get_natural_resources_config()
      
      #use function to make species sidebar
      generate_species_sidebar(
        species_layers, 
        score_values, 
        current_tab = current_tab_natural_resources,
        submodel_config = natural_resources_config
      )
  
      # Bird tab sidebar content
    } else if (current_tab_natural_resources == "birds") {
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
    } else if(current_tab_natural_resources == "combined_model_natural_resources") {
      natural_resources_config <- get_natural_resources_config()
      generate_natural_resources_combined_sidebar(natural_resources_config, combined_maps_data)
    }
  })
  
  # Dynamic sidebar content for industry and operations
  output$dynamicSidebar_industry_operations <- renderUI({
    current_tab_industry_operations <- input$dataTabs_industry_operations %||% "surveys"
    
    if (current_tab_industry_operations == "surveys") {
      # Get the layer names for surveys
      surveys_layers <- names(surveys_layer)
      industry_operations_config <- get_industry_operations_config()
      
      #use function to make surveys sidebar
      generate_surveys_sidebar(
        surveys_layers, 
        score_values, 
        current_tab = current_tab_industry_operations,
        submodel_config = industry_operations_config
      )
      
    } else if (current_tab_industry_operations == "misc") {
      # Get the layer names for misc
      misc_layers <- names(misc_layer)
      
      #use function to make misc sidebar
      generate_misc_sidebar(misc_layers, score_values)
      

      # Combined Model Tab
    } else if(current_tab_industry_operations == "combined_model_industry_operations") {
      industry_operations_config <- get_industry_operations_config()
      generate_combined_model_sidebar(industry_operations_config)
    }
  })
  
  # Multiple maps container for habitat
  output$multipleMapsContainer_habitat <- renderUI({
    valid_configs <- natural_resources_valid_configs()
    selected_methods <- input$habitatCalculationMethods %||% character(0)
    
    create_maps_container(
      configs = valid_configs,
      namespace = "naturalresources",
      combined_map_output_id = "combinedHabitatMap",
      combined_map_generated = combined_maps_data$habitat_combined_map_generated,
      combined_map_title = "Combined Map Result",
      selected_methods = selected_methods
    )
  })
  
  # Combined map logic
  observeEvent(input$generateCombinedHabitatMap, {
    # Get selected calculation methods
    selected_methods <- input$habitatCalculationMethods
    
    if(is.null(selected_methods) || length(selected_methods) == 0) {
      showNotification("Please select at least one calculation method.", type = "warning")
      return()
    }
    
    # Show modal with spinner
    show_spinner_modal("Generating Combined Map(s)", 
                       paste("Please wait while", length(selected_methods), "combined map(s) are being generated..."))
    
    # Add a small delay to ensure the modal is visible
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
    
    # Generate all maps at once using the new function
    all_results <- generate_combined_maps_all_methods(
      valid_configs = valid_configs,
      dataset_mapping = habitat_dataset_mapping,
      selected_methods = selected_methods
    )
    
    # Now assign each result to its appropriate output
    for(method in selected_methods) {
      if(method %in% names(all_results)) {
        result <- all_results[[method]]
        
        # Create output for each method
        if(method == "geometric_mean") {
          output$combinedHabitatMap <- renderLeaflet(result$map)
          # Store the combined data for the first method (for combined model use)
          combined_maps_data$habitat <- result$combined_data
        } else {
          output_id <- paste0("combinedHabitatMap_", method)
          output[[output_id]] <- renderLeaflet(result$map)
        }
      }
    }
    
    # Set flag to indicate combined map has been generated
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
      
      # Get filtered timestamp information for the selected layers
      timestamp_info <- get_filtered_timestamp_data(valid_configs, "habitat")
      
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
          combined_map_title = "Combined Habitat Geometric Mean",
          data_timestamps = timestamp_info
        ),
        envir = new.env(parent = globalenv())
      )
      
      # Remove the modal when done
      removeModal()
    }
  )
  
  # Multiple maps container for species
  output$multipleMapsContainer_species <- renderUI({
    valid_configs <- natural_resources_valid_configs()
    
    create_maps_container(
      configs = valid_configs,
      namespace = "naturalresources",
      combined_map_output_id = "combinedSpeciesMap",
      combined_map_generated = combined_maps_data$species_combined_map_generated,
      combined_map_title = "Combined Map Result (Geometric Mean)"
    )
  })
  
  # Combined map logic
  observeEvent(input$generateCombinedSpeciesMap, {
    # Show modal with spinner that covers the whole tab
    show_spinner_modal("Generating Combined Map", 
                       "Please wait while the combined map is being generated...")
    
    # Add a small delay to ensure the modal is visible before proceeding
    Sys.sleep(0.5)
    
    # Define dataset mapping for species tab
    species_dataset_mapping <- list(
      "ESA Critical Habitat for Southern Resident Killer Whales" = list(data = killer_whale, score_column = "Score.killer_whale"),
      "ESA Critical Habitat for Leatherback Sea Turtles"  = list(data = leatherback_turtle, score_column = "Score.leatherback_turtle"),
      "ESA Critical Habitat for Humpback Whales - Mexico and Central DPS" = list(data = humpback_whale, score_column = "Score.humpback_whale")
    )
    
    # Get valid configurations
    valid_configs <- natural_resources_valid_configs()
    
    # Generate the combined map
    result <- generate_combined_map(
      valid_configs = valid_configs,
      dataset_mapping = species_dataset_mapping,
      map_title = "Combined Species Score"
    )
    
    # Use the result
    output$combinedSpeciesMap <- renderLeaflet(result$map)
    
    # store the combined habitat data for use in the combined model
    combined_maps_data$species <- result$combined_data
    
    # set flag to indicate combined map has been generated
    combined_maps_data$species_combined_map_generated <- TRUE
    
    # Remove modal spinner
    removeModal()
  })
  
  # Multiple maps container for surveys
  output$multipleMapsContainer_surveys <- renderUI({
    valid_configs <- industry_operations_valid_configs()
    
    create_maps_container(
      configs = valid_configs,
      namespace = "industryoperations",
      combined_map_output_id = "combinedSurveysMap",
      combined_map_generated = combined_maps_data$surveys_combined_map_generated,
      combined_map_title = "Combined Map Result (Geometric Mean)"
    )
  })
  
  # Surveys combined map logic
  observeEvent(input$generateSurveysMap, {
    # Show modal with spinner
    show_spinner_modal("Generating Combined Map", 
                       "Please wait while the combined map is being generated...")
    
    # Add a small delay to ensure the modal is visible before proceeding
    Sys.sleep(0.5)
    
    # Define dataset mapping for industry tab
    surveys_dataset_mapping <- list(
      "Fixed Surveys" = list(data = surveys_fixed, score_column = "Score.Surveys_fixed"),
      "Periodic Surveys" = list(data = surveys_periodic, score_column = "Score.Surveys_periodic")
    )
    
    # Get valid configurations
    valid_configs <- industry_operations_valid_configs()
    
    # Generate the combined map
    result <- generate_combined_map(
      valid_configs = valid_configs,
      dataset_mapping = surveys_dataset_mapping,
      map_title = "Combined Scientific Surveys Score"
    )
    
    # Use the result
    output$surveysMap <- renderLeaflet(result$map)
    
    # store the combined industry data for use in the combined model
    combined_maps_data$surveys <- result$combined_data
    
    # set flag to indicate combined map has been clicked
    combined_maps_data$surveys_combined_map_generated <- TRUE
    
    # Remove modal spinner
    removeModal()
  })
   
  # Natural Resources submodel status
  output$combinedModelStatus_natural_resources <- renderUI({
    check_submodel_status("natural_resources", combined_maps_data)
  })
  
  # Industry & Operations submodel status  
  output$combinedModelStatus_industry_operations <- renderUI({
    check_submodel_status("industry_operations", combined_maps_data)
  })
  
  # Dynamic sidebar content for overall model tab
  output$dynamicSidebar_overall_model <- renderUI({
    generate_overall_model_sidebar()
  })
  
  # Weight validation for overall model
  output$overallWeightValidation <- renderUI({
    # Get the current weight values
    natural_resources_weight <- input$weightNaturalResources %||% 0
    fisheries_weight <- input$weightFisheries %||% 0
    industry_weight <- input$weightIndustryOperations %||% 0
    
    # Get which submodels are enabled
    nr_enabled <- input$enableNaturalResources %||% FALSE
    fisheries_enabled <- input$enableFisheries %||% FALSE
    industry_enabled <- input$enableIndustryOperations %||% FALSE
    
    # Calculate total weight for enabled submodels only
    enabled_weights <- c()
    if(nr_enabled) enabled_weights <- c(enabled_weights, natural_resources_weight)
    if(fisheries_enabled) enabled_weights <- c(enabled_weights, fisheries_weight)
    if(industry_enabled) enabled_weights <- c(enabled_weights, industry_weight)
    
    total_weight <- sum(enabled_weights)
    num_enabled <- length(enabled_weights)
    
    # Validation messages
    if(num_enabled == 0) {
      div(class = "alert alert-warning", 
          "No submodels selected. Please enable at least one submodel.")
    } else if(total_weight == 0) {
      div(class = "alert alert-warning", 
          "Total weight is 0. Please set weights greater than 0.")
    } else if(abs(total_weight - 1) > 0.01) {
      div(class = "alert alert-info", 
          paste("Total weight:", round(total_weight, 3), "- Weights will be normalized to sum to 1.0"))
    } else {
      div(class = "alert alert-success", 
          paste("✓ Total weight:", round(total_weight, 3), "- Ready to generate combined model"))
    }
  })
  
  # Check if submodels have generated combined maps
  submodel_status <- reactive({
    list(
      natural_resources = list(
        available = combined_maps_data$habitat_combined_map_generated || 
          combined_maps_data$species_combined_map_generated,
        habitat_ready = combined_maps_data$habitat_combined_map_generated,
        species_ready = combined_maps_data$species_combined_map_generated,
        birds_ready = FALSE  # Add this when birds is implemented
      ),
      fisheries = list(
        available = FALSE,  # Update this when fisheries is implemented
        ready_components = c()
      ),
      industry_operations = list(
        available = combined_maps_data$surveys_combined_map_generated,
        surveys_ready = combined_maps_data$surveys_combined_map_generated,
        misc_ready = FALSE  # Update this when misc is implemented
      )
    )
  })
  
  # Update checkbox availability based on submodel status
  observe({
    status <- submodel_status()
    
    # Enable/disable checkboxes based on submodel availability
    if(status$natural_resources$available) {
      updateCheckboxInput(session, "enableNaturalResources", value = TRUE)
    } else {
      updateCheckboxInput(session, "enableNaturalResources", value = FALSE)
    }
    
    if(status$fisheries$available) {
      updateCheckboxInput(session, "enableFisheries", value = FALSE)  # Default off
    } else {
      updateCheckboxInput(session, "enableFisheries", value = FALSE)
    }
    
    if(status$industry_operations$available) {
      updateCheckboxInput(session, "enableIndustryOperations", value = TRUE)
    } else {
      updateCheckboxInput(session, "enableIndustryOperations", value = FALSE)
    }
  })
  
  # Display detailed status for each submodel
  output$overallModelSubmodelStatus <- renderUI({
    status <- submodel_status()
    
    tagList(
      # Natural Resources Status
      div(class = if(status$natural_resources$available) "alert alert-success" else "alert alert-warning",
          h6("Natural Resources Submodel"),
          if(status$natural_resources$available) {
            tagList(
              "✓ Available",
              if(status$natural_resources$habitat_ready) div("• Habitat combined map ready"),
              if(status$natural_resources$species_ready) div("• Species combined map ready")
            )
          } else {
            "⚠ Not ready - Generate combined maps in Natural Resources tabs first"
          }
      ),
      
      # Fisheries Status  
      div(class = if(status$fisheries$available) "alert alert-success" else "alert alert-warning",
          h6("Fisheries Submodel"),
          if(status$fisheries$available) {
            "✓ Available"
          } else {
            "⚠ Not implemented yet"
          }
      ),
      
      # Industry & Operations Status
      div(class = if(status$industry_operations$available) "alert alert-success" else "alert alert-warning",
          h6("Industry & Operations Submodel"),
          if(status$industry_operations$available) {
            tagList(
              "✓ Available",
              if(status$industry_operations$surveys_ready) div("• Scientific Surveys combined map ready")
            )
          } else {
            "⚠ Not ready - Generate combined maps in Industry & Operations tabs first"
          }
      )
    )
  })
  # Data tab timestamp table
  output$data_timestamps_table <- renderTable({
    data_timestamps %>%
      select(dataset_name, description, formatted_date) %>%
      rename(
        "Dataset" = dataset_name,
        "Description" = description,
        "Last Updated" = formatted_date
      )
  })
}