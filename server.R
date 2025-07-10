function(input, output, session) {
  
  # Create a reactive values object to store combined maps data
  combined_maps_data <- reactiveValues(
    habitat_geo = NULL,
    habitat_lowest = NULL,
    habitat_product = NULL,
    habitat_combined_map_generated = FALSE,
    species_geo = NULL,
    species_lowest = NULL,
    species_product = NULL,
    species_combined_map_generated = FALSE,
    birds = NULL,
    natural_resources_combined_submodel = NULL,
    natural_resources_combined_submodel_generated = FALSE,
    industry = NULL, 
    surveys_geo = NULL,
    surveys_lowest = NULL, 
    surveys_product = NULL,
    surveys_combined_map_generated = FALSE, 
    cables_geo = NULL,
    cables_lowest = NULL,
    cables_product = NULL, 
    cables_combined_map_generated = FALSE
  )
  
  #WEA area selector options
  observe({
    if(!is.null(WEA) && "Area_Name" %in% names(WEA)){
      area_names <-sort(unique(WEA$Area_Name))
      updatePickerInput(
        session = session,
        inputId = "weaAreaSelector",
        choices = area_names, 
        selected = NULL
      )
    }
  })
  
  # Reactive expression for filtered WEA data with debug
  filtered_wea_data <- reactive({
    
    if(is.null(input$weaAreaSelector) || input$weaAreaSelector == "") {

      if(!is.null(WEA)) {
  
      }
      return(WEA)
    }

    # Filter the data
    filtered_data <- WEA[WEA$Area_Name == input$weaAreaSelector, ]
 
    return(filtered_data)
  })
  
  # WEA Map Output - Fixed version
  output$weaMap <- renderLeaflet({
    
    tryCatch({
      # Call the reactive expression to get the actual data
      wea_data <- filtered_wea_data()
      
      if(is.null(wea_data) || nrow(wea_data) == 0) {
        return(leaflet() %>% 
                 addProviderTiles("Esri.OceanBasemap") %>% 
                 addControl("No WEA data available", position = "center"))
      }
      
      # Transform to WGS84 if needed
      if(!st_is_longlat(wea_data)) {
        wea_data <- st_transform(wea_data, 4326)
      }
      
      # Remove Z & M dimensions for leaflet compatability
      wea_data <- st_zm(wea_data)
      
      # Create the map
      leaflet() %>%
        addProviderTiles("Esri.OceanBasemap") %>%
        addPolygons(
          data = wea_data,
          fillColor = "blue",
          weight = 1,
          color = "black",
          fillOpacity = 0.5
        )
      
    }, error = function(e) {
      return(leaflet() %>% 
               addProviderTiles("Esri.OceanBasemap") %>% 
               addControl("Error loading WEA data", position = "topright"))
    })
  })
  
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
                     "cables" = "Submarine Cables",
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
                         "cables" = submarine_cables_layers,
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
                                 input$dataTabs_industry_operations %in% c("surveys", "cables", "combined_model_industry_operations")) ||
      (!is.null(input$navbar) && input$navbar == "Industry & Operations Submodel")
    
    if(!is_industry_operations) {
      return(list())
    }
    
    # Default to surveys if dataTabs is not set
    current_tab_industry_operations <- input$dataTabs_industry_operations %||% "surveys"
    
    # Set the layer data based on the current data tab
    layer_data <- switch(current_tab_industry_operations,
                         "surveys" = surveys_layer,
                         "cables" = submarine_cables_layer,
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
      
    } else if (current_tab_industry_operations == "cables") {
      # Get the layer names for cables
      cable_layers <- names(submarine_cables_layer)
      
      #use function to make cables sidebar
      generate_cables_sidebar(submarine_cables_layer, score_values)
      
      #use function to make cables sidebar
      generate_cables_sidebar(
        submarine_cables_layer, 
        score_values, 
        current_tab = current_tab_industry_operations,
        submodel_config = industry_operations_config
      )
      
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
    
    # Generate all maps at once
    all_results <- generate_combined_maps_all_methods(
      valid_configs = valid_configs,
      dataset_mapping = habitat_dataset_mapping,
      selected_methods = selected_methods,
      map_type = "Habitat"
    )
    
    # Store results for all methods
    if("geometric_mean" %in% selected_methods && "geometric_mean" %in% names(all_results)) {
      local({
        result <- all_results[["geometric_mean"]]
        output$combinedHabitatMap_geo <- renderLeaflet({ result$map })
        combined_maps_data$habitat_geo <- result$combined_data
      })
    }
    
    if("lowest" %in% selected_methods && "lowest" %in% names(all_results)) {
      local({
        result <- all_results[["lowest"]]
        output$combinedHabitatMap_lowest <- renderLeaflet({ result$map })
        combined_maps_data$habitat_lowest <- result$combined_data
      })
    }
    
    if("product" %in% selected_methods && "product" %in% names(all_results)) {  
      local({
        result <- all_results[["product"]]
        output$combinedHabitatMap_product <- renderLeaflet({ result$map })
        combined_maps_data$habitat_product <- result$combined_data  # This was already correct
      })
    }
    
    # Set flag to indicate combined map has been generated
    combined_maps_data$habitat_combined_map_generated <- TRUE
    
    # Remove modal spinner
    removeModal()
  })
  
  # Habitat/Natural Resources tab export
  output$habitatExportRmd <- downloadHandler(
    filename = function() {
      paste("Habitat_Component_Natural_Resources_Submodel_Report_", format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".html", sep = "")
    },
    content = function(file) {
      # Show modal with spinner
      show_spinner_modal("Generating Report", 
                         "Please wait while the Habitat Component of the Natural Resources report is being generated...")
      
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
      
      # Get selected calculation methods from habitat tab
      selected_methods <- input$habitatCalculationMethods %||% character(0)
      
      # Get combined data for each selected method if available
      combined_data_list <- list()
      if(combined_maps_data$habitat_combined_map_generated && length(selected_methods) > 0) {
        if("geometric_mean" %in% selected_methods && !is.null(combined_maps_data$habitat_geo)) {
          combined_data_list[["geometric_mean"]] <- combined_maps_data$habitat_geo
        }
        
        if("lowest" %in% selected_methods && !is.null(combined_maps_data$habitat_lowest)) {
          combined_data_list[["lowest"]] <- combined_maps_data$habitat_lowest
        }
        
        if("product" %in% selected_methods && !is.null(combined_maps_data$habitat_product)) {
          combined_data_list[["product"]] <- combined_maps_data$habitat_product
        }
      }
      
      # Render the RMarkdown report with updated parameters
      rmarkdown::render(
        input = "Submodel_Component_Report_Template.Rmd", 
        output_file = file,
        params = list(
          map_configs = valid_configs,
          combined_data_list = combined_data_list,  
          selected_methods = selected_methods,     
          tab_name = "Natural Resources",
          combined_map_title = "Combined Habitat Maps",
          data_timestamps = timestamp_info, 
          component_name = "Habitat"
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
    selected_methods <- input$speciesCalculationMethods %||% character(0)
    
    create_maps_container(
      configs = valid_configs,
      namespace = "naturalresources",
      combined_map_output_id = "combinedSpeciesMap",
      combined_map_generated = combined_maps_data$species_combined_map_generated,
      combined_map_title = "Combined Map Result",
      selected_methods = selected_methods
    )
  })
  
  # Combined map logic
  observeEvent(input$generateCombinedSpeciesMap, {
    # Get selected calculation methods
    selected_methods <- input$speciesCalculationMethods
    
    if(is.null(selected_methods) || length(selected_methods) == 0) {
      showNotification("Please select at least one calculation method.", type = "warning")
      return()
    }
    
    # Show modal with spinner
    show_spinner_modal("Generating Combined Map(s)", 
                       paste("Please wait while", length(selected_methods), "combined map(s) are being generated..."))
    
    # Add a small delay to ensure the modal is visible
    Sys.sleep(0.5)
    
    # Define dataset mapping for species tab
    species_dataset_mapping <- list(
      "ESA Critical Habitat for Southern Resident Killer Whales" = list(data = killer_whale, score_column = "Score.killer_whale"),
      "ESA Critical Habitat for Leatherback Sea Turtles"  = list(data = leatherback_turtle, score_column = "Score.leatherback_turtle"),
      "ESA Critical Habitat for Humpback Whales - Mexico and Central DPS" = list(data = humpback_whale, score_column = "Score.humpback_whale")
    )
    
    # Get valid configurations
    valid_configs <- natural_resources_valid_configs()
    
    # Generate all maps at once
    all_results <- generate_combined_maps_all_methods(
      valid_configs = valid_configs,
      dataset_mapping = species_dataset_mapping,
      selected_methods = selected_methods,
      map_type = "Species"
    )
    
    # Store results for all methods
    if("geometric_mean" %in% selected_methods && "geometric_mean" %in% names(all_results)) {
      local({
        result <- all_results[["geometric_mean"]]
        output$combinedSpeciesMap_geo <- renderLeaflet({ result$map })
        combined_maps_data$species_geo <- result$combined_data
      })
    }
    
    if("lowest" %in% selected_methods && "lowest" %in% names(all_results)) {
      local({
        result <- all_results[["lowest"]]
        output$combinedSpeciesMap_lowest <- renderLeaflet({ result$map })
        combined_maps_data$species_lowest <- result$combined_data
      })
    }
    
    if("product" %in% selected_methods && "product" %in% names(all_results)) {  
      local({
        result <- all_results[["product"]]
        output$combinedSpeciesMap_product <- renderLeaflet({ result$map })
        combined_maps_data$species_product <- result$combined_data  # This was already correct
      })
    }
    
    # Set flag to indicate combined map has been generated
    combined_maps_data$species_combined_map_generated <- TRUE
    
    # Remove modal spinner
    removeModal()
  })
  
  # Species/Natural Resources tab export
  output$speciesExportRmd <- downloadHandler(
    filename = function() {
      paste("Species_Component_Natural_Resources_Submodel_Report_", format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".html", sep = "")
    },
    content = function(file) {
      # Show modal with spinner
      show_spinner_modal("Generating Report", 
                         "Please wait while the Species Component of the Natural Resources report is being generated...")
      
      # Get valid configurations
      valid_configs <- natural_resources_valid_configs()
      
      # Get filtered timestamp information for the selected layers
      timestamp_info <- get_filtered_timestamp_data(valid_configs, "species")
      
      # Make sure each valid_config has valid spatial data
      for(i in seq_along(valid_configs)) {
        # Ensure data is transformed to WGS84 for leaflet
        if(!is.null(valid_configs[[i]]$data) && inherits(valid_configs[[i]]$data, "sf")) {
          valid_configs[[i]]$data <- st_transform(valid_configs[[i]]$data, '+proj=longlat +datum=WGS84')
        }
      }
      
      # Get selected calculation methods from habitat tab
      selected_methods <- input$speciesCalculationMethods %||% character(0)
      
      # Get combined data for each selected method if available
      combined_data_list <- list()
      if(combined_maps_data$species_combined_map_generated && length(selected_methods) > 0) {
        if("geometric_mean" %in% selected_methods && !is.null(combined_maps_data$species_geo)) {
          combined_data_list[["geometric_mean"]] <- combined_maps_data$species_geo
        }
        
        if("lowest" %in% selected_methods && !is.null(combined_maps_data$species_lowest)) {
          combined_data_list[["lowest"]] <- combined_maps_data$species_lowest
        }
        
        if("product" %in% selected_methods && !is.null(combined_maps_data$species_product)) {
          combined_data_list[["product"]] <- combined_maps_data$species_product
        }
      }
      
      # Render the RMarkdown report with updated parameters
      rmarkdown::render(
        input = "Submodel_Component_Report_Template.Rmd", 
        output_file = file,
        params = list(
          map_configs = valid_configs,
          combined_data_list = combined_data_list,  
          selected_methods = selected_methods,     
          tab_name = "Natural Resources",
          combined_map_title = "Combined Species Maps",
          data_timestamps = timestamp_info, 
          component_name = "Species"
        ),
        envir = new.env(parent = globalenv())
      )
      
      # Remove the modal when done
      removeModal()
    }
  )
  
  # Multiple maps container for surveys
  output$multipleMapsContainer_surveys <- renderUI({
    valid_configs <- industry_operations_valid_configs()
    selected_methods <- input$surveysCalculationMethods %||% character(0)
    
    create_maps_container(
      configs = valid_configs,
      namespace = "industryoperations",
      combined_map_output_id = "combinedSurveysMap",
      combined_map_generated = combined_maps_data$surveys_combined_map_generated,
      combined_map_title = "Combined Map Result",
      selected_methods = selected_methods
    )
  })
  
  # Combined map logic
  observeEvent(input$generateCombinedSurveysMap, {
    # Get selected calculation methods
    selected_methods <- input$surveysCalculationMethods
    
    if(is.null(selected_methods) || length(selected_methods) == 0) {
      showNotification("Please select at least one calculation method.", type = "warning")
      return()
    }
    
    # Show modal with spinner
    show_spinner_modal("Generating Combined Map(s)", 
                       paste("Please wait while", length(selected_methods), "combined map(s) are being generated..."))
    
    # Add a small delay to ensure the modal is visible
    Sys.sleep(0.5)
    
    # Define dataset mapping for industry tab
    surveys_dataset_mapping <- list(
      "Fixed Surveys" = list(data = surveys_fixed, score_column = "Score.Surveys_fixed"),
      "Periodic Surveys" = list(data = surveys_periodic, score_column = "Score.Surveys_periodic")
    )
    
    # Get valid configurations
    valid_configs <- industry_operations_valid_configs()
    
    # Generate all maps at once
    all_results <- generate_combined_maps_all_methods(
      valid_configs = valid_configs,
      dataset_mapping = surveys_dataset_mapping,
      selected_methods = selected_methods,
      map_type = "Surveys"
    )
      
      # Store results for all methods
      if("geometric_mean" %in% selected_methods && "geometric_mean" %in% names(all_results)) {
        local({
          result <- all_results[["geometric_mean"]]
          output$combinedSurveysMap_geo <- renderLeaflet({ result$map })
          combined_maps_data$surveys_geo <- result$combined_data
        })
      }
      
      if("lowest" %in% selected_methods && "lowest" %in% names(all_results)) {
        local({
          result <- all_results[["lowest"]]
          output$combinedSurveysMap_lowest <- renderLeaflet({ result$map })
          combined_maps_data$surveys_lowest <- result$combined_data
        })
      }
      
      if("product" %in% selected_methods && "product" %in% names(all_results)) {  
        local({
          result <- all_results[["product"]]
          output$combinedSurveysMap_product <- renderLeaflet({ result$map })
          combined_maps_data$surveys_product <- result$combined_data  # This was already correct
        })
      }
      
      # Set flag to indicate combined map has been generated
      combined_maps_data$surveys_combined_map_generated <- TRUE
      
      # Remove modal spinner
      removeModal()
    })
  
  # Surveys/Industry and Operations tab export
  output$surveysExportRmd <- downloadHandler(
    filename = function() {
      paste("Surveys_Component_Industry_Operations_Submodel_Report_", format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".html", sep = "")
    },
    content = function(file) {
      # Show modal with spinner
      show_spinner_modal("Generating Report", 
                         "Please wait while the Surveys Component of the Industry and Operations report is being generated...")
      
      # Get valid configurations
      valid_configs <- industry_operations_valid_configs()
      
      # Get filtered timestamp information for the selected layers
      timestamp_info <- get_filtered_timestamp_data(valid_configs, "surveys")
      
      # Make sure each valid_config has valid spatial data
      for(i in seq_along(valid_configs)) {
        # Ensure data is transformed to WGS84 for leaflet
        if(!is.null(valid_configs[[i]]$data) && inherits(valid_configs[[i]]$data, "sf")) {
          valid_configs[[i]]$data <- st_transform(valid_configs[[i]]$data, '+proj=longlat +datum=WGS84')
        }
      }
      
      # Get selected calculation methods from surveys tab
      selected_methods <- input$surveysCalculationMethods %||% character(0)
      
      # Get combined data for each selected method if available
      combined_data_list <- list()
      if(combined_maps_data$surveys_combined_map_generated && length(selected_methods) > 0) {
        if("geometric_mean" %in% selected_methods && !is.null(combined_maps_data$surveys_geo)) {
          combined_data_list[["geometric_mean"]] <- combined_maps_data$surveys_geo
        }
        
        if("lowest" %in% selected_methods && !is.null(combined_maps_data$surveys_lowest)) {
          combined_data_list[["lowest"]] <- combined_maps_data$surveys_lowest
        }
        
        if("product" %in% selected_methods && !is.null(combined_maps_data$surveys_product)) {
          combined_data_list[["product"]] <- combined_maps_data$surveys_product
        }
      }
      
      # Render the RMarkdown report with updated parameters
      rmarkdown::render(
        input = "Submodel_Component_Report_Template.Rmd", 
        output_file = file,
        params = list(
          map_configs = valid_configs,
          combined_data_list = combined_data_list,  
          selected_methods = selected_methods,     
          tab_name = "Industry and Operations",
          combined_map_title = "Combined Surveys Maps",
          data_timestamps = timestamp_info, 
          component_name = "Surveys"
        ),
        envir = new.env(parent = globalenv())
      )
      
      # Remove the modal when done
      removeModal()
    }
  )
  
  # Multiple maps container for cables
  output$multipleMapsContainer_cables <- renderUI({
    valid_configs <- industry_operations_valid_configs()
    selected_methods <- input$cablesCalculationMethods %||% character(0)
    
    create_maps_container(
      configs = valid_configs,
      namespace = "industryoperations",
      combined_map_output_id = "combinedCablesMap",
      combined_map_generated = combined_maps_data$cables_combined_map_generated,
      combined_map_title = "Combined Map Result",
      selected_methods = selected_methods
    )
  })
  
  # Combined map logic
  observeEvent(input$generateCombinedCablesMap, {
    # Get selected calculation methods
    selected_methods <- input$cablesCalculationMethods
    
    if(is.null(selected_methods) || length(selected_methods) == 0) {
      showNotification("Please select at least one calculation method.", type = "warning")
      return()
    }
    
    # Show modal with spinner
    show_spinner_modal("Generating Combined Map(s)", 
                       paste("Please wait while", length(selected_methods), "combined map(s) are being generated..."))
    
    # Add a small delay to ensure the modal is visible
    Sys.sleep(0.5)
    
    # Define dataset mapping for species tab
    cables_dataset_mapping <- list(
      "Submarine Cables" = list(data = submarine_cable, score_column = "Score.submarine_cable"),
      "Submarine Cables 0-500 m setback" = list(data = submarine_cable_500m, score_column = "Score.submarine_cable_500m"),
      "Submarine Cables 501-1000 m setback" = list(data = submarine_cable_501_1000m, score_column = "Score.submarine_cable_501_1000m")
    )
    
    # Get valid configurations
    valid_configs <- industry_operations_valid_configs()
    
    # Generate all maps at once
    all_results <- generate_combined_maps_all_methods(
      valid_configs = valid_configs,
      dataset_mapping = cables_dataset_mapping,
      selected_methods = selected_methods,
      map_type = "Cables"
    )
    
    # Store results for all methods
    if("geometric_mean" %in% selected_methods && "geometric_mean" %in% names(all_results)) {
      local({
        result <- all_results[["geometric_mean"]]
        output$combinedCablesMap_geo <- renderLeaflet({ result$map })
        combined_maps_data$cables_geo <- result$combined_data
      })
    }
    
    if("lowest" %in% selected_methods && "lowest" %in% names(all_results)) {
      local({
        result <- all_results[["lowest"]]
        output$combinedCablesMap_lowest <- renderLeaflet({ result$map })
        combined_maps_data$cables_lowest <- result$combined_data
      })
    }
    
    if("product" %in% selected_methods && "product" %in% names(all_results)) {  
      local({
        result <- all_results[["product"]]
        output$combinedCablesMap_product <- renderLeaflet({ result$map })
        combined_maps_data$cables_product <- result$combined_data  # This was already correct
      })
    }
    
    # Set flag to indicate combined map has been generated
    combined_maps_data$cables_combined_map_generated <- TRUE
    
    # Remove modal spinner
    removeModal()
  })
  
  # Submarine Cables/Industry and Operations tab export
  output$cablesExportRmd <- downloadHandler(
    filename = function() {
      paste("Submarine_Cables_Component_Industry_Operations_Submodel_Report_", format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".html", sep = "")
    },
    content = function(file) {
      # Show modal with spinner
      show_spinner_modal("Generating Report", 
                         "Please wait while the Submarine Cables Component of the Industry and Operations report is being generated...")
      
      # Get valid configurations
      valid_configs <- industry_operations_valid_configs()
      
      # Get filtered timestamp information for the selected layers
      timestamp_info <- get_filtered_timestamp_data(valid_configs, "cables")
      
      # Make sure each valid_config has valid spatial data
      for(i in seq_along(valid_configs)) {
        # Ensure data is transformed to WGS84 for leaflet
        if(!is.null(valid_configs[[i]]$data) && inherits(valid_configs[[i]]$data, "sf")) {
          valid_configs[[i]]$data <- st_transform(valid_configs[[i]]$data, '+proj=longlat +datum=WGS84')
        }
      }
      
      # Get selected calculation methods from surveys tab
      selected_methods <- input$cablesCalculationMethods %||% character(0)
      
      # Get combined data for each selected method if available
      combined_data_list <- list()
      if(combined_maps_data$cables_combined_map_generated && length(selected_methods) > 0) {
        if("geometric_mean" %in% selected_methods && !is.null(combined_maps_data$cables_geo)) {
          combined_data_list[["geometric_mean"]] <- combined_maps_data$cables_geo
        }
        
        if("lowest" %in% selected_methods && !is.null(combined_maps_data$cables_lowest)) {
          combined_data_list[["lowest"]] <- combined_maps_data$cables_lowest
        }
        
        if("product" %in% selected_methods && !is.null(combined_maps_data$cables_product)) {
          combined_data_list[["product"]] <- combined_maps_data$cables_product
        }
      }
      
      # Render the RMarkdown report with updated parameters
      rmarkdown::render(
        input = "Submodel_Component_Report_Template.Rmd", 
        output_file = file,
        params = list(
          map_configs = valid_configs,
          combined_data_list = combined_data_list,  
          selected_methods = selected_methods,     
          tab_name = "Industry and Operations",
          combined_map_title = "Combined Cables Maps",
          data_timestamps = timestamp_info, 
          component_name = "Cables"
        ),
        envir = new.env(parent = globalenv())
      )
      
      # Remove the modal when done
      removeModal()
    }
  )
  
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
  
  # Add this output to handle validation messages
  output$naturalResourcesCombinedValidation <- renderUI({
    # Get component selections
    include_habitat <- input$includeHabitat %||% FALSE
    include_species <- input$includeSpecies %||% FALSE
    include_birds <- input$includeBirds %||% FALSE
    
    # Check if any components are selected
    any_selected <- include_habitat || include_species || include_birds
    
    if(!any_selected) {
      div(class = "alert alert-warning", 
          "Please select at least one component to generate the combined submodel.")
    } else {
      # Check if selected components have valid data
      selected_components <- c()
      if(include_habitat && combined_maps_data$habitat_combined_map_generated) {
        selected_components <- c(selected_components, "Habitat")
      }
      if(include_species && combined_maps_data$species_combined_map_generated) {
        selected_components <- c(selected_components, "Species")
      }
      if(include_birds && FALSE) {  # Update when birds is implemented
        selected_components <- c(selected_components, "Birds")
      }
      
      if(length(selected_components) == 0) {
        div(class = "alert alert-danger", 
            "Selected components do not have combined maps generated. Please generate component maps first.")
      } else {
        div(class = "alert alert-success", 
            paste("✓ Ready to generate combined submodel using:", paste(selected_components, collapse = ", ")))
      }
    }
  })
  
  # Add this observeEvent for the generate button
  observeEvent(input$generateNaturalResourcesCombinedSubmodel, {
    # Add error handling wrapper
    tryCatch({
      # Get component selections
      include_habitat <- isTRUE(input$includeHabitat)
      include_species <- isTRUE(input$includeSpecies) 
      include_birds <- isTRUE(input$includeBirds)
      
      # Add debug prints
      cat("DEBUG: Component selections - Habitat:", include_habitat, 
          "Species:", include_species, "Birds:", include_birds, "\n")
      
      # Validate selections
      if(!include_habitat && !include_species && !include_birds) {
        showNotification("Please select at least one component.", type = "warning")
        return()
      }
      
      # Show spinner modal
      show_spinner_modal("Generating Combined Natural Resources Submodel", 
                         "Please wait while the combined submodel is being calculated...")
      
      # Add debug for data availability
      cat("DEBUG: Data availability - Habitat generated:", 
          combined_maps_data$habitat_combined_map_generated,
          "Species generated:", combined_maps_data$species_combined_map_generated, "\n")
      
      # Collect component data based on user selections
      component_data_list <- list()
      
      if(include_habitat && combined_maps_data$habitat_combined_map_generated) {
        method <- input$habitatCalculationMethod %||% "geometric_mean"
        cat("DEBUG: Habitat method selected:", method, "\n")
        
        habitat_data <- switch(method,
                               "geometric_mean" = combined_maps_data$habitat_geo,
                               "lowest" = combined_maps_data$habitat_lowest,
                               "product" = combined_maps_data$habitat_product,
                               combined_maps_data$habitat_geo)  # fallback
        
        cat("DEBUG: Habitat data is null:", is.null(habitat_data), "\n")
        if(!is.null(habitat_data)) {
          cat("DEBUG: Habitat data rows:", nrow(habitat_data), "\n")
          component_data_list[["habitat"]] <- habitat_data
        }
      }
      
      if(include_species && combined_maps_data$species_combined_map_generated) {
        method <- input$speciesCalculationMethod %||% "geometric_mean"
        cat("DEBUG: Species method selected:", method, "\n")
        
        species_data <- switch(method,
                               "geometric_mean" = combined_maps_data$species_geo,
                               "lowest" = combined_maps_data$species_lowest,
                               "product" = combined_maps_data$species_product,
                               combined_maps_data$species_geo)  # fallback
        
        cat("DEBUG: Species data is null:", is.null(species_data), "\n")
        if(!is.null(species_data)) {
          cat("DEBUG: Species data rows:", nrow(species_data), "\n")
          component_data_list[["species"]] <- species_data
        }
      }
      
      cat("DEBUG: Component data list length:", length(component_data_list), "\n")
      
      # Generate the combined submodel using geometric mean
      if(length(component_data_list) > 0) {
        cat("DEBUG: Calling create_combined_submodel\n")
        
        combined_submodel_result <- create_combined_submodel(component_data_list)
        
        cat("DEBUG: Combined submodel result is null:", is.null(combined_submodel_result), "\n")
        
        # Store the result
        combined_maps_data$natural_resources_combined_submodel <- combined_submodel_result$combined_data
        combined_maps_data$natural_resources_combined_submodel_generated <- TRUE
        
        # IMPORTANT: Store the map object for rendering
        combined_maps_data$natural_resources_combined_map <- combined_submodel_result$map
        
        showNotification("Combined Natural Resources Submodel generated successfully!", type = "message")
      } else {
        showNotification("No valid component data available for selected components.", type = "error")
      }
      
      # Remove spinner modal
      removeModal()
      
    }, error = function(e) {
      # Remove modal on error
      removeModal()
      
      # Show error notification
      showNotification(paste("Error generating combined submodel:", e$message), 
                       type = "error", duration = 10)
      
      # Print full error to console
      cat("ERROR in generateNaturalResourcesCombinedSubmodel:", e$message, "\n")
      cat("Full error object:", str(e), "\n")
    })
  })
  
  # SEPARATE renderLeaflet - this is the key fix
  output$naturalResourcesCombinedMap <- renderLeaflet({
    cat("DEBUG: Rendering combined map\n")
    
    # Check if the map is available
    if(!is.null(combined_maps_data$natural_resources_combined_map)) {
      cat("DEBUG: Map object found, rendering\n")
      combined_maps_data$natural_resources_combined_map
    } else {
      cat("DEBUG: No map object found\n")
      # Return a placeholder map
      leaflet() %>%
        addProviderTiles("Esri.OceanBasemap") %>%
        addControl("Generate combined submodel to see map", position = "center")
    }
  })
  
  # Render the map container content
  output$naturalResourcesCombinedMapContainer <- renderUI({
    cat("DEBUG: Rendering map container, generated flag:", 
        combined_maps_data$natural_resources_combined_submodel_generated, "\n")
    
    if(combined_maps_data$natural_resources_combined_submodel_generated) {
      tagList(
        p("This map shows the combined Natural Resources submodel calculated using the geometric mean of selected components."),
        leafletOutput("naturalResourcesCombinedMap")
      )
    } else {
      div(
        style = "text-align: center; padding: 40px; color: #666;",
        p("Combined submodel map will appear here after generation."),
        p("Use the sidebar to configure and generate the combined submodel.")
      )
    }
  })
  
  # Add this downloadHandler for the combined submodel export
  output$naturalResourcesCombinedExport <- downloadHandler(
    filename = function() {
      paste("Natural_Resources_Combined_Submodel_Report_", format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".html", sep = "")
    },
    content = function(file) {
      # Show spinner modal
      show_spinner_modal("Generating Combined Submodel Report", 
                         "Please wait while the Natural Resources Combined Submodel report is being generated...")
      
      # Get component selections for the report
      selected_components <- c()
      component_methods <- c()
      
      if(input$includeHabitat %||% FALSE) {
        selected_components <- c(selected_components, "Habitat")
        component_methods <- c(component_methods, paste("Habitat:", input$habitatCalculationMethod %||% "geometric_mean"))
      }
      if(input$includeSpecies %||% FALSE) {
        selected_components <- c(selected_components, "Species")
        component_methods <- c(component_methods, paste("Species:", input$speciesCalculationMethod %||% "geometric_mean"))
      }
      if(input$includeBirds %||% FALSE) {
        selected_components <- c(selected_components, "Birds")
        component_methods <- c(component_methods, paste("Birds:", input$birdsCalculationMethod %||% "geometric_mean"))
      }
      
      # Render the report
      rmarkdown::render(
        input = "Natural_Resources_Submodel.Rmd", 
        output_file = file,
        params = list(
          map_configs = list(),  # No individual map configs for combined submodel
          combined_data_list = list("combined_submodel" = combined_maps_data$natural_resources_combined_submodel),
          selected_methods = c("combined_submodel"),
          tab_name = "Natural Resources",
          combined_map_title = "Combined Natural Resources Submodel",
          data_timestamps = NULL,
          component_name = "Combined Natural Resources",
          selected_components = selected_components,
          component_methods = component_methods
        ),
        envir = new.env(parent = globalenv())
      )
      
      # Remove modal
      removeModal()
    }
  )
}