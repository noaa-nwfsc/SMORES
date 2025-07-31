function(input, output, session) {
  
  # Create a reactive values object to store data so it can be used throughout the app
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
    natural_resources_combined_map = NULL,
    natural_resources_combined_map_cropped = NULL,
    natural_resources_combined_map_cropped_normalized = NULL,
    industry = NULL, 
    surveys_geo = NULL,
    surveys_lowest = NULL, 
    surveys_product = NULL,
    surveys_combined_map_generated = FALSE, 
    cables_geo = NULL,
    cables_lowest = NULL,
    cables_product = NULL, 
    cables_combined_map_generated = FALSE,
    industry_operations_combined_submodel = NULL,
    industry_operations_combined_submodel_generated = FALSE, 
    industry_operations_combined_map = NULL,
    industry_operations_combined_map_cropped = NULL, 
    industry_operations_combined_map_cropped_normalized = NULL,
    overall_combined_model = NULL,
    overall_combined_model_generated = FALSE,
    overall_combined_map = NULL,
    overall_combined_map_cropped = NULL, 
    overall_combined_map_cropped_normalized = NULL
  )
  
  #AOI selector options
  observe({
    if(!is.null(AOI) && "Area_Name" %in% names(AOI)){
      area_names <- sort(unique(AOI$Area_Name))
      updateRadioButtons(
        session = session,
        inputId = "aoiAreaSelector",
        choices = c("All Areas" = "all", area_names),  # Add "All Areas" option
        selected = "all"  # Select "All Areas" by default
      )
    }
  })
  
  # Reactive expression for filtered WEA data with debug
  filtered_aoi_data <- reactive({
    
    # If no area is selected or "All Areas" is selected, return all WEA data
    if(is.null(input$aoiAreaSelector) || input$aoiAreaSelector == "" || 
       input$aoiAreaSelector == "loading" || input$aoiAreaSelector == "all") {
      return(AOI)  # Return all WEAs
    }
    
    # Filter the data when a specific area is selected
    filtered_data <- AOI[AOI$Area_Name == input$aoiAreaSelector, ]
    
    return(filtered_data)
  })
  
  # WEA Map Output - Modified to show all WEAs initially
  output$aoiMap <- renderLeaflet({
    
    tryCatch({
      # Call the reactive expression to get the actual data
      aoi_data <- filtered_aoi_data()
      
      if(is.null(aoi_data) || nrow(aoi_data) == 0) {
        return(leaflet() %>% 
                 addProviderTiles("Esri.OceanBasemap") %>% 
                 addControl("No Area of Interest data available", position = "center"))
      }
      
      # Transform to WGS84 if needed
      if(!st_is_longlat(aoi_data)) {
        aoi_data <- st_transform(aoi_data, 4326)
      }
      
      # Remove Z & M dimensions for leaflet compatability
      aoi_data <- st_zm(aoi_data)
      
      # Create the map with different styling based on selection
      map <- leaflet() %>%
        addProviderTiles("Esri.OceanBasemap")
      
      # Check if showing all areas or just one
      if(is.null(input$aoiAreaSelector) || input$aoiAreaSelector == "" || input$aoiAreaSelector == "loading") {
        # Show all WEAs with lighter styling
        map <- map %>%
          addPolygons(
            data = aoi_data,
            fillColor = "lightblue",
            weight = 1,
            color = "navy",
            fillOpacity = 0.3,
            popup = ~paste("Area:", Area_Name),
            highlight = highlightOptions(
              weight = 2,
              color = "blue",
              fillOpacity = 0.6,
              bringToFront = TRUE
            )
          )
      } else {
        # Show selected WEA with highlighted styling
        map <- map %>%
          addPolygons(
            data = aoi_data,
            fillColor = "blue",
            weight = 2,
            color = "darkblue",
            fillOpacity = 0.6,
            popup = ~paste("Selected Area:", Area_Name)
          )
      }
      
      return(map)
      
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
    
    # Special handling for Z Membership selection
    if(selected_score == "Z Membership") {
      # For Z Membership, return all data since it's already the correct dataset
      # and we want to show the continuous Z membership values
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
      # Convert both to character for comparison to handle numeric vs character issues
      rows_to_keep <- rows_to_keep | (as.character(df[[col]]) == as.character(selected_score))
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
    valid_configs <- natural_resources_valid_configs()
    aoi_data <- filtered_aoi_data()
    
    # Generate each map directly using the pure function
    for(config in valid_configs) {
      local({
        local_config <- config
        map_id <- paste0("naturalresources_map_", local_config$index)
        
        # Generate map using pure function and assign to output
        output[[map_id]] <- renderLeaflet({
          create_individual_map(local_config, aoi_data)
        })
      })
    }
  })
  
  # Industry & Operations maps
  observe({
    valid_configs <- industry_operations_valid_configs()
    aoi_data <- filtered_aoi_data()
    
    # Generate each map directly using the pure function
    for(config in valid_configs) {
      local({
        local_config <- config
        map_id <- paste0("industryoperations_map_", local_config$index)
        
        # Generate map using pure function and assign to output
        output[[map_id]] <- renderLeaflet({
          create_individual_map(local_config, aoi_data)
        })
      })
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
      generate_industry_operations_combined_sidebar(industry_operations_config, combined_maps_data)
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
      "Deep Sea Coral Robust High Suitability" = list(
        data = function(score) {
          if(score == "Z Membership") {
            return(DSC_RH_z_membership)
          } else {
            return(DSC_RH)
          }
        }, 
        score_column = function(score) {
          if(score == "Z Membership") {
            return("Score.Z_Membership")
          } else {
            return("Score.DSC_RH")
          }
        }
      ),
      "Seeps" = list(data = seeps, score_column = "Score.Seeps"),
      "Shelf Break" = list(data = shlfbrk, score_column = "Score.ShlfBrk"),
      "EFHCA" = list(data = efhca, score_column = "Score.EFHCA"), 
      "EFHCA 700 fathom" = list(data = efhca_700, score_column = "Score.EFHCA.700"), 
      "HAPC AOI" = list(data = HAPCaoi, score_column = "Score.HAPC.AOI"), 
      "HAPC Rocky Reef" = list(data = HAPCreef, score_column = "Score.HAPC.Reef")
    )
    
    # Get valid configurations
    valid_configs <- natural_resources_valid_configs()
    
    # Generate maps using the restructured approach
    all_results <- list()
    for(method in selected_methods) {
      all_results[[method]] <- generate_combined_map_for_method(
        valid_configs = valid_configs,
        dataset_mapping = habitat_dataset_mapping,
        method = method,
        map_type = "Habitat", 
        aoi_data = filtered_aoi_data(),
        base_grid = grid_test
      )
    }
    
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
      
      # Get filtered AOI data for the report
      aoi_data <- filtered_aoi_data()
      
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
          component_name = "Habitat",
          aoi_data = aoi_data
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
      "ESA Critical Habitat for Humpback Whales - Mexico and Central DPS" = list(data = humpback_whale, score_column = "Score.humpback_whale"),
      "Biologically Important Area - Blue Whale" = list(data = blue_whale, score_column = "Score.blue_whale")
    )
    
    # Get valid configurations
    valid_configs <- natural_resources_valid_configs()
    
    # Generate maps using the restructured approach
    all_results <- list()
    for(method in selected_methods) {
      all_results[[method]] <- generate_combined_map_for_method(
        valid_configs = valid_configs,
        dataset_mapping = species_dataset_mapping,
        method = method,
        map_type = "Species", 
        aoi_data = filtered_aoi_data(),
        base_grid = grid_test
      )
    }
    
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
      
      # Get filtered AOI data for the report
      aoi_data <- filtered_aoi_data()
      
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
          component_name = "Species",
          aoi_data = aoi_data
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
    
    # Generate maps using the restructured approach
    all_results <- list()
    for(method in selected_methods) {
      all_results[[method]] <- generate_combined_map_for_method(
        valid_configs = valid_configs,
        dataset_mapping = surveys_dataset_mapping,
        method = method,
        map_type = "Surveys",
        aoi_data = filtered_aoi_data(),
        base_grid = grid_test
      )
    }
      
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
      
      # Get filtered AOI data for the report
      aoi_data <- filtered_aoi_data()
      
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
          component_name = "Surveys",
          aoi_data = aoi_data
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
      "Submarine Cables" = list(data = submarine_cable, score_column = "Score.submarine_cable")
    )
    
    # Get valid configurations
    valid_configs <- industry_operations_valid_configs()
    
    # Generate maps using the restructured approach
    all_results <- list()
    for(method in selected_methods) {
      all_results[[method]] <- generate_combined_map_for_method(
        valid_configs = valid_configs,
        dataset_mapping = cables_dataset_mapping,
        method = method,
        map_type = "Cables",
        aoi_data = filtered_aoi_data(),
        base_grid = grid_test
      )
    }
    
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
      
      # Get filtered AOI data for the report
      aoi_data <- filtered_aoi_data()
      
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
          component_name = "Cables",
          aoi_data = aoi_data
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
  
  # Enhanced submodel status using the new function
  submodel_status <- reactive({
    list(
      natural_resources = list(
        available = combined_maps_data$habitat_combined_map_generated || 
          combined_maps_data$species_combined_map_generated,
        components = list(
          habitat = combined_maps_data$habitat_combined_map_generated,
          species = combined_maps_data$species_combined_map_generated,
          birds = FALSE  # Add this when birds is a go
        )
      ),
      fisheries = list(
        available = FALSE,  # Update this when fisheries is a go
        components = list()
      ),
      industry_operations = list(
        available = combined_maps_data$surveys_combined_map_generated || 
          combined_maps_data$cables_combined_map_generated,
        components = list(
          surveys = combined_maps_data$surveys_combined_map_generated,
          cables = combined_maps_data$cables_combined_map_generated
        )
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
  
  # status for each submodel
  output$overallModelSubmodelStatus <- renderUI({
    status <- submodel_status()
    
    tagList(
      submodel_status_display("Natural Resources", status$natural_resources),
      submodel_status_display("Fisheries", status$fisheries),
      submodel_status_display("Industry & Operations", status$industry_operations)
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
      
      # Validate selections
      if(!include_habitat && !include_species && !include_birds) {
        showNotification("Please select at least one component.", type = "warning")
        return()
      }
      
      # Show spinner modal
      show_spinner_modal("Generating Combined Natural Resources Submodel", 
                         "Please wait while the combined submodel is being calculated...")
     
      
      # Collect component data based on user selections
      component_data_list <- list()
      
      if(include_habitat && combined_maps_data$habitat_combined_map_generated) {
        method <- input$habitatCalculationMethod %||% "geometric_mean"
     
        habitat_data <- switch(method,
                               "geometric_mean" = combined_maps_data$habitat_geo,
                               "lowest" = combined_maps_data$habitat_lowest,
                               "product" = combined_maps_data$habitat_product,
                               combined_maps_data$habitat_geo)  # fallback
        
        if(!is.null(habitat_data)) {
          component_data_list[["habitat"]] <- habitat_data
        }
      }
      
      if(include_species && combined_maps_data$species_combined_map_generated) {
        method <- input$speciesCalculationMethod %||% "geometric_mean"
        
        species_data <- switch(method,
                               "geometric_mean" = combined_maps_data$species_geo,
                               "lowest" = combined_maps_data$species_lowest,
                               "product" = combined_maps_data$species_product,
                               combined_maps_data$species_geo)  # fallback
        
        if(!is.null(species_data)) {
          component_data_list[["species"]] <- species_data
        }
      }
      
      # Generate the combined submodel using geometric mean
      if(length(component_data_list) > 0) {
        
        combined_submodel_result <- create_combined_submodel(component_data_list,
                                                             base_grid = grid_test,
                                                             aoi_data_reactive = filtered_aoi_data)
        
        # Store the result
        combined_maps_data$natural_resources_combined_submodel <- combined_submodel_result$combined_data
        combined_maps_data$natural_resources_combined_submodel_generated <- TRUE
        
        # Store the map object for rendering
        combined_maps_data$natural_resources_combined_map <- combined_submodel_result$map
        
        # Generate and store the cropped map
        if(!is.null(combined_submodel_result$combined_data)) {
          cropped_map <- create_aoi_cropped_map(
            combined_data = combined_submodel_result$combined_data,
            aoi_data_reactive = filtered_aoi_data,
            map_title = "Natural Resources AOI-Cropped",
            full_data_range = combined_submodel_result$full_data_range
          )
          combined_maps_data$natural_resources_combined_map_cropped <- cropped_map
        
        
        # Generate and store the normalized cropped map
        normalized_cropped_map <- create_aoi_cropped_normalized_map(
          combined_data = combined_submodel_result$combined_data,
          aoi_data_reactive = filtered_aoi_data,
          map_title = "Natural Resources AOI-Cropped Normalized"
        )
        combined_maps_data$natural_resources_combined_map_cropped_normalized <- normalized_cropped_map
      }
        
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
    })
  })
  
  # Natural Resources combined map
  output$naturalResourcesCombinedMap <- renderLeaflet({
    # Check if the map is available
    if(!is.null(combined_maps_data$natural_resources_combined_map)) {
      combined_maps_data$natural_resources_combined_map
    } else {
      # Return a placeholder map
      leaflet() %>%
        addProviderTiles("Esri.OceanBasemap") %>%
        addControl("Generate combined submodel to see map", position = "center")
    }
  })
  
  # Natural Resources cropped map output
  output$naturalResourcesCombinedMapCropped <- renderLeaflet({
    if(!is.null(combined_maps_data$natural_resources_combined_map_cropped)) {
      combined_maps_data$natural_resources_combined_map_cropped
    } else {
      leaflet() %>%
        addProviderTiles("Esri.OceanBasemap") %>%
        addControl("Generate combined submodel and select a AOI to see cropped map", position = "center")
    }
  })
  
  # Natural Resources normalized cropped map output
  output$naturalResourcesCombinedMapCroppedNormalized <- renderLeaflet({
    if(!is.null(combined_maps_data$natural_resources_combined_map_cropped_normalized)) {
      combined_maps_data$natural_resources_combined_map_cropped_normalized
    } else {
      leaflet() %>%
        addProviderTiles("Esri.OceanBasemap") %>%
        addControl("Generate combined submodel and select a AOI to see normalized cropped map", position = "center")
    }
  })
  
  # Render the map container content
  output$naturalResourcesCombinedMapContainer <- renderUI({
    
    if(combined_maps_data$natural_resources_combined_submodel_generated) {
      tagList(
        # Main combined map section
        div(
          h4("Combined Natural Resources Submodel Map"),
          p("This map shows the combined Natural Resources submodel calculated using the geometric mean of selected components."),
          leafletOutput("naturalResourcesCombinedMap", height = "500px")
        ),
        
        br(),
        
        # Cropped map section
        div(
          h4("AOI-Cropped Natural Resources Submodel Map"),
          p("This map shows the same combined submodel data cropped to the selected Area of Interest (AOI)."),
          leafletOutput("naturalResourcesCombinedMapCropped", height = "500px")
        ),
        
        br(),
        
        # Normalized cropped map section
        div(
          h4("AOI-Cropped Normalized Natural Resources Submodel Map"),
          p("This map shows the AOI-cropped data normalized to a 0-1 scale for easier comparison across different areas."),
          leafletOutput("naturalResourcesCombinedMapCroppedNormalized", height = "500px")
        )
      )
    } else {
      div(
        style = "text-align: center; padding: 40px; color: #666;",
        p("Combined submodel maps will appear here after generation."),
        p("Use the sidebar to configure and generate the combined submodel.")
      )
    }
  })
  
  # downloadHandler for the natural resources combined submodel export
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
      component_layer_details <- list()
      
      if(input$includeHabitat %||% FALSE) {
        selected_components <- c(selected_components, "Habitat")
        component_methods <- c(component_methods, input$habitatCalculationMethod %||% "geometric_mean")
        
        # Get habitat layer details with scores
        habitat_configs <- get_valid_configs_for_tab(input, "habitat", habitat_layer, score_colors, filter_by_score)
        if(length(habitat_configs) > 0) {
          component_layer_details[["Habitat"]] <- list(
            method = input$habitatCalculationMethod %||% "geometric_mean",
            layers = lapply(habitat_configs, function(config) {
              list(
                layer_name = config$layer %||% "Unknown",
                score_used = config$score %||% "Unknown"
              )
            })
          )
        }}
        if(input$includeSpecies %||% FALSE) {
          selected_components <- c(selected_components, "Species")
          component_methods <- c(component_methods, input$speciesCalculationMethod %||% "geometric_mean")
          
          # Get species layer details with scores
          species_configs <- get_valid_configs_for_tab(input, "species", species_layer, score_colors, filter_by_score)
          if(length(species_configs) > 0) {
            component_layer_details[["Species"]] <- list(
              method = input$speciesCalculationMethod %||% "geometric_mean",
              layers = lapply(species_configs, function(config) {
                list(
                  layer_name = config$layer %||% "Unknown",
                  score_used = config$score %||% "Unknown"
                )
              })
            )
          }}
      # Create component data summary for the report
      component_data_summary <- list()
      
      if(input$includeHabitat %||% FALSE) {
        method <- input$habitatCalculationMethod %||% "geometric_mean"
        habitat_data <- switch(method,
                               "geometric_mean" = combined_maps_data$habitat_geo,
                               "lowest" = combined_maps_data$habitat_lowest,
                               "product" = combined_maps_data$habitat_product,
                               combined_maps_data$habitat_geo)
        
        if(!is.null(habitat_data)) {
          component_data_summary[["Habitat"]] <- list(
            data_points = nrow(habitat_data),
            description = "Habitat suitability analysis for marine ecosystems"
          )
        }
      }
      
      if(input$includeSpecies %||% FALSE) {
        method <- input$speciesCalculationMethod %||% "geometric_mean"
        species_data <- switch(method,
                               "geometric_mean" = combined_maps_data$species_geo,
                               "lowest" = combined_maps_data$species_lowest,
                               "product" = combined_maps_data$species_product,
                               combined_maps_data$species_geo)
        
        if(!is.null(species_data)) {
          component_data_summary[["Species"]] <- list(
            data_points = nrow(species_data),
            description = "Critical habitat analysis for protected species"
          )
        }
      }
      
      # Get filtered timestamp information for the combined submodel
      all_configs <- list()
      
      # Get timestamp data for all included components
      timestamp_info <- get_filtered_timestamp_data(all_configs, "combined")
      
      # Get filtered AOI data for the report
      aoi_data <- filtered_aoi_data()
      
      # Get cropped map and cropped normalized map
      aoi_cropped_map <- combined_maps_data$natural_resources_combined_map_cropped  
      aoi_cropped_normalized_map <- combined_maps_data$natural_resources_combined_map_cropped_normalized
      
      
      # Render the combined submodel report
      rmarkdown::render(
        input = "Submodel_Combined_Report_Template.Rmd", 
        output_file = file,
        params = list(
          submodel_name = "Natural Resources",
          selected_components = selected_components,
          component_methods = component_methods,
          combined_data = combined_maps_data$natural_resources_combined_submodel,
          combined_map_title = "Natural Resources Combined Submodel",
          data_timestamps = timestamp_info,
          component_data_summary = component_data_summary,
          aoi_data = aoi_data,
          component_layer_details = component_layer_details,
          aoi_cropped_map = aoi_cropped_map,
          aoi_cropped_normalized_map = aoi_cropped_normalized_map
        ),
        envir = new.env(parent = globalenv())
      )
      
      # Remove modal
      removeModal()
    }
  )
  
  # Add this output to handle validation messages
  output$industryOperationsCombinedValidation <- renderUI({
    # Get component selections
    include_surveys <- input$includeSurveys %||% FALSE
    include_cables <- input$includeCables %||% FALSE
    
    # Check if any components are selected
    any_selected <- include_surveys || include_cables 
    
    if(!any_selected) {
      div(class = "alert alert-warning", 
          "Please select at least one component to generate the combined submodel.")
    } else {
      # Check if selected components have valid data
      selected_components <- c()
      if(include_surveys && combined_maps_data$surveys_combined_map_generated) {
        selected_components <- c(selected_components, "Surveys")
      }
      if(include_cables && combined_maps_data$cables_combined_map_generated) {
        selected_components <- c(selected_components, "Cables")
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
  observeEvent(input$generateIndustryOperationsCombinedSubmodel, {
    # Add error handling wrapper
    tryCatch({
      # Get component selections
      include_surveys <- isTRUE(input$includeSurveys)
      include_cables <- isTRUE(input$includeCables)
      
      # Validate selections
      if(!include_surveys && !include_cables) {
        showNotification("Please select at least one component.", type = "warning")
        return()
      }
      
      # Show spinner modal
      show_spinner_modal("Generating Combined Industry & Operations Submodel", 
                         "Please wait while the combined submodel is being calculated...")
      
      # Collect component data based on user selections
      component_data_list <- list()
      
      if(include_surveys && combined_maps_data$surveys_combined_map_generated) {
        method <- input$surveysCalculationMethod %||% "geometric_mean"
      
        surveys_data <- switch(method,
                               "geometric_mean" = combined_maps_data$surveys_geo,
                               "lowest" = combined_maps_data$surveys_lowest,
                               "product" = combined_maps_data$surveys_product,
                               combined_maps_data$surveys_geo)  # fallback
        
        if(!is.null(surveys_data)) {
          component_data_list[["surveys"]] <- surveys_data
        }
      }
      
      if(include_cables && combined_maps_data$cables_combined_map_generated) {
        method <- input$cablesCalculationMethod %||% "geometric_mean"
    
        cables_data <- switch(method,
                               "geometric_mean" = combined_maps_data$cables_geo,
                               "lowest" = combined_maps_data$cables_lowest,
                               "product" = combined_maps_data$cables_product,
                               combined_maps_data$cables_geo)  # fallback
        
        if(!is.null(cables_data)) {
          component_data_list[["cables"]] <- cables_data
        }
      }
      
      # Generate the combined submodel using geometric mean
      if(length(component_data_list) > 0) {
      
        combined_submodel_result <- create_combined_submodel(component_data_list, 
                                                             base_grid = grid_test, 
                                                             aoi_data_reactive = filtered_aoi_data)
        
        # Store the result
        combined_maps_data$industry_operations_combined_submodel <- combined_submodel_result$combined_data
        combined_maps_data$industry_operations_combined_submodel_generated <- TRUE
        
        # Store the map object for rendering
        combined_maps_data$industry_operations_combined_map <- combined_submodel_result$map
        
        # Generate and store the cropped map
        if(!is.null(combined_submodel_result$combined_data)) {
          cropped_map <- create_aoi_cropped_map(
            combined_data = combined_submodel_result$combined_data,
            aoi_data_reactive = filtered_aoi_data,
            map_title = "Industry Operations AOI-Cropped",
            full_data_range = combined_submodel_result$full_data_range
          )
          combined_maps_data$industry_operations_combined_map_cropped <- cropped_map
          
          # Generate and store the normalized cropped map
          normalized_cropped_map <- create_aoi_cropped_normalized_map(
            combined_data = combined_submodel_result$combined_data,
            aoi_data_reactive = filtered_aoi_data,
            map_title = "Industry Operations AOI-Cropped Normalized"
          )
          combined_maps_data$industry_operations_combined_map_cropped_normalized <- normalized_cropped_map
        }
        
        showNotification("Combined Industry & Operations Submodel generated successfully!", type = "message")
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
    })
  })
  
  # Industry and operations 
  output$industryOperationsCombinedMap <- renderLeaflet({
    # Check if the map is available
    if(!is.null(combined_maps_data$industry_operations_combined_map)) {
      combined_maps_data$industry_operations_combined_map
    } else {
      # Return a placeholder map
      leaflet() %>%
        addProviderTiles("Esri.OceanBasemap") %>%
        addControl("Generate combined submodel to see map", position = "center")
    }
  })
  
  # Industry Operations cropped map output
  output$industryOperationsCombinedMapCropped <- renderLeaflet({
    if(!is.null(combined_maps_data$industry_operations_combined_map_cropped)) {
      combined_maps_data$industry_operations_combined_map_cropped
    } else {
      leaflet() %>%
        addProviderTiles("Esri.OceanBasemap") %>%
        addControl("Generate combined submodel and select a AOI to see cropped map", position = "center")
    }
  })
  
  # Industry Operations normalized cropped map output
  output$industryOperationsCombinedMapCroppedNormalized <- renderLeaflet({
    if(!is.null(combined_maps_data$industry_operations_combined_map_cropped_normalized)) {
      combined_maps_data$industry_operations_combined_map_cropped_normalized
    } else {
      leaflet() %>%
        addProviderTiles("Esri.OceanBasemap") %>%
        addControl("Generate combined submodel and select a AOI to see normalized cropped map", position = "center")
    }
  })
  
  # Render the map container content
  output$industryOperationsCombinedMapContainer <- renderUI({
    
    if(combined_maps_data$industry_operations_combined_submodel_generated) {
      tagList(
        # Main combined map section
        div(
          h4("Combined Industry & Operations Submodel Map"),
          p("This map shows the combined Industry & Operations submodel calculated using the geometric mean of selected components."),
          leafletOutput("industryOperationsCombinedMap", height = "500px")
        ),
        
        br(),
        
        # Cropped map section
        div(
          h4("AOI-Cropped Industry & Operations Submodel Map"),
          p("This map shows the same combined submodel data cropped to the selected Area of Interest (AOI)."),
          leafletOutput("industryOperationsCombinedMapCropped", height = "500px")
        ),
        
        br(),
        
        # Normalized cropped map section
        div(
          h4("AOI-Cropped Normalized Industry & Operations Submodel Map"),
          p("This map shows the AOI-cropped data normalized to a 0-1 scale for easier comparison across different areas."),
          leafletOutput("industryOperationsCombinedMapCroppedNormalized", height = "500px")
        )
      )
    } else {
      div(
        style = "text-align: center; padding: 40px; color: #666;",
        p("Combined submodel maps will appear here after generation."),
        p("Use the sidebar to configure and generate the combined submodel.")
      )
    }
  })
  
  # downloadHandler for the industry and operations combined submodel export
  output$industryOperationsCombinedExport <- downloadHandler(
    filename = function() {
      paste("Industry_Operations_Combined_Submodel_Report_", format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".html", sep = "")
    },
    content = function(file) {
      # Show spinner modal
      show_spinner_modal("Generating Combined Submodel Report", 
                         "Please wait while the Industry & Operations Combined Submodel report is being generated...")
      
      # Get component selections for the report
      selected_components <- c()
      component_methods <- c()
      component_layer_details <- list()
      
      if(input$includeSurveys %||% FALSE) {
        selected_components <- c(selected_components, "Scientific Surveys")
        component_methods <- c(component_methods, input$surveysCalculationMethod %||% "geometric_mean")
        
        # Get habitat layer details with scores
        surveys_configs <- get_valid_configs_for_tab(input, "surveys", surveys_layer, score_colors, filter_by_score)
        if(length(surveys_configs) > 0) {
          component_layer_details[["Scientific Surveys"]] <- list(
            method = input$surveysCalculationMethod %||% "geometric_mean",
            layers = lapply(surveys_configs, function(config) {
              list(
                layer_name = config$layer %||% "Unknown",
                score_used = config$score %||% "Unknown"
              )
            })
          )
        }}
      if(input$includeCables %||% FALSE) {
        selected_components <- c(selected_components, "Submarine Cables")
        component_methods <- c(component_methods, input$cablesCalculationMethod %||% "geometric_mean")
        
        # Get habitat layer details with scores
        cables_configs <- get_valid_configs_for_tab(input, "cables", cables_layer, score_colors, filter_by_score)
        if(length(cables_configs) > 0) {
          component_layer_details[["Submarine Cables"]] <- list(
            method = input$cablesCalculationMethod %||% "geometric_mean",
            layers = lapply(cables_configs, function(config) {
              list(
                layer_name = config$layer %||% "Unknown",
                score_used = config$score %||% "Unknown"
              )
            })
          )
        }}
      # Create component data summary for the report
      component_data_summary <- list()
      
      if(input$includeSurveys %||% FALSE) {
        method <- input$surveysCalculationMethod %||% "geometric_mean"
        surveys_data <- switch(method,
                               "geometric_mean" = combined_maps_data$surveys_geo,
                               "lowest" = combined_maps_data$surveys_lowest,
                               "product" = combined_maps_data$surveys_product,
                               combined_maps_data$surveys_geo)
        
        if(!is.null(surveys_data)) {
          component_data_summary[["Scientific Surveys"]] <- list(
            data_points = nrow(surveys_data),
            description = "Scientific survey activities and their spatial impact assessment"
          )
        }
      }
      if(input$includeCables %||% FALSE) {
        method <- input$cablesCalculationMethod %||% "geometric_mean"
        cables_data <- switch(method,
                              "geometric_mean" = combined_maps_data$cables_geo,
                              "lowest" = combined_maps_data$cables_lowest,
                              "product" = combined_maps_data$cables_product,
                              combined_maps_data$cables_geo)
        
        if(!is.null(cables_data)) {
          component_data_summary[["Submarine Cables"]] <- list(
            data_points = nrow(cables_data),
            description = "Submarine cable infrastructure and associated buffer zones"
          )
        }
      }
      
      # Get filtered timestamp information for the combined submodel
      all_configs <- list()
      
      # Get timestamp data for all included components
      timestamp_info <- get_filtered_timestamp_data(all_configs, "combined")
      
      # Get filtered AOI data for the report
      aoi_data <- filtered_aoi_data()
      
      aoi_cropped_map <- combined_maps_data$industry_operations_combined_map_cropped
      aoi_cropped_normalized_map <- combined_maps_data$industry_operations_combined_map_cropped_normalized
      
      
      # Render the combined submodel report
      rmarkdown::render(
        input = "Submodel_Combined_Report_Template.Rmd", 
        output_file = file,
        params = list(
          submodel_name = "Industry & Operations",
          selected_components = selected_components,
          component_methods = component_methods,
          combined_data = combined_maps_data$industry_operations_combined_submodel,
          combined_map_title = "Industry & Operations Combined Submodel",
          data_timestamps = timestamp_info,
          component_data_summary = component_data_summary,
          aoi_data = aoi_data, 
          aoi_cropped_map = aoi_cropped_map,
          aoi_cropped_normalized_map = aoi_cropped_normalized_map
        ),
        envir = new.env(parent = globalenv())
      )
      
      # Remove modal
      removeModal()
    }
  )
  
  # Generate Overall Model Button Logic
  observeEvent(input$generateOverallModel, {
    tryCatch({
      # Get weight values
      natural_resources_weight <- input$weightNaturalResources %||% 0
      fisheries_weight <- input$weightFisheries %||% 0
      industry_weight <- input$weightIndustryOperations %||% 0
      
      # Get which submodels are enabled
      nr_enabled <- input$enableNaturalResources %||% FALSE
      fisheries_enabled <- input$enableFisheries %||% FALSE
      industry_enabled <- input$enableIndustryOperations %||% FALSE
      
      # Validate that at least one submodel is enabled with weight > 0
      enabled_submodels <- c()
      enabled_weights <- c()
      
      if(nr_enabled && natural_resources_weight > 0 && 
         !is.null(combined_maps_data$natural_resources_combined_submodel)) {
        enabled_submodels <- c(enabled_submodels, "natural_resources")
        enabled_weights <- c(enabled_weights, natural_resources_weight)
      }
      
      if(fisheries_enabled && fisheries_weight > 0) {
        # Add fisheries here
        # enabled_submodels <- c(enabled_submodels, "fisheries")
        # enabled_weights <- c(enabled_weights, fisheries_weight)
      }
      
      if(industry_enabled && industry_weight > 0 && 
         !is.null(combined_maps_data$industry_operations_combined_submodel)) {
        enabled_submodels <- c(enabled_submodels, "industry_operations")
        enabled_weights <- c(enabled_weights, industry_weight)
      }
      
      # Check if we have any valid submodels
      if(length(enabled_submodels) == 0) {
        showNotification(
          "No valid submodels selected. Please enable submodels with weights > 0 and ensure they have been generated.", 
          type = "warning"
        )
        return()
      }
      
      # Show spinner modal
      show_spinner_modal("Generating Overall Combined Model", 
                         paste("Please wait while the overall model is being calculated using", 
                               length(enabled_submodels), "submodel(s)..."))
      
      # Collect submodel data and weights
      submodels <- list()
      weights <- list()
      
      for(i in seq_along(enabled_submodels)) {
        submodel_name <- enabled_submodels[i]
        
        if(submodel_name == "natural_resources") {
          submodels[["natural_resources"]] <- combined_maps_data$natural_resources_combined_submodel
          weights[["natural_resources"]] <- enabled_weights[i]
        } else if(submodel_name == "industry_operations") {
          submodels[["industry_operations"]] <- combined_maps_data$industry_operations_combined_submodel
          weights[["industry_operations"]] <- enabled_weights[i]
        }
        # Add fisheries here
      }
      
      # Generate the overall combined model
      overall_result <- create_overall_combined_model(
        submodels = submodels,
        weights = weights,
        base_grid = grid_test,
        aoi_data_reactive = filtered_aoi_data
      )
      
      # Store the results
      combined_maps_data$overall_combined_model <- overall_result$combined_data
      combined_maps_data$overall_combined_model_generated <- TRUE
      combined_maps_data$overall_combined_map <- overall_result$map
      
      # Generate cropped maps if we have valid data
      if(!is.null(overall_result$combined_data) && 
         "Overall_Geo_mean" %in% names(overall_result$combined_data)) {
        
        # Create a modified version of the data with Geo_mean column for compatibility
        cropped_data <- overall_result$combined_data %>%
          mutate(Geo_mean = Overall_Geo_mean)
        
        # Get the data range for consistent coloring
        overall_values <- cropped_data$Geo_mean[!is.na(cropped_data$Geo_mean)]
        full_data_range <- list(
          min = min(overall_values, na.rm = TRUE),
          max = max(overall_values, na.rm = TRUE)
        )
        
        # Generate AOI-cropped map
        cropped_map <- create_aoi_cropped_map(
          combined_data = cropped_data,
          aoi_data_reactive = filtered_aoi_data,
          map_title = "Overall Model AOI-Cropped",
          full_data_range = full_data_range
        )
        combined_maps_data$overall_combined_map_cropped <- cropped_map
        
        # Generate normalized cropped map
        normalized_cropped_map <- create_aoi_cropped_normalized_map(
          combined_data = cropped_data,
          aoi_data_reactive = filtered_aoi_data,
          map_title = "Overall Model AOI-Cropped Normalized"
        )
        combined_maps_data$overall_combined_map_cropped_normalized <- normalized_cropped_map
      }
      
      # Show success notification
      showNotification(
        paste("Overall Combined Model generated successfully using", 
              length(enabled_submodels), "submodel(s)!"), 
        type = "message"
      )
      
      # Remove spinner modal
      removeModal()
      
    }, error = function(e) {
      # Remove modal on error
      removeModal()
      
      # Show error notification
      showNotification(
        paste("Error generating overall combined model:", e$message), 
        type = "error", 
        duration = 10
      )
    })
  })
  
  output$overallCombinedMapContainer <- renderUI({
    if(combined_maps_data$overall_combined_model_generated) {
      tagList(
        # Main combined map section
        div(
          h4("Overall Combined Model Map"),
          p("This map shows the overall combined model calculated using the weighted geometric mean of selected submodels."),
          leafletOutput("overallCombinedMap", height = "500px")
        ),
        
        br(),
        
        # Cropped map section
        div(
          h4("AOI-Cropped Overall Model Map"),
          p("This map shows the same overall model data cropped to the selected Area of Interest (AOI)."),
          leafletOutput("overallCombinedMapCropped", height = "500px")
        ),
        
        br(),
        
        # Normalized cropped map section
        div(
          h4("AOI-Cropped Normalized Overall Model Map"),
          p("This map shows the AOI-cropped data normalized to a 0-1 scale for easier comparison across different areas."),
          leafletOutput("overallCombinedMapCroppedNormalized", height = "500px")
        )
      )
    } else {
      div(
        style = "text-align: center; padding: 40px; color: #666;",
        p("Overall model maps will appear here after generation."),
        p("Use the sidebar to configure and generate the overall model.")
      )
    }
  })
  
  # Overall Combined Model map outputs
  output$overallCombinedMap <- renderLeaflet({
    if(!is.null(combined_maps_data$overall_combined_map)) {
      combined_maps_data$overall_combined_map
    } else {
      leaflet() %>%
        addProviderTiles("Esri.OceanBasemap") %>%
        addControl("Generate overall model to see map", position = "center")
    }
  })
  
  output$overallCombinedMapCropped <- renderLeaflet({
    if(!is.null(combined_maps_data$overall_combined_map_cropped)) {
      combined_maps_data$overall_combined_map_cropped
    } else {
      leaflet() %>%
        addProviderTiles("Esri.OceanBasemap") %>%
        addControl("Generate overall model and select an AOI to see cropped map", position = "center")
    }
  })
  
  output$overallCombinedMapCroppedNormalized <- renderLeaflet({
    if(!is.null(combined_maps_data$overall_combined_map_cropped_normalized)) {
      combined_maps_data$overall_combined_map_cropped_normalized
    } else {
      leaflet() %>%
        addProviderTiles("Esri.OceanBasemap") %>%
        addControl("Generate overall model and select an AOI to see normalized cropped map", position = "center")
    }
  })
}