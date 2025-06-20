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
    industry_input_names <- names(input)[grepl("^EnableIndustryMap", names(input))]
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
    
    if(length(valid_configs) == 0) {
      return(card(
        card_body(
          p("No maps configured yet. Please enable and configure maps in the sidebar.")
        )
      ))
    }
    
    # Create map cards
    map_cards <- lapply(valid_configs, function(config) {
      i <- config$index
      map_id <- paste0("naturalresources_map_", i)
      
      card(
        card_header(paste0("Map ", i, ": ", config$layer, " - ", config$score)),
        card_body(
          leafletOutput(map_id, height = 250)
        )
      )
    })
    
    # Arrange cards in rows of 2
    rows <- list()
    for(i in seq(1, length(map_cards), by = 2)) {
      row_cards <- map_cards[i:min(i+1, length(map_cards))]
      rows[[length(rows) + 1]] <- do.call(layout_columns, row_cards)
    }
    
    # Add combined map at the bottom if it has been generated
    if(combined_maps_data$habitat_combined_map_generated) {
      rows[[length(rows) + 1]] <- card(
        card_header(h3("Combined Map Result (Geometric Mean)")),
        card_body(
          leafletOutput("combinedMap", height = 400) 
        )
      )
    }
    
    tagList(rows)
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
    valid_configs <- get_valid_configs()
    
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
  
  # Habitat tab RMarkdown export handler
  output$habitatExportRmd <- downloadHandler(
    filename = function() {
      paste("Natural_Resources_Submodel_Report_", format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".html", sep = "")
    },
    content = function(file) {
      # Show modal with spinner
      show_spinner_modal("Generating Report", 
                         "Please wait while the report is being generated...")
      
      # Get valid configurations
      valid_configs <- get_valid_configs()
      
      # Make sure each valid_config has valid spatial data
      for(i in seq_along(valid_configs)) {
        # Ensure data is transformed to WGS84 for leaflet
        if(!is.null(valid_configs[[i]]$data) && inherits(valid_configs[[i]]$data, "sf")) {
          valid_configs[[i]]$data <- st_transform(valid_configs[[i]]$data, '+proj=longlat +datum=WGS84')
        }
      }
      
      # Calculate combined data if needed
      combined_data <- NULL
      if(length(valid_configs) > 0) {
        # Use grid_test as the base spatial grid for combining data
        combined_data <- grid_test
        
        # For each valid configuration, extract the data and join with the base grid
        for(config in valid_configs) {
          layer_name <- config$layer
          score_value <- config$score
          
          # Determine which dataset and score columns to use
          if(layer_name == "Canyon") {
            dataset <- canyon
            score_column <- "Score.Canyon"
          } else if(layer_name == "Deep Sea Coral Robust High Suitability") {
            dataset <- DSC_RH
            score_column <- "Score.DSC_RH"
          } else if(layer_name == "Seeps") {
            dataset <- seeps
            score_column <- "Score.Seeps"
          } else if(layer_name == "Shelf Break") {
            dataset <- shlfbrk
            score_column <- "Score.ShlfBrk"
          } else if(layer_name == "EFHCA") {
            dataset <- efhca
            score_column <- "Score.EFHCA"
          } else if(layer_name == "EFHCA 700 fathom") {
            dataset <- efhca_700
            score_column <- "Score.EFHCA.700"
          } else if(layer_name == "HAPC AOI") {
            dataset <- HAPCaoi
            score_column <- "Score.HAPC.AOI"
          } else if(layer_name == "HAPC Rocky Reef") {
            dataset <- HAPCreef
            score_column <- "Score.HAPC.Reef"
          } else {
            next  # Skip if layer name doesn't match
          }
          
          # Filter for the selected score value and prepare for joining
          temp_data <- dataset %>%
            filter(.data[[score_column]] == score_value) %>%
            st_drop_geometry() %>%
            select(CellID_2km, !!score_column)
          
          # Convert the score column to numeric (explicit conversion)
          temp_data[[score_column]] <- as.numeric(temp_data[[score_column]])
          
          # Join with the combined data
          combined_data <- left_join(combined_data, temp_data, by = "CellID_2km")
        }
        
        # Find all score columns in the combined data
        score_cols <- names(combined_data)[grep("^Score\\.", names(combined_data))]
        
        if(length(score_cols) > 0) {
          # geometric mean calculation that handles all edge cases
          combined_data$Geo_mean <- sapply(1:nrow(combined_data), function(i) {
            # Get the row of data
            row_data <- combined_data[i, score_cols, drop = TRUE]
            
            # Convert to numeric and remove NAs
            values <- as.numeric(unlist(row_data))
            values <- values[!is.na(values)]
            
            # Check if we have any values
            if(length(values) == 0) {
              return(NA)
            }
            
            # Check for zeros or negative values
            if(any(values <= 0)) {
              return(0)
            }
            
            # Calculate geometric mean
            exp(mean(log(values)))
          })
          
          # Make sure geometry is set properly for leaflet
          combined_data <- st_transform(combined_data, '+proj=longlat +datum=WGS84')
        }
      }
      
      # Create a temporary directory for debugging output
      debug_dir <- tempdir()
      write(paste("Number of valid configs:", length(valid_configs)), 
            file = file.path(debug_dir, "debug_info.txt"))
      
      # If we have valid configs, write some details to debug
      if(length(valid_configs) > 0) {
        for(i in seq_along(valid_configs)) {
          config <- valid_configs[[i]]
          write(paste("Config", i, "- Layer:", config$layer, "Score:", config$score, 
                      "Rows:", nrow(config$data), 
                      "Is SF:", inherits(config$data, "sf")),
                file = file.path(debug_dir, "debug_info.txt"), 
                append = TRUE)
        }
      }
      
      # Output combined data info as well
      if(!is.null(combined_data)) {
        write(paste("Combined data rows:", nrow(combined_data), 
                    "Has Geo_mean:", "Geo_mean" %in% names(combined_data)),
              file = file.path(debug_dir, "debug_info.txt"), 
              append = TRUE)
      }
      
      # Render the RMarkdown report with both valid_configs and combined_data
      rmarkdown::render(
        input = "Natural_Resources_Submodel.Rmd", 
        output_file = file,
        params = list(
          map_configs = valid_configs,
          combined_data = combined_data
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
    valid_configs <- get_valid_configs()
    
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
    
    if(length(valid_configs) == 0) {
      return(card(
        card_body(
          p("No maps configured yet. Please enable and configure maps in the sidebar.")
        )
      ))
    }
    
    # Create map cards
    map_cards <- lapply(valid_configs, function(config) {
      i <- config$index
      map_id <- paste0("industry_map_", i)
      
      card(
        card_header(paste0("Map ", i, ": ", config$layer, " - ", config$score)),
        card_body(
          leafletOutput(map_id, height = 250)
        )
      )
    })
    
    # Arrange cards in rows of 2
    rows <- list()
    for(i in seq(1, length(map_cards), by = 2)) {
      row_cards <- map_cards[i:min(i+1, length(map_cards))]
      rows[[length(rows) + 1]] <- do.call(layout_columns, row_cards)
    }
    
    # Add combined map at the bottom if it has been generated
  if(combined_maps_data$industry_combined_map_generated) {
    rows[[length(rows) + 1]] <- card(
      card_header(h4("Combined Industry & Operations Map Result (Geometric Mean")),
      card_body(
        leafletOutput("industryMap", height = 400)
      )
    )
  }
    
    tagList(rows)
  })
  
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
        " No submodels have been generated yet. Please go to each tab (Habitat, Species, Birds) and generate the combined maps first."
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