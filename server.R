function(input, output, session) {

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
    enable_input <- input[[paste0("enableMap", i)]]
    layer_input <- input[[paste0("layerPicker", i)]]
    score_input <- input[[paste0("scorePicker", i)]]
    
    !is.null(enable_input) && enable_input &&
      !is.null(layer_input) && layer_input != "None" &&
      !is.null(score_input) && score_input != "None" &&
      layer_input %in% names(layer)
  }
  
  # Get valid map configurations
  get_valid_configs <- reactive({
    valid_configs <- list()
    
    for(i in 1:5) {
      if(is_valid_config(i)) {
        layer_name <- input[[paste0("layerPicker", i)]]
        score_name <- input[[paste0("scorePicker", i)]]
        dataset <- layer[[layer_name]]
        filtered_data <- filter_by_score(dataset, score_name)
        
        # Get the color for this score
        color <- score_colors[[score_name]]
        
        valid_configs[[length(valid_configs) + 1]] <- list(
          index = i,
          layer = layer_name,
          score = score_name,
          data = filtered_data,
          color = color
        )
      }
    }
    
    return(valid_configs)
  })
  
  # combined map placeholder with message indicating that you need to hit the button
  output$combinedMap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("Esri.OceanBasemap",
                       options = providerTileOptions(variant = "Ocean/World_Ocean_Base")) %>%
      addProviderTiles("Esri.OceanBasemap",
                       options = providerTileOptions(variant = "Ocean/World_Ocean_Reference")) %>%
      setView(lng = -70, lat = 40, zoom = 5) %>%
      addControl("Click 'Generate Combined Map' button to create the combined map.", position = "topright")
  })
  
  # Dynamic sidebar content
  output$dynamicSidebar <- renderUI({
    current_tab <- input$dataTabs %||% "habitat"
    
    if(current_tab == "habitat") {
      map_inputs <- lapply(1:5, function(i) {
        tagList(
          hr(),
          h5(paste("Map", i, "Configuration")),
          checkboxInput(paste0("enableMap", i), paste("Enable Map", i), value = FALSE),
          conditionalPanel(
            condition = paste0("input.enableMap", i, " == true"),
            pickerInput(
              paste0("layerPicker", i),
              paste("Select Layer for Map", i),
              choices = c("None", names(layer)),
              selected = "None"
            ),
            pickerInput(
              paste0("scorePicker", i),
              paste("Select score for Map", i),
              choices = c("None", score_values),
              selected = "None"
            )
          )
        )
      })
      
      #this currently does not have active functionality: need to update
      tagList(
        h4("Habitat Map Settings"),
        map_inputs,
        hr(),
        h4("Combined Map Settings"),
        helpText("The combined map will calculate the geometric mean"),
        actionButton("generateCombinedMap", "Generate Combined Map", 
                     class = "btn-primary btn-block")
      )
    }
  })
  
  # Multiple maps container
  output$multipleMapsContainer <- renderUI({
    valid_configs <- get_valid_configs()
    
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
      map_id <- paste0("map_", i)
      
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
    
    # Add combined map at the bottom
    rows[[length(rows) + 1]] <- card(
      card_header(h3("Combined Map Result (Geometric Mean)")),
      card_body(
        leafletOutput("combinedMap", height = 400) 
      )
    )
    
    tagList(rows)
  })
  
  # create individual maps
  observe({
    valid_configs <- get_valid_configs()
    
    for(config in valid_configs) {
      local({
        local_config <- config
        map_id <- paste0("map_", local_config$index)
        
        output[[map_id]] <- renderLeaflet({
          # Ensure we have data to display
          if(nrow(local_config$data) == 0) {
            return(leaflet() %>%
                     addProviderTiles("Esri.OceanBasemap",
                                      options = providerTileOptions(variant = "Ocean/World_Ocean_Base")) %>%
                     addProviderTiles("Esri.OceanBasemap",
                                      options = providerTileOptions(variant = "Ocean/World_Ocean_Reference")) %>%
                     setView(lng = -70, lat = 40, zoom = 5) %>%
                     addControl("No data matching selected score", position = "topright"))
          }
          
          # Create the map with legend
          leaflet() %>%
            addProviderTiles("Esri.OceanBasemap",
                             options = providerTileOptions(variant = "Ocean/World_Ocean_Base")) %>%
            addProviderTiles("Esri.OceanBasemap",
                             options = providerTileOptions(variant = "Ocean/World_Ocean_Reference")) %>%
            addPolygons(
              data = local_config$data, 
              color = "#33333300",  
              weight = 1,            
              fillColor = local_config$color,
              fillOpacity = 0.7,
              popup = ~paste("Score:", local_config$score)
            ) %>%
            # Add a legend
            addLegend(
              position = "bottomright",
              colors = local_config$color,
              labels = paste("Score:", local_config$score),
              opacity = 0.7,
              title = local_config$layer
            )
        })
      })
    }
  })
  
  # Combined map logic
  observeEvent(input$generateCombinedMap, {
    # Show modal with spinner that covers the whole tab
    showModal(modalDialog(
      title = "Generating Combined Map",
      div(
        style = "text-align: center; padding: 40px;",
        tags$div(
          style = "position: relative; min-height: 100px;",
          shinycssloaders::withSpinner(
            uiOutput("modalSpinnerPlaceholder"),
            type = 3, 
            color = "#033c73", 
            color.background = "#FFFFFF",
            size = 2
          )
        ),
        p("Please wait while the combined map is being generated...")
      ),
      footer = NULL,
      size = "l",
      easyClose = FALSE,
      fade = TRUE
    ))
    
    # Render a placeholder for the spinner
    output$modalSpinnerPlaceholder <- renderUI({
      div(style = "height: 100px;")
    })
    
    # Add a small delay to ensure the modal is visible before proceeding
    Sys.sleep(0.5)
    
    valid_configs <- get_valid_configs()
    
    if(length(valid_configs) > 0) {
      # Use grid_test as the base spatial grid for combining data this will ensure that all grid cells are retained
      combined_data <- grid_test
      
      # For each valid configuration, extract the data and join with the base grid
      for(config in valid_configs) {
        layer_name <- config$layer
        score_value <- config$score
        
        # Determine which dataset and score columns to use
        if(layer_name == "Canyon") {
          dataset <- canyon_data
          score_column <- "Score.Canyon"
        } else if(layer_name == "DSC_RH") {
          dataset <- DSC_RH_data
          score_column <- "Score.DSC_RH"
        } else if(layer_name == "Fixed Surveys") {
          dataset <- surveys_fixed
          score_column <- "Score.Surveys_fixed"
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
        
        # Create color palette for the geometric mean
        pal <- colorNumeric("viridis", domain = c(min(combined_data$Geo_mean, na.rm = TRUE), 
                                                  max(combined_data$Geo_mean, na.rm = TRUE)), 
                            na.color = "transparent")
        
        # Create the map
        output$combinedMap <- renderLeaflet({
          leaflet() %>%
            addProviderTiles("Esri.OceanBasemap",
                             options = providerTileOptions(variant = "Ocean/World_Ocean_Base")) %>%
            addProviderTiles("Esri.OceanBasemap",
                             options = providerTileOptions(variant = "Ocean/World_Ocean_Reference")) %>%
            addPolygons(
              data = combined_data, 
              color = "#33333300", #adding 00 at the end makes this color transparent
              weight = 1, 
              fillColor = ~pal(Geo_mean), 
              fillOpacity = 1,
              popup = ~paste("Geometric Mean Score:", round(Geo_mean, 2))
            ) %>%
            addLegend(
              position = "bottomright",
              pal = pal,
              values = combined_data$Geo_mean,
              title = "Combined Geometric Mean",
              opacity = 1
            )
        })
      } else {
        # No score columns found
        output$combinedMap <- renderLeaflet({
          leaflet() %>%
            addProviderTiles("Esri.OceanBasemap") %>%
            setView(lng = -70, lat = 40, zoom = 5) %>%
            addControl("No score data available for the selected layers.", position = "topright")
        })
      }
    } else {
      # No valid configurations
      output$combinedMap <- renderLeaflet({
        leaflet() %>%
          addProviderTiles("Esri.OceanBasemap") %>%
          setView(lng = -70, lat = 40, zoom = 5) %>%
          addControl("Please configure at least one map to generate a combined map.", position = "topright")
      })
    }
    
    # Remove modal spinner once combined map is finished being generated
    removeModal()
    
  })
}