function(input, output, session) {
  
  # Define a qualitative color palette for scores
  score_colors <- list(
    "0.1" = "#E41A1C",  # red
    "0.2" = "#377EB8",  # blue
    "0.3" = "#4DAF4A",  # green
    "0.4" = "#984EA3",  # purple
    "0.5" = "#FF7F00",  # orange
    "0.6" = "#FFFF33",  # yellow
    "0.7" = "#A65628",  # brown
    "0.8" = "#F781BF",  # pink
    "0.9" = "#999999",  # grey
    "1" = "#000000"     # black
  )
  
  # filter dataframe by score for long format data
  filter_by_score <- function(df, selected_score) {
    if(is.null(df) || is.null(selected_score) || selected_score == "None") {
      return(df)
    }
    
    # Find the score column for this layer
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
        local_config <- config  # Create local copy to avoid issues with loop variables
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
              color = "#333333",     # Border color
              weight = 1,            # Border width
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
    valid_configs <- get_valid_configs()
    
    if(length(valid_configs) > 0) {
      # Create a data frame to track which layers and scores are selected
      selected_layers_scores <- data.frame(
        layer = sapply(valid_configs, function(config) config$layer),
        score = sapply(valid_configs, function(config) config$score),
        stringsAsFactors = FALSE
      )
      
      # Start with the full dataset
      filtered_data <- full_data
      
      # Filter the full dataset based on selected layer-score combinations
      for(i in 1:nrow(selected_layers_scores)) {
        layer_name <- selected_layers_scores$layer[i]
        score_value <- selected_layers_scores$score[i]
        
        # Determine the score column based on the layer name
        if(layer_name == "Canyon") {
          score_column <- "Score.Canyon"
        } else if(layer_name == "DSC_RH") {
          score_column <- "Score.DSC_RH"
        } else if(layer_name == "Fixed Surveys") {
          score_column <- "Score.Surveys_fixed"
        } else {
          next  # Skip if layer name doesn't match
        }
        
        # Filter the dataset for this layer-score combination
        filtered_data <- filtered_data[filtered_data[[score_column]] == score_value, ]
      }
      
      # If we have results, calculate geometric mean and create map
      if(nrow(filtered_data) > 0) {
        # Get score columns from selected layers
        selected_score_columns <- c()
        for(layer_name in selected_layers_scores$layer) {
          if(layer_name == "Canyon") selected_score_columns <- c(selected_score_columns, "Score.Canyon")
          else if(layer_name == "DSC_RH") selected_score_columns <- c(selected_score_columns, "Score.DSC_RH")
          else if(layer_name == "Fixed Surveys") selected_score_columns <- c(selected_score_columns, "Score.Surveys_fixed")
        }
        selected_score_columns <- unique(selected_score_columns)
        
        # Calculate geometric mean
        filtered_data$Geo_mean <- apply(filtered_data[, selected_score_columns, drop = FALSE], 1, function(x) {
          exp(mean(log(as.numeric(x)), na.rm = TRUE))
        })
        
        # Determine colors based on the geometric mean
        geo_mean_colors <- sapply(filtered_data$Geo_mean, function(score) {
          score_str <- as.character(round(score, 1))
          if(score_str %in% names(score_colors)) {
            return(score_colors[[score_str]])
          } else {
            closest_score <- names(score_colors)[which.min(abs(as.numeric(names(score_colors)) - score))]
            return(score_colors[[closest_score]])
          }
        })
        
        # Create a unique palette for the legend
        unique_scores <- sort(unique(round(filtered_data$Geo_mean, 1)))
        unique_colors <- sapply(unique_scores, function(score) {
          score_str <- as.character(score)
          if(score_str %in% names(score_colors)) {
            return(score_colors[[score_str]])
          } else {
            closest_score <- names(score_colors)[which.min(abs(as.numeric(names(score_colors)) - score))]
            return(score_colors[[closest_score]])
          }
        })
        
        # # Transform the spatial data to WGS84 if it's an sf object
        # if(inherits(filtered_data, "sf")) {
        #   # Transform to WGS84 for leaflet
        #   filtered_data <- st_transform(filtered_data, '+proj=longlat +datum=WGS84')
        # }
        
        output$combinedMap <- renderLeaflet({
          leaflet() %>%
            addProviderTiles("Esri.OceanBasemap",
                             options = providerTileOptions(variant = "Ocean/World_Ocean_Base")) %>%
            addProviderTiles("Esri.OceanBasemap",
                             options = providerTileOptions(variant = "Ocean/World_Ocean_Reference")) %>%
            addPolygons(
              data = filtered_data, 
              color = "#333333", 
              weight = 1, 
              fillColor = geo_mean_colors, 
              fillOpacity = 0.7,
              popup = ~paste("Geometric Mean Score:", round(Geo_mean, 2))
            ) %>%
            addLegend(
              position = "bottomright",
              colors = unique_colors,
              labels = paste("Score:", unique_scores),
              opacity = 0.7,
              title = "Combined Geometric Mean"
            )
        })
      } else {
        # If we couldn't create combined data, show an empty map with a message
        output$combinedMap <- renderLeaflet({
          leaflet() %>%
            addProviderTiles("Esri.OceanBasemap",
                             options = providerTileOptions(variant = "Ocean/World_Ocean_Base")) %>%
            addProviderTiles("Esri.OceanBasemap",
                             options = providerTileOptions(variant = "Ocean/World_Ocean_Reference")) %>%
            setView(lng = -70, lat = 40, zoom = 5) %>%
            addControl("No matching data found with the selected combination of scores.", position = "topright")
        })
      }
    } else {
      # No valid configurations
      output$combinedMap <- renderLeaflet({
        leaflet() %>%
          addProviderTiles("Esri.OceanBasemap",
                           options = providerTileOptions(variant = "Ocean/World_Ocean_Base")) %>%
          addProviderTiles("Esri.OceanBasemap",
                           options = providerTileOptions(variant = "Ocean/World_Ocean_Reference")) %>%
          setView(lng = -70, lat = 40, zoom = 5) %>%
          addControl("Please configure at least one map to generate a combined map.", position = "topright")
      })
    }
  })
}