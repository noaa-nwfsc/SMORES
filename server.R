function(input, output, session) {
  
  # Function to filter dataframe by weight
  # This function keeps columns that don't contain other weight values
  filter_by_weight <- function(df, selected_weight) {
    if(is.null(df) || is.null(selected_weight) || selected_weight == "None") {
      return(df)
    }
    
    # Get all column names
    all_cols <- names(df)
    
    # Create a list of weights to exclude (all weights except the selected one)
    exclude_weights <- setdiff(weight_values, selected_weight)
    
    # Find columns to keep (those that don't contain any of the excluded weights in their names)
    cols_to_keep <- all_cols[!sapply(all_cols, function(col_name) {
      any(sapply(exclude_weights, function(w) grepl(w, col_name)))
    })]
    
    # Return filtered dataframe with only the kept columns
    return(df[, cols_to_keep, drop = FALSE])
  }
  
  # Create reactive dataset based on user selection
  selected_data <- reactive({
    # Get the map index (could be from an input or other reactive value)
    # For demonstration, let's use the first valid config
    valid_configs <- get_valid_configs()
    if(length(valid_configs) > 0) {
      map_index <- valid_configs[[1]]$index
    } else {
      # Default to first map if no valid configs
      map_index <- 1
    }
    
    layer_input_name <- paste0("layerPicker", map_index)
    selected_layer <- input[[layer_input_name]]
    
    # Return the selected dataset from our list if it exists
    if(!is.null(selected_layer) && selected_layer != "None" && selected_layer %in% names(layer)) {
      return(layer[[selected_layer]])
    }
    # Return NULL if nothing selected
    return(NULL)
  })
  
  # Update weight pickerInput based on available columns in the selected dataset
  observe({
    data <- selected_data()
    
    # Get column names from the dataset that match our weight patterns
    avail_weights <- weight_values[weight_values %in% names(data)]
    
    # Update the weight pickerInput
    updatePickerInput(
      session = session,
      inputId = "weight",
      choices = avail_weights,
      selected = if(length(avail_weights) > 0) avail_weights[1] else NULL
    )
  })
  
  
  # Create 5 map configuration sets
  map_configs <- 1:5
  
  # Dynamic sidebar content based on selected tab
  output$dynamicSidebar <- renderUI({
    # Get current tab
    current_tab <- input$dataTabs
    
    if (is.null(current_tab)) {
      current_tab <- "habitat"  # Default to first tab
    }
    
    # Return different picker inputs based on tab
    switch(current_tab,
           "habitat" = tagList(
             h4("Habitat Map Settings"),
             # Create 5 sets of layer and weight pickers
             lapply(map_configs, function(i) {
               tagList(
                 hr(),
                 h5(paste("Map", i, "Configuration")),
                 # Add a checkbox to enable/disable this map configuration
                 checkboxInput(
                   inputId = paste0("enableMap", i),
                   label = paste("Enable Map", i),
                   # indicates whether a user sees all options or just enable at first
                   value = FALSE
                 ),
                 # Only show these inputs if the map is enabled
                 conditionalPanel(
                   condition = paste0("input.enableMap", i, " == true"),
                   pickerInput(
                     inputId = paste0("layerPicker", i),
                     label = paste("Select Layer to view for Map", i),
                     choices = c("None", names(layer)),
                     multiple = FALSE,
                     selected = "None"
                   ),
                   pickerInput(
                     inputId = paste0("weightPicker", i),
                     label = paste("Select Scoring Weight for Map", i),
                     choices = c("None", weight_values),
                     multiple = FALSE,
                     selected = "None"
                   )
                 )
               )
             })
           ),
           
           # "species" = tagList(...),
           # "fisheries" = tagList(...)
    )
  })
  
  # Helper function to check if a configuration is valid and should be displayed
  is_valid_config <- function(i) {
    # Check if map is enabled
    enable_input <- input[[paste0("enableMap", i)]]
    if (is.null(enable_input) || !enable_input) {
      return(FALSE)
    }
    
    # Check if both layer and weight are selected and not "None"
    layer_input <- input[[paste0("layerPicker", i)]]
    weight_input <- input[[paste0("weightPicker", i)]]
    
    if (is.null(layer_input) || is.null(weight_input) ||
        layer_input == "None" || weight_input == "None") {
      return(FALSE)
    }
    
    # Check if layer exists in our list
    if (!(layer_input %in% names(layer))) {
      return(FALSE)
    }
    
    return(TRUE)
  }
  
  # Modify get_valid_configs to use the filter_by_weight function
  get_valid_configs <- reactive({
    valid_configs <- lapply(map_configs, function(i) {
      if (is_valid_config(i)) {
        layer_input <- input[[paste0("layerPicker", i)]]
        weight_input <- input[[paste0("weightPicker", i)]]
        
        # Get the full dataset
        full_dataset <- layer[[layer_input]]
        
        # Filter the dataset to include only columns related to the selected weight
        filtered_data <- filter_by_weight(full_dataset, weight_input)
        
        return(list(
          index = i,
          layer = layer_data,
          weight = weight_input
        ))
      } else {
        return(NULL)
      }
    })
    
    # Filter out NULL entries
    valid_configs[!sapply(valid_configs, is.null)]
  })
  
  
  # Container for multiple maps
  output$multipleMapsContainer <- renderUI({
    # Create map cards for all configured maps
    map_cards <- lapply(map_configs, function(i) {
      # Check if this configuration is valid
      if (is_valid_config(i)) {
        layer_input <- input[[paste0("layerPicker", i)]]
        weight_input <- input[[paste0("weightPicker", i)]]
        
        # Map ID for this specific map
        map_id <- paste0("map_", i)
        
        card(
          card_header(paste0("Map ", i, ": ", layer_input, " - ", weight_input)),
          card_body(
            leafletOutput(map_id, height = 250)
          )
        )
      } else {
        NULL
      }
    })
    
    # Filter out NULL entries (unconfigured maps)
    map_cards <- map_cards[!sapply(map_cards, is.null)]
    
    # If no maps configured, return message
    if (length(map_cards) == 0) {
      return(
        card(
          card_body(
            p("No maps configured yet. Please enable and configure maps in the sidebar.")
          )
        )
      )
    }
    
    # Calculate how many maps per row (max 2 maps per row for better visibility)
    maps_per_row <- 2
    num_maps <- length(map_cards)
    
    # Group the cards into rows using layout_columns
    if (num_maps <= maps_per_row) {
      # If we have fewer or equal to maps_per_row, just put them all in one row
      do.call(layout_columns, map_cards)
    } else {
      # Otherwise, create multiple rows
      rows <- list()
      for(i in seq(1, num_maps, by = maps_per_row)) {
        end_idx <- min(i + maps_per_row - 1, num_maps)
        row_cards <- map_cards[i:end_idx]
        rows[[length(rows) + 1]] <- do.call(layout_columns, row_cards)
      }
      
      # Return all rows
      tagList(rows)
    }
  })
  
  # Create the observe events for all possible maps (1-5)
  lapply(map_configs, function(i) {
    map_id <- paste0("map_", i)
    
    # Observer for this specific map
    observe({
      # Only render if configuration is valid
      if (is_valid_config(i)) {
        layer_input <- input[[paste0("layerPicker", i)]]
        weight_input <- input[[paste0("weightPicker", i)]]
        
        # Get the full dataset
        full_dataset <- layer[[layer_input]]
        
        # Filter the dataset based on selected weight
        filtered_data <- filter_by_weight(full_dataset, weight_input)
        
        # Render the map with filtered data
        output[[map_id]] <- renderLeaflet({
          leaflet() %>%
            # add ESRI Ocean basemap
            addProviderTiles("Esri.OceanBasemap",
                             options = providerTileOptions(variant = "Ocean/World_Ocean_Base")) %>% 
            # add ESRI Ocean placename labels and borders to basemap
            addProviderTiles("Esri.OceanBasemap",
                             options = providerTileOptions(variant = "Ocean/World_Ocean_Reference")) %>% 
            addPolygons(data = filtered_data, color = "lightpink", fillOpacity = 0.5)
        })
      }
    })
  })
}