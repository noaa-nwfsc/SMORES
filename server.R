function(input, output, session) {
 
  # Track which individual map outputs have been created
  individual_maps_created <- reactiveValues(
    naturalresources = character(0),
    fisheries = character(0), 
    industryoperations = character(0)
  )
  
  # Track the last known configurations for each individual map
  individual_maps_last_configs <- reactiveValues(
    naturalresources = list(),
    fisheries = list(), 
    industryoperations = list()
  )
  
   # reactive values object for submodel component combined data, and submodel combined data so it can be used throughout the app and for report generation
  combined_maps_data <- reactiveValues(
  
    habitat_geo = NULL,
    habitat_geo_map = NULL,  
    habitat_lowest = NULL,
    habitat_lowest_map = NULL,  
    habitat_product = NULL,
    habitat_product_map = NULL,  
    habitat_combined_map_generated = FALSE,
    
    species_geo = NULL,
    species_geo_map = NULL,
    species_lowest = NULL,
    species_lowest_map = NULL,
    species_product = NULL,
    species_product_map = NULL,
    species_combined_map_generated = FALSE,
    
    natural_resources_combined_submodel = NULL,
    natural_resources_combined_submodel_generated = FALSE,
    natural_resources_combined_map = NULL,
    natural_resources_combined_map_cropped_normalized = NULL,
    
    fisheries_geo = NULL,
    fisheries_geo_map = NULL,
    fisheries_lowest = NULL,
    fisheries_lowest_map = NULL,
    fisheries_product = NULL,
    fisheries_product_map = NULL,
    fisheries_combined_map_generated = FALSE,
    
    trawl_geo = NULL,
    trawl_geo_map = NULL, 
    trawl_lowest = NULL,
    trawl_lowest_map = NULL,
    trawl_product = NULL,
    trawl_product_map = NULL,
    trawl_combined_map_generated = FALSE,
    
    fisheries_combined_submodel = NULL,
    fisheries_combined_submodel_generated = FALSE,
    fisheries_combined_map = NULL,
    fisheries_combined_map_cropped_normalized = NULL,
    
    industry = NULL, #? where did i use this?
    
    surveys_geo = NULL,
    surveys_geo_map = NULL, 
    surveys_lowest = NULL, 
    surveys_lowest_map = NULL,
    surveys_product = NULL,
    surveys_product_map = NULL,
    surveys_combined_map_generated = FALSE, 
    
    cables_geo = NULL,
    cables_geo_map = NULL,
    cables_lowest = NULL,
    cables_lowest_map = NULL,
    cables_product = NULL, 
    cables_product_map = NULL,
    cables_combined_map_generated = FALSE,
    
    industry_operations_combined_submodel = NULL,
    industry_operations_combined_submodel_generated = FALSE, 
    industry_operations_combined_map = NULL,
    industry_operations_combined_map_cropped_normalized = NULL,
    
    full_model = NULL,
    full_model_generated = FALSE,
    full_map = NULL,
    full_map_cropped = NULL
  )
  
  individual_processed_data <- reactiveValues(
    naturalresources = list(),
    fisheries = list(),
    industryoperations = list()
  )
  
  # Global AOI bounds cache with invalidation
  aoi_bounds_cache <- reactiveValues(
    current_bounds = NULL,
    current_area = NULL,
    last_update = NULL
  )
  
  # Observer to update AOI bounds cache when selection changes
  observe({
    current_area <- input$aoiAreaSelector %||% "all"
    
    # Only recalculate if area selection changed
    if(is.null(aoi_bounds_cache$current_area) || 
       aoi_bounds_cache$current_area != current_area) {
      
      aoi_data <- filtered_aoi_data()
      
      if(!is.null(aoi_data) && nrow(aoi_data) > 0) {
        tryCatch({
          # Use st_bbox directly on filtered data
          bbox <- st_bbox(aoi_data)
          
          aoi_bounds_cache$current_bounds <- list(
            lng1 = bbox[["xmin"]], lat1 = bbox[["ymin"]],
            lng2 = bbox[["xmax"]], lat2 = bbox[["ymax"]],
            bbox = bbox
          )
          aoi_bounds_cache$current_area <- current_area
          aoi_bounds_cache$last_update <- Sys.time()
          
        }, error = function(e) {
          aoi_bounds_cache$current_bounds <- NULL
          aoi_bounds_cache$current_area <- current_area
        })
      } else {
        aoi_bounds_cache$current_bounds <- NULL
        aoi_bounds_cache$current_area <- current_area
      }
    }
  })
  
  #AOI selector options
  observe({
    if(!is.null(AOI) && "Area_Name" %in% names(AOI)){
      area_names <- sort(unique(AOI$Area_Name))
      updateRadioButtons(
        session = session,
        inputId = "aoiAreaSelector",
        choices = c("All Areas" = "all", "Brookings", "Coos Bay", "Morro Bay", "Humboldt", "Southern California", "Central California", "Northern California", "Washington/Oregon"),  # Add "All Areas" option
        selected = "all"  # Select "All" by default
      )
    }
  })
  
  # Reactive expression for filtered AOI data
  filtered_aoi_data <- reactive({
    
    # If no area is selected or "All Areas" is selected, return all AOI data
    if(is.null(input$aoiAreaSelector) || input$aoiAreaSelector == "" || 
       input$aoiAreaSelector == "loading" || input$aoiAreaSelector == "all") {
      return(AOI)  # Return all AOIs
    }
    
    # Filter the data when a specific area is selected
    filtered_data <- AOI[AOI$Area_Name == input$aoiAreaSelector, ]
    
    return(filtered_data)
  })
  
  # AOI Map Output showing all AOIs initially
  output$aoiMap <- renderLeaflet({
    
    tryCatch({
      # Call the reactive expression to get the actual data
      aoi_data <- filtered_aoi_data()
      
      if(is.null(aoi_data) || nrow(aoi_data) == 0) {
        return(leaflet() %>% 
                 addProviderTiles("Esri.OceanBasemap") %>% 
                 addControl("No Area of Interest data available", position = "center"))
      }
      
      # Create the map with different styling based on selection
      map <- leaflet() %>%
        addProviderTiles("Esri.OceanBasemap")
      
      # Check if showing all areas or just one
      if(is.null(input$aoiAreaSelector) || input$aoiAreaSelector == "" || input$aoiAreaSelector == "loading") {
        # Show all AOIs with lighter styling
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
        # Show selected AOI with highlighted styling
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
               addControl("Error loading AOI data", position = "topright"))
    })
  })
  
  # Helper function to check if a configuration is valid
  is_valid_config <- function(i) {
    # Determine which tab we're on
    current_tab <- input$dataTabs %||% "habitat"
    
    # Set the prefix based on the tab
    prefix <- switch(current_tab,
                     "habitat" = "Habitat",
                     "species" = "Species",
                     "surveys" = "Scientific Surveys",
                     "cables" = "Submarine Cables",
                     "fisheries" = "Fisheries",
                     "trawl" = "Trawl Fisheries",
                     "")
    
    if(prefix == "") return(FALSE) # Invalid tab
    
    # Check if configuration is valid
    !is.null(enable_input) && enable_input &&
      !is.null(layer_input) && layer_input != "None" &&
      !is.null(score_input) && score_input != "None" &&
      layer_input %in% names(layer_data)
  }
  
  # Reactive expression for Natural Resources tab valid configs
  natural_resources_valid_configs <- reactive({
    
    # For Natural Resources, we check if we have dataTabs 
    # OR if navbar is explicitly set to Natural Resources
    is_natural_resources <- (!is.null(input$dataTabs_natural_resources) && 
                               input$dataTabs_natural_resources %in% c("habitat", "species", "combined_model_natural_resources")) ||
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
                         NULL)
    
    # Use your existing function to get valid configurations
    configs <- get_valid_configs_for_tab(input, current_tab_natural_resources, layer_data, score_colors, filter_by_score, grid_test)
    
    return(configs)
  })
  
  # Reactive expression for Fisheries tab valid configs
  fisheries_valid_configs <- reactive({
    
    is_fisheries <- (!is.null(input$dataTabs_fisheries) && 
                               input$dataTabs_fisheries %in% c("fisheries", "trawl", "combined_model_fisheries")) ||
      (!is.null(input$navbar) && input$navbar == "Fisheries Submodel")
    
    if(!is_fisheries) {
      return(list())
    }
    
    # Default to habitat if dataTabs is not set
    current_tab_fisheries <- input$dataTabs_fisheries %||% "fisheries"
    
    # Set the layer data based on the current data tab
    layer_data <- switch(current_tab_fisheries,
                         "fisheries" = fisheries_layer,
                         "trawl" = trawl_fisheries_layer,
                         NULL)
    
    # Use your existing function to get valid configurations
    configs <- get_valid_configs_for_tab(input, current_tab_fisheries, layer_data, score_colors, filter_by_score, grid_test)
    
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
    configs <- get_valid_configs_for_tab(input, current_tab_industry_operations, layer_data, score_colors, filter_by_score, grid_test)
    
    return(configs)
  })
  
  # Natural Resources maps 
  observeEvent(natural_resources_valid_configs(), {
    valid_configs <- natural_resources_valid_configs()
    aoi_data <- filtered_aoi_data()
    
    # Apply cropping to each config's data BEFORE map creation
    for(config in valid_configs) {
      local({
        local_config <- config
        map_id <- paste0("naturalresources_map_", local_config$index)
        
        # Create a unique identifier for this configuration
        config_key <- paste(local_config$layer, local_config$score, local_config$index, sep = "_")
        
        # Check if this configuration has changed
        last_config <- individual_maps_last_configs$naturalresources[[config_key]]
        current_config_hash <- digest::digest(list(
          layer = local_config$layer,
          score = local_config$score,
          index = local_config$index,
          aoi_area = input$aoiAreaSelector %||% "all"
        ))
        
        # Only update if configuration has changed OR map doesn't exist yet
        if(is.null(last_config) || last_config != current_config_hash || 
           !map_id %in% individual_maps_created$naturalresources) {
          
          # PROCESS AND CACHE THE DATA
          processed_config_data <- local_config$data
          if(!is.null(processed_config_data) && !is.null(aoi_data)) {
            processed_config_data <- crop_data_to_aoi(processed_config_data, aoi_data)
          }
          
          # Get the score column for this layer
          score_col <- switch(local_config$layer,
                              "Canyon" = "Score.Canyon",
                              "Deep Sea Coral Robust High Suitability" = if(local_config$score == "Z Membership") "Score.Z_Membership" else "Score.DSC_RH",
                              "Seeps" = "Score.Seeps",
                              "Shelf Break" = "Score.ShlfBrk",
                              "EFHCA" = "Score.EFHCA",
                              "EFHCA 700 fathoms" = "Score.EFHCA.700",
                              "HAPC AOI" = "Score.HAPC.AOI",
                              "HAPC Rocky Reef" = "Score.HAPC.Reef",
                              "ESA Critical Habitat for Southern Resident Killer Whales" = "Score.killer_whale",
                              "ESA Critical Habitat for Leatherback Sea Turtles" = "Score.leatherback_turtle",
                              "ESA Critical Habitat for Humpback Whales - Mexico and Central DPS" = "Score.humpback_whale",
                              "Biologically Important Area - Blue Whale" = "Score.blue_whale",
                              NULL
          )
          
          # Cache the processed data for reuse in combined maps
          individual_processed_data$naturalresources[[config_key]] <- list(
            data = processed_config_data,
            layer = local_config$layer,
            score = local_config$score,
            score_column = score_col,
            config = local_config, 
            component_type = determine_component_type(local_config$layer)
          )
          
          # Update the map with processed data
          output[[map_id]] <- renderLeaflet({
            # Update config with processed data for map creation
            local_config$data <- processed_config_data
            create_individual_map(local_config, aoi_data, aoi_bounds = aoi_bounds_cache$current_bounds)
          })
          
          # Store the current configuration hash
          individual_maps_last_configs$naturalresources[[config_key]] <- current_config_hash
          
          # Mark as created if not already tracked
          if(!map_id %in% individual_maps_created$naturalresources) {
            individual_maps_created$naturalresources <- c(individual_maps_created$naturalresources, map_id)
          }
        }
      })
    }
  }, ignoreNULL = FALSE, ignoreInit = FALSE)
  
  # Fisheries maps
  observeEvent(fisheries_valid_configs(), {
    valid_configs <- fisheries_valid_configs()
    aoi_data <- filtered_aoi_data()
    
    # Apply cropping to each config's data BEFORE map creation
    for(config in valid_configs) {
      local({
        local_config <- config
        map_id <- paste0("fisheries_map_", local_config$index)
        
        # Create a unique identifier for this configuration
        config_key <- paste(local_config$layer, local_config$score, local_config$index, sep = "_")
        
        # Check if this configuration has changed
        last_config <- individual_maps_last_configs$fisheries[[config_key]]
        current_config_hash <- digest::digest(list(
          layer = local_config$layer,
          score = local_config$score,
          index = local_config$index,
          aoi_area = input$aoiAreaSelector %||% "all"
        ))
        
        # Only update if configuration has changed OR map doesn't exist yet
        if(is.null(last_config) || last_config != current_config_hash || 
           !map_id %in% individual_maps_created$fisheries) {
          
          # PROCESS AND CACHE THE DATA
          processed_config_data <- local_config$data
          if(!is.null(processed_config_data) && !is.null(aoi_data)) {
            processed_config_data <- crop_data_to_aoi(processed_config_data, aoi_data)
          }
          
          # Get the score column for this layer
          score_col <- switch(local_config$layer,
                              # Fisheries layers with different score types
                              "At-Sea Hake Mid-Water Trawl" = if(local_config$score == "Ranked Importance") "Score.ASH_Ranked_Importance" else "Score.ASH",
                              "Shoreside Hake Mid-Water Trawl" = if(local_config$score == "Ranked Importance") "Score.SSH_Ranked_Importance" else "Score.SSH",
                              "Groundfish Bottom Trawl" = if(local_config$score == "Ranked Importance") "Score.GFBT_Ranked_Importance" else "Score.GFBT",
                              "Groundfish Pot Gear" = if(local_config$score == "Ranked Importance") "Score.GFP_Ranked_Importance" else "Score.GFP",
                              "Groundfish Long Line Gear" = if(local_config$score == "Ranked Importance") "Score.GFLL_Ranked_Importance" else "Score.GFLL",
                              "Pink Shrimp Trawl" = if(local_config$score == "Ranked Importance") "Score.PS_Ranked_Importance" else "Score.PS",
                              "Dungeness Crab" = if(local_config$score == "Ranked Importance") "Score.CRAB_Ranked_Importance" else "Score.CRAB",
                              "Commercial Troll/Hook and Line Albacore" = if(local_config$score == "Ranked Importance") "Score.ALCO_Ranked_Importance" else "Score.ALCO",
                              "Charter Vessel Albacore Troll/Hook and Line" = if(local_config$score == "Ranked Importance") "Score.ALCH_Ranked_Importance" else "Score.ALCH",
                              # Trawl fisheries layer
                              "Trawl Fisheries @ 75%" = "Score.Trawl_Fisheries",
                              NULL
          )
          
          # Cache the processed data for reuse in combined maps
          individual_processed_data$fisheries[[config_key]] <- list(
            data = processed_config_data,
            layer = local_config$layer,
            score = local_config$score,
            score_column = score_col,
            config = local_config,
            component_type = determine_component_type(local_config$layer)
          )
          
          # Update the map with processed data
          output[[map_id]] <- renderLeaflet({
            # Update config with processed data for map creation
            local_config$data <- processed_config_data
            create_individual_map(local_config, aoi_data, aoi_bounds = aoi_bounds_cache$current_bounds)
          })
          
          # Store the current configuration hash
          individual_maps_last_configs$fisheries[[config_key]] <- current_config_hash
          
          # Mark as created if not already tracked
          if(!map_id %in% individual_maps_created$fisheries) {
            individual_maps_created$fisheries <- c(individual_maps_created$fisheries, map_id)
          }
        }
      })
    }
  }, ignoreNULL = FALSE, ignoreInit = FALSE)
  
  # Industry & Operations maps
  observeEvent(industry_operations_valid_configs(), {
    valid_configs <- industry_operations_valid_configs()
    aoi_data <- filtered_aoi_data()
    
    # Apply cropping to each config's data BEFORE map creation
    for(config in valid_configs) {
      local({
        local_config <- config
        map_id <- paste0("industryoperations_map_", local_config$index)
        
        # Create a unique identifier for this configuration
        config_key <- paste(local_config$layer, local_config$score, local_config$index, sep = "_")
        
        # Check if this configuration has changed
        last_config <- individual_maps_last_configs$industryoperations[[config_key]]
        current_config_hash <- digest::digest(list(
          layer = local_config$layer,
          score = local_config$score,
          index = local_config$index,
          aoi_area = input$aoiAreaSelector %||% "all"
        ))
        
        # Only update if configuration has changed OR map doesn't exist yet
        if(is.null(last_config) || last_config != current_config_hash || 
           !map_id %in% individual_maps_created$industryoperations) {
          
          # PROCESS AND CACHE THE DATA
          processed_config_data <- local_config$data
          if(!is.null(processed_config_data) && !is.null(aoi_data)) {
            processed_config_data <- crop_data_to_aoi(processed_config_data, aoi_data)
          }
          
          # Get the score column for this layer
          score_col <- switch(local_config$layer,
                              # Scientific Surveys layers
                              "Fixed Surveys" = "Score.Surveys_fixed",
                              "Periodic Surveys" = "Score.Surveys_periodic",
                              # Submarine Cables layer
                              "Submarine Cables" = "Score.submarine_cable",
                              NULL
          )
          
          # Cache the processed data for reuse in combined maps
          individual_processed_data$industryoperations[[config_key]] <- list(
            data = processed_config_data,
            layer = local_config$layer,
            score = local_config$score,
            score_column = score_col,
            config = local_config,
            component_type = determine_component_type(local_config$layer)
          )
          
          # Update the map with processed data
          output[[map_id]] <- renderLeaflet({
            # Update config with processed data for map creation
            local_config$data <- processed_config_data
            create_individual_map(local_config, aoi_data, aoi_bounds = aoi_bounds_cache$current_bounds)
          })
          
          # Store the current configuration hash
          individual_maps_last_configs$industryoperations[[config_key]] <- current_config_hash
          
          # Mark as created if not already tracked
          if(!map_id %in% individual_maps_created$industryoperations) {
            individual_maps_created$industryoperations <- c(individual_maps_created$industryoperations, map_id)
          }
        }
      })
    }
  }, ignoreNULL = FALSE, ignoreInit = FALSE)
  
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
      # Combined Model Tab
    } else if(current_tab_natural_resources == "combined_model_natural_resources") {
      natural_resources_config <- get_natural_resources_config()
      generate_natural_resources_combined_sidebar(natural_resources_config, combined_maps_data)
    }
  })
  
  # Dynamic sidebar content for fisheries
  output$dynamicSidebar_fisheries <- renderUI({
    current_tab_fisheries <- input$dataTabs_fisheries %||% "fisheries"
    
    if (current_tab_fisheries == "fisheries") {
      fisheries_layer_names <- names(fisheries_layer)
      fisheries_config <- get_fisheries_config()
      
      #use function to make fisheries sidebar
      generate_fisheries_sidebar(
        fisheries_layer_names,  
        score_values_ranked_importance,
        current_tab = current_tab_fisheries,
        submodel_config = fisheries_config
      )
      
    } else if (current_tab_fisheries == "trawl") {
      # Get the layer names for species
      trawl_fisheries_layers <- names(trawl_fisheries_layer)
      fisheries_config <- get_fisheries_config()
      
      #use function to make trawl fisheries sidebar
      generate_trawl_fisheries_sidebar(
        trawl_fisheries_layer, 
        score_values_trawl_fisheries, 
        current_tab = current_tab_fisheries,
        submodel_config = fisheries_config
      )
      
      # Combined Model Tab
    } else if(current_tab_fisheries == "combined_model_fisheries") {
      fisheries_config <- get_fisheries_config()
      generate_fisheries_combined_sidebar(fisheries_config, combined_maps_data)
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
  
  # Combined map logic for habitat
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
    
    # Get valid configurations and AOI data
    valid_configs <- natural_resources_valid_configs()
    aoi_data <- filtered_aoi_data()
    
    # Generate maps using cached individual data
    all_results <- list()
    for(method in selected_methods) {

      tryCatch({
        # Use cached individual data
        combined_data <- make_combined_map_from_cached_data(
          valid_configs = valid_configs,
          cached_data = individual_processed_data$naturalresources,
          method = method,
          base_grid = grid_test,
          aoi_data = aoi_data
        )
        
        # Get score columns for verification
        score_cols <- names(combined_data)[grep("^Score\\.", names(combined_data))]
        
        if(length(score_cols) == 0) {
          cat("ERROR: No score columns found for calculation\n")
          next
        }
        
        # Calculate based on method
        if(method == "geometric_mean") {
          combined_data <- calculate_geometric_mean(combined_data)
        } else if(method == "lowest") {
          combined_data <- calculate_lowest_value(combined_data)
        } else if(method == "product") {
          combined_data <- calculate_product_value(combined_data)
        }
        
        # Verify calculation result
        expected_col <- switch(method,
                               "geometric_mean" = "Geo_mean",
                               "lowest" = "Lowest_value", 
                               "product" = "Product_value")
        
        # Create the map
        map_result <- create_combined_map(
          combined_data = combined_data,
          map_title = paste("Offshore Wind Energy Suitability Score <br> for Habitat Component -", 
                            switch (method,
                                    "geometric_mean" = "Geometric Mean", 
                                    "lowest" = "Lowest Value",
                                    "product" = "Product")),
          method = method,
          aoi_data = aoi_data, 
          aoi_bounds = aoi_bounds_cache$current_bounds
        )
        
        # Store the result
        all_results[[method]] <- list(
          combined_data = combined_data,
          map = map_result
        )
        
      }, error = function(e) {
        cat("ERROR in method", method, ":", e$message, "\n")
        showNotification(paste("Error generating", method, "map:", e$message), type = "error")
      })
    } 
    
    if("geometric_mean" %in% selected_methods && "geometric_mean" %in% names(all_results)) {
      local({
        result <- all_results[["geometric_mean"]]
        output$combinedHabitatMap_geo <- renderLeaflet({ result$map })
        combined_maps_data$habitat_geo <- result$combined_data
        combined_maps_data$habitat_geo_map <- result$map  
      })
    }
    
    if("lowest" %in% selected_methods && "lowest" %in% names(all_results)) {
      local({
        result <- all_results[["lowest"]]
        output$combinedHabitatMap_lowest <- renderLeaflet({ result$map })
        combined_maps_data$habitat_lowest <- result$combined_data
        combined_maps_data$habitat_lowest_map <- result$map  
      })
    }
    
    if("product" %in% selected_methods && "product" %in% names(all_results)) {  
      local({
        result <- all_results[["product"]]
        output$combinedHabitatMap_product <- renderLeaflet({ result$map })
        combined_maps_data$habitat_product <- result$combined_data
        combined_maps_data$habitat_product_map <- result$map  
      })
    }
    
    # Set flag to indicate combined map has been generated
    combined_maps_data$habitat_combined_map_generated <- TRUE
    
    # Remove modal spinner
    removeModal()

  }) # END of observeEvent
  
  # Habitat/Natural Resources tab export
  output$habitatExportRmd <- downloadHandler(
    filename = function() {
      paste("Habitat_Component_Natural_Resources_Submodel_Report_", 
            format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".html", sep = "")
    },
    content = function(file) {
      
      # Extract combined data
      combined_data_extracted <- list()
      if(combined_maps_data$habitat_combined_map_generated) {
        if(!is.null(combined_maps_data$habitat_geo)) {
          combined_data_extracted$habitat_geo <- combined_maps_data$habitat_geo
        }
        if(!is.null(combined_maps_data$habitat_lowest)) {
          combined_data_extracted$habitat_lowest <- combined_maps_data$habitat_lowest
        }
        if(!is.null(combined_maps_data$habitat_product)) {
          combined_data_extracted$habitat_product <- combined_maps_data$habitat_product
        }
      }
      
      generate_submodel_component_report(
        component_type = "habitat",
        submodel_type = "natural_resources", 
        valid_configs = natural_resources_valid_configs(),
        individual_processed_data = individual_processed_data$naturalresources, 
        combined_data_extracted = combined_data_extracted,
        combined_maps_data = combined_maps_data,
        input = input,
        filtered_aoi_data = filtered_aoi_data,
        file = file
      )
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
    
    # Get valid configurations
    valid_configs <- natural_resources_valid_configs()
    aoi_data <- filtered_aoi_data()
    
    # Generate maps using cached individual data
    all_results <- list()
    for(method in selected_methods) {
      
      tryCatch({
        # Use cached individual data
        combined_data <- make_combined_map_from_cached_data(
          valid_configs = valid_configs,
          cached_data = individual_processed_data$naturalresources,
          method = method,
          base_grid = grid_test,
          aoi_data = aoi_data
        )
        
        # Get score columns for verification
        score_cols <- names(combined_data)[grep("^Score\\.", names(combined_data))]
        
        if(length(score_cols) == 0) {
          cat("ERROR: No score columns found for calculation\n")
          next
        }
        
        # Calculate based on method
        if(method == "geometric_mean") {
          combined_data <- calculate_geometric_mean(combined_data)
        } else if(method == "lowest") {
          combined_data <- calculate_lowest_value(combined_data)
        } else if(method == "product") {
          combined_data <- calculate_product_value(combined_data)
        }
        
        # Verify calculation result
        expected_col <- switch(method,
                               "geometric_mean" = "Geo_mean",
                               "lowest" = "Lowest_value", 
                               "product" = "Product_value")
        
        # Create the map
        map_result <- create_combined_map(
          combined_data = combined_data,
          map_title = paste("Offshore Wind Energy Suitability Score <br> for Species Component -", 
                            switch (method,
                                    "geometric_mean" = "Geometric Mean", 
                                    "lowest" = "Lowest Value",
                                    "product" = "Product")),
          method = method,
          aoi_data = aoi_data,
          aoi_bounds = aoi_bounds_cache$current_bounds
        )
        
        # Store the result
        all_results[[method]] <- list(
          combined_data = combined_data,
          map = map_result
        )
        
      }, error = function(e) {
        cat("ERROR in method", method, ":", e$message, "\n")
        showNotification(paste("Error generating", method, "map:", e$message), type = "error")
      })
    } 
    
    if("geometric_mean" %in% selected_methods && "geometric_mean" %in% names(all_results)) {
      local({
        result <- all_results[["geometric_mean"]]
        output$combinedSpeciesMap_geo <- renderLeaflet({ result$map })
        combined_maps_data$species_geo <- result$combined_data
        combined_maps_data$species_geo_map <- result$map 
      })
    }
    
    if("lowest" %in% selected_methods && "lowest" %in% names(all_results)) {
      local({
        result <- all_results[["lowest"]]
        output$combinedSpeciesMap_lowest <- renderLeaflet({ result$map })
        combined_maps_data$species_lowest <- result$combined_data
        combined_maps_data$species_lowest_map <- result$map 
      })
    }
    
    if("product" %in% selected_methods && "product" %in% names(all_results)) {  
      local({
        result <- all_results[["product"]]
        output$combinedSpeciesMap_product <- renderLeaflet({ result$map })
        combined_maps_data$species_product <- result$combined_data
        combined_maps_data$species_product_map <- result$map
      })
    }
    
    # Set flag to indicate combined map has been generated
    combined_maps_data$species_combined_map_generated <- TRUE
    
    # Remove modal spinner
    removeModal()
    
  }) # END of observeEvent

  # Species/Natural Resources tab export
  output$speciesExportRmd <- downloadHandler(
    filename = function() {
      paste("Species_Component_Natural_Resources_Submodel_Report_", 
            format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".html", sep = "")
    },
    content = function(file) {
      # Extract combined data
      combined_data_extracted <- list()
      if(combined_maps_data$species_combined_map_generated) {
        if(!is.null(combined_maps_data$species_geo)) {
          combined_data_extracted$species_geo <- combined_maps_data$species_geo
        }
        if(!is.null(combined_maps_data$species_lowest)) {
          combined_data_extracted$species_lowest <- combined_maps_data$species_lowest
        }
        if(!is.null(combined_maps_data$habitat_product)) {
          combined_data_extracted$species_product <- combined_maps_data$species_product
        }
      }
      
      generate_submodel_component_report(
        component_type = "species",
        submodel_type = "natural_resources", 
        valid_configs = natural_resources_valid_configs(),
        individual_processed_data = individual_processed_data$naturalresources, 
        combined_data_extracted = combined_data_extracted,
        combined_maps_data = combined_maps_data,
        input = input,
        filtered_aoi_data = filtered_aoi_data,
        file = file
      )
    }
  )
  
  # Multiple maps container for fisheries
  output$multipleMapsContainer_fisheries <- renderUI({
    valid_configs <- fisheries_valid_configs()
    selected_methods <- input$fisheriesCalculationMethods %||% character(0)
    
    create_maps_container(
      configs = valid_configs,
      namespace = "fisheries",
      combined_map_output_id = "combinedFisheriesMap",
      combined_map_generated = combined_maps_data$fisheries_combined_map_generated,
      combined_map_title = "Combined Map Result",
      selected_methods = selected_methods
    )
  })
  
  # Combined map logic
  observeEvent(input$generateCombinedFisheriesMap, {
    
    # Get selected calculation methods
    selected_methods <- input$fisheriesCalculationMethods
    
    if(is.null(selected_methods) || length(selected_methods) == 0) {
      showNotification("Please select at least one calculation method.", type = "warning")
      return()
    }
    
    # Show modal with spinner
    show_spinner_modal("Generating Combined Map(s)", 
                       paste("Please wait while", length(selected_methods), "combined map(s) are being generated..."))
    
    # Add a small delay to ensure the modal is visible
    Sys.sleep(0.5)
    
    # Get valid configurations
    valid_configs <- fisheries_valid_configs()
    aoi_data <- filtered_aoi_data()
    
    # Generate maps using cached individual data
    all_results <- list()
    for(method in selected_methods) {
      
      tryCatch({
        # Use cached individual data
        combined_data <- make_combined_map_from_cached_data(
          valid_configs = valid_configs,
          cached_data = individual_processed_data$fisheries,
          method = method,
          base_grid = grid_test,
          aoi_data = aoi_data
        )
        
        # Get score columns for verification
        score_cols <- names(combined_data)[grep("^Score\\.", names(combined_data))]
        
        if(length(score_cols) == 0) {
          cat("ERROR: No score columns found for calculation\n")
          next
        }
        
        # Calculate based on method
        if(method == "geometric_mean") {
          combined_data <- calculate_geometric_mean(combined_data)
        } else if(method == "lowest") {
          combined_data <- calculate_lowest_value(combined_data)
        } else if(method == "product") {
          combined_data <- calculate_product_value(combined_data)
        }
        
        # Verify calculation result
        expected_col <- switch(method,
                               "geometric_mean" = "Geo_mean",
                               "lowest" = "Lowest_value", 
                               "product" = "Product_value")
        
        # Create the map
        map_result <- create_combined_map(
          combined_data = combined_data,
          map_title = paste("Offshore Wind Energy Suitability Score <br> for Fisheries Component -", 
                            switch (method,
                                    "geometric_mean" = "Geometric Mean", 
                                    "lowest" = "Lowest Value",
                                    "product" = "Product")),
          method = method,
          aoi_data = aoi_data, 
          aoi_bounds = aoi_bounds_cache$current_bounds
        )
        
        # Store the result
        all_results[[method]] <- list(
          combined_data = combined_data,
          map = map_result
        )
        
      }, error = function(e) {
        cat("ERROR in method", method, ":", e$message, "\n")
        showNotification(paste("Error generating", method, "map:", e$message), type = "error")
      })
    } 
    # Store results - but only for methods that were actually selected
    if("geometric_mean" %in% selected_methods && "geometric_mean" %in% names(all_results)) {
      local({
        result <- all_results[["geometric_mean"]]
        output$combinedFisheriesMap_geo <- renderLeaflet({ result$map })
        combined_maps_data$fisheries_geo <- result$combined_data
        combined_maps_data$fisheries_geo_map <- result$map
      })
    }
    
    if("lowest" %in% selected_methods && "lowest" %in% names(all_results)) {
      local({
        result <- all_results[["lowest"]]
        output$combinedFisheriesMap_lowest <- renderLeaflet({ result$map })
        combined_maps_data$fisheries_lowest <- result$combined_data
        combined_maps_data$fisheries_lowest_map <- result$map
      })
    }
    
    if("product" %in% selected_methods && "product" %in% names(all_results)) {  
      local({
        result <- all_results[["product"]]
        output$combinedFisheriesMap_product <- renderLeaflet({ result$map })
        combined_maps_data$fisheries_product <- result$combined_data
        combined_maps_data$fisheries_product_map <- result$map
      })
    }
    
    # Set flag to indicate combined map has been generated
    combined_maps_data$fisheries_combined_map_generated <- TRUE
    
    # Remove modal spinner
    removeModal()
    
  }) # END of observeEvent

  # Fisheries/Fisheries tab export
  output$fisheriesExportRmd <- downloadHandler(
    filename = function() {
      paste("Fisheries_Component_Fisheries_Submodel_Report_", 
            format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".html", sep = "")
    },
    content = function(file) {
      # Extract combined data
      combined_data_extracted <- list()
      if(combined_maps_data$fisheries_combined_map_generated) {
        if(!is.null(combined_maps_data$fisheries_geo)) {
          combined_data_extracted$fisheries_geo <- combined_maps_data$fisheries_geo
        }
        if(!is.null(combined_maps_data$fisheries_lowest)) {
          combined_data_extracted$fisheries_lowest <- combined_maps_data$fisheries_lowest
        }
        if(!is.null(combined_maps_data$fisheries_product)) {
          combined_data_extracted$fisheries_product <- combined_maps_data$fisheries_product
        }
      }
      
      generate_submodel_component_report(
        component_type = "fisheries",
        submodel_type = "fisheries", 
        valid_configs = fisheries_valid_configs(),
        individual_processed_data = individual_processed_data$fisheries, 
        combined_data_extracted = combined_data_extracted,
        combined_maps_data = combined_maps_data,
        input = input,
        filtered_aoi_data = filtered_aoi_data,
        file = file
      )
    }
  )
  
  # Multiple maps container for trawl fisheries
  output$multipleMapsContainer_trawl <- renderUI({
    valid_configs <- fisheries_valid_configs()
    selected_methods <- input$trawlCalculationMethods %||% character(0)
    
    create_maps_container(
      configs = valid_configs,
      namespace = "fisheries",
      combined_map_output_id = "combinedTrawlMap",
      combined_map_generated = combined_maps_data$trawl_combined_map_generated,
      combined_map_title = "Combined Map Result",
      selected_methods = selected_methods
    )
  })
  
  # Trawl fisheries maps
  observeEvent(input$generateCombinedTrawlMap, {
    
    # Get selected calculation methods
    selected_methods <- input$trawlCalculationMethods
    
    if(is.null(selected_methods) || length(selected_methods) == 0) {
      showNotification("Please select at least one calculation method.", type = "warning")
      return()
    }
    
    # Show modal with spinner
    show_spinner_modal("Generating Combined Map(s)", 
                       paste("Please wait while", length(selected_methods), "combined map(s) are being generated..."))
    
    # Add a small delay to ensure the modal is visible
    Sys.sleep(0.5)
    
    # Get valid configurations
    valid_configs <- fisheries_valid_configs()
    aoi_data <- filtered_aoi_data()
    
    # Generate maps using cached individual data
    all_results <- list()
    for(method in selected_methods) {
      
      tryCatch({
        # Use cached individual data
        combined_data <- make_combined_map_from_cached_data(
          valid_configs = valid_configs,
          cached_data = individual_processed_data$fisheries,
          method = method,
          base_grid = grid_test,
          aoi_data = aoi_data
        )
        
        # Get score columns for verification
        score_cols <- names(combined_data)[grep("^Score\\.", names(combined_data))]
        
        if(length(score_cols) == 0) {
          cat("ERROR: No score columns found for calculation\n")
          next
        }
        
        # Calculate based on method
        if(method == "geometric_mean") {
          combined_data <- calculate_geometric_mean(combined_data)
        } else if(method == "lowest") {
          combined_data <- calculate_lowest_value(combined_data)
        } else if(method == "product") {
          combined_data <- calculate_product_value(combined_data)
        }
        
        # Verify calculation result
        expected_col <- switch(method,
                               "geometric_mean" = "Geo_mean",
                               "lowest" = "Lowest_value", 
                               "product" = "Product_value")
        
        # Create the map
        map_result <- create_combined_map(
          combined_data = combined_data,
          map_title = paste("Offshore Wind Energy Suitability Score <br> for Trawl Fisheries Component -", 
                            switch (method,
                                    "geometric_mean" = "Geometric Mean", 
                                    "lowest" = "Lowest Value",
                                    "product" = "Product")),
          method = method,
          aoi_data = aoi_data, 
          aoi_bounds = aoi_bounds_cache$current_bounds
        )
        
        # Store the result
        all_results[[method]] <- list(
          combined_data = combined_data,
          map = map_result
        )
        
      }, error = function(e) {
        cat("ERROR in method", method, ":", e$message, "\n")
        showNotification(paste("Error generating", method, "map:", e$message), type = "error")
      })
    } 
    # Store results - but only for methods that were actually selected
    if("geometric_mean" %in% selected_methods && "geometric_mean" %in% names(all_results)) {
      local({
        result <- all_results[["geometric_mean"]]
        output$combinedTrawlMap_geo <- renderLeaflet({ result$map })
        combined_maps_data$trawl_geo <- result$combined_data
        combined_maps_data$trawl_geo_map <- result$map  
      })
    }
    
    if("lowest" %in% selected_methods && "lowest" %in% names(all_results)) {
      local({
        result <- all_results[["lowest"]]
        output$combinedTrawlMap_lowest <- renderLeaflet({ result$map })
        combined_maps_data$trawl_lowest <- result$combined_data
        combined_maps_data$trawl_lowest_map <- result$map 
      })
    }
    
    if("product" %in% selected_methods && "product" %in% names(all_results)) {  
      local({
        result <- all_results[["product"]]
        output$combinedTrawlMap_product <- renderLeaflet({ result$map })
        combined_maps_data$trawl_product <- result$combined_data
        combined_maps_data$trawl_product_map <- result$map
      })
    }
    
    # Set flag to indicate combined map has been generated
    combined_maps_data$trawl_combined_map_generated <- TRUE
    
    # Remove modal spinner
    removeModal()
    
  }) # END of observeEvent
  
  # Trawl/Fisheries tab export
  output$trawlExportRmd <- downloadHandler(
    filename = function() {
      paste("Trawl_Component_Fisheries_Submodel_Report_", 
            format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".html", sep = "")
    },
    content = function(file) {
      # Extract combined data
      combined_data_extracted <- list()
      if(combined_maps_data$trawl_combined_map_generated) {
        if(!is.null(combined_maps_data$trawl_geo)) {
          combined_data_extracted$trawl_geo <- combined_maps_data$trawl_geo
        }
        if(!is.null(combined_maps_data$trawl_lowest)) {
          combined_data_extracted$trawl_lowest <- combined_maps_data$trawl_lowest
        }
        if(!is.null(combined_maps_data$trawl_product)) {
          combined_data_extracted$trawl_product <- combined_maps_data$trawl_product
        }
      }
      
      generate_submodel_component_report(
        component_type = "trawl",
        submodel_type = "fisheries", 
        valid_configs = fisheries_valid_configs(),
        individual_processed_data = individual_processed_data$fisheries, 
        combined_data_extracted = combined_data_extracted,
        combined_maps_data = combined_maps_data,
        input = input,
        filtered_aoi_data = filtered_aoi_data,
        file = file
      )
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
  
  
  # Surveys maps
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
    
    # Get valid configurations
    valid_configs <- industry_operations_valid_configs()
    aoi_data <- filtered_aoi_data()
    
    # Generate maps using cached individual data
    all_results <- list()
    for(method in selected_methods) {
      
      tryCatch({
        # Use cached individual data
        combined_data <- make_combined_map_from_cached_data(
          valid_configs = valid_configs,
          cached_data = individual_processed_data$industryoperations,
          method = method,
          base_grid = grid_test,
          aoi_data = aoi_data
        )
        
        # Get score columns for verification
        score_cols <- names(combined_data)[grep("^Score\\.", names(combined_data))]
        
        if(length(score_cols) == 0) {
          cat("ERROR: No score columns found for calculation\n")
          next
        }
        
        # Calculate based on method
        if(method == "geometric_mean") {
          combined_data <- calculate_geometric_mean(combined_data)
        } else if(method == "lowest") {
          combined_data <- calculate_lowest_value(combined_data)
        } else if(method == "product") {
          combined_data <- calculate_product_value(combined_data)
        }
        
        # Verify calculation result
        expected_col <- switch(method,
                               "geometric_mean" = "Geo_mean",
                               "lowest" = "Lowest_value", 
                               "product" = "Product_value")
        
        # Create the map
        map_result <- create_combined_map(
          combined_data = combined_data,
          map_title = paste("Offshore Wind Energy Suitability Score <br> for Surveys Component -", 
                            switch (method,
                                    "geometric_mean" = "Geometric Mean", 
                                    "lowest" = "Lowest Value",
                                    "product" = "Product")),
          method = method,
          aoi_data = aoi_data, 
          aoi_bounds = aoi_bounds_cache$current_bounds
        )
        
        # Store the result
        all_results[[method]] <- list(
          combined_data = combined_data,
          map = map_result
        )
        
      }, error = function(e) {
        cat("ERROR in method", method, ":", e$message, "\n")
        showNotification(paste("Error generating", method, "map:", e$message), type = "error")
      })
    } 
    # Store results - but only for methods that were actually selected
    if("geometric_mean" %in% selected_methods && "geometric_mean" %in% names(all_results)) {
      local({
        result <- all_results[["geometric_mean"]]
        output$combinedSurveysMap_geo <- renderLeaflet({ result$map })
        combined_maps_data$surveys_geo <- result$combined_data
        combined_maps_data$surveys_geo_map <- result$map
      })
    }
    
    if("lowest" %in% selected_methods && "lowest" %in% names(all_results)) {
      local({
        result <- all_results[["lowest"]]
        output$combinedSurveysMap_lowest <- renderLeaflet({ result$map })
        combined_maps_data$surveys_lowest <- result$combined_data
        combined_maps_data$surveys_lowest_map <- result$map 
      })
    }
    
    if("product" %in% selected_methods && "product" %in% names(all_results)) {  
      local({
        result <- all_results[["product"]]
        output$combinedSurveysMap_product <- renderLeaflet({ result$map })
        combined_maps_data$surveys_product <- result$combined_data
        combined_maps_data$surveys_product_map <- result$map 
      })
    }
    
    # Set flag to indicate combined map has been generated
    combined_maps_data$surveys_combined_map_generated <- TRUE
    
    # Remove modal spinner
    removeModal()
    
  }) # END of observeEvent
  
  # Surveys/Industry operations tab export
  output$surveysExportRmd <- downloadHandler(
    filename = function() {
      paste("Surveys_Component_Industry_Operations_Submodel_Report_", 
            format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".html", sep = "")
    },
    content = function(file) {
      # Extract combined data
      combined_data_extracted <- list()
      if(combined_maps_data$surveys_combined_map_generated) {
        if(!is.null(combined_maps_data$surveys_geo)) {
          combined_data_extracted$surveys_geo <- combined_maps_data$surveys_geo
        }
        if(!is.null(combined_maps_data$surveys_lowest)) {
          combined_data_extracted$surveys_lowest <- combined_maps_data$surveys_lowest
        }
        if(!is.null(combined_maps_data$surveys_product)) {
          combined_data_extracted$surveys_product <- combined_maps_data$surveys_product
        }
      }
      
      generate_submodel_component_report(
        component_type = "surveys",
        submodel_type = "industry_operations", 
        valid_configs = industry_operations_valid_configs(),
        individual_processed_data = individual_processed_data$industryoperations, 
        combined_data_extracted = combined_data_extracted,
        combined_maps_data = combined_maps_data,
        input = input,
        filtered_aoi_data = filtered_aoi_data,
        file = file
      )
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
  
  # Combined map logic for habitat
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
    
    # Get valid configurations and AOI data
    valid_configs <- industry_operations_valid_configs()
    aoi_data <- filtered_aoi_data()
    
    # Generate maps using cached individual data
    all_results <- list()
    for(method in selected_methods) {
      
      tryCatch({
        # Use cached individual data
        combined_data <- make_combined_map_from_cached_data(
          valid_configs = valid_configs,
          cached_data = individual_processed_data$industryoperations,
          method = method,
          base_grid = grid_test,
          aoi_data = aoi_data
        )
        
        # Get score columns for verification
        score_cols <- names(combined_data)[grep("^Score\\.", names(combined_data))]
        
        if(length(score_cols) == 0) {
          cat("ERROR: No score columns found for calculation\n")
          next
        }
        
        # Calculate based on method
        if(method == "geometric_mean") {
          combined_data <- calculate_geometric_mean(combined_data)
        } else if(method == "lowest") {
          combined_data <- calculate_lowest_value(combined_data)
        } else if(method == "product") {
          combined_data <- calculate_product_value(combined_data)
        }
        
        # Verify calculation result
        expected_col <- switch(method,
                               "geometric_mean" = "Geo_mean",
                               "lowest" = "Lowest_value", 
                               "product" = "Product_value")
        
        # Create the map
        map_result <- create_combined_map(
          combined_data = combined_data,
          map_title = paste("Offshore Wind Energy Suitability Score <br> for Cables Component -", 
                            switch (method,
                                    "geometric_mean" = "Geometric Mean", 
                                    "lowest" = "Lowest Value",
                                    "product" = "Product")),
          method = method,
          aoi_data = aoi_data,
          aoi_bounds = aoi_bounds_cache$current_bounds
        )
        
        # Store the result
        all_results[[method]] <- list(
          combined_data = combined_data,
          map = map_result
        )
        
      }, error = function(e) {
        cat("ERROR in method", method, ":", e$message, "\n")
        showNotification(paste("Error generating", method, "map:", e$message), type = "error")
      })
    } 
    
    # Store results - but only for methods that were actually selected
    if("geometric_mean" %in% selected_methods && "geometric_mean" %in% names(all_results)) {
      local({
        result <- all_results[["geometric_mean"]]
        output$combinedCablesMap_geo <- renderLeaflet({ result$map })
        combined_maps_data$cables_geo <- result$combined_data
        combined_maps_data$cables_geo_map <- result$map  
      })
    }
    
    if("lowest" %in% selected_methods && "lowest" %in% names(all_results)) {
      local({
        result <- all_results[["lowest"]]
        output$combinedCablesMap_lowest <- renderLeaflet({ result$map })
        combined_maps_data$cables_lowest <- result$combined_data
        combined_maps_data$cables_lowest_map <- result$map 
      })
    }
    
    if("product" %in% selected_methods && "product" %in% names(all_results)) {  
      local({
        result <- all_results[["product"]]
        output$combinedCablesMap_product <- renderLeaflet({ result$map })
        combined_maps_data$cables_product <- result$combined_data
        combined_maps_data$cables_product_map <- result$map  
      })
    }
    
    # Set flag to indicate combined map has been generated
    combined_maps_data$cables_combined_map_generated <- TRUE
    
    # Remove modal spinner
    removeModal()
    
  }) # END of observeEvent
  
  # Cables/Industry Operations tab export
  output$cablesExportRmd <- downloadHandler(
    filename = function() {
      paste("Cables_Component_Industry_Operations_Submodel_Report_", 
            format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".html", sep = "")
    },
    content = function(file) {
      # Extract combined data
      combined_data_extracted <- list()
      if(combined_maps_data$cables_combined_map_generated) {
        if(!is.null(combined_maps_data$cables_geo)) {
          combined_data_extracted$cables_geo <- combined_maps_data$cables_geo
        }
        if(!is.null(combined_maps_data$cables_lowest)) {
          combined_data_extracted$cables_lowest <- combined_maps_data$cables_lowest
        }
        if(!is.null(combined_maps_data$cables_product)) {
          combined_data_extracted$cables_product <- combined_maps_data$cables_product
        }
      }
      
      generate_submodel_component_report(
        component_type = "cables",
        submodel_type = "industry_operations", 
        valid_configs = industry_operations_valid_configs(),
        individual_processed_data = individual_processed_data$industryoperations, 
        combined_data_extracted = combined_data_extracted,
        combined_maps_data = combined_maps_data,
        input = input,
        filtered_aoi_data = filtered_aoi_data,
        file = file
      )
    }
  )
  
  # Natural Resources submodel status
  output$combinedModelStatus_natural_resources <- renderUI({
    check_submodel_status("natural_resources", combined_maps_data)
  })
  
  # Fisheries submodel status
  output$combinedModelStatus_fisheries <- renderUI({
    check_submodel_status("fisheries", combined_maps_data)
  })
  
  # Industry & Operations submodel status  
  output$combinedModelStatus_industry_operations <- renderUI({
    check_submodel_status("industry_operations", combined_maps_data)
  })

  
  # Dynamic sidebar content for full model tab
  output$dynamicSidebar_full_model <- renderUI({
    generate_full_model_sidebar()
  })
  
  # submodel status
  submodel_status <- reactive({
    list(
      natural_resources = list(
        available = combined_maps_data$habitat_combined_map_generated || 
          combined_maps_data$species_combined_map_generated,
        components = list(
          habitat = combined_maps_data$habitat_combined_map_generated,
          species = combined_maps_data$species_combined_map_generated
        )
      ),
      fisheries = list(
        available = combined_maps_data$fisheries_combined_map_generated || 
          combined_maps_data$trawl_combined_map_generated,
        components = list(
          fisheries = combined_maps_data$fisheries_combined_map_generated,
          trawl = combined_maps_data$trawl_combined_map_generated
        )
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
      updateCheckboxInput(session, "enableFisheries", value = TRUE)  
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
  output$fullModelSubmodelStatus <- renderUI({
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
      select(dataset_name, description, data_type, formatted_date) %>%
      rename(
        "Dataset" = dataset_name,
        "Description" = description,
        "Data Type" = data_type,
        "Last Updated" = formatted_date
      )
  })
  
  # Add this output to handle validation messages
  output$naturalResourcesCombinedValidation <- renderUI({
    # Get component selections
    include_habitat <- input$includeHabitat %||% FALSE
    include_species <- input$includeSpecies %||% FALSE
    
    # Check if any components are selected
    any_selected <- include_habitat || include_species
    
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
      
      if(length(selected_components) == 0) {
        div(class = "alert alert-danger", 
            "Selected components do not have combined maps generated. Please generate component maps first.")
      } else {
        div(class = "alert alert-success", 
            paste("✓ Ready to generate combined submodel using:", paste(selected_components, collapse = ", ")))
      }
    }
  })
  
  # Natural Resources Combined Submodel Export 
  observeEvent(input$generateNaturalResourcesCombinedSubmodel, {
    # Add error handling wrapper
    tryCatch({
      # Get component selections
      include_habitat <- isTRUE(input$includeHabitat)
      include_species <- isTRUE(input$includeSpecies)
      
      # Validate selections
      if(!include_habitat && !include_species) {
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
        
        combined_submodel_result <- create_combined_submodel_map(component_data_list,
                                                             base_grid = grid_test,
                                                             aoi_data_reactive = filtered_aoi_data, 
                                                             aoi_bounds = aoi_bounds_cache$current_bounds)
        
        # Store the result
        combined_maps_data$natural_resources_combined_submodel <- combined_submodel_result$combined_data
        combined_maps_data$natural_resources_combined_submodel_generated <- TRUE
        
        # Store the map object for rendering
        combined_maps_data$natural_resources_combined_map <- combined_submodel_result$map
        
        # Generate and store the cropped map
        if(!is.null(combined_submodel_result$combined_data)) {

        # Generate and store the normalized cropped map
        normalized_cropped_map <- create_aoi_cropped_normalized_map(
          combined_data = combined_submodel_result$combined_data,
          aoi_data_reactive = filtered_aoi_data,
          map_title = "Natural Resources AOI-Cropped Normalized", 
          aoi_bounds = aoi_bounds_cache$current_bounds
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
        
        # Normalized cropped map section
        div(
          h4("Normalized Natural Resources Submodel Map"),
          p("This map shows the data normalized to a 0-1 scale for easier comparison across different areas. This map is intended to provide additional visual clarity for separating score values that are close together by showing them with a different color palette and relative scale. The scores showed in this map are **not** used in any further calculations, and are purely for visual aid."),
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
  
  # Natural Resources combined export
  output$naturalResourcesCombinedExport <- downloadHandler(
    filename = function() {
      paste("Natural_Resources_Combined_Submodel_Report_", format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".html", sep = "")
    },
    content = function(file) {
      generate_submodel_combined_report(
        submodel_type = "natural_resources",
        input = input,
        combined_maps_data = combined_maps_data,
        filtered_aoi_data = filtered_aoi_data,
        data_timestamps = data_timestamps,
        file = file
      )
    }
  )
  
  # Add this output to handle validation messages
  output$fisheriesCombinedValidation <- renderUI({
    # Get component selections
    include_fisheries <- input$includeFisheries %||% FALSE
    include_trawl <- input$includeTrawl %||% FALSE
    
    # Check if any components are selected
    any_selected <- include_fisheries || include_trawl 
    
    if(!any_selected) {
      div(class = "alert alert-warning", 
          "Please select at least one component to generate the combined submodel.")
    } else {
      # Check if selected components have valid data
      selected_components <- c()
      if(include_fisheries && combined_maps_data$fisheries_combined_map_generated) {
        selected_components <- c(selected_components, "Fisheries")
      }
      if(include_trawl && combined_maps_data$trawl_combined_map_generated) {
        selected_components <- c(selected_components, "Trawl")
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
  observeEvent(input$generateFisheriesCombinedSubmodel, {
    # Add error handling wrapper
    tryCatch({
      # Get component selections
      include_fisheries <- isTRUE(input$includeFisheries)
      include_trawl <- isTRUE(input$includeTrawl)
      
      # Validate selections
      if(!include_fisheries && !include_trawl) {
        showNotification("Please select at least one component.", type = "warning")
        return()
      }
      
      # Show spinner modal
      show_spinner_modal("Generating Combined Fisheries Submodel", 
                         "Please wait while the combined submodel is being calculated...")
      
      # Collect component data based on user selections
      component_data_list <- list()
      
      if(include_fisheries && combined_maps_data$fisheries_combined_map_generated) {
        method <- input$fisheriesCalculationMethod %||% "geometric_mean"
        
        fisheries_data <- switch(method,
                               "geometric_mean" = combined_maps_data$fisheries_geo,
                               "lowest" = combined_maps_data$fisheries_lowest,
                               "product" = combined_maps_data$fisheries_product,
                               combined_maps_data$fisheries_geo)  # fallback
        
        if(!is.null(fisheries_data)) {
          component_data_list[["fisheries"]] <- fisheries_data
        }
      }
      
      if(include_trawl && combined_maps_data$trawl_combined_map_generated) {
        method <- input$trawlCalculationMethod %||% "geometric_mean"
        
        trawl_data <- switch(method,
                              "geometric_mean" = combined_maps_data$trawl_geo,
                              "lowest" = combined_maps_data$trawl_lowest,
                              "product" = combined_maps_data$trawl_product,
                              combined_maps_data$trawl_geo)  # fallback
        
        if(!is.null(trawl_data)) {
          component_data_list[["trawl"]] <- trawl_data
        }
      }
      
      # Generate the combined submodel using geometric mean
      if(length(component_data_list) > 0) {
        
        combined_submodel_result <- create_combined_submodel_map(component_data_list, 
                                                                 base_grid = grid_test, 
                                                                 aoi_data_reactive = filtered_aoi_data,
                                                                 submodel_type = "fisheries", 
                                                                 aoi_bounds = aoi_bounds_cache$current_bounds)
        
        # Store the result
        combined_maps_data$fisheries_combined_submodel <- combined_submodel_result$combined_data
        combined_maps_data$fisheries_combined_submodel_generated <- TRUE
        
        # Store the map object for rendering
        combined_maps_data$fisheries_combined_map <- combined_submodel_result$map
        
        # Generate and store the cropped map
        if(!is.null(combined_submodel_result$combined_data)) {
        
          # Generate and store the normalized cropped map
          normalized_cropped_map <- create_aoi_cropped_normalized_map(
            combined_data = combined_submodel_result$combined_data,
            aoi_data_reactive = filtered_aoi_data,
            map_title = "Fisheries AOI-Cropped Normalized", 
            aoi_bounds = aoi_bounds_cache$current_bounds
          )
          combined_maps_data$fisheries_combined_map_cropped_normalized <- normalized_cropped_map
        }
        
        showNotification("Combined Fisheries Submodel generated successfully!", type = "message")
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
  
  # Fisheries 
  output$fisheriesCombinedMap <- renderLeaflet({
    # Check if the map is available
    if(!is.null(combined_maps_data$fisheries_combined_map)) {
      combined_maps_data$fisheries_combined_map
    } else {
      # Return a placeholder map
      leaflet() %>%
        addProviderTiles("Esri.OceanBasemap") %>%
        addControl("Generate combined submodel to see map", position = "center")
    }
  })
  
  # Fisheries normalized cropped map output
  output$fisheriesCombinedMapCroppedNormalized <- renderLeaflet({
    if(!is.null(combined_maps_data$fisheries_combined_map_cropped_normalized)) {
      combined_maps_data$fisheries_combined_map_cropped_normalized
    } else {
      leaflet() %>%
        addProviderTiles("Esri.OceanBasemap") %>%
        addControl("Generate combined submodel and select a AOI to see normalized cropped map", position = "center")
    }
  })
  
  # Render the map container content
  output$fisheriesCombinedMapContainer <- renderUI({
    
    if(combined_maps_data$fisheries_combined_submodel_generated) {
      tagList(
        # Main combined map section
        div(
          h4("Combined Fisheries Submodel Map"),
          p("This map shows the combined Fisheries submodel calculated using the geometric mean of selected components."),
          leafletOutput("fisheriesCombinedMap", height = "500px")
        ),
        
        br(),
        
        # Normalized cropped map section
        div(
          h4("Normalized Fisheries Submodel Map"),
          p("This map shows the data normalized to a 0-1 scale for easier comparison across different areas. This map is intended to provide additional visual clarity for separating score values that are close together by showing them with a different color palette and relative scale. The scores showed in this map are **not** used in any further calculations, and are purely for visual aid."),
          leafletOutput("fisheriesCombinedMapCroppedNormalized", height = "500px")
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
  
  # Fisheries combined export
  output$fisheriesCombinedExport <- downloadHandler(
    filename = function() {
      paste("Fisheries_Combined_Submodel_Report_", format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".html", sep = "")
    },
    content = function(file) {
      generate_submodel_combined_report(
        submodel_type = "fisheries",
        input = input,
        combined_maps_data = combined_maps_data,
        filtered_aoi_data = filtered_aoi_data,
        data_timestamps = data_timestamps,
        file = file
      )
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
  
  # Add this observeEvent for the generate button for Industry Operations
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
      
        combined_submodel_result <- create_combined_submodel_map(component_data_list, 
                                                             base_grid = grid_test, 
                                                             aoi_data_reactive = filtered_aoi_data, 
                                                             aoi_bounds = aoi_bounds_cache$current_bounds)
        
        # Store the result
        combined_maps_data$industry_operations_combined_submodel <- combined_submodel_result$combined_data
        combined_maps_data$industry_operations_combined_submodel_generated <- TRUE
        
        # Store the map object for rendering
        combined_maps_data$industry_operations_combined_map <- combined_submodel_result$map
        
        # Generate and store the cropped map
        if(!is.null(combined_submodel_result$combined_data)) {
       
          # Generate and store the normalized cropped map
          normalized_cropped_map <- create_aoi_cropped_normalized_map(
            combined_data = combined_submodel_result$combined_data,
            aoi_data_reactive = filtered_aoi_data,
            map_title = "Industry Operations AOI-Cropped Normalized",
            aoi_bounds = aoi_bounds_cache$current_bounds
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
        
        # Normalized cropped map section
        div(
          h4("Normalized Industry & Operations Submodel Map"),
          p("This map shows the data normalized to a 0-1 scale for easier comparison across different areas. This map is intended to provide additional visual clarity for separating score values that are close together by showing them with a different color palette and relative scale. The scores showed in this map are **not** used in any further calculations, and are purely for visual aid."),
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
  
  # Industry & Operations combined export
  output$industryOperationsCombinedExport <- downloadHandler(
    filename = function() {
      paste("Industry_Operations_Combined_Submodel_Report_", format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".html", sep = "")
    },
    content = function(file) {
      generate_submodel_combined_report(
        submodel_type = "industry_operations",
        input = input,
        combined_maps_data = combined_maps_data,
        filtered_aoi_data = filtered_aoi_data,
        data_timestamps = data_timestamps,
        file = file
      )
    }
  )
  
  # Weight validation for full model 
  output$fullWeightValidation <- renderUI({
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
    } else {
      div(class = "alert alert-success", 
          paste("✓ Total weight:", round(total_weight, 3), "- Ready to generate full model"))
    }
  })
  
  # Generate full Model Button Logic
  observeEvent(input$generateFullModel, {
    tryCatch({
    
      # Get weight values
      natural_resources_weight <- input$weightNaturalResources %||% 0
      fisheries_weight <- input$weightFisheries %||% 0
      industry_weight <- input$weightIndustryOperations %||% 0
      
      # Get which submodels are enabled
      nr_enabled <- input$enableNaturalResources %||% FALSE
      fisheries_enabled <- input$enableFisheries %||% FALSE
      industry_enabled <- input$enableIndustryOperations %||% FALSE
      
      # Validate that at least one submodel is enabled and has data
      enabled_submodels <- c()
      enabled_weights <- c()
      
      if(nr_enabled && !is.null(combined_maps_data$natural_resources_combined_submodel)) {
        enabled_submodels <- c(enabled_submodels, "natural_resources")
        enabled_weights <- c(enabled_weights, natural_resources_weight)
        cat("✓ Natural Resources added to enabled submodels\n")
      } else {
        cat("✗ Natural Resources NOT added - enabled:", nr_enabled, "data exists:", !is.null(combined_maps_data$natural_resources_combined_submodel), "\n")
      }
      
      if(fisheries_enabled && !is.null(combined_maps_data$fisheries_combined_submodel)) {
        enabled_submodels <- c(enabled_submodels, "fisheries")
        enabled_weights <- c(enabled_weights, fisheries_weight)
        cat("✓ Fisheries added to enabled submodels\n")
      } else {
        cat("✗ Fisheries NOT added - enabled:", fisheries_enabled, "data exists:", !is.null(combined_maps_data$fisheries_combined_submodel), "\n")
      }
      
      if(industry_enabled && !is.null(combined_maps_data$industry_operations_combined_submodel)) {
        enabled_submodels <- c(enabled_submodels, "industry_operations")
        enabled_weights <- c(enabled_weights, industry_weight)
        cat("✓ Industry Operations added to enabled submodels\n")
      } else {
        cat("✗ Industry Operations NOT added - enabled:", industry_enabled, "data exists:", !is.null(combined_maps_data$industry_operations_combined_submodel), "\n")
      }
      
      # Check if we have any valid submodels
      if(length(enabled_submodels) == 0) {
        cat("ERROR: No valid submodels found\n")
        showNotification(
          "No valid submodels selected. Please enable submodels and ensure they have been generated.", 
          type = "warning"
        )
        return()
      }
      
      # Show spinner modal
      show_spinner_modal("Generating Full Model", 
                         paste("Please wait while the full model is being calculated using", 
                               length(enabled_submodels), "submodel(s)..."))
      
      # Collect submodel data and weights
      submodels <- list()
      weights <- list()
      
      for(i in seq_along(enabled_submodels)) {
        submodel_name <- enabled_submodels[i]
        
        if(submodel_name == "natural_resources") {
          submodels[["natural_resources"]] <- combined_maps_data$natural_resources_combined_submodel
          weights[["natural_resources"]] <- enabled_weights[i]
        } else if(submodel_name == "fisheries") {
          submodels[["fisheries"]] <- combined_maps_data$fisheries_combined_submodel
          weights[["fisheries"]] <- enabled_weights[i]
        } else if(submodel_name == "industry_operations") {
          submodels[["industry_operations"]] <- combined_maps_data$industry_operations_combined_submodel
          weights[["industry_operations"]] <- enabled_weights[i]
        }
      }
      
      # Call the weighted geometric mean function
      full_model_data <- calculate_geometric_mean_full(
        submodels = submodels,
        weights = weights,
        base_grid = grid_test
      )
      
      # Create the full model map
      full_model_map <- create_full_model_map(
        combined_data = full_model_data,
        aoi_data_reactive = filtered_aoi_data,
        aoi_bounds = aoi_bounds_cache$current_bounds
      )

      # Store the results
      combined_maps_data$full_model <- full_model_data
      combined_maps_data$full_model_generated <- TRUE
      combined_maps_data$full_map <- full_model_map
      
      # Show success notification
      showNotification(
        paste("Full Model generated successfully using", 
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
        paste("Error generating full model:", e$message), 
        type = "error", 
        duration = 10
      )
      
      # Print error to console for debugging
      cat("ERROR:", e$message, "\n")
      print(traceback())
    })
  })
  
  output$fullCombinedMapContainer <- renderUI({
    if(combined_maps_data$full_model_generated) {
      tagList(
        # Main combined map section
        div(
          h4("Full Model Map"),
          p("This map shows the full model calculated using the weighted geometric mean of selected submodels."),
          leafletOutput("fullMap", height = "500px")
        )
      )
    } else {
      div(
        style = "text-align: center; padding: 40px; color: #666;",
        p("Full model maps will appear here after generation."),
        p("Use the sidebar to configure and generate the full model.")
      )
    }
  })
  
  output$fullMap <- renderLeaflet({
    if(!is.null(combined_maps_data$full_map)) {
      combined_maps_data$full_map
    } else {
      leaflet() %>%
        addProviderTiles("Esri.OceanBasemap") %>%
        addControl("Generate full model to see map", position = "center")
    }
  })
  
  # Full Model Report Export Handler
  output$fullModelExportRmd <- downloadHandler(
    filename = function() {
      paste("Full_Model_Report_", format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".html", sep = "")
    },
    content = function(file) {
      generate_full_model_report(
        input = input,
        combined_maps_data = combined_maps_data,
        filtered_aoi_data = filtered_aoi_data,
        data_timestamps = data_timestamps,
        file = file
      )
    }
  )
}