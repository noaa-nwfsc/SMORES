function(input, output, session) {
  # Register shinystate metadata callbacks
  app_storage$register_metadata()

  # Define who has "Master Keys" to delete any scenario
  ADMIN_USERS <- c("melissa.widas", "curt.whitmire")

  # Track the currently loaded scenario
  active_loaded_scenario <- reactiveVal("None")

  # exclude action buttons from being captured in the bookmarking process
  setBookmarkExclude(c(
    "scenario_name",
    "scenario_author",
    "scenario_date",
    "scenario_desc",
    "save_scenario_btn",
    "load_scenario_btn",
    "update_habitat_map_btn",
    "update_species_map_btn",
    "update_fisheries_map_btn",
    "update_trawl_map_btn",
    "update_surveys_map_btn",
    "update_cables_map_btn",
    "generateCombinedHabitatMap",
    "generateCombinedSpeciesMap",
    "generateCombinedFisheriesMap",
    "generateCombinedTrawlMap",
    "generateCombinedSurveysMap",
    "generateCombinedCablesMap",
    "generateNaturalResourcesCombinedSubmodel",
    "generateFisheriesCombinedSubmodel",
    "generateIndustryOperationsCombinedSubmodel",
    "generateFullModel"
  ))

  onRestored(function(state) {
    # clear all individual map memory caches
    individual_processed_data$naturalresources <- list()
    individual_processed_data$fisheries <- list()
    individual_processed_data$industryoperations <- list()

    individual_maps_created$naturalresources <- character(0)
    individual_maps_created$fisheries <- character(0)
    individual_maps_created$industryoperations <- character(0)

    # reset all "generated" flags to FALSE
    # (triggers the UI observers to disable the download buttons)
    combined_maps_data$habitat_combined_map_generated <- FALSE
    combined_maps_data$species_combined_map_generated <- FALSE
    combined_maps_data$natural_resources_combined_submodel_generated <- FALSE

    combined_maps_data$fisheries_combined_map_generated <- FALSE
    combined_maps_data$trawl_combined_map_generated <- FALSE
    combined_maps_data$fisheries_combined_submodel_generated <- FALSE

    combined_maps_data$surveys_combined_map_generated <- FALSE
    combined_maps_data$cables_combined_map_generated <- FALSE
    combined_maps_data$industry_operations_combined_submodel_generated <- FALSE

    combined_maps_data$full_model_generated <- FALSE

    # garbage collection to free up server RAM
    gc()

    # warn the user
    showNotification(
      "ℹ️ Scenario Configuration Loaded! Please navigate to the component tabs and click 'Generate' to rebuild the maps.",
      type = "warning",
      duration = 15 # Keep it on screen for 15 seconds
    )
  })

  # identify app grid size being called
  current_resolution <- reactive({
    selected_area <- input$aoiAreaSelector

    # Handle initial UI load states
    if (
      is.null(selected_area) ||
        selected_area == "" ||
        selected_area == "loading"
    ) {
      selected_area <- "all"
    }

    # check if the area exists in our vector
    if (selected_area %in% names(resolution_for_aoi)) {
      # extract the string
      return(unname(resolution_for_aoi[selected_area]))
    }

    # Default fallback
    return("5km")
  })

  active_grid_test <- reactive({
    res <- current_resolution()
    # Assuming you named the file 'grid_test.parquet' inside both the 2km and 5km folders
    file_path <- file.path("data", res, "grid_full.parquet")
    readRDS_preprocessed(file_path, "Grid")
  })

  active_base_grid_df <- reactive({
    res <- current_resolution()
    file_path <- file.path("data", res, "base_grid_df.parquet")
    arrow::read_parquet(file_path)
  })

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
    if (
      is.null(aoi_bounds_cache$current_area) ||
        aoi_bounds_cache$current_area != current_area
    ) {
      # if user changes aoi selection clear memory stores so we are not holding 2km and 5km memory at the same time
      individual_processed_data$naturalresources <- list()
      individual_processed_data$fisheries <- list()
      individual_processed_data$industryoperations <- list()

      # Reset combined map flags (forces user to click 'Generate' again)
      combined_maps_data$habitat_combined_map_generated <- FALSE
      combined_maps_data$species_combined_map_generated <- FALSE
      combined_maps_data$fisheries_combined_map_generated <- FALSE
      combined_maps_data$trawl_combined_map_generated <- FALSE
      combined_maps_data$surveys_combined_map_generated <- FALSE
      combined_maps_data$cables_combined_map_generated <- FALSE
      combined_maps_data$full_model_generated <- FALSE

      # Explicitly free up RAM by forcing memory to clear
      gc()

      res <- current_resolution()

      aoi_data <- filtered_aoi_data()

      if (!is.null(aoi_data) && nrow(aoi_data) > 0) {
        tryCatch(
          {
            # Use st_bbox directly on filtered data
            bbox <- st_bbox(aoi_data)

            aoi_bounds_cache$current_bounds <- list(
              lng1 = bbox[["xmin"]],
              lat1 = bbox[["ymin"]],
              lng2 = bbox[["xmax"]],
              lat2 = bbox[["ymax"]],
              bbox = bbox
            )
            aoi_bounds_cache$current_area <- current_area
            aoi_bounds_cache$last_update <- Sys.time()
          },
          error = function(e) {
            aoi_bounds_cache$current_bounds <- NULL
            aoi_bounds_cache$current_area <- current_area
          }
        )
      } else {
        aoi_bounds_cache$current_bounds <- NULL
        aoi_bounds_cache$current_area <- current_area
      }
    }
  })

  # Reactive expression for filtered AOI data
  filtered_aoi_data <- reactive({
    # If no area is selected or "All Areas" is selected, return all AOI data
    if (
      is.null(input$aoiAreaSelector) ||
        input$aoiAreaSelector == "" ||
        input$aoiAreaSelector == "loading" ||
        input$aoiAreaSelector == "all"
    ) {
      return(AOI) # Return all AOIs
    }

    # Filter the data when a specific area is selected
    filtered_data <- AOI[AOI$Area_Name == input$aoiAreaSelector, ]

    return(filtered_data)
  })

  # AOI Map Output showing all AOIs initially
  output$aoiMap <- renderLeaflet({
    tryCatch(
      {
        # Call the reactive expression to get the actual data
        aoi_data <- filtered_aoi_data()

        if (is.null(aoi_data) || nrow(aoi_data) == 0) {
          return(
            leaflet() %>%
              addProviderTiles("Esri.OceanBasemap") %>%
              addControl(
                "No Area of Interest data available",
                position = "center"
              )
          )
        }

        # Create the map with different styling based on selection
        map <- leaflet() %>%
          addProviderTiles("Esri.OceanBasemap")

        # Check if showing all areas or just one
        if (
          is.null(input$aoiAreaSelector) ||
            input$aoiAreaSelector == "" ||
            input$aoiAreaSelector == "loading"
        ) {
          # Show all AOIs with lighter styling
          map <- map %>%
            addPolygons(
              data = aoi_data,
              fillColor = "lightblue",
              weight = 1,
              color = "navy",
              fillOpacity = 0.3,
              popup = ~ paste("Area:", Area_Name),
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
              popup = ~ paste("Selected Area:", Area_Name)
            )
        }

        return(map)
      },
      error = function(e) {
        return(
          leaflet() %>%
            addProviderTiles("Esri.OceanBasemap") %>%
            addControl("Error loading AOI data", position = "topright")
        )
      }
    )
  })

  # Helper function to check if a configuration is valid
  is_valid_config <- function(i) {
    # Determine which tab we're on
    current_tab <- input$dataTabs %||% "habitat"

    # Set the prefix based on the tab
    prefix <- switch(
      current_tab,
      "habitat" = "Habitat",
      "species" = "Species",
      "surveys" = "Scientific Surveys",
      "cables" = "Submarine Cables",
      "fisheries" = "Fisheries",
      "trawl" = "Trawl Fisheries",
      ""
    )

    if (prefix == "") {
      return(FALSE)
    } # Invalid tab

    # Check if configuration is valid
    !is.null(enable_input) &&
      enable_input &&
      !is.null(layer_input) &&
      layer_input != "None" &&
      !is.null(score_input) &&
      score_input != "None" &&
      layer_input %in% names(layer_data)
  }

  # Reactive expression for Natural Resources tab valid configs
  natural_resources_valid_configs <- eventReactive(
    list(input$update_habitat_map_btn, input$update_species_map_btn),
    {
      # Check if either button has been clicked at least once
      if (
        isTRUE(input$update_habitat_map_btn > 0) ||
          isTRUE(input$update_species_map_btn > 0)
      ) {
        show_spinner_modal(
          "Generating Natural Resources Maps",
          "Please wait while data is loaded and maps are created..."
        )
      }

      is_natural_resources <- (!is.null(input$dataTabs_natural_resources) &&
        input$dataTabs_natural_resources %in%
          c("habitat", "species", "combined_model_natural_resources")) ||
        (!is.null(input$navbar) && input$navbar == "Natural Resources Submodel")

      if (!is_natural_resources) {
        return(list())
      }

      current_tab_natural_resources <- input$dataTabs_natural_resources %||%
        "habitat"

      # Set layer data AND prefix based on the tab
      layer_data <- switch(
        current_tab_natural_resources,
        "habitat" = habitat_layer,
        "species" = species_layer,
        NULL
      )

      input_prefix <- switch(
        current_tab_natural_resources,
        "habitat" = "Habitat",
        "species" = "Species",
        NULL
      )

      # Call the simplified generic function
      configs <- get_valid_configs_for_tab(
        input,
        layer_data,
        score_colors,
        input_prefix
      )

      return(configs)
    }
  )

  fisheries_valid_configs <- eventReactive(
    list(input$update_fisheries_map_btn, input$update_trawl_map_btn),
    {
      if (
        isTRUE(input$update_fisheries_map_btn > 0) ||
          isTRUE(input$update_trawl_map_btn > 0)
      ) {
        show_spinner_modal(
          "Generating Fisheries Maps",
          "Please wait while data is loaded and maps are created..."
        )
      }

      is_fisheries <- (!is.null(input$dataTabs_fisheries) &&
        input$dataTabs_fisheries %in%
          c("fisheries", "trawl", "combined_model_fisheries")) ||
        (!is.null(input$navbar) && input$navbar == "Fisheries Submodel")

      if (!is_fisheries) {
        return(list())
      }

      current_tab_fisheries <- input$dataTabs_fisheries %||% "fisheries"

      # Set layer data AND prefix based on the tab
      layer_data <- switch(
        current_tab_fisheries,
        "fisheries" = fisheries_layer,
        "trawl" = trawl_fisheries_layer,
        NULL
      )

      input_prefix <- switch(
        current_tab_fisheries,
        "fisheries" = "Fisheries",
        "trawl" = "Trawl",
        NULL
      )

      # Call the simplified generic function
      configs <- get_valid_configs_for_tab(
        input,
        layer_data,
        score_colors,
        input_prefix
      )

      return(configs)
    }
  )

  # Reactive expression for Industry & Operations tab valid configs
  industry_operations_valid_configs <- eventReactive(
    list(input$update_surveys_map_btn, input$update_cables_map_btn),
    {
      if (
        isTRUE(input$update_surveys_map_btn > 0) ||
          isTRUE(input$update_cables_map_btn > 0)
      ) {
        show_spinner_modal(
          "Generating Industry & Operations Maps",
          "Please wait while data is loaded and maps are created..."
        )
      }

      is_industry_operations <- (!is.null(input$dataTabs_industry_operations) &&
        input$dataTabs_industry_operations %in%
          c("surveys", "cables", "combined_model_industry_operations")) ||
        (!is.null(input$navbar) &&
          input$navbar == "Industry & Operations Submodel")

      if (!is_industry_operations) {
        return(list())
      }

      current_tab_industry_operations <- input$dataTabs_industry_operations %||%
        "surveys"

      # Set layer data AND prefix based on the tab
      layer_data <- switch(
        current_tab_industry_operations,
        "surveys" = surveys_layer,
        "cables" = submarine_cables_layer,
        NULL
      )

      input_prefix <- switch(
        current_tab_industry_operations,
        "surveys" = "Surveys",
        "cables" = "Cables",
        NULL
      )

      # Call the simplified generic function
      configs <- get_valid_configs_for_tab(
        input,
        layer_data,
        score_colors,
        input_prefix
      )

      return(configs)
    }
  )

  # Natural Resources maps
  observeEvent(
    natural_resources_valid_configs(),
    {
      valid_configs <- natural_resources_valid_configs()
      aoi_data <- filtered_aoi_data()

      current_tab <- input$dataTabs_natural_resources %||% "habitat"
      ns_prefix <- if (current_tab == "species") "species" else "habitat"

      # Apply cropping to each config's data BEFORE map creation
      for (config in valid_configs) {
        local({
          local_config <- config
          map_id <- paste0(ns_prefix, "_map_", local_config$index)

          # Create a unique identifier for this configuration
          config_key <- paste(
            local_config$layer,
            local_config$score,
            local_config$index,
            sep = "_"
          )

          # Check if this configuration has changed
          last_config <- individual_maps_last_configs$naturalresources[[
            config_key
          ]]
          current_config_hash <- digest::digest(list(
            layer = local_config$layer,
            score = local_config$score,
            index = local_config$index,
            aoi_area = input$aoiAreaSelector %||% "all"
          ))

          # Only update if configuration has changed OR map doesn't exist yet
          if (
            is.null(last_config) ||
              last_config != current_config_hash ||
              !map_id %in% individual_maps_created$naturalresources
          ) {
            # local_config$data is now the base filename string (e.g., "DSC_RH_scored_full.parquet")
            base_filename <- local_config$data

            # handle "Hidden" layers based on the selected score
            target_filename <- switch(
              local_config$layer,
              "Deep Sea Coral Robust High Suitability" = if (
                local_config$score == "Z Membership"
              ) {
                "DSC_RH_z_membership_scored_full.parquet"
              } else {
                base_filename
              },
              base_filename # Default to the base filename for everything else
            )

            # build the dynamic file path using the Resolution Tracker
            res <- current_resolution()

            file_path <- file.path("data", res, target_filename)

            # read the file from the disk
            raw_data <- readRDS_preprocessed(file_path, local_config$layer)

            scored_data <- filter_by_score(
              raw_data,
              local_config$score,
              active_grid_test(),
              local_config$layer
            )

            # crop and prepare for caching
            processed_config_data <- scored_data
            if (!is.null(processed_config_data) && !is.null(aoi_data)) {
              processed_config_data <- crop_data_to_aoi(
                processed_config_data,
                aoi_data
              )
            }

            # set continuous color palette for DSC RH Z Membership layer
            if (
              !is.null(local_config$color) && local_config$color == "continuous"
            ) {
              score_type <- if (
                local_config$layer == "Deep Sea Coral Robust High Suitability"
              ) {
                "z_membership"
              } else {
                "ranked_importance"
              }
              local_config$color_palette <- create_continuous_palette(
                processed_config_data,
                score_type,
                local_config$layer
              )
            }

            # get the score column for this layer
            score_col <- switch(
              local_config$layer,
              "Canyon" = "Score.Canyon",
              "Deep Sea Coral Robust High Suitability" = if (
                local_config$score == "Z Membership"
              ) {
                "Score.Z_Membership"
              } else {
                "Score.DSC_RH"
              },
              "Seeps" = "Score.Seeps",
              "Shelf Break" = "Score.ShlfBrk",
              "EFHCA" = "Score.EFHCA",
              "EFHCA 700 fathoms" = "Score.EFHCA.700",
              "HAPC AOI" = "Score.HAPC.AOI",
              "HAPC Rocky Reef" = "Score.HAPC.RR",
              "ESA Critical Habitat for Southern Resident Killer Whales" = "Score.killer_whale",
              "ESA Critical Habitat for Leatherback Sea Turtles" = "Score.leatherback",
              "ESA Critical Habitat for Humpback Whales - Mexico and Central DPS" = "Score.humpback",
              "Biologically Important Area - Blue Whale" = "Score.blue",
              NULL
            )

            # cache the processed data for reuse in combined maps
            individual_processed_data$naturalresources[[config_key]] <- list(
              data = processed_config_data,
              layer = local_config$layer,
              score = local_config$score,
              score_column = score_col,
              config = local_config,
              component_type = determine_component_type(local_config$layer)
            )

            # update the map with processed data
            output[[map_id]] <- renderLeaflet({
              # Update config with processed data for map creation
              local_config$data <- processed_config_data
              create_individual_map(
                local_config,
                aoi_data,
                aoi_bounds = aoi_bounds_cache$current_bounds
              )
            })

            # store the current configuration hash
            individual_maps_last_configs$naturalresources[[
              config_key
            ]] <- current_config_hash

            # mark as created if not already tracked
            if (!map_id %in% individual_maps_created$naturalresources) {
              individual_maps_created$naturalresources <- c(
                individual_maps_created$naturalresources,
                map_id
              )
            }
          }
        })
      }

      # remove modal after individual maps have generated
      if (
        isTRUE(input$update_habitat_map_btn > 0) ||
          isTRUE(input$update_species_map_btn > 0)
      ) {
        removeModal()
      }
    },
    ignoreNULL = FALSE,
    ignoreInit = FALSE
  )

  # Fisheries maps
  observeEvent(
    fisheries_valid_configs(),
    {
      valid_configs <- fisheries_valid_configs()
      aoi_data <- filtered_aoi_data()

      current_tab <- input$dataTabs_fisheries %||% "fisheries"
      ns_prefix <- if (current_tab == "trawl") "trawl" else "fisheries"

      # Apply cropping to each config's data BEFORE map creation
      for (config in valid_configs) {
        local({
          local_config <- config
          map_id <- paste0(ns_prefix, "_map_", local_config$index)

          # Create a unique identifier for this configuration
          config_key <- paste(
            local_config$layer,
            local_config$score,
            local_config$index,
            sep = "_"
          )

          # Check if this configuration has changed
          last_config <- individual_maps_last_configs$fisheries[[config_key]]
          current_config_hash <- digest::digest(list(
            layer = local_config$layer,
            score = local_config$score,
            index = local_config$index,
            aoi_area = input$aoiAreaSelector %||% "all"
          ))

          # Only update if configuration has changed OR map doesn't exist yet
          if (
            is.null(last_config) ||
              last_config != current_config_hash ||
              !map_id %in% individual_maps_created$fisheries
          ) {
            # --- NEW JUST-IN-TIME LOADING LOGIC (FISHERIES) ---
            # local_config$data is now the base filename string (e.g., "ASH_scored_full.parquet")
            base_filename <- local_config$data

            # 1. Handle "Hidden" layers based on the selected score
            # If the user selected "Ranked Importance", swap to the specific ranked file
            target_filename <- if (local_config$score == "Ranked Importance") {
              switch(
                local_config$layer,
                "At-Sea Hake Mid-Water Trawl" = "ASH_Ranked_Importance_scored_full.parquet",
                "Shoreside Hake Mid-Water Trawl" = "SSH_Ranked_Importance_scored_full.parquet",
                "Groundfish Bottom Trawl" = "GFBT_Ranked_Importance_scored_full.parquet",
                "Groundfish Pot Gear" = "GFP_Ranked_Importance_scored_full.parquet",
                "Groundfish Long Line Gear" = "GFLL_Ranked_Importance_scored_full.parquet",
                "Pink Shrimp Trawl" = "PS_Ranked_Importance_scored_full.parquet",
                "Dungeness Crab" = "CRAB_Ranked_Importance_scored_full.parquet",
                "Commercial Troll/Hook and Line Albacore" = "ALCO_Ranked_Importance_scored_full.parquet",
                "Charter Vessel Albacore Troll/Hook and Line" = "ALCH_Ranked_Importance_scored_full.parquet",
                base_filename # Fallback just in case
              )
            } else {
              base_filename # Standard scores (0.1, 0.2, etc.) use the base file
            }

            # 2. Build the dynamic file path using the Resolution Tracker
            res <- current_resolution()
            file_path <- file.path("data", res, target_filename)

            # 3. Read the file from the disk
            raw_data <- readRDS_preprocessed(file_path, local_config$layer)

            scored_data <- filter_by_score(
              raw_data,
              local_config$score,
              active_grid_test(),
              local_config$layer
            )

            # 4. Crop and prepare for caching
            processed_config_data <- scored_data
            if (!is.null(processed_config_data) && !is.null(aoi_data)) {
              processed_config_data <- crop_data_to_aoi(
                processed_config_data,
                aoi_data
              )
            }

            if (
              !is.null(local_config$color) && local_config$color == "continuous"
            ) {
              local_config$color_palette <- create_continuous_palette(
                processed_config_data,
                "ranked_importance",
                local_config$layer
              )
            }

            # Get the score column for this layer
            score_col <- switch(
              local_config$layer,
              # Fisheries layers with different score types
              "At-Sea Hake Mid-Water Trawl" = if (
                local_config$score == "Ranked Importance"
              ) {
                "Score.ASH_Ranked_Importance"
              } else {
                "Score.ASH"
              },
              "Shoreside Hake Mid-Water Trawl" = if (
                local_config$score == "Ranked Importance"
              ) {
                "Score.SSH_Ranked_Importance"
              } else {
                "Score.SSH"
              },
              "Groundfish Bottom Trawl" = if (
                local_config$score == "Ranked Importance"
              ) {
                "Score.GFBT_Ranked_Importance"
              } else {
                "Score.GFBT"
              },
              "Groundfish Pot Gear" = if (
                local_config$score == "Ranked Importance"
              ) {
                "Score.GFP_Ranked_Importance"
              } else {
                "Score.GFP"
              },
              "Groundfish Long Line Gear" = if (
                local_config$score == "Ranked Importance"
              ) {
                "Score.GFLL_Ranked_Importance"
              } else {
                "Score.GFLL"
              },
              "Pink Shrimp Trawl" = if (
                local_config$score == "Ranked Importance"
              ) {
                "Score.PS_Ranked_Importance"
              } else {
                "Score.PS"
              },
              "Dungeness Crab" = if (
                local_config$score == "Ranked Importance"
              ) {
                "Score.CRAB_Ranked_Importance"
              } else {
                "Score.CRAB"
              },
              "Commercial Troll/Hook and Line Albacore" = if (
                local_config$score == "Ranked Importance"
              ) {
                "Score.ALCO_Ranked_Importance"
              } else {
                "Score.ALCO"
              },
              "Charter Vessel Albacore Troll/Hook and Line" = if (
                local_config$score == "Ranked Importance"
              ) {
                "Score.ALCH_Ranked_Importance"
              } else {
                "Score.ALCH"
              },
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
              create_individual_map(
                local_config,
                aoi_data,
                aoi_bounds = aoi_bounds_cache$current_bounds
              )
            })

            # Store the current configuration hash
            individual_maps_last_configs$fisheries[[
              config_key
            ]] <- current_config_hash

            # Mark as created if not already tracked
            if (!map_id %in% individual_maps_created$fisheries) {
              individual_maps_created$fisheries <- c(
                individual_maps_created$fisheries,
                map_id
              )
            }
          }
        })
      }

      # remove modal after individual maps have generated
      if (
        isTRUE(input$update_fisheries_map_btn > 0) ||
          isTRUE(input$update_trawl_map_btn > 0)
      ) {
        removeModal()
      }
    },
    ignoreNULL = FALSE,
    ignoreInit = FALSE
  )

  # Industry & Operations maps
  observeEvent(
    industry_operations_valid_configs(),
    {
      valid_configs <- industry_operations_valid_configs()
      aoi_data <- filtered_aoi_data()

      current_tab <- input$dataTabs_industry_operations %||% "surveys"
      ns_prefix <- if (current_tab == "cables") "cables" else "surveys"

      # Apply cropping to each config's data BEFORE map creation
      for (config in valid_configs) {
        local({
          local_config <- config
          map_id <- paste0(ns_prefix, "_map_", local_config$index)

          # Create a unique identifier for this configuration
          config_key <- paste(
            local_config$layer,
            local_config$score,
            local_config$index,
            sep = "_"
          )

          # Check if this configuration has changed
          last_config <- individual_maps_last_configs$industryoperations[[
            config_key
          ]]
          current_config_hash <- digest::digest(list(
            layer = local_config$layer,
            score = local_config$score,
            index = local_config$index,
            aoi_area = input$aoiAreaSelector %||% "all"
          ))

          # Only update if configuration has changed OR map doesn't exist yet
          if (
            is.null(last_config) ||
              last_config != current_config_hash ||
              !map_id %in% individual_maps_created$industryoperations
          ) {
            # --- NEW JUST-IN-TIME LOADING LOGIC (INDUSTRY & OPERATIONS) ---
            # local_config$data is now the base filename string (e.g., "submarine_cable_scored_full.parquet")
            base_filename <- local_config$data

            # 1. Build the dynamic file path using the Resolution Tracker
            # (No hidden score files to swap here, so we go straight to building the path)
            res <- current_resolution()
            file_path <- file.path("data", res, base_filename)

            # 2. Read the file from the disk
            raw_data <- readRDS_preprocessed(file_path, local_config$layer)

            scored_data <- filter_by_score(
              raw_data,
              local_config$score,
              active_grid_test(),
              local_config$layer
            )

            # 3. Crop and prepare for caching
            processed_config_data <- scored_data
            if (!is.null(processed_config_data) && !is.null(aoi_data)) {
              processed_config_data <- crop_data_to_aoi(
                processed_config_data,
                aoi_data
              )
            }
            # --------------------------------------------------------------

            # Get the score column for this layer
            score_col <- switch(
              local_config$layer,
              # Scientific Surveys layers
              "Fixed Surveys" = "Score.surveys.fixed",
              "Periodic Surveys" = "Score.surveys.periodic",
              # Submarine Cables layer
              "Submarine Cables" = "Score.cable",
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
              create_individual_map(
                local_config,
                aoi_data,
                aoi_bounds = aoi_bounds_cache$current_bounds
              )
            })

            # Store the current configuration hash
            individual_maps_last_configs$industryoperations[[
              config_key
            ]] <- current_config_hash

            # Mark as created if not already tracked
            if (!map_id %in% individual_maps_created$industryoperations) {
              individual_maps_created$industryoperations <- c(
                individual_maps_created$industryoperations,
                map_id
              )
            }
          }
        })
      }

      # remove modal after individual maps have generated
      if (
        isTRUE(input$update_surveys_map_btn > 0) ||
          isTRUE(input$update_cables_map_btn > 0)
      ) {
        removeModal()
      }
    },
    ignoreNULL = FALSE,
    ignoreInit = FALSE
  )

  # Multiple maps container for habitat
  output$multipleMapsContainer_habitat <- renderUI({
    valid_configs <- natural_resources_valid_configs()
    selected_methods <- input$habitatCalculationMethods %||% character(0)

    create_maps_container(
      configs = valid_configs,
      namespace = "habitat",
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

    if (is.null(selected_methods) || length(selected_methods) == 0) {
      showNotification(
        "Please select at least one calculation method.",
        type = "warning"
      )
      return()
    }

    # Show modal with spinner
    show_spinner_modal(
      "Generating Combined Map(s)",
      paste(
        "Please wait while",
        length(selected_methods),
        "combined map(s) are being generated..."
      )
    )

    # Add a small delay to ensure the modal is visible
    Sys.sleep(0.5)

    # Get valid configurations and AOI data
    valid_configs <- natural_resources_valid_configs()
    aoi_data <- filtered_aoi_data()

    # Generate maps using cached individual data
    all_results <- list()
    for (method in selected_methods) {
      tryCatch(
        {
          # Use cached individual data
          combined_data <- make_combined_map_from_cached_data(
            valid_configs = valid_configs,
            cached_data = individual_processed_data$naturalresources,
            method = method,
            base_grid = active_grid_test(),
            aoi_data = aoi_data
          )

          # Get score columns for verification
          score_cols <- names(combined_data)[grep(
            "^Score\\.",
            names(combined_data)
          )]

          if (length(score_cols) == 0) {
            cat("ERROR: No score columns found for calculation\n")
            next
          }

          # Calculate based on method
          if (method == "geometric_mean") {
            combined_data <- calculate_geometric_mean(combined_data)
          } else if (method == "lowest") {
            combined_data <- calculate_lowest_value(combined_data)
          } else if (method == "product") {
            combined_data <- calculate_product_value(combined_data)
          }

          # Verify calculation result
          expected_col <- switch(
            method,
            "geometric_mean" = "Geo_mean",
            "lowest" = "Lowest_value",
            "product" = "Product_value"
          )

          # Create the map
          map_result <- create_combined_map(
            combined_data = combined_data,
            map_title = paste(
              "Offshore Wind Energy Suitability Score <br> for Habitat Component -",
              switch(
                method,
                "geometric_mean" = "Geometric Mean",
                "lowest" = "Lowest Value",
                "product" = "Product"
              )
            ),
            method = method,
            aoi_data = aoi_data,
            aoi_bounds = aoi_bounds_cache$current_bounds
          )

          # Store the result
          all_results[[method]] <- list(
            combined_data = combined_data,
            map = map_result
          )
        },
        error = function(e) {
          cat("ERROR in method", method, ":", e$message, "\n")
          showNotification(
            paste("Error generating", method, "map:", e$message),
            type = "error"
          )
        }
      )
    }

    if (
      "geometric_mean" %in%
        selected_methods &&
        "geometric_mean" %in% names(all_results)
    ) {
      local({
        result <- all_results[["geometric_mean"]]
        output$combinedHabitatMap_geo <- renderLeaflet({
          result$map
        })
        combined_maps_data$habitat_geo <- result$combined_data
        combined_maps_data$habitat_geo_map <- result$map
      })
    }

    if ("lowest" %in% selected_methods && "lowest" %in% names(all_results)) {
      local({
        result <- all_results[["lowest"]]
        output$combinedHabitatMap_lowest <- renderLeaflet({
          result$map
        })
        combined_maps_data$habitat_lowest <- result$combined_data
        combined_maps_data$habitat_lowest_map <- result$map
      })
    }

    if ("product" %in% selected_methods && "product" %in% names(all_results)) {
      local({
        result <- all_results[["product"]]
        output$combinedHabitatMap_product <- renderLeaflet({
          result$map
        })
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
      paste(
        "Habitat_Component_Natural_Resources_Submodel_Report_",
        format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),
        ".html",
        sep = ""
      )
    },
    content = function(file) {
      # Extract combined data
      combined_data_extracted <- list()
      if (combined_maps_data$habitat_combined_map_generated) {
        if (!is.null(combined_maps_data$habitat_geo)) {
          combined_data_extracted$habitat_geo <- combined_maps_data$habitat_geo
        }
        if (!is.null(combined_maps_data$habitat_lowest)) {
          combined_data_extracted$habitat_lowest <- combined_maps_data$habitat_lowest
        }
        if (!is.null(combined_maps_data$habitat_product)) {
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
        file = file,
        current_res = current_resolution()
      )
    }
  )

  # Multiple maps container for species
  output$multipleMapsContainer_species <- renderUI({
    valid_configs <- natural_resources_valid_configs()
    selected_methods <- input$speciesCalculationMethods %||% character(0)

    create_maps_container(
      configs = valid_configs,
      namespace = "species",
      combined_map_output_id = "combinedSpeciesMap",
      combined_map_generated = combined_maps_data$species_combined_map_generated,
      combined_map_title = "Combined Map Result",
      selected_methods = selected_methods
    )
  })

  # Combined map logic for species
  observeEvent(input$generateCombinedSpeciesMap, {
    # Get selected calculation methods
    selected_methods <- input$speciesCalculationMethods

    if (is.null(selected_methods) || length(selected_methods) == 0) {
      showNotification(
        "Please select at least one calculation method.",
        type = "warning"
      )
      return()
    }

    # Show modal with spinner
    show_spinner_modal(
      "Generating Combined Map(s)",
      paste(
        "Please wait while",
        length(selected_methods),
        "combined map(s) are being generated..."
      )
    )

    # Add a small delay to ensure the modal is visible
    Sys.sleep(0.5)

    # Get valid configurations
    valid_configs <- natural_resources_valid_configs()
    aoi_data <- filtered_aoi_data()

    # Generate maps using cached individual data
    all_results <- list()
    for (method in selected_methods) {
      tryCatch(
        {
          # Use cached individual data
          combined_data <- make_combined_map_from_cached_data(
            valid_configs = valid_configs,
            cached_data = individual_processed_data$naturalresources,
            method = method,
            base_grid = active_grid_test(),
            aoi_data = aoi_data
          )

          # Get score columns for verification
          score_cols <- names(combined_data)[grep(
            "^Score\\.",
            names(combined_data)
          )]

          if (length(score_cols) == 0) {
            cat("ERROR: No score columns found for calculation\n")
            next
          }

          # Calculate based on method
          if (method == "geometric_mean") {
            combined_data <- calculate_geometric_mean(combined_data)
          } else if (method == "lowest") {
            combined_data <- calculate_lowest_value(combined_data)
          } else if (method == "product") {
            combined_data <- calculate_product_value(combined_data)
          }

          # Verify calculation result
          expected_col <- switch(
            method,
            "geometric_mean" = "Geo_mean",
            "lowest" = "Lowest_value",
            "product" = "Product_value"
          )

          # Create the map
          map_result <- create_combined_map(
            combined_data = combined_data,
            map_title = paste(
              "Offshore Wind Energy Suitability Score <br> for Species Component -",
              switch(
                method,
                "geometric_mean" = "Geometric Mean",
                "lowest" = "Lowest Value",
                "product" = "Product"
              )
            ),
            method = method,
            aoi_data = aoi_data,
            aoi_bounds = aoi_bounds_cache$current_bounds
          )

          # Store the result
          all_results[[method]] <- list(
            combined_data = combined_data,
            map = map_result
          )
        },
        error = function(e) {
          cat("ERROR in method", method, ":", e$message, "\n")
          showNotification(
            paste("Error generating", method, "map:", e$message),
            type = "error"
          )
        }
      )
    }

    if (
      "geometric_mean" %in%
        selected_methods &&
        "geometric_mean" %in% names(all_results)
    ) {
      local({
        result <- all_results[["geometric_mean"]]
        output$combinedSpeciesMap_geo <- renderLeaflet({
          result$map
        })
        combined_maps_data$species_geo <- result$combined_data
        combined_maps_data$species_geo_map <- result$map
      })
    }

    if ("lowest" %in% selected_methods && "lowest" %in% names(all_results)) {
      local({
        result <- all_results[["lowest"]]
        output$combinedSpeciesMap_lowest <- renderLeaflet({
          result$map
        })
        combined_maps_data$species_lowest <- result$combined_data
        combined_maps_data$species_lowest_map <- result$map
      })
    }

    if ("product" %in% selected_methods && "product" %in% names(all_results)) {
      local({
        result <- all_results[["product"]]
        output$combinedSpeciesMap_product <- renderLeaflet({
          result$map
        })
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
      paste(
        "Species_Component_Natural_Resources_Submodel_Report_",
        format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),
        ".html",
        sep = ""
      )
    },
    content = function(file) {
      # Extract combined data
      combined_data_extracted <- list()
      if (combined_maps_data$species_combined_map_generated) {
        if (!is.null(combined_maps_data$species_geo)) {
          combined_data_extracted$species_geo <- combined_maps_data$species_geo
        }
        if (!is.null(combined_maps_data$species_lowest)) {
          combined_data_extracted$species_lowest <- combined_maps_data$species_lowest
        }
        if (!is.null(combined_maps_data$species_product)) {
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
        file = file,
        current_res = current_resolution()
      )
    }
  )

  # Multiple maps container for fisheries
  output$multipleMapsContainer_fisheries <- renderUI({
    # Get the full configuration (list with data for individual maps) which holds the large spatial objects in memory because they are needed for the maps
    valid_configs <- fisheries_valid_configs()
    selected_methods <- input$fisheriesCalculationMethods %||% character(0)

    # Pass the LIGHTWEIGHT list to the container generator
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

    if (is.null(selected_methods) || length(selected_methods) == 0) {
      showNotification(
        "Please select at least one calculation method.",
        type = "warning"
      )
      return()
    }

    # Show modal with spinner
    show_spinner_modal(
      "Generating Combined Map(s)",
      paste(
        "Please wait while",
        length(selected_methods),
        "combined map(s) are being generated..."
      )
    )

    # Add a small delay to ensure the modal is visible
    Sys.sleep(0.5)

    # Get valid configurations
    valid_configs <- fisheries_valid_configs()
    aoi_data <- filtered_aoi_data()

    # Generate maps using cached individual data
    all_results <- list()
    for (method in selected_methods) {
      tryCatch(
        {
          # Use cached individual data
          combined_data <- make_combined_map_from_cached_data(
            valid_configs = valid_configs,
            cached_data = individual_processed_data$fisheries,
            method = method,
            base_grid = active_grid_test(),
            aoi_data = aoi_data
          )

          # Get score columns for verification
          score_cols <- names(combined_data)[grep(
            "^Score\\.",
            names(combined_data)
          )]

          if (length(score_cols) == 0) {
            cat("ERROR: No score columns found for calculation\n")
            next
          }

          # Calculate based on method
          if (method == "geometric_mean") {
            combined_data <- calculate_geometric_mean(combined_data)
          } else if (method == "lowest") {
            combined_data <- calculate_lowest_value(combined_data)
          } else if (method == "product") {
            combined_data <- calculate_product_value(combined_data)
          }

          # 2. DEBUGGING: Print size to console
          print(paste(
            "Fisheries Method:",
            method,
            "- Rows:",
            nrow(combined_data)
          ))

          # Verify calculation result
          expected_col <- switch(
            method,
            "geometric_mean" = "Geo_mean",
            "lowest" = "Lowest_value",
            "product" = "Product_value"
          )

          # Create the map
          map_result <- create_combined_map(
            combined_data = combined_data,
            map_title = paste(
              "Offshore Wind Energy Suitability Score <br> for Fisheries Component -",
              switch(
                method,
                "geometric_mean" = "Geometric Mean",
                "lowest" = "Lowest Value",
                "product" = "Product"
              )
            ),
            method = method,
            aoi_data = aoi_data,
            aoi_bounds = aoi_bounds_cache$current_bounds
          )

          # Store the result
          all_results[[method]] <- list(
            combined_data = combined_data,
            map = map_result
          )
        },
        error = function(e) {
          cat("ERROR in method", method, ":", e$message, "\n")
          showNotification(
            paste("Error generating", method, "map:", e$message),
            type = "error"
          )
        }
      )
    }

    # Store results - but only for methods that were actually selected
    if (
      "geometric_mean" %in%
        selected_methods &&
        "geometric_mean" %in% names(all_results)
    ) {
      local({
        result <- all_results[["geometric_mean"]]
        output$combinedFisheriesMap_geo <- renderLeaflet({
          result$map
        })
        combined_maps_data$fisheries_geo <- result$combined_data
        combined_maps_data$fisheries_geo_map <- result$map
      })
    }

    if ("lowest" %in% selected_methods && "lowest" %in% names(all_results)) {
      local({
        result <- all_results[["lowest"]]
        output$combinedFisheriesMap_lowest <- renderLeaflet({
          result$map
        })
        combined_maps_data$fisheries_lowest <- result$combined_data
        combined_maps_data$fisheries_lowest_map <- result$map
      })
    }

    if ("product" %in% selected_methods && "product" %in% names(all_results)) {
      local({
        result <- all_results[["product"]]
        output$combinedFisheriesMap_product <- renderLeaflet({
          result$map
        })
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
      paste(
        "Fisheries_Component_Fisheries_Submodel_Report_",
        format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),
        ".html",
        sep = ""
      )
    },
    content = function(file) {
      # Extract combined data
      combined_data_extracted <- list()
      if (combined_maps_data$fisheries_combined_map_generated) {
        if (!is.null(combined_maps_data$fisheries_geo)) {
          combined_data_extracted$fisheries_geo <- combined_maps_data$fisheries_geo
        }
        if (!is.null(combined_maps_data$fisheries_lowest)) {
          combined_data_extracted$fisheries_lowest <- combined_maps_data$fisheries_lowest
        }
        if (!is.null(combined_maps_data$fisheries_product)) {
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
        file = file,
        current_res = current_resolution()
      )
    }
  )

  # Multiple maps container for trawl fisheries
  output$multipleMapsContainer_trawl <- renderUI({
    valid_configs <- fisheries_valid_configs()
    selected_methods <- input$trawlCalculationMethods %||% character(0)

    create_maps_container(
      configs = valid_configs,
      namespace = "trawl",
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

    if (is.null(selected_methods) || length(selected_methods) == 0) {
      showNotification(
        "Please select at least one calculation method.",
        type = "warning"
      )
      return()
    }

    # Show modal with spinner
    show_spinner_modal(
      "Generating Combined Map(s)",
      paste(
        "Please wait while",
        length(selected_methods),
        "combined map(s) are being generated..."
      )
    )

    # Add a small delay to ensure the modal is visible
    Sys.sleep(0.5)

    # Get valid configurations
    valid_configs <- fisheries_valid_configs()
    aoi_data <- filtered_aoi_data()

    # Generate maps using cached individual data
    all_results <- list()
    for (method in selected_methods) {
      tryCatch(
        {
          # Use cached individual data
          combined_data <- make_combined_map_from_cached_data(
            valid_configs = valid_configs,
            cached_data = individual_processed_data$fisheries,
            method = method,
            base_grid = active_grid_test(),
            aoi_data = aoi_data
          )

          # Get score columns for verification
          score_cols <- names(combined_data)[grep(
            "^Score\\.",
            names(combined_data)
          )]

          if (length(score_cols) == 0) {
            cat("ERROR: No score columns found for calculation\n")
            next
          }

          # Calculate based on method
          if (method == "geometric_mean") {
            combined_data <- calculate_geometric_mean(combined_data)
          } else if (method == "lowest") {
            combined_data <- calculate_lowest_value(combined_data)
          } else if (method == "product") {
            combined_data <- calculate_product_value(combined_data)
          }

          # Verify calculation result
          expected_col <- switch(
            method,
            "geometric_mean" = "Geo_mean",
            "lowest" = "Lowest_value",
            "product" = "Product_value"
          )

          # Create the map
          map_result <- create_combined_map(
            combined_data = combined_data,
            map_title = paste(
              "Offshore Wind Energy Suitability Score <br> for Trawl Fisheries Component -",
              switch(
                method,
                "geometric_mean" = "Geometric Mean",
                "lowest" = "Lowest Value",
                "product" = "Product"
              )
            ),
            method = method,
            aoi_data = aoi_data,
            aoi_bounds = aoi_bounds_cache$current_bounds
          )

          # Store the result
          all_results[[method]] <- list(
            combined_data = combined_data,
            map = map_result
          )
        },
        error = function(e) {
          cat("ERROR in method", method, ":", e$message, "\n")
          showNotification(
            paste("Error generating", method, "map:", e$message),
            type = "error"
          )
        }
      )
    }
    # Store results - but only for methods that were actually selected
    if (
      "geometric_mean" %in%
        selected_methods &&
        "geometric_mean" %in% names(all_results)
    ) {
      local({
        result <- all_results[["geometric_mean"]]
        output$combinedTrawlMap_geo <- renderLeaflet({
          result$map
        })
        combined_maps_data$trawl_geo <- result$combined_data
        combined_maps_data$trawl_geo_map <- result$map
      })
    }

    if ("lowest" %in% selected_methods && "lowest" %in% names(all_results)) {
      local({
        result <- all_results[["lowest"]]
        output$combinedTrawlMap_lowest <- renderLeaflet({
          result$map
        })
        combined_maps_data$trawl_lowest <- result$combined_data
        combined_maps_data$trawl_lowest_map <- result$map
      })
    }

    if ("product" %in% selected_methods && "product" %in% names(all_results)) {
      local({
        result <- all_results[["product"]]
        output$combinedTrawlMap_product <- renderLeaflet({
          result$map
        })
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
      paste(
        "Trawl_Component_Fisheries_Submodel_Report_",
        format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),
        ".html",
        sep = ""
      )
    },
    content = function(file) {
      # Extract combined data
      combined_data_extracted <- list()
      if (combined_maps_data$trawl_combined_map_generated) {
        if (!is.null(combined_maps_data$trawl_geo)) {
          combined_data_extracted$trawl_geo <- combined_maps_data$trawl_geo
        }
        if (!is.null(combined_maps_data$trawl_lowest)) {
          combined_data_extracted$trawl_lowest <- combined_maps_data$trawl_lowest
        }
        if (!is.null(combined_maps_data$trawl_product)) {
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
        file = file,
        current_res = current_resolution()
      )
    }
  )

  # Multiple maps container for surveys
  output$multipleMapsContainer_surveys <- renderUI({
    valid_configs <- industry_operations_valid_configs()
    selected_methods <- input$surveysCalculationMethods %||% character(0)

    create_maps_container(
      configs = valid_configs,
      namespace = "surveys",
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

    if (is.null(selected_methods) || length(selected_methods) == 0) {
      showNotification(
        "Please select at least one calculation method.",
        type = "warning"
      )
      return()
    }

    # Show modal with spinner
    show_spinner_modal(
      "Generating Combined Map(s)",
      paste(
        "Please wait while",
        length(selected_methods),
        "combined map(s) are being generated..."
      )
    )

    # Add a small delay to ensure the modal is visible
    Sys.sleep(0.5)

    # Get valid configurations
    valid_configs <- industry_operations_valid_configs()
    aoi_data <- filtered_aoi_data()

    # Generate maps using cached individual data
    all_results <- list()
    for (method in selected_methods) {
      tryCatch(
        {
          # Use cached individual data
          combined_data <- make_combined_map_from_cached_data(
            valid_configs = valid_configs,
            cached_data = individual_processed_data$industryoperations,
            method = method,
            base_grid = active_grid_test(),
            aoi_data = aoi_data
          )

          # Get score columns for verification
          score_cols <- names(combined_data)[grep(
            "^Score\\.",
            names(combined_data)
          )]

          if (length(score_cols) == 0) {
            cat("ERROR: No score columns found for calculation\n")
            next
          }

          # Calculate based on method
          if (method == "geometric_mean") {
            combined_data <- calculate_geometric_mean(combined_data)
          } else if (method == "lowest") {
            combined_data <- calculate_lowest_value(combined_data)
          } else if (method == "product") {
            combined_data <- calculate_product_value(combined_data)
          }

          # Verify calculation result
          expected_col <- switch(
            method,
            "geometric_mean" = "Geo_mean",
            "lowest" = "Lowest_value",
            "product" = "Product_value"
          )

          # Create the map
          map_result <- create_combined_map(
            combined_data = combined_data,
            map_title = paste(
              "Offshore Wind Energy Suitability Score <br> for Surveys Component -",
              switch(
                method,
                "geometric_mean" = "Geometric Mean",
                "lowest" = "Lowest Value",
                "product" = "Product"
              )
            ),
            method = method,
            aoi_data = aoi_data,
            aoi_bounds = aoi_bounds_cache$current_bounds
          )

          # Store the result
          all_results[[method]] <- list(
            combined_data = combined_data,
            map = map_result
          )
        },
        error = function(e) {
          cat("ERROR in method", method, ":", e$message, "\n")
          showNotification(
            paste("Error generating", method, "map:", e$message),
            type = "error"
          )
        }
      )
    }
    # Store results - but only for methods that were actually selected
    if (
      "geometric_mean" %in%
        selected_methods &&
        "geometric_mean" %in% names(all_results)
    ) {
      local({
        result <- all_results[["geometric_mean"]]
        output$combinedSurveysMap_geo <- renderLeaflet({
          result$map
        })
        combined_maps_data$surveys_geo <- result$combined_data
        combined_maps_data$surveys_geo_map <- result$map
      })
    }

    if ("lowest" %in% selected_methods && "lowest" %in% names(all_results)) {
      local({
        result <- all_results[["lowest"]]
        output$combinedSurveysMap_lowest <- renderLeaflet({
          result$map
        })
        combined_maps_data$surveys_lowest <- result$combined_data
        combined_maps_data$surveys_lowest_map <- result$map
      })
    }

    if ("product" %in% selected_methods && "product" %in% names(all_results)) {
      local({
        result <- all_results[["product"]]
        output$combinedSurveysMap_product <- renderLeaflet({
          result$map
        })
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
      paste(
        "Surveys_Component_Industry_Operations_Submodel_Report_",
        format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),
        ".html",
        sep = ""
      )
    },
    content = function(file) {
      # Extract combined data
      combined_data_extracted <- list()
      if (combined_maps_data$surveys_combined_map_generated) {
        if (!is.null(combined_maps_data$surveys_geo)) {
          combined_data_extracted$surveys_geo <- combined_maps_data$surveys_geo
        }
        if (!is.null(combined_maps_data$surveys_lowest)) {
          combined_data_extracted$surveys_lowest <- combined_maps_data$surveys_lowest
        }
        if (!is.null(combined_maps_data$surveys_product)) {
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
        file = file,
        current_res = current_resolution()
      )
    }
  )

  # Multiple maps container for cables
  output$multipleMapsContainer_cables <- renderUI({
    valid_configs <- industry_operations_valid_configs()
    selected_methods <- input$cablesCalculationMethods %||% character(0)

    create_maps_container(
      configs = valid_configs,
      namespace = "cables",
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

    if (is.null(selected_methods) || length(selected_methods) == 0) {
      showNotification(
        "Please select at least one calculation method.",
        type = "warning"
      )
      return()
    }

    # Show modal with spinner
    show_spinner_modal(
      "Generating Combined Map(s)",
      paste(
        "Please wait while",
        length(selected_methods),
        "combined map(s) are being generated..."
      )
    )

    # Add a small delay to ensure the modal is visible
    Sys.sleep(0.5)

    # Get valid configurations and AOI data
    valid_configs <- industry_operations_valid_configs()
    aoi_data <- filtered_aoi_data()

    # Generate maps using cached individual data
    all_results <- list()
    for (method in selected_methods) {
      tryCatch(
        {
          # Use cached individual data
          combined_data <- make_combined_map_from_cached_data(
            valid_configs = valid_configs,
            cached_data = individual_processed_data$industryoperations,
            method = method,
            base_grid = active_grid_test(),
            aoi_data = aoi_data
          )

          # Get score columns for verification
          score_cols <- names(combined_data)[grep(
            "^Score\\.",
            names(combined_data)
          )]

          if (length(score_cols) == 0) {
            cat("ERROR: No score columns found for calculation\n")
            next
          }

          # Calculate based on method
          if (method == "geometric_mean") {
            combined_data <- calculate_geometric_mean(combined_data)
          } else if (method == "lowest") {
            combined_data <- calculate_lowest_value(combined_data)
          } else if (method == "product") {
            combined_data <- calculate_product_value(combined_data)
          }

          # Verify calculation result
          expected_col <- switch(
            method,
            "geometric_mean" = "Geo_mean",
            "lowest" = "Lowest_value",
            "product" = "Product_value"
          )

          # Create the map
          map_result <- create_combined_map(
            combined_data = combined_data,
            map_title = paste(
              "Offshore Wind Energy Suitability Score <br> for Cables Component -",
              switch(
                method,
                "geometric_mean" = "Geometric Mean",
                "lowest" = "Lowest Value",
                "product" = "Product"
              )
            ),
            method = method,
            aoi_data = aoi_data,
            aoi_bounds = aoi_bounds_cache$current_bounds
          )

          # Store the result
          all_results[[method]] <- list(
            combined_data = combined_data,
            map = map_result
          )
        },
        error = function(e) {
          cat("ERROR in method", method, ":", e$message, "\n")
          showNotification(
            paste("Error generating", method, "map:", e$message),
            type = "error"
          )
        }
      )
    }

    # Store results - but only for methods that were actually selected
    if (
      "geometric_mean" %in%
        selected_methods &&
        "geometric_mean" %in% names(all_results)
    ) {
      local({
        result <- all_results[["geometric_mean"]]
        output$combinedCablesMap_geo <- renderLeaflet({
          result$map
        })
        combined_maps_data$cables_geo <- result$combined_data
        combined_maps_data$cables_geo_map <- result$map
      })
    }

    if ("lowest" %in% selected_methods && "lowest" %in% names(all_results)) {
      local({
        result <- all_results[["lowest"]]
        output$combinedCablesMap_lowest <- renderLeaflet({
          result$map
        })
        combined_maps_data$cables_lowest <- result$combined_data
        combined_maps_data$cables_lowest_map <- result$map
      })
    }

    if ("product" %in% selected_methods && "product" %in% names(all_results)) {
      local({
        result <- all_results[["product"]]
        output$combinedCablesMap_product <- renderLeaflet({
          result$map
        })
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
      paste(
        "Cables_Component_Industry_Operations_Submodel_Report_",
        format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),
        ".html",
        sep = ""
      )
    },
    content = function(file) {
      # Extract combined data
      combined_data_extracted <- list()
      if (combined_maps_data$cables_combined_map_generated) {
        if (!is.null(combined_maps_data$cables_geo)) {
          combined_data_extracted$cables_geo <- combined_maps_data$cables_geo
        }
        if (!is.null(combined_maps_data$cables_lowest)) {
          combined_data_extracted$cables_lowest <- combined_maps_data$cables_lowest
        }
        if (!is.null(combined_maps_data$cables_product)) {
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
        file = file,
        current_res = current_resolution()
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
    if (status$natural_resources$available) {
      updateCheckboxInput(session, "enableNaturalResources", value = TRUE)
    } else {
      updateCheckboxInput(session, "enableNaturalResources", value = FALSE)
    }

    if (status$fisheries$available) {
      updateCheckboxInput(session, "enableFisheries", value = TRUE)
    } else {
      updateCheckboxInput(session, "enableFisheries", value = FALSE)
    }

    if (status$industry_operations$available) {
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
      submodel_status_display(
        "Industry & Operations",
        status$industry_operations
      )
    )
  })

  # Update Natural Resources Combined Sidebar Labels and Method Dropdown
  observe({
    # 1. Update the Checkbox Labels
    hab_ready <- isTRUE(combined_maps_data$habitat_combined_map_generated)
    spec_ready <- isTRUE(combined_maps_data$species_combined_map_generated)

    updateCheckboxInput(
      session,
      "includeHabitat",
      label = HTML(paste(
        "Include Habitat Component",
        if (hab_ready) {
          "<span class='text-success'> ✓ Ready</span>"
        } else {
          "<span class='text-warning'> ⚠ Generate maps first</span>"
        }
      ))
    )

    updateCheckboxInput(
      session,
      "includeSpecies",
      label = HTML(paste(
        "Include Species Component",
        if (spec_ready) {
          "<span class='text-success'> ✓ Ready</span>"
        } else {
          "<span class='text-warning'> ⚠ Generate maps first</span>"
        }
      ))
    )

    # 2. Update the Dynamic Dropdowns
    # --- Habitat ---
    hab_choices <- c()
    if (!is.null(combined_maps_data$habitat_geo)) {
      hab_choices <- c(hab_choices, "Geometric Mean" = "geometric_mean")
    }
    if (!is.null(combined_maps_data$habitat_lowest)) {
      hab_choices <- c(hab_choices, "Lowest Value" = "lowest")
    }
    if (!is.null(combined_maps_data$habitat_product)) {
      hab_choices <- c(hab_choices, "Product" = "product")
    }

    if (length(hab_choices) > 0) {
      shinyjs::show("habitatCalculationMethod")
      current_sel <- input$habitatCalculationMethod
      new_sel <- if (!is.null(current_sel) && current_sel %in% hab_choices) {
        current_sel
      } else {
        hab_choices[1]
      }
      updateSelectInput(
        session,
        "habitatCalculationMethod",
        choices = hab_choices,
        selected = new_sel
      )
    } else {
      shinyjs::hide("habitatCalculationMethod")
    }

    # --- Species ---
    spec_choices <- c()
    if (!is.null(combined_maps_data$species_geo)) {
      spec_choices <- c(spec_choices, "Geometric Mean" = "geometric_mean")
    }
    if (!is.null(combined_maps_data$species_lowest)) {
      spec_choices <- c(spec_choices, "Lowest Value" = "lowest")
    }
    if (!is.null(combined_maps_data$species_product)) {
      spec_choices <- c(spec_choices, "Product" = "product")
    }

    if (length(spec_choices) > 0) {
      shinyjs::show("speciesCalculationMethod")
      current_sel <- input$speciesCalculationMethod
      new_sel <- if (!is.null(current_sel) && current_sel %in% spec_choices) {
        current_sel
      } else {
        spec_choices[1]
      }
      updateSelectInput(
        session,
        "speciesCalculationMethod",
        choices = spec_choices,
        selected = new_sel
      )
    } else {
      shinyjs::hide("speciesCalculationMethod")
    }
  })

  # Add this output to handle validation messages
  output$naturalResourcesCombinedValidation <- renderUI({
    # Get component selections
    include_habitat <- input$includeHabitat %||% FALSE
    include_species <- input$includeSpecies %||% FALSE

    # Check if any components are selected
    any_selected <- include_habitat || include_species

    if (!any_selected) {
      div(
        class = "alert alert-warning",
        "Please select at least one component to generate the combined submodel."
      )
    } else {
      # Check if selected components have valid data
      selected_components <- c()
      if (
        include_habitat && combined_maps_data$habitat_combined_map_generated
      ) {
        selected_components <- c(selected_components, "Habitat")
      }
      if (
        include_species && combined_maps_data$species_combined_map_generated
      ) {
        selected_components <- c(selected_components, "Species")
      }

      if (length(selected_components) == 0) {
        div(
          class = "alert alert-danger",
          "Selected components do not have combined maps generated. Please generate component maps first."
        )
      } else {
        div(
          class = "alert alert-success",
          paste(
            "✓ Ready to generate combined submodel using:",
            paste(selected_components, collapse = ", ")
          )
        )
      }
    }
  })

  # Natural Resources Combined Submodel Export
  observeEvent(input$generateNaturalResourcesCombinedSubmodel, {
    # Add error handling wrapper
    tryCatch(
      {
        # Get component selections
        include_habitat <- isTRUE(input$includeHabitat)
        include_species <- isTRUE(input$includeSpecies)

        # Validate selections
        if (!include_habitat && !include_species) {
          showNotification(
            "Please select at least one component.",
            type = "warning"
          )
          return()
        }

        # Show spinner modal
        show_spinner_modal(
          "Generating Combined Natural Resources Submodel",
          "Please wait while the combined submodel is being calculated..."
        )

        # Collect component data based on user selections
        component_data_list <- list()

        if (
          include_habitat && combined_maps_data$habitat_combined_map_generated
        ) {
          method <- input$habitatCalculationMethod %||% "geometric_mean"

          habitat_data <- switch(
            method,
            "geometric_mean" = combined_maps_data$habitat_geo,
            "lowest" = combined_maps_data$habitat_lowest,
            "product" = combined_maps_data$habitat_product,
            combined_maps_data$habitat_geo
          ) # fallback

          if (!is.null(habitat_data)) {
            component_data_list[["habitat"]] <- habitat_data
          }
        }

        if (
          include_species && combined_maps_data$species_combined_map_generated
        ) {
          method <- input$speciesCalculationMethod %||% "geometric_mean"

          species_data <- switch(
            method,
            "geometric_mean" = combined_maps_data$species_geo,
            "lowest" = combined_maps_data$species_lowest,
            "product" = combined_maps_data$species_product,
            combined_maps_data$species_geo
          ) # fallback

          if (!is.null(species_data)) {
            component_data_list[["species"]] <- species_data
          }
        }

        # Generate the combined submodel using geometric mean
        if (length(component_data_list) > 0) {
          combined_submodel_result <- create_combined_submodel_map(
            component_data_list,
            base_grid = active_grid_test(),
            aoi_data_reactive = filtered_aoi_data,
            aoi_bounds = aoi_bounds_cache$current_bounds
          )

          # Store the result
          combined_maps_data$natural_resources_combined_submodel <- combined_submodel_result$combined_data
          combined_maps_data$natural_resources_combined_submodel_generated <- TRUE

          # Store the map object for rendering
          combined_maps_data$natural_resources_combined_map <- combined_submodel_result$map

          # Generate and store the cropped map
          if (!is.null(combined_submodel_result$combined_data)) {
            # Generate and store the normalized cropped map
            normalized_cropped_map <- create_aoi_cropped_normalized_map(
              combined_data = combined_submodel_result$combined_data,
              aoi_data_reactive = filtered_aoi_data,
              map_title = "Natural Resources AOI-Cropped Normalized",
              aoi_bounds = aoi_bounds_cache$current_bounds
            )
            combined_maps_data$natural_resources_combined_map_cropped_normalized <- normalized_cropped_map
          }

          showNotification(
            "Combined Natural Resources Submodel generated successfully!",
            type = "message"
          )
        } else {
          showNotification(
            "No valid component data available for selected components.",
            type = "error"
          )
        }

        # Remove spinner modal
        removeModal()
      },
      error = function(e) {
        # Remove modal on error
        removeModal()

        # Show error notification
        showNotification(
          paste("Error generating combined submodel:", e$message),
          type = "error",
          duration = 10
        )
      }
    )
  })

  # Natural Resources combined map
  output$naturalResourcesCombinedMap <- renderLeaflet({
    # Check if the map is available
    if (!is.null(combined_maps_data$natural_resources_combined_map)) {
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
    if (
      !is.null(
        combined_maps_data$natural_resources_combined_map_cropped_normalized
      )
    ) {
      combined_maps_data$natural_resources_combined_map_cropped_normalized
    } else {
      leaflet() %>%
        addProviderTiles("Esri.OceanBasemap") %>%
        addControl(
          "Generate combined submodel and select a AOI to see normalized cropped map",
          position = "center"
        )
    }
  })

  # Render the map container content
  output$naturalResourcesCombinedMapContainer <- renderUI({
    if (combined_maps_data$natural_resources_combined_submodel_generated) {
      tagList(
        # Main combined map section
        div(
          h4("Combined Natural Resources Submodel Map"),
          p(
            "This map shows the combined Natural Resources submodel calculated using the geometric mean of selected components."
          ),
          leafletOutput("naturalResourcesCombinedMap", height = "500px")
        ),

        br(),

        # Normalized cropped map section
        div(
          h4("Normalized Natural Resources Submodel Map"),
          p(
            "This map shows the data normalized to a 0-1 scale for easier comparison across different areas. This map is intended to provide additional visual clarity for separating score values that are close together by showing them with a different color palette and relative scale. The scores showed in this map are **not** used in any further calculations, and are purely for visual aid."
          ),
          leafletOutput(
            "naturalResourcesCombinedMapCroppedNormalized",
            height = "500px"
          )
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
      paste(
        "Natural_Resources_Combined_Submodel_Report_",
        format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),
        ".html",
        sep = ""
      )
    },
    content = function(file) {
      generate_submodel_combined_report(
        submodel_type = "natural_resources",
        input = input,
        combined_maps_data = combined_maps_data,
        filtered_aoi_data = filtered_aoi_data,
        data_timestamps = data_timestamps,
        file = file,
        current_res = current_resolution()
      )
    }
  )

  # Update Fisheries Combined Sidebar Labels & Dropdowns
  observe({
    # 1. Update the Checkbox Labels
    fish_ready <- isTRUE(combined_maps_data$fisheries_combined_map_generated)
    trawl_ready <- isTRUE(combined_maps_data$trawl_combined_map_generated)

    updateCheckboxInput(
      session,
      "includeFisheries",
      label = HTML(paste(
        "Include Fisheries Component",
        if (fish_ready) {
          "<span class='text-success'> ✓ Ready</span>"
        } else {
          "<span class='text-warning'> ⚠ Generate maps first</span>"
        }
      ))
    )

    updateCheckboxInput(
      session,
      "includeTrawl",
      label = HTML(paste(
        "Include Trawl Component",
        if (trawl_ready) {
          "<span class='text-success'> ✓ Ready</span>"
        } else {
          "<span class='text-warning'> ⚠ Generate maps first</span>"
        }
      ))
    )

    # 2. Update the Dynamic Dropdowns
    # --- Fisheries ---
    fish_choices <- c()
    if (!is.null(combined_maps_data$fisheries_geo)) {
      fish_choices <- c(fish_choices, "Geometric Mean" = "geometric_mean")
    }
    if (!is.null(combined_maps_data$fisheries_lowest)) {
      fish_choices <- c(fish_choices, "Lowest Value" = "lowest")
    }
    if (!is.null(combined_maps_data$fisheries_product)) {
      fish_choices <- c(fish_choices, "Product" = "product")
    }

    if (length(fish_choices) > 0) {
      shinyjs::show("fisheriesCalculationMethod")
      current_sel <- input$fisheriesCalculationMethod
      new_sel <- if (!is.null(current_sel) && current_sel %in% fish_choices) {
        current_sel
      } else {
        fish_choices[1]
      }
      updateSelectInput(
        session,
        "fisheriesCalculationMethod",
        choices = fish_choices,
        selected = new_sel
      )
    } else {
      shinyjs::hide("fisheriesCalculationMethod")
    }

    # --- Trawl ---
    trawl_choices <- c()
    if (!is.null(combined_maps_data$trawl_geo)) {
      trawl_choices <- c(trawl_choices, "Geometric Mean" = "geometric_mean")
    }
    if (!is.null(combined_maps_data$trawl_lowest)) {
      trawl_choices <- c(trawl_choices, "Lowest Value" = "lowest")
    }
    if (!is.null(combined_maps_data$trawl_product)) {
      trawl_choices <- c(trawl_choices, "Product" = "product")
    }

    if (length(trawl_choices) > 0) {
      shinyjs::show("trawlCalculationMethod")
      current_sel <- input$trawlCalculationMethod
      new_sel <- if (!is.null(current_sel) && current_sel %in% trawl_choices) {
        current_sel
      } else {
        trawl_choices[1]
      }
      updateSelectInput(
        session,
        "trawlCalculationMethod",
        choices = trawl_choices,
        selected = new_sel
      )
    } else {
      shinyjs::hide("trawlCalculationMethod")
    }
  })

  # Add this output to handle validation messages
  output$fisheriesCombinedValidation <- renderUI({
    # Get component selections
    include_fisheries <- input$includeFisheries %||% FALSE
    include_trawl <- input$includeTrawl %||% FALSE

    # Check if any components are selected
    any_selected <- include_fisheries || include_trawl

    if (!any_selected) {
      div(
        class = "alert alert-warning",
        "Please select at least one component to generate the combined submodel."
      )
    } else {
      # Check if selected components have valid data
      selected_components <- c()
      if (
        include_fisheries && combined_maps_data$fisheries_combined_map_generated
      ) {
        selected_components <- c(selected_components, "Fisheries")
      }
      if (include_trawl && combined_maps_data$trawl_combined_map_generated) {
        selected_components <- c(selected_components, "Trawl")
      }
      if (length(selected_components) == 0) {
        div(
          class = "alert alert-danger",
          "Selected components do not have combined maps generated. Please generate component maps first."
        )
      } else {
        div(
          class = "alert alert-success",
          paste(
            "✓ Ready to generate combined submodel using:",
            paste(selected_components, collapse = ", ")
          )
        )
      }
    }
  })

  # Add this observeEvent for the generate button
  observeEvent(input$generateFisheriesCombinedSubmodel, {
    # Add error handling wrapper
    tryCatch(
      {
        # Get component selections
        include_fisheries <- isTRUE(input$includeFisheries)
        include_trawl <- isTRUE(input$includeTrawl)

        # Validate selections
        if (!include_fisheries && !include_trawl) {
          showNotification(
            "Please select at least one component.",
            type = "warning"
          )
          return()
        }

        # Show spinner modal
        show_spinner_modal(
          "Generating Combined Fisheries Submodel",
          "Please wait while the combined submodel is being calculated..."
        )

        # Collect component data based on user selections
        component_data_list <- list()

        if (
          include_fisheries &&
            combined_maps_data$fisheries_combined_map_generated
        ) {
          method <- input$fisheriesCalculationMethod %||% "geometric_mean"

          fisheries_data <- switch(
            method,
            "geometric_mean" = combined_maps_data$fisheries_geo,
            "lowest" = combined_maps_data$fisheries_lowest,
            "product" = combined_maps_data$fisheries_product,
            combined_maps_data$fisheries_geo
          ) # fallback

          if (!is.null(fisheries_data)) {
            component_data_list[["fisheries"]] <- fisheries_data
          }
        }

        if (include_trawl && combined_maps_data$trawl_combined_map_generated) {
          method <- input$trawlCalculationMethod %||% "geometric_mean"

          trawl_data <- switch(
            method,
            "geometric_mean" = combined_maps_data$trawl_geo,
            "lowest" = combined_maps_data$trawl_lowest,
            "product" = combined_maps_data$trawl_product,
            combined_maps_data$trawl_geo
          ) # fallback

          if (!is.null(trawl_data)) {
            component_data_list[["trawl"]] <- trawl_data
          }
        }

        # Generate the combined submodel using geometric mean
        if (length(component_data_list) > 0) {
          combined_submodel_result <- create_combined_submodel_map(
            component_data_list,
            base_grid = active_grid_test(),
            aoi_data_reactive = filtered_aoi_data,
            submodel_type = "fisheries",
            aoi_bounds = aoi_bounds_cache$current_bounds
          )

          # Store the result
          combined_maps_data$fisheries_combined_submodel <- combined_submodel_result$combined_data
          combined_maps_data$fisheries_combined_submodel_generated <- TRUE

          # Store the map object for rendering
          combined_maps_data$fisheries_combined_map <- combined_submodel_result$map

          # Generate and store the cropped map
          if (!is.null(combined_submodel_result$combined_data)) {
            # Generate and store the normalized cropped map
            normalized_cropped_map <- create_aoi_cropped_normalized_map(
              combined_data = combined_submodel_result$combined_data,
              aoi_data_reactive = filtered_aoi_data,
              map_title = "Fisheries AOI-Cropped Normalized",
              aoi_bounds = aoi_bounds_cache$current_bounds
            )
            combined_maps_data$fisheries_combined_map_cropped_normalized <- normalized_cropped_map
          }

          showNotification(
            "Combined Fisheries Submodel generated successfully!",
            type = "message"
          )
        } else {
          showNotification(
            "No valid component data available for selected components.",
            type = "error"
          )
        }

        # Remove spinner modal
        removeModal()
      },
      error = function(e) {
        # Remove modal on error
        removeModal()

        # Show error notification
        showNotification(
          paste("Error generating combined submodel:", e$message),
          type = "error",
          duration = 10
        )
      }
    )
  })

  # Fisheries
  output$fisheriesCombinedMap <- renderLeaflet({
    # Check if the map is available
    if (!is.null(combined_maps_data$fisheries_combined_map)) {
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
    if (
      !is.null(combined_maps_data$fisheries_combined_map_cropped_normalized)
    ) {
      combined_maps_data$fisheries_combined_map_cropped_normalized
    } else {
      leaflet() %>%
        addProviderTiles("Esri.OceanBasemap") %>%
        addControl(
          "Generate combined submodel and select a AOI to see normalized cropped map",
          position = "center"
        )
    }
  })

  # Render the map container content
  output$fisheriesCombinedMapContainer <- renderUI({
    if (combined_maps_data$fisheries_combined_submodel_generated) {
      tagList(
        # Main combined map section
        div(
          h4("Combined Fisheries Submodel Map"),
          p(
            "This map shows the combined Fisheries submodel calculated using the geometric mean of selected components."
          ),
          leafletOutput("fisheriesCombinedMap", height = "500px")
        ),

        br(),

        # Normalized cropped map section
        div(
          h4("Normalized Fisheries Submodel Map"),
          p(
            "This map shows the data normalized to a 0-1 scale for easier comparison across different areas. This map is intended to provide additional visual clarity for separating score values that are close together by showing them with a different color palette and relative scale. The scores showed in this map are **not** used in any further calculations, and are purely for visual aid."
          ),
          leafletOutput(
            "fisheriesCombinedMapCroppedNormalized",
            height = "500px"
          )
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
      paste(
        "Fisheries_Combined_Submodel_Report_",
        format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),
        ".html",
        sep = ""
      )
    },
    content = function(file) {
      generate_submodel_combined_report(
        submodel_type = "fisheries",
        input = input,
        combined_maps_data = combined_maps_data,
        filtered_aoi_data = filtered_aoi_data,
        data_timestamps = data_timestamps,
        file = file,
        current_res = current_resolution()
      )
    }
  )

  # Update Industry & Operations Combined Sidebar Labels & Dropdowns
  observe({
    # 1. Update the Checkbox Labels
    surveys_ready <- isTRUE(combined_maps_data$surveys_combined_map_generated)
    cables_ready <- isTRUE(combined_maps_data$cables_combined_map_generated)

    updateCheckboxInput(
      session,
      "includeSurveys",
      label = HTML(paste(
        "Include Scientific Surveys Component",
        if (surveys_ready) {
          "<span class='text-success'> ✓ Ready</span>"
        } else {
          "<span class='text-warning'> ⚠ Generate maps first</span>"
        }
      ))
    )

    updateCheckboxInput(
      session,
      "includeCables",
      label = HTML(paste(
        "Include Submarine Cables Component",
        if (cables_ready) {
          "<span class='text-success'> ✓ Ready</span>"
        } else {
          "<span class='text-warning'> ⚠ Generate maps first</span>"
        }
      ))
    )

    # 2. Update the Dynamic Dropdowns
    # --- Scientific Surveys ---
    surveys_choices <- c()
    if (!is.null(combined_maps_data$surveys_geo)) {
      surveys_choices <- c(surveys_choices, "Geometric Mean" = "geometric_mean")
    }
    if (!is.null(combined_maps_data$surveys_lowest)) {
      surveys_choices <- c(surveys_choices, "Lowest Value" = "lowest")
    }
    if (!is.null(combined_maps_data$surveys_product)) {
      surveys_choices <- c(surveys_choices, "Product" = "product")
    }

    if (length(surveys_choices) > 0) {
      shinyjs::show("surveysCalculationMethod")
      current_sel <- input$surveysCalculationMethod
      new_sel <- if (
        !is.null(current_sel) && current_sel %in% surveys_choices
      ) {
        current_sel
      } else {
        surveys_choices[1]
      }
      updateSelectInput(
        session,
        "surveysCalculationMethod",
        choices = surveys_choices,
        selected = new_sel
      )
    } else {
      shinyjs::hide("surveysCalculationMethod")
    }

    # --- Submarine Cables ---
    cables_choices <- c()
    if (!is.null(combined_maps_data$cables_geo)) {
      cables_choices <- c(cables_choices, "Geometric Mean" = "geometric_mean")
    }
    if (!is.null(combined_maps_data$cables_lowest)) {
      cables_choices <- c(cables_choices, "Lowest Value" = "lowest")
    }
    if (!is.null(combined_maps_data$cables_product)) {
      cables_choices <- c(cables_choices, "Product" = "product")
    }

    if (length(cables_choices) > 0) {
      shinyjs::show("cablesCalculationMethod")
      current_sel <- input$cablesCalculationMethod
      new_sel <- if (!is.null(current_sel) && current_sel %in% cables_choices) {
        current_sel
      } else {
        cables_choices[1]
      }
      updateSelectInput(
        session,
        "cablesCalculationMethod",
        choices = cables_choices,
        selected = new_sel
      )
    } else {
      shinyjs::hide("cablesCalculationMethod")
    }
  })

  # Add this output to handle validation messages
  output$industryOperationsCombinedValidation <- renderUI({
    # Get component selections
    include_surveys <- input$includeSurveys %||% FALSE
    include_cables <- input$includeCables %||% FALSE

    # Check if any components are selected
    any_selected <- include_surveys || include_cables

    if (!any_selected) {
      div(
        class = "alert alert-warning",
        "Please select at least one component to generate the combined submodel."
      )
    } else {
      # Check if selected components have valid data
      selected_components <- c()
      if (
        include_surveys && combined_maps_data$surveys_combined_map_generated
      ) {
        selected_components <- c(selected_components, "Surveys")
      }
      if (include_cables && combined_maps_data$cables_combined_map_generated) {
        selected_components <- c(selected_components, "Cables")
      }
      if (length(selected_components) == 0) {
        div(
          class = "alert alert-danger",
          "Selected components do not have combined maps generated. Please generate component maps first."
        )
      } else {
        div(
          class = "alert alert-success",
          paste(
            "✓ Ready to generate combined submodel using:",
            paste(selected_components, collapse = ", ")
          )
        )
      }
    }
  })

  # Add this observeEvent for the generate button for Industry Operations
  observeEvent(input$generateIndustryOperationsCombinedSubmodel, {
    # Add error handling wrapper
    tryCatch(
      {
        # Get component selections
        include_surveys <- isTRUE(input$includeSurveys)
        include_cables <- isTRUE(input$includeCables)

        # Validate selections
        if (!include_surveys && !include_cables) {
          showNotification(
            "Please select at least one component.",
            type = "warning"
          )
          return()
        }

        # Show spinner modal
        show_spinner_modal(
          "Generating Combined Industry & Operations Submodel",
          "Please wait while the combined submodel is being calculated..."
        )

        # Collect component data based on user selections
        component_data_list <- list()

        if (
          include_surveys && combined_maps_data$surveys_combined_map_generated
        ) {
          method <- input$surveysCalculationMethod %||% "geometric_mean"

          surveys_data <- switch(
            method,
            "geometric_mean" = combined_maps_data$surveys_geo,
            "lowest" = combined_maps_data$surveys_lowest,
            "product" = combined_maps_data$surveys_product,
            combined_maps_data$surveys_geo
          ) # fallback

          if (!is.null(surveys_data)) {
            component_data_list[["surveys"]] <- surveys_data
          }
        }

        if (
          include_cables && combined_maps_data$cables_combined_map_generated
        ) {
          method <- input$cablesCalculationMethod %||% "geometric_mean"

          cables_data <- switch(
            method,
            "geometric_mean" = combined_maps_data$cables_geo,
            "lowest" = combined_maps_data$cables_lowest,
            "product" = combined_maps_data$cables_product,
            combined_maps_data$cables_geo
          ) # fallback

          if (!is.null(cables_data)) {
            component_data_list[["cables"]] <- cables_data
          }
        }

        # Generate the combined submodel using geometric mean
        if (length(component_data_list) > 0) {
          combined_submodel_result <- create_combined_submodel_map(
            component_data_list,
            base_grid = active_grid_test(),
            aoi_data_reactive = filtered_aoi_data,
            aoi_bounds = aoi_bounds_cache$current_bounds
          )

          # Store the result
          combined_maps_data$industry_operations_combined_submodel <- combined_submodel_result$combined_data
          combined_maps_data$industry_operations_combined_submodel_generated <- TRUE

          # Store the map object for rendering
          combined_maps_data$industry_operations_combined_map <- combined_submodel_result$map

          # Generate and store the cropped map
          if (!is.null(combined_submodel_result$combined_data)) {
            # Generate and store the normalized cropped map
            normalized_cropped_map <- create_aoi_cropped_normalized_map(
              combined_data = combined_submodel_result$combined_data,
              aoi_data_reactive = filtered_aoi_data,
              map_title = "Industry Operations AOI-Cropped Normalized",
              aoi_bounds = aoi_bounds_cache$current_bounds
            )
            combined_maps_data$industry_operations_combined_map_cropped_normalized <- normalized_cropped_map
          }

          showNotification(
            "Combined Industry & Operations Submodel generated successfully!",
            type = "message"
          )
        } else {
          showNotification(
            "No valid component data available for selected components.",
            type = "error"
          )
        }

        # Remove spinner modal
        removeModal()
      },
      error = function(e) {
        # Remove modal on error
        removeModal()

        # Show error notification
        showNotification(
          paste("Error generating combined submodel:", e$message),
          type = "error",
          duration = 10
        )
      }
    )
  })

  # Industry and operations
  output$industryOperationsCombinedMap <- renderLeaflet({
    # Check if the map is available
    if (!is.null(combined_maps_data$industry_operations_combined_map)) {
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
    if (
      !is.null(
        combined_maps_data$industry_operations_combined_map_cropped_normalized
      )
    ) {
      combined_maps_data$industry_operations_combined_map_cropped_normalized
    } else {
      leaflet() %>%
        addProviderTiles("Esri.OceanBasemap") %>%
        addControl(
          "Generate combined submodel and select a AOI to see normalized cropped map",
          position = "center"
        )
    }
  })

  # Render the map container content
  output$industryOperationsCombinedMapContainer <- renderUI({
    if (combined_maps_data$industry_operations_combined_submodel_generated) {
      tagList(
        # Main combined map section
        div(
          h4("Combined Industry & Operations Submodel Map"),
          p(
            "This map shows the combined Industry & Operations submodel calculated using the geometric mean of selected components."
          ),
          leafletOutput("industryOperationsCombinedMap", height = "500px")
        ),

        br(),

        # Normalized cropped map section
        div(
          h4("Normalized Industry & Operations Submodel Map"),
          p(
            "This map shows the data normalized to a 0-1 scale for easier comparison across different areas. This map is intended to provide additional visual clarity for separating score values that are close together by showing them with a different color palette and relative scale. The scores showed in this map are **not** used in any further calculations, and are purely for visual aid."
          ),
          leafletOutput(
            "industryOperationsCombinedMapCroppedNormalized",
            height = "500px"
          )
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
      paste(
        "Industry_Operations_Combined_Submodel_Report_",
        format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),
        ".html",
        sep = ""
      )
    },
    content = function(file) {
      generate_submodel_combined_report(
        submodel_type = "industry_operations",
        input = input,
        combined_maps_data = combined_maps_data,
        filtered_aoi_data = filtered_aoi_data,
        data_timestamps = data_timestamps,
        file = file,
        current_res = current_resolution()
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
    if (nr_enabled) {
      enabled_weights <- c(enabled_weights, natural_resources_weight)
    }
    if (fisheries_enabled) {
      enabled_weights <- c(enabled_weights, fisheries_weight)
    }
    if (industry_enabled) {
      enabled_weights <- c(enabled_weights, industry_weight)
    }

    total_weight <- sum(enabled_weights)
    num_enabled <- length(enabled_weights)

    # Validation messages
    if (num_enabled == 0) {
      div(
        class = "alert alert-warning",
        "No submodels selected. Please enable at least one submodel."
      )
    } else {
      div(
        class = "alert alert-success",
        paste(
          "✓ Total weight:",
          round(total_weight, 3),
          "- Ready to generate full model"
        )
      )
    }
  })

  # Generate full Model Button Logic
  observeEvent(input$generateFullModel, {
    tryCatch(
      {
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

        if (
          nr_enabled &&
            !is.null(combined_maps_data$natural_resources_combined_submodel)
        ) {
          enabled_submodels <- c(enabled_submodels, "natural_resources")
          enabled_weights <- c(enabled_weights, natural_resources_weight)
          cat("✓ Natural Resources added to enabled submodels\n")
        } else {
          cat(
            "✗ Natural Resources NOT added - enabled:",
            nr_enabled,
            "data exists:",
            !is.null(combined_maps_data$natural_resources_combined_submodel),
            "\n"
          )
        }

        if (
          fisheries_enabled &&
            !is.null(combined_maps_data$fisheries_combined_submodel)
        ) {
          enabled_submodels <- c(enabled_submodels, "fisheries")
          enabled_weights <- c(enabled_weights, fisheries_weight)
          cat("✓ Fisheries added to enabled submodels\n")
        } else {
          cat(
            "✗ Fisheries NOT added - enabled:",
            fisheries_enabled,
            "data exists:",
            !is.null(combined_maps_data$fisheries_combined_submodel),
            "\n"
          )
        }

        if (
          industry_enabled &&
            !is.null(combined_maps_data$industry_operations_combined_submodel)
        ) {
          enabled_submodels <- c(enabled_submodels, "industry_operations")
          enabled_weights <- c(enabled_weights, industry_weight)
          cat("✓ Industry Operations added to enabled submodels\n")
        } else {
          cat(
            "✗ Industry Operations NOT added - enabled:",
            industry_enabled,
            "data exists:",
            !is.null(combined_maps_data$industry_operations_combined_submodel),
            "\n"
          )
        }

        # Check if we have any valid submodels
        if (length(enabled_submodels) == 0) {
          cat("ERROR: No valid submodels found\n")
          showNotification(
            "No valid submodels selected. Please enable submodels and ensure they have been generated.",
            type = "warning"
          )
          return()
        }

        # Show spinner modal
        show_spinner_modal(
          "Generating Full Model",
          paste(
            "Please wait while the full model is being calculated using",
            length(enabled_submodels),
            "submodel(s)..."
          )
        )

        # Collect submodel data and weights
        submodels <- list()
        weights <- list()

        for (i in seq_along(enabled_submodels)) {
          submodel_name <- enabled_submodels[i]

          if (submodel_name == "natural_resources") {
            submodels[[
              "natural_resources"
            ]] <- combined_maps_data$natural_resources_combined_submodel
            weights[["natural_resources"]] <- enabled_weights[i]
          } else if (submodel_name == "fisheries") {
            submodels[[
              "fisheries"
            ]] <- combined_maps_data$fisheries_combined_submodel
            weights[["fisheries"]] <- enabled_weights[i]
          } else if (submodel_name == "industry_operations") {
            submodels[[
              "industry_operations"
            ]] <- combined_maps_data$industry_operations_combined_submodel
            weights[["industry_operations"]] <- enabled_weights[i]
          }
        }

        # crop base grid used for geometric mean full calculation to aoi selected
        aoi_data <- filtered_aoi_data()
        working_grid <- crop_data_to_aoi(active_grid_test(), aoi_data)

        # Call the weighted geometric mean function
        full_model_data <- calculate_geometric_mean_full(
          submodels = submodels,
          weights = weights,
          base_grid = working_grid
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
          paste(
            "Full Model generated successfully using",
            length(enabled_submodels),
            "submodel(s)!"
          ),
          type = "message"
        )

        # Remove spinner modal
        removeModal()
      },
      error = function(e) {
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
      }
    )
  })

  output$fullCombinedMapContainer <- renderUI({
    if (combined_maps_data$full_model_generated) {
      tagList(
        # Main combined map section
        div(
          h4("Full Model Map"),
          p(
            "This map shows the full model calculated using the weighted geometric mean of selected submodels."
          ),
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
    if (!is.null(combined_maps_data$full_map)) {
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
      paste(
        "Full_Model_Report_",
        format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),
        ".html",
        sep = ""
      )
    },
    content = function(file) {
      generate_full_model_report(
        input = input,
        combined_maps_data = combined_maps_data,
        filtered_aoi_data = filtered_aoi_data,
        data_timestamps = data_timestamps,
        file = file,
        current_res = current_resolution()
      )
    }
  )

  # Data tab timestamp table
  output$data_timestamps_table <- renderTable({
    res <- current_resolution()

    # Dynamically highlight the active column header
    col_2km <- if (res == "2km") {
      "2km Grid Last Updated (ACTIVE)"
    } else {
      "2km Grid Last Updated"
    }
    col_5km <- if (res == "5km") {
      "5km Grid Last Updated (ACTIVE)"
    } else {
      "5km Grid Last Updated"
    }

    df <- data_timestamps %>%
      select(
        dataset_name,
        description,
        data_type,
        formatted_date_2km,
        formatted_date_5km
      )

    # Rename columns using our dynamic headers
    names(df) <- c("Dataset", "Description", "Data Type", col_2km, col_5km)

    return(df)
  })
  # banner for data tab
  output$active_grid_banner <- renderUI({
    res <- current_resolution()
    div(
      class = "alert alert-info",
      style = "margin-top: 15px; margin-bottom: 15px; background-color: #e7f3fe; border-left: 6px solid #2196F3;",
      HTML(paste(
        "ℹ️ <strong>Active Grid:</strong> Based on your Area of Interest, the model is currently using the <strong>",
        res,
        "</strong> base grid."
      ))
    )
  })

  # banner for aoi tab
  output$aoi_grid_banner <- renderUI({
    current_area <- input$aoiAreaSelector

    # Only render if a specific area is actively selected
    if (
      !is.null(current_area) &&
        current_area != "all" &&
        current_area != "" &&
        current_area != "loading"
    ) {
      res <- current_resolution()

      div(
        class = "alert alert-info",
        style = "margin-top: 15px; margin-bottom: 15px; background-color: #e7f3fe; border-left: 6px solid #2196F3;",
        HTML(paste(
          "ℹ️ <strong>Active Area:</strong> You selected <strong>",
          current_area,
          "</strong>. The model is now utilizing the <strong>",
          res,
          "</strong> base grid."
        ))
      )
    }
  })

  pin_refresh_trigger <- reactiveVal(0)

  output$scenario_table <- DT::renderDT({
    pin_refresh_trigger() # Take a dependency on the trigger

    # Safely read the pin
    sessions_df <- tryCatch(
      {
        pins::pin_read(app_board, "melissa.widas/sessions")
      },
      error = function(e) {
        return(NULL)
      }
    )

    # Check for empty fallback
    if (is.null(sessions_df) || nrow(sessions_df) == 0) {
      return(DT::datatable(
        data.frame(
          Name = character(),
          Author = character(),
          `Date Created` = character(),
          Description = character(),
          check.names = FALSE
        ),
        options = list(language = list(emptyTable = "No scenarios saved yet."))
      ))
    }

    # Build the display dataframe directly from the flattened columns
    display_df <- data.frame(
      Name = if ("name" %in% names(sessions_df)) {
        sessions_df$name
      } else {
        "Unknown"
      },
      Author = if ("author_display" %in% names(sessions_df)) {
        sessions_df$author_display
      } else {
        "Unknown"
      },
      `Date Created` = if ("date_created" %in% names(sessions_df)) {
        sessions_df$date_created
      } else {
        "Unknown"
      },
      Description = if ("desc" %in% names(sessions_df)) {
        sessions_df$desc
      } else {
        ""
      },
      # Keep the URL for the Load button, but we will hide it visually
      URL = if ("url" %in% names(sessions_df)) sessions_df$url else "",
      stringsAsFactors = FALSE,
      check.names = FALSE # Prevents R from changing `Date Created` to `Date.Created`
    )

    # Render the final table
    DT::datatable(
      display_df,
      selection = "single",
      rownames = FALSE, # Removes the empty number column on the far left
      options = list(
        pageLength = 5,
        dom = 'tip',
        # In Javascript, column indexes start at 0. URL is the 5th column, so its index is 4.
        columnDefs = list(list(targets = 4, visible = FALSE))
      )
    )
  })

  # save scenario
  observeEvent(input$save_scenario_btn, {
    # Require the new date field
    req(input$scenario_name, input$scenario_author, input$scenario_date)

    current_user <- session$user %||% "local_dev"

    show_spinner_modal(
      "Saving Scenario",
      "Pushing configuration to the cloud..."
    )

    tryCatch(
      {
        # 1. THE FIX: Grab the full historical ledger manually BEFORE shinystate runs
        current_history <- tryCatch(
          {
            pins::pin_read(app_board, "melissa.widas/sessions")
          },
          error = function(e) NULL
        )

        # 2. Let shinystate do its normal snapshot (which will temporarily overwrite the pin with 1 row)
        app_storage$snapshot(
          session_metadata = list(
            name = input$scenario_name,
            author_display = input$scenario_author,
            date_created = format(as.Date(input$scenario_date), "%m/%d/%Y"),
            creator_username = current_user,
            desc = input$scenario_desc
          )
        )

        # 3. THE FIX: Immediately grab that new 1-row save
        new_save <- tryCatch(
          {
            pins::pin_read(app_board, "melissa.widas/sessions")
          },
          error = function(e) NULL
        )

        # 4. THE FIX: Stitch them together and push the fully restored ledger back up!
        if (!is.null(current_history) && !is.null(new_save)) {
          # Bind them and remove any exact duplicates based on the URL
          restored_ledger <- dplyr::bind_rows(current_history, new_save) %>%
            dplyr::distinct(url, .keep_all = TRUE)

          # Overwrite the pin with the complete history
          pins::pin_write(
            app_board,
            restored_ledger,
            name = "sessions",
            type = "rds"
          )
        }

        showNotification("Scenario saved successfully!", type = "message")

        # Clear the form inputs
        updateTextInput(session, "scenario_name", value = "")
        updateTextAreaInput(session, "scenario_desc", value = "")
        updateDateInput(session, "scenario_date", value = Sys.Date())

        # Trigger the table to refresh
        pin_refresh_trigger(pin_refresh_trigger() + 1)
      },
      error = function(e) {
        showNotification(
          paste("Error saving scenario:", e$message),
          type = "error"
        )
      }
    )

    removeModal()
  })

  # load button
  observeEvent(input$load_scenario_btn, {
    selected_row <- input$scenario_table_rows_selected

    if (is.null(selected_row)) {
      showNotification(
        "Please select a scenario from the table first.",
        type = "warning"
      )
      return()
    }

    show_spinner_modal("Loading Scenario", "Applying saved configuration...")

    tryCatch(
      {
        # 1. Read directly from pins to get the target URL
        sessions_df <- pins::pin_read(app_board, "melissa.widas/sessions")
        selected_url <- sessions_df$url[selected_row]
        selected_name <- sessions_df$name[selected_row]

        # 2. Update the active scenario tracker
        active_loaded_scenario(selected_name)

        # 3. Let shinystate do the heavy lifting
        app_storage$restore(url = selected_url)
      },
      error = function(e) {
        showNotification(
          paste("Error loading scenario:", e$message),
          type = "error"
        )
        removeModal()
      }
    )
  })

  # Render the active scenario text
  output$active_scenario_display <- renderUI({
    HTML(paste0(
      "Actively Loaded Scenario: <span style='font-weight: normal; color: #333;'>",
      active_loaded_scenario(),
      "</span>"
    ))
  })

  # dynamic delete button for scenarios based on posit connect usernames
  observe({
    selected_row <- input$scenario_table_rows_selected
    can_delete <- FALSE # Default state is locked

    # Only proceed if a row is actually selected
    if (length(selected_row) > 0) {
      tryCatch(
        {
          # BYPASS SHINYSTATE
          sessions_df <- pins::pin_read(app_board, "melissa.widas/sessions")

          if (!is.null(sessions_df) && nrow(sessions_df) >= selected_row) {
            current_user <- session$user %||% "local_dev"

            # Safely extract the creator username from the flattened column
            creator <- sessions_df$creator_username[selected_row] %||% "unknown"

            # Check permissions
            if (current_user == creator || current_user %in% ADMIN_USERS) {
              can_delete <- TRUE
            }
          }
        },
        error = function(e) {
          # Fail silently and leave the button locked if the board errors
        }
      )
    }

    # toggleState handles both enabling and disabling safely
    shinyjs::toggleState("delete_scenario_btn", condition = can_delete)
  })

  # Posit Connect pins board management
  # 1. Intercept the Delete Click and show a Warning
  observeEvent(input$delete_scenario_btn, {
    showModal(modalDialog(
      title = "⚠️ Confirm Deletion",
      "Are you sure you want to permanently delete this scenario? This action cannot be undone and will be removed for all users.",
      size = "s",
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_delete_btn", "Yes, Delete", class = "btn-danger")
      )
    ))
  })

  # 2. Execute the actual deletion when they confirm
  observeEvent(input$confirm_delete_btn, {
    removeModal()

    selected_row <- input$scenario_table_rows_selected

    # Failsafe: Ensure a row is still selected
    req(selected_row)

    tryCatch(
      {
        # 1. Read the ledger directly
        sessions_df <- pins::pin_read(app_board, "melissa.widas/sessions")

        # 2. Extract the true backend system name of the pin from the URL
        url_string <- sessions_df$url[selected_row]
        pin_hash <- sub(".*_state_id_=", "", url_string)
        pin_to_delete <- paste0("melissa.widas/", pin_hash)

        # 3. Physically delete the state pin from the Posit Connect board
        pins::pin_delete(app_board, pin_to_delete)

        # 4. Remove the scenario from the ledger and overwrite it
        sessions_df <- sessions_df[-selected_row, , drop = FALSE]
        pins::pin_write(app_board, sessions_df, name = "sessions", type = "rds")

        showNotification("Scenario permanently deleted.", type = "message")

        # Trigger the table to refresh so the deleted scenario vanishes
        pin_refresh_trigger(pin_refresh_trigger() + 1)
      },
      error = function(e) {
        showNotification(
          paste("Error deleting scenario:", e$message),
          type = "error"
        )
      }
    )
  })

  # watches map flags to disable and initiate button clicking ability
  observe({
    # Natural Resources Exports
    shinyjs::toggleState(
      "habitatExportRmd",
      condition = isTRUE(combined_maps_data$habitat_combined_map_generated)
    )
    shinyjs::toggleState(
      "speciesExportRmd",
      condition = isTRUE(combined_maps_data$species_combined_map_generated)
    )
    shinyjs::toggleState(
      "naturalResourcesCombinedExport",
      condition = isTRUE(
        combined_maps_data$natural_resources_combined_submodel_generated
      )
    )

    # Fisheries Exports
    shinyjs::toggleState(
      "fisheriesExportRmd",
      condition = isTRUE(combined_maps_data$fisheries_combined_map_generated)
    )
    shinyjs::toggleState(
      "trawlExportRmd",
      condition = isTRUE(combined_maps_data$trawl_combined_map_generated)
    )
    shinyjs::toggleState(
      "fisheriesCombinedExport",
      condition = isTRUE(
        combined_maps_data$fisheries_combined_submodel_generated
      )
    )

    # Industry & Operations Exports
    shinyjs::toggleState(
      "surveysExportRmd",
      condition = isTRUE(combined_maps_data$surveys_combined_map_generated)
    )
    shinyjs::toggleState(
      "cablesExportRmd",
      condition = isTRUE(combined_maps_data$cables_combined_map_generated)
    )
    shinyjs::toggleState(
      "industryOperationsCombinedExport",
      condition = isTRUE(
        combined_maps_data$industry_operations_combined_submodel_generated
      )
    )

    # Full Model Export
    shinyjs::toggleState(
      "fullModelExportRmd",
      condition = isTRUE(combined_maps_data$full_model_generated)
    )
  })
}
