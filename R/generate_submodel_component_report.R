generate_submodel_component_report <- function(
  component_type,
  submodel_type,
  valid_configs,
  individual_processed_data,
  combined_data_extracted,
  combined_maps_data,
  input,
  filtered_aoi_data,
  file
) {
  # Component configuration
  component_config <- list(
    habitat = list(
      display_name = "Habitat",
      tab_name = "Natural Resources",
      methods_input = "habitatCalculationMethods",
      combined_title = "Combined Habitat Maps",
      modal_message = "Please wait while the Habitat Component report is being generated..."
    ),
    species = list(
      display_name = "Species",
      tab_name = "Natural Resources",
      methods_input = "speciesCalculationMethods",
      combined_title = "Combined Species Maps",
      modal_message = "Please wait while the Species Component report is being generated..."
    ),
    fisheries = list(
      display_name = "Fisheries",
      tab_name = "Fisheries",
      methods_input = "fisheriesCalculationMethods",
      combined_title = "Combined Fisheries Maps",
      modal_message = "Please wait while the Fisheries Component report is being generated..."
    ),
    trawl = list(
      display_name = "Trawl Fisheries",
      tab_name = "Fisheries",
      methods_input = "trawlCalculationMethods",
      combined_title = "Combined Trawl Maps",
      modal_message = "Please wait while the Trawl Component report is being generated..."
    ),
    surveys = list(
      display_name = "Surveys",
      tab_name = "Industry & Operations",
      methods_input = "surveysCalculationMethods",
      combined_title = "Combined Surveys Maps",
      modal_message = "Please wait while the Surveys Component report is being generated..."
    ),
    cables = list(
      display_name = "Submarine Cables",
      tab_name = "Industry & Operations",
      methods_input = "cablesCalculationMethods",
      combined_title = "Combined Cables Maps",
      modal_message = "Please wait while the Cables Component report is being generated..."
    )
  )

  # Get component configuration
  config <- component_config[[component_type]]
  if (is.null(config)) {
    stop("Invalid component_type specified")
  }

  # Show modal with spinner
  show_spinner_modal("Generating Report", config$modal_message)

  methods_input_name <- config$methods_input
  selected_methods <- input[[methods_input_name]] %||% character(0)

  # Define method keys mapping
  method_keys <- list(
    "geometric_mean" = "geo",
    "lowest" = "lowest",
    "product" = "product"
  )

  # Also extract combined_data_list for the template
  combined_data_list <- list()
  if (length(selected_methods) > 0) {
    for (method in selected_methods) {
      method_key <- method_keys[[method]]
      data_key <- paste0(component_type, "_", method_key)

      if (!is.null(combined_data_extracted[[data_key]])) {
        combined_data_list[[method]] <- combined_data_extracted[[data_key]]
      }
    }
  }

  # Get filtered timestamp information
  timestamp_info <- get_filtered_timestamp_data(valid_configs, component_type)

  # Get AOI data
  aoi_data <- filtered_aoi_data()

  # Filter individual processed data by component type before creating map configs
  component_specific_data <- list()

  if (!is.null(individual_processed_data)) {
    for (config_key in names(individual_processed_data)) {
      processed_item <- individual_processed_data[[config_key]]

      # Only include if this item belongs to the current component type
      if (
        !is.null(processed_item$component_type) &&
          processed_item$component_type == component_type
      ) {
        component_specific_data[[config_key]] <- processed_item
      }
    }
  }

  # Use component_specific_data instead of individual_processed_data
  map_configs_with_cropped_data <- list()

  if (!is.null(component_specific_data)) {
    for (config_key in names(component_specific_data)) {
      processed_item <- component_specific_data[[config_key]]
      if (!is.null(processed_item$data)) {
        # Use the already-cropped data from the server
        processed_config <- processed_item$config
        processed_config$data <- processed_item$data # This is already AOI-cropped
        processed_config$color <- score_colors[[processed_item$score]] %||%
          "#E41A1C"

        map_configs_with_cropped_data[[config_key]] <- processed_config
      }
    }
  }

  combined_map_objects <- list()

  # Check each method individually and extract the corresponding map
  if ("geometric_mean" %in% selected_methods) {
    geo_map_key <- paste0(component_type, "_geo_map")
    if (!is.null(combined_maps_data[[geo_map_key]])) {
      combined_map_objects[["geometric_mean"]] <- combined_maps_data[[
        geo_map_key
      ]]
    }
  }

  if ("lowest" %in% selected_methods) {
    lowest_map_key <- paste0(component_type, "_lowest_map")
    if (!is.null(combined_maps_data[[lowest_map_key]])) {
      combined_map_objects[["lowest"]] <- combined_maps_data[[lowest_map_key]]
    }
  }

  if ("product" %in% selected_methods) {
    product_map_key <- paste0(component_type, "_product_map")
    if (!is.null(combined_maps_data[[product_map_key]])) {
      combined_map_objects[["product"]] <- combined_maps_data[[product_map_key]]
    }
  }

  # Render call for report
  rmarkdown::render(
    input = "Submodel_Component_Report_Template.Rmd",
    output_file = file,
    params = list(
      map_configs = map_configs_with_cropped_data,
      combined_data_list = combined_data_list,
      combined_map_objects = combined_map_objects,
      selected_methods = selected_methods,
      tab_name = config$tab_name,
      combined_map_title = config$combined_title,
      data_timestamps = timestamp_info,
      component_name = config$display_name,
      aoi_data = aoi_data
    ),
    envir = new.env(parent = globalenv())
  )

  # Remove the modal when done
  removeModal()
}
