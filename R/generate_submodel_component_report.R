# Function to generate component reports
# Optimized function to generate component reports using app data
generate_submodel_component_report <- function(
    component_type,
    submodel_type,
    valid_configs,
    combined_data_extracted, # Change this parameter name from combined_maps_data
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
      display_name = "Trawl",
      tab_name = "Fisheries", 
      methods_input = "trawlCalculationMethods",
      combined_title = "Combined Trawl Maps",
      modal_message = "Please wait while the Trawl Component report is being generated..."
    ),
    surveys = list(
      display_name = "Surveys",
      tab_name = "Industry and Operations",
      methods_input = "surveysCalculationMethods", 
      combined_title = "Combined Surveys Maps",
      modal_message = "Please wait while the Surveys Component report is being generated..."
    ),
    cables = list(
      display_name = "Cables", 
      tab_name = "Industry and Operations",
      methods_input = "cablesCalculationMethods",
      combined_title = "Combined Cables Maps", 
      modal_message = "Please wait while the Submarine Cables Component report is being generated..."
    )
  )
  
  # Get component configuration
  config <- component_config[[component_type]]
  if(is.null(config)) {
    stop("Invalid component_type specified")
  }
  
  # Show modal with spinner
  show_spinner_modal("Generating Report", config$modal_message)
  
  # Get filtered timestamp information
  timestamp_info <- get_filtered_timestamp_data(valid_configs, component_type)
  
  # Get AOI data
  aoi_data <- filtered_aoi_data()
  
  # Get selected calculation methods
  selected_methods <- input[[config$methods_input]] %||% character(0)
  
  # Prepare combined data list from the extracted data
  combined_data_list <- list()
  
  # Use the extracted data for each component type
  if(component_type == "fisheries") {
    if("geometric_mean" %in% selected_methods && !is.null(combined_data_extracted$fisheries_geo)) {
      combined_data_list[["geometric_mean"]] <- combined_data_extracted$fisheries_geo
    }
    if("lowest" %in% selected_methods && !is.null(combined_data_extracted$fisheries_lowest)) {
      combined_data_list[["lowest"]] <- combined_data_extracted$fisheries_lowest
    }
    if("product" %in% selected_methods && !is.null(combined_data_extracted$fisheries_product)) {
      combined_data_list[["product"]] <- combined_data_extracted$fisheries_product
    }
  }
  # Add similar blocks for other component types if needed...
  
  # Render the RMarkdown report
  rmarkdown::render(
    input = "Submodel_Component_Report_Template.Rmd",
    output_file = file,
    params = list(
      map_configs = valid_configs,
      combined_data_list = combined_data_list,  
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