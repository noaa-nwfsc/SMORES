# Function to generate component reports
generate_submodel_component_report <- function(
    component_type,          # "habitat", "species", "surveys", "cables"
    submodel_type,           # "natural_resources", "industry_operations"
    valid_configs,           # The configuration data
    combined_maps_data,      # Reactive values object
    input,                   # Shiny input object
    filtered_aoi_data,       # AOI data function
    file                     # Output file path
) {
  
  # Component-specific mappings
  component_config <- list(
    habitat = list(
      display_name = "Habitat",
      tab_name = "Natural Resources",
      methods_input = "habitatCalculationMethods",
      combined_title = "Combined Habitat Maps",
      geo_data = "habitat_geo",
      lowest_data = "habitat_lowest", 
      product_data = "habitat_product",
      generated_flag = "habitat_combined_map_generated",
      filename_prefix = "Habitat_Component_Natural_Resources_Submodel_Report_",
      modal_message = "Please wait while the Habitat Component of the Natural Resources report is being generated..."
    ),
    species = list(
      display_name = "Species",
      tab_name = "Natural Resources", 
      methods_input = "speciesCalculationMethods",
      combined_title = "Combined Species Maps",
      geo_data = "species_geo",
      lowest_data = "species_lowest",
      product_data = "species_product", 
      generated_flag = "species_combined_map_generated",
      filename_prefix = "Species_Component_Natural_Resources_Submodel_Report_",
      modal_message = "Please wait while the Species Component of the Natural Resources report is being generated..."
    ),
    surveys = list(
      display_name = "Surveys",
      tab_name = "Industry and Operations",
      methods_input = "surveysCalculationMethods", 
      combined_title = "Combined Surveys Maps",
      geo_data = "surveys_geo",
      lowest_data = "surveys_lowest",
      product_data = "surveys_product",
      generated_flag = "surveys_combined_map_generated",
      filename_prefix = "Surveys_Component_Industry_Operations_Submodel_Report_",
      modal_message = "Please wait while the Surveys Component of the Industry and Operations report is being generated..."
    ),
    cables = list(
      display_name = "Cables", 
      tab_name = "Industry and Operations",
      methods_input = "cablesCalculationMethods",
      combined_title = "Combined Cables Maps", 
      geo_data = "cables_geo",
      lowest_data = "cables_lowest",
      product_data = "cables_product",
      generated_flag = "cables_combined_map_generated", 
      filename_prefix = "Submarine_Cables_Component_Industry_Operations_Submodel_Report_",
      modal_message = "Please wait while the Submarine Cables Component of the Industry and Operations report is being generated..."
    )
  )
  
  # Get component configuration
  config <- component_config[[component_type]]
  if(is.null(config)) {
    stop("Invalid component_type specified")
  }
  
  # Show modal with spinner
  show_spinner_modal("Generating Report", config$modal_message)
  
  # Get filtered timestamp information for the selected layers
  timestamp_info <- get_filtered_timestamp_data(valid_configs, component_type)
  
  # Get filtered AOI data for the report
  aoi_data <- filtered_aoi_data()
  
  # Make sure each valid_config has valid spatial data
  for(i in seq_along(valid_configs)) {
    # Ensure data is transformed to WGS84 for leaflet
    if(!is.null(valid_configs[[i]]$data) && inherits(valid_configs[[i]]$data, "sf")) {
      valid_configs[[i]]$data <- st_transform(valid_configs[[i]]$data, '+proj=longlat +datum=WGS84')
    }
  }
  
  # Get selected calculation methods
  selected_methods <- input[[config$methods_input]] %||% character(0)
  
  # Get combined data for each selected method if available
  combined_data_list <- list()
  if(combined_maps_data[[config$generated_flag]] && length(selected_methods) > 0) {
    
    if("geometric_mean" %in% selected_methods && !is.null(combined_maps_data[[config$geo_data]])) {
      combined_data_list[["geometric_mean"]] <- combined_maps_data[[config$geo_data]]
    }
    
    if("lowest" %in% selected_methods && !is.null(combined_maps_data[[config$lowest_data]])) {
      combined_data_list[["lowest"]] <- combined_maps_data[[config$lowest_data]]
    }
    
    if("product" %in% selected_methods && !is.null(combined_maps_data[[config$product_data]])) {
      combined_data_list[["product"]] <- combined_maps_data[[config$product_data]]
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