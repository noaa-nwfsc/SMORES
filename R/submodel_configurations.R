# Configuration objects for submodels
get_natural_resources_config <- function() {
  list(
    name = "Natural Resources",
    status_output_id = "combinedModelStatus_natural_resources",
    generate_button_id = "generateCombinedModel",
    components = list(
      list(
        name = "Habitat",
        weight_id = "HabitatWeight",
        weight_label = "Habitat Weight",
        default_weight = 0.5,
        data_key = "habitat"
      ),
      list(
        name = "Species",
        weight_id = "SpeciesWeight", 
        weight_label = "Species Weight",
        default_weight = 0.5,
        data_key = "species"
      )
    ),
    export_config = list(
      button_id = "combinedModelExportRmd",
      button_label = "Export to HTML"
    )
  )
}

get_fisheries_config <- function() {
  list(
    name = "Fisheries",
    status_output_id = "combinedModelStatus_fisheries",
    generate_button_id = "generateCombinedModel",
    components = list(
      list(
        name = "Fisheries",
        weight_id = "FisheriesWeight",
        weight_label = "Fisheries Weight",
        default_weight = 0.5,
        data_key = "fisheries"
      ),
      list(
        name = "Trawl",
        weight_id = "TrawlWeight", 
        weight_label = "Trawl Weight",
        default_weight = 0.5,
        data_key = "trawl"
      )
    ),
    export_config = list(
      button_id = "combinedModelExportRmd",
      button_label = "Export to HTML"
    )
  )
}

get_industry_operations_config <- function() {
  list(
    name = "Industry & Operations",
    status_output_id = "combinedModelStatus_industry_operations", 
    generate_button_id = "generateCombinedModelIndustry",
    components = list(
      list(
        name = "Scientific Surveys",
        weight_id = "SurveysWeight",
        weight_label = "Scientific Surveys Weight",
        default_weight = 0.5,
        data_key = "surveys"
      ),
      list(
        name = "Cables",
        weight_id = "cablesWeight",
        weight_label = "Cables Weight", 
        default_weight = 0.5,
        data_key = "cables"
      )
    ),
    export_config = list(
      button_id = "industryModelExportRmd",
      button_label = "Export to HTML"
    )
  )
}