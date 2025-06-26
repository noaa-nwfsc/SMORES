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
        default_weight = 0.33,
        data_key = "habitat"
      ),
      list(
        name = "Species",
        weight_id = "SpeciesWeight", 
        weight_label = "Species Weight",
        default_weight = 0.33,
        data_key = "species"
      ),
      list(
        name = "Birds",
        weight_id = "BirdsWeight",
        weight_label = "Birds Weight", 
        default_weight = 0.34,
        data_key = "birds"
      )
    ),
    export_config = list(
      button_id = "combinedModelExportRmd",
      button_label = "Export to R Markdown"
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
        name = "Miscellaneous",
        weight_id = "MiscWeight",
        weight_label = "Miscellaneous Weight", 
        default_weight = 0.5,
        data_key = "misc"
      )
    ),
    export_config = list(
      button_id = "industryModelExportRmd",
      button_label = "Export to R Markdown"
    )
  )
}