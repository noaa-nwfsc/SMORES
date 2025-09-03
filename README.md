# SMORES
Repository to house the materials to develop the Suitability Modeling for Offshore Resources and Energy Siting (SMORES) Shiny Application.

## Table of Contents

- [Decription](#decription)
- [Usage](#usage)
- [Repository Structure](#repository-structure)
- [Resources](#resources)


## Description

Repository to house the materials to develop the Suitability Modeling for Offshore Resources and Energy Siting (SMORES) Shiny Application.

## Usage
 This application was built to create a flexible framework that will allow scientists, decision-makers, and planners to model data inputs that *could* be used as part of the marine spatial planning process when determining new areas of offshore development on the West Coast of the United States.

The dashboard is organized by the navigation bar at the top. There are 6 tabs: Area of Interest, Natural Resources Submodel, Fisheries Submodel, Industry & Operations Submodel, Full Model, Methods, and Data.


To generate a full model run you will move through the first 5 tabs from left to right.

Each sub-tab contains further detail on scoring suggestions and cues to generate the layers of your choice.

**Step 1**: Navigate to the Area of Interest tab and select a region you would like your analyses to focus on.

**Step 2**: Navigate to the Natural Resources Submodel Tab. You will start at the Habitat sub-tab and select the scores you would like in this model run. Once you have configured your individual layer scores you will select the calculation method you would like used to combine these layers (geometric mean, lowest value, product). This will generate your combined maps. You can opt to export a copy of your results by selecting the Export button at the bottom of the page.

**Step 3**: Navigate to the Species tab and repeat the process you used for the habitat tab.

**Step 4**: Navigate to the Combined Submodel Tab. You will select which calculation method you would like for your combined maps to be included in the overall submodel calculation. You will then click on the generate Combined Submodel button which will produce 3 maps. The first map will represent the combined submodel score for the whole west coast. The second map will show the combined submodel score for the area of interest you previously selected. The third map will show the combined submodel for the area of interest normalized using a minimum maximum normalization.You can opt to export a copy of your results by selecting the Export button at the bottom of the page.

*At this stage the Natural Resources Submodel that will be included in the Full Model has been generated.*

**Step 5**: Repeat steps 2-4 for the Fisheries Submodel which will include selecting scores for fisheries and trawl fisheries layers. *Note that when you combine the fisheries layers with the trawl fishery layers the trawl fishery score will replace the score in grid cells within the top 75% of the trawl fisheries' ranked importance values. This is a different methodology than previous sections. 

*At this stage the Fisheries Submodel that will be included in the Full Model has been generated.*

**Step 6**: Repeat steps 2-4 for the Industry & Operations Submodel which will include selecting scores for scientific surveys and submarine cable layers.

*At this stage the Industry & Operations Submodel that will be included in the Full Model has been generated.*

**Step 7**: Navigate to the Full Model Tab. You will select which submodels you would like to be included in the calculation of the full model and then select the weight you would like applied to each submodel. Once you have configured your submodels you will click on the generate Full Model button which will produce 3 maps.The first map will represent the full model scores for the whole west coast. The second map will show the full model scores for the area of interest you previously selected. The third map will show the full model for the area of interest normalized using a minimum maximum normalization.You can opt to export a copy of your results by selecting the Export button at the bottom of the page.


## Repository Structure
```{r}
├── _brand.yml                        # Theming for the app
├── .gitignore 
├── data_production.R
├── Full_Model_Report_template.Rmd
├── global.R
├── LICENSE                           # Apache 2.0
├── markdown                          # Folder containing all markdown files used and where text is stored
│   ├── area_of_interest.md
│   ├── cables.md
│   ├── combined_fisheries_submodel.md
│   ├── combined_industry_operations_submodel.md
│   ├── combined_natural_resources_submodel.md
│   ├── data.md
│   ├── fisheries.md
│   ├── habitat.md
│   ├── methods.html
│   ├── methods.md
│   ├── overall_model.md
│   ├── overview.md
│   ├── species.md
│   ├── surveys.md
│   └── trawl_fisheries.md
├── R                                # Folder containing all functions used in Shiny App
│   ├── apply_calculation_method.R
│   ├── calculate_geometric_mean_combined.R
│   ├── calculate_lowest_combined.R
│   ├── calculate_product_combined.R
│   ├── calculate_submodel_geometric_mean.R
│   ├── check_submodel_status.R
│   ├── create_aoi_cropped_map.R
│   ├── create_aoi_cropped_map_normalized.R
│   ├── create_continuous_palette.R
│   ├── create_full_model_map.R
│   ├── create_individual_map.R
│   ├── create_maps_container.R
│   ├── data_timestamps.R
│   ├── filter_by_score.R
│   ├── surveys.md
│   ├── data_timestamps.R
│   ├── filter_by_score.R
│   ├── filtered_data_timestamps.R
│   ├── generate_area_of_interest_sidebar.R
│   ├── generate_cables_sidebar.R
│   ├── generate_combined_map_for_method.R
│   ├── generate_fisheries_combined_sidebar.R
│   ├── generate_fisheries_sidebar.R
│   ├── generate_full_model_report.R
│   ├── generate_full_model_sidebar.R
│   ├── generate_habitat_sidebar.R
│   ├── generate_industry_operations_combined_sidebar.R
│   ├── generate_natural_resources_combined_sidebar.R
│   ├── generate_species_sidebar.R
│   ├── generate_submodel_combined_report.R
│   ├── generate_submodel_component_report.R
│   ├── generate_surveys_sidebar.R
│   ├── generate_trawl_fisheries_sidebar.R
│   ├── get_valid_configs_for_tab.R
│   ├── make_combined_map_dataset.R
│   ├── modal.R
│   ├── sidebar_helpers.R
│   ├── submodel_configurations.R
│   └── submodel_status_display.R
├── README.md
├── server.R
├── SMORES.Rproj
├── Submodel_Combined_Report_Template.Rmd
├── Submodel_Component_Report_Template.Rmd
├── ui.R
└── www                                 # graphics folder
    ├── logos
    |  ├── NOAA_FISHERIES_logoH.png
    └── styles.css.png
```

## Resources
