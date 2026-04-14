# SMORES

Repository to house the materials to develop the Suitability Modeling for Offshore Resources and Energy Siting (SMORES) Shiny Application.

## Table of Contents

-   [Decription](#decription)
-   [Usage](#usage)
-   [Repository Structure](#repository-structure)
-   [Deployment](#deployment)
-   [Resources](#resources)
-   [Disclaimer](#disclaimer)

## Description

Repository to house the materials to develop the Suitability Modeling for Offshore Resources and Energy Siting (SMORES) Shiny Application.

## Usage

This application was built to create a flexible framework that will allow scientists, decision-makers, and planners to model data inputs that *could* be used as part of the marine spatial planning process when determining new areas of offshore development on the West Coast of the United States.

The dashboard is organized by the navigation bar at the top. There are 6 tabs: Area of Interest, Natural Resources Submodel, Fisheries Submodel, Industry & Operations Submodel, Full Model, Methods, and Data.

For directions on how to execute a model run, save a scenario, load a saved scenario, and delete scenarios please see the [GUI Quickstart Guides](https://noaa-nwfsc.github.io/SMORES/) on the SMORES Documentation website.

## Repository Structure
<details>

<summary>To view the repository structure view the collapsed section</summary>

```{r}
.
├── .gitignore
├── Full_Model_Report_Template.Rmd
├── generate_data_timestamps.R
├── global.R
├── global_report_generation.R
├── LICENSE
├── manifest.json                                                                # file for Posit Connect deployment
├── README.md
├── server.R
├── SMORES.Rproj
├── Submodel_Combined_Report_Template.Rmd
├── Submodel_Component_Report_Template.Rmd
├── ui.R
├── _brand.yml                                                                   # unified NOAA branding
├── .quarto/
├── data/
│   ├── AOI_Full.parquet
│   ├── data_timestamps.rds
│   ├── 2km/                                                                     # geoParquet files for 2km grid cell size
│   │   ├── ALCH_Ranked_Importance_scored_full.parquet
│   │   ├── ALCH_scored_full.parquet
│   │   ├── ALCO_Ranked_Importance_scored_full.parquet
│   │   ├── ALCO_scored_full.parquet
│   │   ├── ASH_Ranked_Importance_scored_full.parquet
│   │   ├── ASH_scored_full.parquet
│   │   ├── base_grid_df.parquet
│   │   ├── Blue_whale_scored_full.parquet
│   │   ├── Canyon_scored_full.parquet
│   │   ├── CRAB_Ranked_Importance_scored_full.parquet
│   │   ├── CRAB_scored_full.parquet
│   │   ├── DSC_RH_scored_full.parquet
│   │   ├── DSC_RH_z_membership_scored_full.parquet
│   │   ├── EFHCA_700_scored_full.parquet
│   │   ├── EFHCA_scored_full.parquet
│   │   ├── GFBT_Ranked_Importance_scored_full.parquet
│   │   ├── GFBT_scored_full.parquet
│   │   ├── GFLL_Ranked_Importance_scored_full.parquet
│   │   ├── GFLL_scored_full.parquet
│   │   ├── GFP_Ranked_Importance_scored_full.parquet
│   │   ├── GFP_scored_full.parquet
│   │   ├── grid_full.parquet
│   │   ├── HAPCaoi_scored_full.parquet
│   │   ├── HAPCreef_scored_full.parquet
│   │   ├── Humpback_whale_scored_full.parquet
│   │   ├── Killer_whale_scored_full.parquet
│   │   ├── Leatherback_turtle_scored_full.parquet
│   │   ├── PS_Ranked_Importance_scored_full.parquet
│   │   ├── PS_scored_full.parquet
│   │   ├── Seeps_scored_full.parquet
│   │   ├── ShlfBrk_scored_full.parquet
│   │   ├── SSH_Ranked_Importance_scored_full.parquet
│   │   ├── SSH_scored_full.parquet
│   │   ├── Submarine_cable_scored_full.parquet
│   │   ├── Surveys_fixed_scored_full.parquet
│   │   ├── Surveys_periodic_scored_full.parquet
│   │   └── Trawl_fisheries_scored_full.parquet
│   └── 5km/                                                                     # geoParquet files for 5km grid cell size
│       ├── ALCH_Ranked_Importance_scored_full.parquet
│       ├── ALCH_scored_full.parquet
│       ├── ALCO_Ranked_Importance_scored_full.parquet
│       ├── ALCO_scored_full.parquet
│       ├── ASH_Ranked_Importance_scored_full.parquet
│       ├── ASH_scored_full.parquet
│       ├── base_grid_df.parquet
│       ├── Blue_whale_scored_full.parquet
│       ├── Canyon_scored_full.parquet
│       ├── CRAB_Ranked_Importance_scored_full.parquet
│       ├── CRAB_scored_full.parquet
│       ├── DSC_RH_scored_full.parquet
│       ├── DSC_RH_z_membership_scored_full.parquet
│       ├── EFHCA_700_scored_full.parquet
│       ├── EFHCA_scored_full.parquet
│       ├── GFBT_Ranked_Importance_scored_full.parquet
│       ├── GFBT_scored_full.parquet
│       ├── GFLL_Ranked_Importance_scored_full.parquet
│       ├── GFLL_scored_full.parquet
│       ├── GFP_Ranked_Importance_scored_full.parquet
│       ├── GFP_scored_full.parquet
│       ├── grid_full.parquet
│       ├── HAPCaoi_scored_full.parquet
│       ├── HAPCreef_scored_full.parquet
│       ├── Humpback_whale_scored_full.parquet
│       ├── Killer_whale_scored_full.parquet
│       ├── Leatherback_turtle_scored_full.parquet
│       ├── PS_Ranked_Importance_scored_full.parquet
│       ├── PS_scored_full.parquet
│       ├── Seeps_scored_full.parquet
│       ├── ShlfBrk_scored_full.parquet
│       ├── SSH_Ranked_Importance_scored_full.parquet
│       ├── SSH_scored_full.parquet
│       ├── Submarine_cable_scored_full.parquet
│       ├── Surveys_fixed_scored_full.parquet
│       ├── Surveys_periodic_scored_full.parquet
│       └── Trawl_fisheries_scored_full.parquet
├── data_processing/                                                          # data scripts for generating application ready data
│   ├── areas_of_interest.qmd
│   ├── binary_data_processing_2km.qmd
│   ├── binary_data_processing_5km.qmd
│   ├── continuous_data_processing_2km.qmd
│   ├── continuous_data_processing_5km.qmd
│   └── data_production_original.R
├── markdown/                                                                 # text used on application
│   ├── area_of_interest.md
│   ├── cables.md
│   ├── combined_fisheries_submodel.md
│   ├── combined_industry_operations_submodel.md
│   ├── combined_natural_resources_submodel.md
│   ├── data.md
│   ├── fisheries.md
│   ├── full_model.md
│   ├── habitat.md
│   ├── methods.md
│   ├── overview.md
│   ├── species.md
│   ├── surveys.md
│   └── trawl_fisheries.md
├── quarto_site/                                                               # Documentation website files                                   
│   ├── .gitignore
│   ├── Access.qmd
│   ├── Data.qmd
│   ├── Functions.qmd
│   ├── GUI.qmd
│   ├── index.qmd
│   ├── Methods.qmd
│   ├── styles.css
│   ├── _quarto.yml
│   ├── docs/                                                                   # GitHub pages access point
│   └── www/
│       └── Images/
├── R/                                                                          # custom functions
│   ├── apply_calculation_method.R
│   ├── calculate_geometric_mean_combined.R
│   ├── calculate_geometric_mean_full.R
│   ├── calculate_lowest_combined.R
│   ├── calculate_product_combined.R
│   ├── calculate_submodel_geometric_mean.R
│   ├── check_submodel_status.R
│   ├── create_aoi_cropped_map.R
│   ├── create_aoi_cropped_normalized_map.R
│   ├── create_combined_map.R
│   ├── create_combined_submodel_map.R
│   ├── create_continuous_palette.R
│   ├── create_full_model_map.R
│   ├── create_individual_map.R
│   ├── create_maps_container.R
│   ├── crop_data_to_aoi.R
│   ├── data_timestamps.R
│   ├── determine_component_type.R
│   ├── filtered_data_timestamps.R
│   ├── filter_by_score.R
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
│   ├── make_combined_map_from_cached_data.R
│   ├── modal.R
│   ├── preprocess_spatial_data.R
│   ├── sidebar_helpers.R
│   ├── submodel_configurations.R
│   └── submodel_status_display.R
└── www/
    ├── documentation_website.png
    ├── styles.css
    └── logos/
        └── NOAA_FISHERIES_logoH.png
```
</details>

## Deployment

The SMORES application is deployed on NOAA Fisheries [Posit Connect Test Instance](https://test-connect.fisheries.noaa.gov/connect/#/content/listing?q=is:published). To access the published application you need to have a CAC login. At this time this app is only accessible to NOAA Fisheries staff, and affiliates for scenario modeling purposes.

## Resources

A Wind Energy Area Siting Analysis for the Oregon Call Areas [[Link]](https://www.boem.gov/sites/default/files/documents/environment/BOEM_2024-015.pdf)

Citation: Carlton J, Jossart JA, Pendleton F, Sumait N, Miller J, Thurston-Keller J, Reeb D, Gilbane L, Pereksta D, Schroeder D, Morris Jr JA. 2024. A wind energy area siting analysis for the Oregon Call Areas. Camarillo (CA): U.S. Department of the Interior, Bureau of Ocean Energy Management. 237 p. Report No.: OCS Study BOEM 2024-015.

## Disclaimer

This repository is a scientific product and is not official communication of the National Oceanic and Atmospheric Administration, or the United States Department of Commerce. All NOAA GitHub project code is provided on an ‘as is’ basis and the user assumes responsibility for its use. Any claims against the Department of Commerce or Department of Commerce bureaus stemming from the use of this GitHub project will be governed by all applicable Federal law. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by the Department of Commerce. The Department of Commerce seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by DOC or the United States Government.
