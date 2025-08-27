---
editor_options: 
  markdown: 
    wrap: 72
---

Welcome to the Suitability Modeling for Offshore Resources and Energy Siting (SMORES) Shiny Application.

This application was built to create a flexible framework that will allow scientists, decision-makers, and planners to model data inputs that *could* be used as part of the marine spatial planning process when determining new areas of offshore development on the West Coast of the United States.

The dashboard is organized by the navigation bar at the top. There are 6 tabs: Area of Interest, Natural Resources Submodel, Fisheries Submodel, Industry & Operations Submodel, Full Model, Methods, and Data.


To generate a full model run you will move through the first 5 tabs from left to right.

Each sub-tab contains further detail on scoring suggestions and cues to generate the layers of your choice.

**Step 1**: Navigate to the Area of Interest tab and select a region you would like your analyses to focus on.

**Step 2**: Navigate to the Natural Resources Submodel Tab. You will start at the Habitat sub-tab and select the scores you would like in this model run. Once you have configured your individual layer scores you will select the calculation method you would like used to combine these layers (geometric mean, lowest value, product). This will generate your combined maps. You can opt to export a copy of your results by selecting the Export button at the bottom of the page.

**Step 3**: Navigate to the Species tab and repeat the process you used for the habitat tab.

**Step 4**: Navigate to the Combined Submodel Tab. You will select which calculation method you would like for your combined maps to be included in the overall submodel calculation. You will then click on the generate Combined Submodel button which will produce 3 maps. The first map will represent the combined submodel score for the whole west coast. The second map will show the combined submodel score for the area of interest you previously selected. The third map will show the combined submodel for the area of interest normalized using a minimum maximum normalization.You can opt to export a copy of your results by selecting the Export button at the bottom of the page.

*At this stage the Natural Resources Submodel that will be included in the Full Model has been generated.*

**Step 5**: Repeat steps 2-4 for the Industry & Operations Submodel which will include selecting scores for scientific surveys and submarine cable layers.

*At this stage the Industry & Operations Submodel that will be included in the Full Model has been generated.*

**Step 6**: Navigate to the Full Model Tab. You will select which submodels you would like to be included in the calculation of the full model and then select the weight you would like applied to each submodel. Once you have configured your submodels you will click on the generate Full Model button which will produce 3 maps.The first map will represent the full model scores for the whole west coast. The second map will show the full model scores for the area of interest you previously selected. The third map will show the full model for the area of interest normalized using a minimum maximum normalization.You can opt to export a copy of your results by selecting the Export button at the bottom of the page.
