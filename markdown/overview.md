---
editor_options: 
  markdown: 
    wrap: 72
---

This application was built to create a flexible framework that will
allow scientists, decision-makers, and planners to model data inputs
that could be used as part of the marine spatial planning process when
determining new areas of offshore development on the West Coast of the
United States.

The dashboard is organized by the navigation bar at the top. There are 7
tabs: Area of Interest, Natural Resources Submodel, Fisheries Submodel,
Industry & Operations Submodel, Full Model, Scenario Management, Methods, and Data.

**To generate a full model run you will move through the first 5 tabs
from left to right.**

*Each sub-tab contains further detail on scoring suggestions and cues to
generate the layers of your choice.*

Step 1: Navigate to the Area of Interest tab and select a region you
would like your analyses to focus on. The Area of Interest opens with a map
showcasing all of the possible options, with the default being to show you all of the areas at once.
In order to generate a model run you will select an area of choice that is not the `All Areas` option.
If you select the `All Areas` option the analyses will not be able to run due to processing constraints.

Step 2: Navigate to the Natural Resources Submodel Tab. You will start
at the Habitat sub-tab and select the scores you would like in this
model run. Once you have configured your individual layer scores you
will select the calculation method you would like used to combine these
layers (geometric mean, lowest value, product). This will generate your
combined maps. You can opt to export a copy of your results by selecting
the Export button at the bottom of the page.

Step 3: Navigate to the Species tab and repeat the process you used for
the habitat tab.

Step 4: Navigate to the Combined Submodel Tab. You will select which
calculation method you would like for your combined maps to be included
in the overall submodel calculation. You will then click on the generate
Combined Submodel button which will produce 3 maps. The first map will
represent the combined submodel score for the whole west coast. The
second map will show the combined submodel score for the area of
interest you previously selected. The third map will show the combined
submodel for the area of interest normalized using a minimum maximum
normalization.You can opt to export a copy of your results by selecting
the Export button at the bottom of the page.

*At this stage the Natural Resources Submodel that will be included in
the Full Model has been generated.*

Step 5: Repeat steps 2-4 for the Fisheries Submodel which will include
selecting scores for fisheries and trawl fisheries layers. \*Note that
when you combine the fisheries layers with the trawl fishery layers the
trawl fishery score will replace the score in grid cells within the top
75% of the trawl fisheries' ranked importance values. This is a
different methodology than previous sections.

*At this stage the Fisheries Submodel that will be included in the Full
Model has been generated.*

Step 6: Repeat steps 2-4 for the Industry & Operations Submodel which
will include selecting scores for scientific surveys and submarine cable
layers.

*At this stage the Industry & Operations Submodel that will be included
in the Full Model has been generated.*

Step 7: Navigate to the Full Model Tab. You will select which submodels
you would like to be included in the calculation of the full model and
then select the weight you would like applied to each submodel. Once you
have configured your submodels you will click on the generate Full Model
button which will produce 3 maps.The first map will represent the full
model scores for the whole west coast. The second map will show the full
model scores for the area of interest you previously selected. The third
map will show the full model for the area of interest normalized using a
minimum maximum normalization.You can opt to export a copy of your
results by selecting the Export button at the bottom of the page.

**To save a scenario navigate to the Scenario Management tab after you have made your selections within the app.**

*Note: you can save a scenario at any portion of the app process. Including prior to creating a full model run (e.g., after you have configured only the habitat component, fisheries submodel, etc.).

Step 1: Enter the information for your saved scenario including title, author, date, and description.

Step 2: Click 'Save to Cloud' button.

Step 3 (Optional): Watch the table at the bottom of the scenario management tab update with the new entry. 

**To load a scenario navigate to the Scenario Management tab at any point**

Step 1: Select the scenario you would like the app to be configured by clicking on the row in the table. 

Step 2: Click 'Load Scenario' button.

Step 3: App will reload and a pop-up will come across the center of the screen with directions for how to move forward. 

Step 4: Navigate to the Area of Interest tab or tab that you would like to see first. If a scenario was configured for that tab you will see that layers, scores, and potentially combination methods have been selected. 

*Note: a scenario tracks the configurations selected not the maps themselves. In order to view maps or the final model run you must manually generate the maps for each of the components, sumbmodels, and final model you are interested in.

Step 5: Generate maps and view what someone has logged as a scenario of interest. 

