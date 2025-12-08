---
editor_options: 
  markdown: 
    wrap: 72
---

This section is to allow a visualization of the scoring scenarios you
have selected for your submodels of choice. The sidebar will indicate
which collections of data have been configured for use. Please start by 
selecting the submodels that you would like included in this scenario. You will then select
the weight that you would like applied to each submodel you have selected.
Selecting all submodels included at a weight of 1, indicates that you would like each
submodel to be considered equally. Some examples for how the weighting schema can be applied are as follows:

- Model run where fisheries is considered 2x more important than natural resources layers, and industry operations. Weights would be set at 1.0 for fisheries, and 0.5 for natural resources and industry operations. 

- Model run where natural resources is considered 3x more important than fisheries, and industry operations. Weights would be set at 0.9 for natural resources, and 0.3 for fisheries and industry operations. 

- Model run where fisheries and industry operations should be equally important, and both should be 4x more important than natural resources. Weights would be set to 0.1 for natural resources, and 0.4 for fisheries and industry operations. 

The full model will take these choices and
combine them through a weighted geometric mean calculation. For more information 
on the weighted geometric mean calculation please visit 
the [methods](https://noaa-nwfsc.github.io/SMORES/Methods.html){target="_blank"} page on the SMORES Documentation site. 

\*Note: The Oregon Wind Energy Area Siting exercise and Aquaculture Opportunity Atlas' did not use weighting in the final model calculation to limit the amount of bias being introduced. If you want to complete a model run 
that follows the methods of that process set the weight for each submodel being included to 1. 