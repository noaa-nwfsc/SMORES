---
editor_options: 
markdown: 
wrap: 72
---

Each data layer was scored on a 0 to 1 scale, with scores approaching 0
representing low suitability and 1 representing high suitability
relative to the other grid cells for wind energy. Then a component
suitability score was calculated for each submodel component using the
geometric mean, lowest method, or product method. Next, a final
suitability score was calculated for each submodel by taking the
geometric mean of all scores within each grid cell. The geometric mean
of all submodels was used to calculate a final overall suitability
score. The geometric mean (Equation 2.4) was chosen because it grants
equal importance to each variable and provides a non-biased weighting of
each submodel as they interact with each other (Bovee 1986; Longdill et
al. 2008; Silva et al. 2011; Muñoz-Mas et al. 2012). Furthermore, all
data layers and submodels had equal weight within the suitability model.
However, we also wanted to provide the functionality to weight each
submodel if that is eventuallly utilised in the future.

Combined maps for the different sub-tabs (habitat, fisheries, scientific
surveys, etc.) used the following equations. If you would like to see
the code behind these equations please click on the equation names.

The [geometric
mean](https://github.com/noaa-nwfsc/SMORES/blob/main/R/calculate_geometric_mean_combined.R)
equation used was:

$$\text{g} = \sqrt[n]{x_1 \times x_2 \times \ldots \times x_i}$$

<center>
n = number of variables<br> x₁ = variable 1<br> x₂ = variable
2<br> xᵢ = additional variables
</center>

The
[product](https://github.com/noaa-nwfsc/SMORES/blob/main/R/calculate_product_combined.R)
equation used was:

$$\text{p} = x_1 \times x_2 \times \ldots \times x_i$$

<center>
x₁ = variable 1<br> x₂ = variable 2<br> xᵢ = additional
variables
</center>

The
[lowest](https://github.com/noaa-nwfsc/SMORES/blob/main/R/calculate_lowest_combined.R)
equation used was:

$$\text{l} = \min(x_1, x_2, \ldots, x_i)$$

<center>
x₁ = variable 1<br> x₂ = variable 2<br> xᵢ = additional
variables
</center>

To generate the full model map a [weighted geometric
mean](https://github.com/noaa-nwfsc/SMORES/blob/main/R/create_full_model_map.R)
calculation was used:

$$\text{wg} = \sqrt[n]{x_1^{w_1} \times x_2^{w_2} \times \ldots \times x_i^{w_i}}$$

<center>
n = number of variables<br> x₁ = variable 1<br> x₂ = variable
2<br> xᵢ = additional variables<br> w₁ = weight for variable 1<br> w₂ =
weight for variable 2<br> wᵢ = weights for additional variables
</center>
