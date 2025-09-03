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

The Geometric mean equation used was:

$$\text{g} = \sqrt[n]{x_1 \times x_2 \times \ldots \times x_i}$$

<center>
n = number of variables<br>
x₁ = variable 1<br>
x₂ = variable 2<br>
xᵢ = additional variables
</center>

The Product equation used was:

$$\text{p} = x_1 \times x_2 \times \ldots \times x_i$$

<center>
x₁ = variable 1<br>
x₂ = variable 2<br>
xᵢ = additional variables
</center>

The lowest equation used was:

$$\text{l} = \min(x_1, x_2, \ldots, x_i)$$

<center>
x₁ = variable 1<br>
x₂ = variable 2<br>
xᵢ = additional variables
</center>