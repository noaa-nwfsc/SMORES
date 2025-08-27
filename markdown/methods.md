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

$$GM = \left(\prod_{i=1}^{n} x_i\right)^{\frac{1}{n}}$$
