---
editor_options: 
  markdown: 
    wrap: 72
---

The methods for this application were based largely on suitability modeling detailed 
in [Morris et al. 2021](https://repository.library.noaa.gov/view/noaa/33303){target="_blank"} and
[Farmer et al. 2022](https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0267333){target="_blank"}.
Each data layer was scored on a 0 to 1 scale, with scores approaching 0
representing low suitability and 1 representing high suitability for offshore wind energy
relative to the other grid cells for wind energy. Then a component
suitability score was calculated for each submodel component using the
geometric mean, lowest method, or product method. Next, a final
suitability score was calculated for each submodel by taking the
geometric mean of all scores within each grid cell. The geometric mean
of all submodels was used to calculate a final overall suitability
score. The geometric mean was chosen because it grants
equal importance to each variable and provides a non-biased weighting of
each submodel as they interact with each other (Bovee 1986; Longdill et
al. 2008; Silva et al. 2011; Muñoz-Mas et al. 2012). Furthermore, all
data layers and submodels had equal weight within the suitability model.
However, we also wanted to provide the functionality to weight each
submodel if that is eventually utilized in the future. If you would like to see 
additional information about the data sources, data processing, functions created for this application,
or a common questions guide please visit the [SMORES Documentation](https://noaa-nwfsc.github.io/SMORES/){target="_blank"} site. 