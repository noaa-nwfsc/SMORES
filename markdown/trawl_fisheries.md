---
editor_options: 
  markdown: 
    wrap: 72
---

Use this page to develop a data layer that gives additional considerations for the four trawl fisheries.

These fisheries have little flexibility in where they fish due to their operational logistics and target species’ site fidelity. To represent the space that trawl fisheries need to
reasonably operate, NMFS and ODFW identified grid cells contained within the top 75%, 60%, and 50% of the ranked importance values across these four fisheries. 

NMFS and ODFW provided NCCOS with 5 recommended scoring scenarios for modeling and for BOEM consideration, in order of preference from Scenario 1 (most preferred) to Scenario 5 (least preferred). BOEM selected Scenario 4 because it was the most
conservative scenario that did not include constraints, and suitability scores of 0.001 replaced the geometric mean in grid cells within the top 75% of the trawl fisheries’ ranked importance
values [(NCCOS Report 2.4.4)](https://www.boem.gov/sites/default/files/documents/renewable-energy/state-activities/Appendix%20B_NCCOS%20Final%20WEA%20Report_Oregon.pdf). Areas outside of the trawl fisheries polygon retained the suitability score calculated across all nine fisheries.

|   | Fisheries | Scenario 1 - Trawl \@ 75% | Scenario 2 - Trawl \@ 60% | Scenario 3 - Trawl \@ 50% | Scenario 4 - Trawl \@ 75% | Scenario 5 - "Baseline" |
|-----------|-----------|-----------|-----------|-----------|-----------|-----------|
| 1 | Four trawl fisheries | 0 | 0 | 0 | 0.001 | Ranked |
| 2 | Remainder of Four trawl fisheries (what is not in constraints) | Ranked | Ranked | Ranked | Ranked | Ranked |
| 3 | Groundfish pot gear (primarily sablefish and hagfish) | Ranked | Ranked | Ranked | Ranked | Ranked |
| 4 | Groundfish longline gear (primarily sablefish) | Ranked | Ranked | Ranked | Ranked | Ranked |
| 5 | Commercial troll/hook-and-line albacore (non charter boats) | Ranked | Ranked | Ranked | Ranked | Ranked |
| 6 | Charter vessel albacore troll/hook-and-line | Ranked | Ranked | Ranked | Ranked | Ranked |
| 7 | Dungeness crab | Ranked | Ranked | Ranked | Ranked | Ranked |