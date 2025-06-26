#load libraries
library(tidyverse)
library(sf)
library(stars)
library(nominatimlite)
library(dplyr)
library(caret) #for 0-1 normalization function "preProcess"
library(normalize)
library(magrittr)
library(tidyr)


# set working directory
grid.2km <- sf::st_read(dsn = "Z:\\GIS_Base\\OSW\\Grid.gdb", layer = "grid2km")

#set standard coordinate reference system if using 2 km square grid
crsOut <- st_crs(grid.2km)

#convert each layer to sf object
melissa_file_path <- "Z:\\ArcGIS\\Projects\\OWEC\\p30\\nccos_share_CA2.gdb"

Canyons <- sf::st_read(dsn = melissa_file_path, layer = "SubmarineCanyons_WestCoast_diss") %>%
  st_transform(crsOut)
DSC.RobustHigh <- sf::st_read(dsn = melissa_file_path, layer = "DSC_HabitatSuitability_RobustHigh") %>%
  st_transform(crsOut)
Surveys.fixed <- sf::st_read(dsn = melissa_file_path, layer = "Surveys_Operation_Area_fixed") %>%
  st_transform(crsOut)
Surveys.per <- sf::st_read(dsn = melissa_file_path, layer = "Surveys_Operation_Area_periodic") %>%
  st_transform(crsOut)
Seeps <- sf::st_read(dsn = melissa_file_path, layer = "CascadiaSeeps_wBuffer") %>%
  st_transform(crsOut)
ShlfBrk <- sf::st_read(dsn = melissa_file_path, layer = "ContinentalShelfBreak_wBuffer") %>%
  st_transform(crsOut)



#area of Interest for analysis - this is the area that data will be cropped and summarized across
#for Northern CA analysis:
NorCal_bbox <- c(-125.5, 34.5, -123, 43)
NorCal <- bbox_to_poly(NorCal_bbox, crs = 4326)
NorCal <- st_transform(NorCal, crs=crsOut)
#ensure that coordinate reference systems are matching
st_crs(NorCal) == st_crs(grid.2km)
#grid for regions of interest less than full coast
grd.norcal <- st_crop(grid.2km, NorCal)

saveRDS(grd.norcal, "U:\\Github\\SMORES\\data\\2km_grid_norcal.rds")

grid_test <- grd.norcal

#NCCOS values to run in modeling
HAPC.RR.Score = 0.001
HAPC.AOI.Score = 0.01 #updated to 0.01
EFHCA.Score = 0.01 #updated to 0.01
EFHCA.700.Score = 0.01
DSC.RH.Score = 0.1
Seeps.Score = 1.0
ShlfBrk.Score = 1.0
Canyons.Score = 0.1
Surveys.fixed.Score = 1.0
Surveys.per.Score = 1.0

#canyons
Canyons.grid <- sf::st_intersection(Canyons, grd.norcal) %>%
  mutate(Score.Canyons = Canyons.Score) %>%
  mutate(area.part = st_area(.)) %>%
  group_by(CellID_2km) %>% #use for 2km grid
  #group_by(GRID_ID) %>% #use for NCCOS hexagonal grid
  slice_max(area.part, n = 1) %>%
  select(CellID_2km, Score.Canyons) #use for 2km grid
canyon <- Canyons.grid %>%
  st_drop_geometry()
canyon_scored <- grd.norcal %>%
  full_join(canyon, by = "CellID_2km") %>%
  filter(Score.Canyons == 0.1) %>%
  rename("0.1" = Score.Canyons) %>%
  mutate("0.2" = 0.2,
         "0.3" = 0.3,
         "0.4" = 0.4,
         "0.5" = 0.5,
         "0.6" = 0.6,
         "0.7" = 0.7,
         "0.8" = 0.8,
         "0.9" = 0.9,
         "1" = 1) 
#this approach would let us know which score is associated with which layer 
canyons_scored_long <- pivot_longer(canyon_scored, cols = starts_with(c("0.", "1")), names_to = "Canyon", values_to = "Score.Canyon") %>% 
  sf::st_transform('+proj=longlat +datum=WGS84') %>% 
  select(-Canyon)
saveRDS(canyons_scored_long, "U:\\Github\\SMORES\\data\\canyon_scored.rds")
canyon_score_full_df <- canyons_scored_long %>% 
  st_drop_geometry()

#Deep sea coral robust high
DSC.RobustHigh.grid <- sf::st_intersection(DSC.RobustHigh, grd.norcal) %>%
  mutate(Score.DSC.RH = DSC.RH.Score) %>%
  mutate(area.part = st_area(.)) %>%
  group_by(CellID_2km) %>% #use for 2km grid
  #group_by(GRID_ID) %>% #use for NCCOS hexagonal grid
  slice_max(area.part, n = 1) %>%
  select(CellID_2km, Score.DSC.RH) #use for 2km grid
DSC_RH <- DSC.RobustHigh.grid %>%
  st_drop_geometry()
DSC_RH_scored <- grd.norcal %>%
  full_join(DSC_RH, by = "CellID_2km") %>%
  filter(Score.DSC.RH == 0.1) %>%
  rename("0.1" = Score.DSC.RH) %>%
  mutate("0.2" = 0.2,
         "0.3" = 0.3,
         "0.4" = 0.4,
         "0.5" = 0.5,
         "0.6" = 0.6,
         "0.7" = 0.7,
         "0.8" = 0.8,
         "0.9" = 0.9,
         "1" = 1) 
#this approach would let us know which score is associated with which layer 
DSC_RH_scored_long <- pivot_longer(DSC_RH_scored, cols = starts_with(c("0.", "1")), names_to = "DSC_RH", values_to = "Score.DSC_RH") %>% 
  sf::st_transform('+proj=longlat +datum=WGS84') %>% 
  select(-DSC_RH)
saveRDS(DSC_RH_scored_long, "U:\\Github\\SMORES\\data\\DSC_RH_scored.rds")
DSC_RH_score_full_df <- DSC_RH_scored_long %>% 
  st_drop_geometry()

#Surveys fixed
Surveys.fixed.grid <- sf::st_intersection(Surveys.fixed, grd.norcal) %>%
  mutate(Score.Surveys.Fixed = Surveys.fixed.Score) %>%
  mutate(area.part = st_area(.)) %>%
  group_by(CellID_2km) %>% #use for 2km grid 
  #group_by(GRID_ID) %>% #use for NCCOS hexagonal grid
  slice_max(area.part, n = 1) %>%
  select(CellID_2km, Score.Surveys.Fixed) #use for 2km grid
Surveys_fixed <- Surveys.fixed.grid %>%
  st_drop_geometry() %>% 
  group_by(CellID_2km) %>% 
  distinct(CellID_2km, .keep_all = TRUE)
Surveys_fixed_scored <- grd.norcal %>%
  full_join(Surveys_fixed, by = "CellID_2km") %>%
  filter(Score.Surveys.Fixed == 1) %>%
  rename("1" = Score.Surveys.Fixed) %>%
  mutate("0.1" = 0.1,
         "0.2" = 0.2,
         "0.3" = 0.3,
         "0.4" = 0.4,
         "0.5" = 0.5,
         "0.6" = 0.6,
         "0.7" = 0.7,
         "0.8" = 0.8,
         "0.9" = 0.9) 
Surveys_fixed_scored_long <- pivot_longer(Surveys_fixed_scored, cols = starts_with(c("0.", "1")), names_to = "Surveys_fixed", values_to = "Score.Surveys_fixed") %>% 
  sf::st_transform('+proj=longlat +datum=WGS84') %>% 
  select(-Surveys_fixed)
st_crs(DSC_RH_scored_long) == st_crs(Surveys_fixed_scored_long)
saveRDS(Surveys_fixed_scored_long, "U:\\Github\\SMORES\\data\\Surveys_fixed_scored.rds")

#Surveys periodic grid
Surveys.per <- sf::st_intersection(Surveys.per, grd.norcal) %>%
  mutate(Score.Surveys.Per = Surveys.per.Score) %>%
  mutate(area.part = st_area(.)) %>%
  group_by(CellID_2km) %>% #use for 2km grid 
  #group_by(GRID_ID) %>% #use for NCCOS hexagonal grid
  slice_max(area.part, n = 1) %>%
  select(CellID_2km, Score.Surveys.Per) #use for 2km grid
Surveys_periodic <- Surveys.per %>%
  st_drop_geometry() %>% 
  group_by(CellID_2km) %>% 
  distinct(CellID_2km, .keep_all = TRUE)
Surveys_periodic_scored <- grd.norcal %>%
  full_join(Surveys_periodic, by = "CellID_2km") %>%
  filter(Score.Surveys.Per == 1) %>%
  rename("1" = Score.Surveys.Per) %>%
  mutate("0.1" = 0.1,
         "0.2" = 0.2,
         "0.3" = 0.3,
         "0.4" = 0.4,
         "0.5" = 0.5,
         "0.6" = 0.6,
         "0.7" = 0.7,
         "0.8" = 0.8,
         "0.9" = 0.9) 
Surveys_periodic_scored_long <- pivot_longer(Surveys_periodic_scored, cols = starts_with(c("0.", "1")), names_to = "Surveys_periodic", values_to = "Score.Surveys_periodic") %>% 
  sf::st_transform('+proj=longlat +datum=WGS84') %>% 
  select(-Surveys_periodic)
st_crs(DSC_RH_scored_long) == st_crs(Surveys_periodic_scored_long)
saveRDS(Surveys_periodic_scored_long, "U:\\Github\\SMORES\\data\\Surveys_periodic_scored.rds")

#Seeps
Seeps.grid <- sf::st_intersection(Seeps, grd.norcal) %>%
  mutate(Score.Seeps = Seeps.Score) %>%
  mutate(area.part = st_area(.)) %>%
  group_by(CellID_2km) %>% #use for 2km grid 
  #group_by(GRID_ID) %>% #use for NCCOS hexagonal grid
  slice_max(area.part, n = 1) %>%
  select(CellID_2km, Score.Seeps) #use for 2km grid
Seeps_score <- Seeps.grid %>%
  st_drop_geometry() %>% 
  group_by(CellID_2km) %>% 
  distinct(CellID_2km, .keep_all = TRUE)
Seeps_scored <- grd.norcal %>%
  full_join(Seeps_score, by = "CellID_2km") %>%
  filter(Score.Seeps == 1) %>%
  rename("1" = Score.Seeps) %>%
  mutate("0.1" = 0.1,
         "0.2" = 0.2,
         "0.3" = 0.3,
         "0.4" = 0.4,
         "0.5" = 0.5,
         "0.6" = 0.6,
         "0.7" = 0.7,
         "0.8" = 0.8,
         "0.9" = 0.9) 
Seeps_scored_long <- pivot_longer(Seeps_scored, cols = starts_with(c("0.", "1")), names_to = "Seeps", values_to = "Score.Seeps") %>% 
  sf::st_transform('+proj=longlat +datum=WGS84') %>% 
  select(-Seeps)
st_crs(DSC_RH_scored_long) == st_crs(Seeps_scored_long)
saveRDS(Seeps_scored_long, "U:\\Github\\SMORES\\data\\Seeps_scored.rds")

#Shelfbreaks
ShlfBrk.grid <- sf::st_intersection(ShlfBrk, grd.norcal) %>%
  mutate(Score.ShlfBrk = ShlfBrk.Score) %>%
  mutate(area.part = st_area(.)) %>%
  group_by(CellID_2km) %>% #use for 2km grid 
  #group_by(GRID_ID) %>% #use for NCCOS hexagonal grid
  slice_max(area.part, n = 1) %>%
  select(CellID_2km, Score.ShlfBrk) #use for 2km grid
ShlfBrk_score <- ShlfBrk.grid %>%
  st_drop_geometry() %>% 
  group_by(CellID_2km) %>% 
  distinct(CellID_2km, .keep_all = TRUE)
ShlfBrk_scored <- grd.norcal %>%
  full_join(ShlfBrk_score, by = "CellID_2km") %>%
  filter(Score.ShlfBrk == 1) %>%
  rename("1" = Score.ShlfBrk) %>%
  mutate("0.1" = 0.1,
         "0.2" = 0.2,
         "0.3" = 0.3,
         "0.4" = 0.4,
         "0.5" = 0.5,
         "0.6" = 0.6,
         "0.7" = 0.7,
         "0.8" = 0.8,
         "0.9" = 0.9) 
ShlfBrk_scored_long <- pivot_longer(ShlfBrk_scored, cols = starts_with(c("0.", "1")), names_to = "ShlfBrk", values_to = "Score.ShlfBrk") %>% 
  sf::st_transform('+proj=longlat +datum=WGS84') %>% 
  select(-ShlfBrk)
st_crs(DSC_RH_scored_long) == st_crs(ShlfBrk_scored_long)
saveRDS(ShlfBrk_scored_long, "U:\\Github\\SMORES\\data\\ShlfBrk_scored.rds")

#EFHCA
EFHCA.grid <- readRDS("U:\\Github\\OWEC\\data\\Lyr_EFHCA.rds")
saveRDS(EFHCA.grid, "U:\\Github\\SMORES\\data\\EFHCA_grid.rds" )
EFHCA_score <- EFHCA.grid %>%
  st_drop_geometry() %>% 
  group_by(CellID_2km) %>% 
  distinct(CellID_2km, .keep_all = TRUE)
EFHCA_scored <- grd.norcal %>%
  full_join(EFHCA_score, by = "CellID_2km") %>%
  filter(Score.EFHCA == 0.01) %>%
  rename("0.01" = Score.EFHCA) %>%
  mutate("0.1" = 0.1,
         "0.2" = 0.2,
         "0.3" = 0.3,
         "0.4" = 0.4,
         "0.5" = 0.5,
         "0.6" = 0.6,
         "0.7" = 0.7,
         "0.8" = 0.8,
         "0.9" = 0.9,
         "1" = 1) %>% 
  select(-"0.01")
EFHCA_scored_long <- pivot_longer(EFHCA_scored, cols = starts_with(c("0.", "1")), names_to = "EFHCA", values_to = "Score.EFHCA") %>% 
  sf::st_transform('+proj=longlat +datum=WGS84') %>% 
  select(-EFHCA)
st_crs(DSC_RH_scored_long) == st_crs(EFHCA_scored_long)
saveRDS(EFHCA_scored_long, "U:\\Github\\SMORES\\data\\EFHCA_scored.rds")

#EFHCA.700
EFHCA_700.grid <- readRDS("U:\\Github\\OWEC\\data\\Lyr_EFHCA700.rds")
saveRDS(EFHCA_700.grid, "U:\\Github\\SMORES\\data\\EFHCA_grid.rds" )
EFHCA_700_score <- EFHCA_700.grid %>%
  st_drop_geometry() %>% 
  group_by(CellID_2km) %>% 
  distinct(CellID_2km, .keep_all = TRUE)
EFHCA_700_scored <- grd.norcal %>%
  full_join(EFHCA_700_score, by = "CellID_2km") %>%
  filter(Score.EFHCA.700 == 0.01) %>%
  rename("0.01" = Score.EFHCA.700) %>%
  mutate("0.1" = 0.1,
         "0.2" = 0.2,
         "0.3" = 0.3,
         "0.4" = 0.4,
         "0.5" = 0.5,
         "0.6" = 0.6,
         "0.7" = 0.7,
         "0.8" = 0.8,
         "0.9" = 0.9,
         "1" = 1) %>% 
  select(-"0.01")
EFHCA_700_scored_long <- pivot_longer(EFHCA_700_scored, cols = starts_with(c("0.", "1")), names_to = "EFHCA_700", values_to = "Score.EFHCA.700") %>% 
  sf::st_transform('+proj=longlat +datum=WGS84') %>% 
  select(-EFHCA_700)
st_crs(DSC_RH_scored_long) == st_crs(EFHCA_700_scored_long)
saveRDS(EFHCA_700_scored_long, "U:\\Github\\SMORES\\data\\EFHCA_700_scored.rds")

# HAPCAOI
HAPCaoi.grid <- readRDS("U:\\Github\\OWEC\\data\\Lyr_HAPCaoi.rds")
saveRDS(HAPCaoi.grid, "U:\\Github\\SMORES\\data\\HAPCaoi_grid.rds" )
HAPCaoi_score <- HAPCaoi.grid %>%
  st_drop_geometry() %>% 
  group_by(CellID_2km) %>% 
  distinct(CellID_2km, .keep_all = TRUE)
HAPCaoi_scored <- grd.norcal %>%
  full_join(HAPCaoi_score, by = "CellID_2km") %>%
  filter(Score.HAPC.AOI == 0.01) %>%
  rename("0.01" = Score.HAPC.AOI) %>%
  mutate("0.1" = 0.1,
         "0.2" = 0.2,
         "0.3" = 0.3,
         "0.4" = 0.4,
         "0.5" = 0.5,
         "0.6" = 0.6,
         "0.7" = 0.7,
         "0.8" = 0.8,
         "0.9" = 0.9,
         "1" = 1) %>% 
  select(-"0.01")
HAPCaoi_scored_long <- pivot_longer(HAPCaoi_scored, cols = starts_with(c("0.", "1")), names_to = "HAPCaoi", values_to = "Score.HAPC.AOI") %>% 
  sf::st_transform('+proj=longlat +datum=WGS84') %>% 
  select(-HAPCaoi)
st_crs(DSC_RH_scored_long) == st_crs(HAPCaoi_scored_long)
saveRDS(HAPCaoi_scored_long, "U:\\Github\\SMORES\\data\\HAPCaoi_scored.rds")

#HAPCReef
HAPCreef.grid <- readRDS("U:\\Github\\OWEC\\data\\Lyr_HAPCReef.rds")
saveRDS(HAPCreef.grid, "U:\\Github\\SMORES\\data\\HAPCreef_grid.rds" )
HAPCreef_score <- HAPCreef.grid %>%
  st_drop_geometry() %>% 
  group_by(CellID_2km) %>% 
  distinct(CellID_2km, .keep_all = TRUE)
HAPCreef_scored <- grd.norcal %>%
  full_join(HAPCreef_score, by = "CellID_2km") %>%
  filter(Score.HAPC.Reef == 0.001) %>%
  rename("0.001" = Score.HAPC.Reef) %>%
  mutate("0.1" = 0.1,
         "0.2" = 0.2,
         "0.3" = 0.3,
         "0.4" = 0.4,
         "0.5" = 0.5,
         "0.6" = 0.6,
         "0.7" = 0.7,
         "0.8" = 0.8,
         "0.9" = 0.9,
         "1" = 1) %>% 
  select(-"0.001")
HAPCreef_scored_long <- pivot_longer(HAPCreef_scored, cols = starts_with(c("0.", "1")), names_to = "HAPCreef", values_to = "Score.HAPC.Reef") %>% 
  sf::st_transform('+proj=longlat +datum=WGS84') %>% 
  select(-HAPCreef)
st_crs(DSC_RH_scored_long) == st_crs(HAPCreef_scored_long)
saveRDS(HAPCreef_scored_long, "U:\\Github\\SMORES\\data\\HAPCreef_scored.rds")

## Species Layers
ESA_Critical_Habitat <- sf::st_read(dsn = "Z:\\ArcGIS\\Projects\\OWEC\\p30\\protected_species_density_layers\\nmfs_combined\\Critical_Habitat_Areas_by_the_National_Marine_Fisheries_Service\\Critical_Habitat_Areas_by_the_National_Marine_Fisheries_Service.shp")  

#killer whale 
killer_whale <- ESA_Critical_Habitat %>% 
  filter(commonName == "Whale, killer") %>% 
  select(-c(statusType, scientific, listingSta, federalReg)) %>% 
  st_transform(crsOut) 
killer_whale.grid <- sf::st_intersection(killer_whale, grd.norcal) %>%
  mutate(Score.killer_whale = 1) %>% 
  mutate(area.part = st_area(.)) %>%
  group_by(CellID_2km) %>% #use for 2km grid 
  #group_by(GRID_ID) %>% #use for NCCOS hexagonal grid
  slice_max(area.part, n = 1) %>%
  select(CellID_2km, Score.killer_whale) #use for 2km grid
killer_whale_score <- killer_whale.grid %>% #no duplicates
  st_drop_geometry() %>% 
  group_by(CellID_2km) %>% 
  distinct(CellID_2km, .keep_all = TRUE)
killer_whale_scored <- grd.norcal %>%
  full_join(killer_whale_score, by = "CellID_2km") %>%
  filter(Score.killer_whale == 1) %>%
  rename("1" = Score.killer_whale) %>%
  mutate("0.1" = 0.1,
         "0.2" = 0.2,
         "0.3" = 0.3,
         "0.4" = 0.4,
         "0.5" = 0.5,
         "0.6" = 0.6,
         "0.7" = 0.7,
         "0.8" = 0.8,
         "0.9" = 0.9) 
killer_whale_scored_long <- pivot_longer(killer_whale_scored, cols = starts_with(c("0.", "1")), names_to = "killer_whale", values_to = "Score.killer_whale") %>% 
  sf::st_transform('+proj=longlat +datum=WGS84') %>% 
  select(-killer_whale)
st_crs(DSC_RH_scored_long) == st_crs(killer_whale_scored_long)
saveRDS(killer_whale_scored_long, "U:\\Github\\SMORES\\data\\killer_whale_scored.rds")

# leatherback sea turtle 
leatherback_turtle <- ESA_Critical_Habitat %>% 
  filter(commonName == "Sea turtle, leatherback") %>% 
  select(-c(statusType, scientific, listingSta, federalReg)) %>% 
  st_transform(crsOut) 
leatherback_turtle.grid <- sf::st_intersection(leatherback_turtle, grd.norcal) %>%
  mutate(Score.leatherback_turtle = 1) %>% 
  mutate(area.part = st_area(.)) %>%
  group_by(CellID_2km) %>% #use for 2km grid 
  #group_by(GRID_ID) %>% #use for NCCOS hexagonal grid
  slice_max(area.part, n = 1) %>%
  select(CellID_2km, Score.leatherback_turtle) #use for 2km grid
leatherback_turtle_score <- leatherback_turtle.grid %>% #no duplicates
  st_drop_geometry() %>% 
  group_by(CellID_2km) %>% 
  distinct(CellID_2km, .keep_all = TRUE)
leatherback_turtle_scored <- grd.norcal %>%
  full_join(leatherback_turtle_score, by = "CellID_2km") %>%
  filter(Score.leatherback_turtle == 1) %>%
  rename("1" = Score.leatherback_turtle) %>%
  mutate("0.1" = 0.1,
         "0.2" = 0.2,
         "0.3" = 0.3,
         "0.4" = 0.4,
         "0.5" = 0.5,
         "0.6" = 0.6,
         "0.7" = 0.7,
         "0.8" = 0.8,
         "0.9" = 0.9) 
leatherback_turtle_scored_long <- pivot_longer(leatherback_turtle_scored, cols = starts_with(c("0.", "1")), names_to = "leatherback_turtle", values_to = "Score.leatherback_turtle") %>% 
  sf::st_transform('+proj=longlat +datum=WGS84') %>% 
  select(-leatherback_turtle)
st_crs(DSC_RH_scored_long) == st_crs(leatherback_turtle_scored_long)
saveRDS(leatherback_turtle_scored_long, "U:\\Github\\SMORES\\data\\leatherback_turtle_scored.rds")

#humpback whale - mexico and central dps
#killer whale 
humpback_whale <- ESA_Critical_Habitat %>% 
  filter(commonName == "Whale, humpback") %>% 
  select(-c(statusType, scientific, listingSta, federalReg)) %>% 
  st_transform(crsOut) 
# humpback_whale.grid <- sf::st_intersection(humpback_whale, grd.norcal) %>%
#   mutate(Score.humpback_whale = 1) %>%
#   mutate(area.part = st_area(.)) %>%
#   group_by(CellID_2km) %>% #use for 2km grid
#   #group_by(GRID_ID) %>% #use for NCCOS hexagonal grid
#   slice_max(area.part, n = 1) %>%
#   select(CellID_2km, Score.humpback_whale) #use for 2km grid

#Error: 
# Error in scan(text = lst[[length(lst)]], quiet = TRUE) : 
# scan() expected 'a real', got '936246.12987604213.'
# Error in (function (msg)  : 
# TopologyException: side location conflict at 92746.754168340936 936246.12987604213. This can occur if the input geometry is invalid.

# returns false for row 5 and 17 of data
st_is_valid(humpback_whale)

# returns that the two false rows have a self-intersection [92746.7541682779 936246.129876046]
st_is_valid(humpback_whale, reason = TRUE)

# suggestion is to apply a buffer of 0 to force the creation of a new geometry -> stack overflow article https://stackoverflow.com/questions/66584191/sfst-intersection-virtually-random-error-action
humpback_whale_buffer <- humpback_whale %>% 
  st_buffer(0)

# buffer has made all geometries valid
st_is_valid(humpback_whale_buffer, reason = TRUE)

humpback_whale.grid <- sf::st_intersection(humpback_whale_buffer, grd.norcal) %>%
  mutate(Score.humpback_whale = 1) %>%
  mutate(area.part = st_area(.)) %>%
  group_by(CellID_2km) %>% #use for 2km grid
  #group_by(GRID_ID) %>% #use for NCCOS hexagonal grid
  slice_max(area.part, n = 1) %>%
  select(CellID_2km, Score.humpback_whale) #use for 2km grid

humpback_whale_score <- humpback_whale.grid %>% #had 16354 duplicates
  st_drop_geometry() %>% 
  group_by(CellID_2km) %>% 
  distinct(CellID_2km, .keep_all = TRUE)

humpback_whale_scored <- grd.norcal %>%
  full_join(humpback_whale_score, by = "CellID_2km") %>%
  filter(Score.humpback_whale == 1) %>%
  rename("1" = Score.humpback_whale) %>%
  mutate("0.1" = 0.1,
         "0.2" = 0.2,
         "0.3" = 0.3,
         "0.4" = 0.4,
         "0.5" = 0.5,
         "0.6" = 0.6,
         "0.7" = 0.7,
         "0.8" = 0.8,
         "0.9" = 0.9) 
humpback_whale_scored_long <- pivot_longer(humpback_whale_scored, cols = starts_with(c("0.", "1")), names_to = "humpback_whale", values_to = "Score.humpback_whale") %>% 
  sf::st_transform('+proj=longlat +datum=WGS84') %>% 
  select(-humpback_whale)
st_crs(DSC_RH_scored_long) == st_crs(humpback_whale_scored_long)
saveRDS(humpback_whale_scored_long, "U:\\Github\\SMORES\\data\\humpback_whale_scored.rds")
