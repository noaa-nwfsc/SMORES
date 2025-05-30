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


## set working directory
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

grid_test <- grd.norcal %>% 
  st_drop_geometry()

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

Surveys_fixed_score_full_df <- Surveys_fixed_scored_long %>% 
  st_drop_geometry()

#how would I want to make the data look so I could calculate the geometric mean
#I need it to be in long format so that the scoring addition is tied to the individual columns 

full_data <- grid_test %>% 
  full_join(canyon_score_full_df) %>% 
  full_join(DSC_RH_score_full_df) %>% 
  full_join(Surveys_fixed_score_full_df) %>% 
  sf::st_transform('+proj=longlat +datum=WGS84')

saveRDS(full_data, "U:\\Github\\SMORES\\data\\full_data.rds")

