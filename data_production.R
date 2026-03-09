# #load libraries
# library(tidyverse)
# library(sf)
# library(stars)
# library(nominatimlite)
# library(dplyr)
# library(caret) #for 0-1 normalization function "preProcess"
# library(normalize)
# library(magrittr)
# library(tidyr)
# library(arcgis)
# library(arrow)
# library(sfarrow)

# # get base grids
# # 2km
# grid.2km <- sf::st_read(
#   dsn = "G:\\Shared drives\\NMFS NWC FRAM GIS\\GIS_Base\\Grid.gdb",
#   layer = "grid2km_ETsquare"
# )

# #5km
# grid.5km <- sf::st_read(
#   dsn = "G:\\Shared drives\\NMFS NWC FRAM GIS\\GIS_Base\\Grid.gdb",
#   layer = "grid5km_ETsquare"
# )

# st_write_parquet(
#   grid.2km,
#   "C:\\GitHub\\SMORES\\data\\2km\\grid_full.parquet"
# )

# st_write_parquet(
#   grid.5km,
#   "C:\\GitHub\\SMORES\\data\\5km\\grid_full.parquet"
# )

# # get base grid as a dataframe
# base_grid_df <- grid.5km %>%
#   st_drop_geometry()

# write_parquet(
#   base_grid_df,
#   "C:\\GitHub\\SMORES\\data\\5km\\base_grid_df.parquet"
# )

# # set standard coordinate reference system if using 2 km square grid
# crsOut <- st_crs(grid.5km)

# #convert each layer to sf object
# melissa_file_path <- "G:\\Shared drives\\NMFS NWC OEI GIS\\ArcGIS\\Projects\\OWEC\\p30\\nccos_share_CA2.gdb"

# Canyons <- sf::st_read(
#   dsn = melissa_file_path,
#   layer = "SubmarineCanyons_WestCoast_diss"
# ) %>%
#   st_transform(crsOut)

# DSC.RobustHigh <- sf::st_read(
#   dsn = melissa_file_path,
#   layer = "DSC_HabitatSuitability_RobustHigh"
# ) %>%
#   st_transform(crsOut)
# Surveys.fixed <- sf::st_read(
#   dsn = melissa_file_path,
#   layer = "Surveys_Operation_Area_fixed"
# ) %>%
#   st_transform(crsOut)
# Surveys.per <- sf::st_read(
#   dsn = melissa_file_path,
#   layer = "Surveys_Operation_Area_periodic"
# ) %>%
#   st_transform(crs = st_crs(grid_test))
# Seeps <- sf::st_read(
#   dsn = melissa_file_path,
#   layer = "CascadiaSeeps_wBuffer"
# ) %>%
#   st_transform(crsOut)
# ShlfBrk <- sf::st_read(
#   dsn = melissa_file_path,
#   layer = "ContinentalShelfBreak_wBuffer"
# ) %>%
#   st_transform(crsOut)
# EFHCA <- sf::st_read(
#   dsn = melissa_file_path,
#   layer = "EFH_Groundfish_ConsArea_diss"
# ) %>%
#   st_transform(crs = st_crs(grid_test))

# EFHCA_700 <- sf::st_read(
#   dsn = melissa_file_path,
#   layer = "EFH_Groundfish_ConsArea_700fm"
# ) %>%
#   st_transform(crs = st_crs(grid_test))
# HAPCaoi <- sf::st_read(
#   dsn = melissa_file_path,
#   layer = "EFH_Groundfish_HAPC_AOI"
# ) %>%
#   st_transform(crs = st_crs(grid_test))
# HAPCreef <- sf::st_read(
#   dsn = melissa_file_path,
#   layer = "EFH_Groundfish_HAPC_RockyReef_v20250627_diss"
# ) %>%
#   st_transform(crs = st_crs(grid_test))

# grid_test <- grid.5km

# #NCCOS values to run in modeling
# HAPC.RR.Score = 0.001
# HAPC.AOI.Score = 0.01 #updated to 0.01
# EFHCA.Score = 0.01 #updated to 0.01
# EFHCA.700.Score = 0.01
# DSC.RH.Score = 0.1
# Seeps.Score = 1.0
# ShlfBrk.Score = 1.0
# Canyons.Score = 0.1
# Surveys.fixed.Score = 1.0
# Surveys.per.Score = 1.0

# #canyons
# Canyons.grid <- sf::st_intersection(Canyons, grid_test) %>%
#   mutate(Score.Canyons = Canyons.Score) %>%
#   mutate(area.part = st_area(.)) %>%
#   group_by(CellID_5km) %>% #use for 2km grid
#   #group_by(GRID_ID) %>% #use for NCCOS hexagonal grid
#   slice_max(area.part, n = 1) %>%
#   select(CellID_5km, Score.Canyons) #use for 2km grid
# canyon <- Canyons.grid %>%
#   st_drop_geometry()

# grid_test_df <- grid_test %>%
#   st_drop_geometry()

# canyon_scored <- grid_test %>%
#   full_join(canyon, by = "CellID_5km") %>%
#   filter(Score.Canyons == 0.1) %>%
#   rename("0.1" = Score.Canyons) %>%
#   mutate(
#     "0" = 0,
#     "0.01" = 0.01,
#     "0.001" = 0.001,
#     "0.2" = 0.2,
#     "0.3" = 0.3,
#     "0.4" = 0.4,
#     "0.5" = 0.5,
#     "0.6" = 0.6,
#     "0.7" = 0.7,
#     "0.8" = 0.8,
#     "0.9" = 0.9,
#     "1" = 1
#   )
# #save data to capture all static values a user can select
# canyons_scored_long <- pivot_longer(
#   canyon_scored,
#   cols = starts_with(c("0", "1")),
#   names_to = "Canyon",
#   values_to = "Score.Canyon"
# ) %>%
#   sf::st_transform(4326) %>%
#   select(-Canyon) %>%
#   st_zm()

# st_write_parquet(
#   canyons_scored_long,
#   "C:\\GitHub\\SMORES\\data\\5km\\canyon_scored_full_5km.parquet"
# )
# canyons_scored_long_reread <- st_read_parquet(
#   "C:\\GitHub\\SMORES\\data\\canyon_scored_full.parquet"
# )

# canyons_scored_crs <- canyons_scored_long_reread %>%
#   sf::st_transform(4326) %>%
#   st_zm()

# st_crs(canyons_scored_crs)

# #Deep sea coral robust high - user selected score data set
# DSC.RobustHigh.grid <- sf::st_intersection(DSC.RobustHigh, grid_test) %>%
#   mutate(Score.DSC.RH = DSC.RH.Score) %>%
#   mutate(area.part = st_area(.)) %>%
#   group_by(CellID_5km) %>% #use for 2km grid
#   #group_by(GRID_ID) %>% #use for NCCOS hexagonal grid
#   slice_max(area.part, n = 1) %>%
#   select(CellID_5km, Score.DSC.RH)
# DSC_RH <- DSC.RobustHigh.grid %>%
#   st_drop_geometry()
# DSC_RH_scored <- grid_test %>%
#   full_join(DSC_RH, by = "CellID_5km") %>%
#   filter(Score.DSC.RH == 0.1) %>%
#   rename("0.1" = Score.DSC.RH) %>%
#   mutate(
#     "0" = 0,
#     "0.01" = 0.01,
#     "0.001" = 0.001,
#     "0.2" = 0.2,
#     "0.3" = 0.3,
#     "0.4" = 0.4,
#     "0.5" = 0.5,
#     "0.6" = 0.6,
#     "0.7" = 0.7,
#     "0.8" = 0.8,
#     "0.9" = 0.9,
#     "1" = 1
#   )
# #this approach would let us know which score is associated with which layer
# DSC_RH_scored_long <- pivot_longer(
#   DSC_RH_scored,
#   cols = starts_with(c("0", "1")),
#   names_to = "DSC_RH",
#   values_to = "Score.DSC_RH"
# ) %>%
#   sf::st_transform('+proj=longlat +datum=WGS84') %>%
#   select(-DSC_RH)

# st_write_parquet(
#   DSC_RH_scored_long,
#   "C:\\GitHub\\SMORES\\data\\5km\\DSC_RH_scored_full_5km.parquet"
# )

# # Deep sea coral robust high - user selected z membership scoring option
# DSC.RobustHigh.grid_z_membership <- sf::st_intersection(
#   DSC.RobustHigh,
#   grid_test
# ) %>%
#   mutate(Score.DSC.RH = DSC.RH.Score) %>%
#   mutate(area.part = st_area(.)) %>%
#   group_by(CellID_5km) %>% #use for 2km grid
#   #group_by(GRID_ID) %>% #use for NCCOS hexagonal grid
#   slice_max(area.part, n = 1) %>%
#   select(CellID_5km, gridcode) #grid code is rasters original assigned score that will be used for z-membership
# DSC_RH_z_membership <- DSC.RobustHigh.grid_z_membership %>%
#   st_drop_geometry()
# #function to create z-membership scores for each column in df
# #df is dataframe to pull column from, col_name is the column you would like to apply score to, and b_variance is the value added depending on indicator
# z_membership <- function(df, col_name, b_variance) {
#   #assign col_names so they can be used to index
#   col_name <- enquo(col_name)

#   zmf.sum <- c()
#   obs.sum <- df %>% pull((!!col_name))
#   a = min(obs.sum, na.rm = TRUE)
#   b = max(obs.sum, na.rm = TRUE) + b_variance #most likely 0.001, but could be 0.000001 for indicators with very small values
#   for (i in 1:length(obs.sum)) {
#     if (is.na(obs.sum[i])) {
#       zmf.sum[i] = 1
#     } else if (obs.sum[i] <= a) {
#       zmf.sum[i] = 1
#     } else if (obs.sum[i] <= (a + b) / 2) {
#       zmf.sum[i] = 1 - 2 * ((obs.sum[i] - a) / (b - a))^2
#     } else if (obs.sum[i] <= b) {
#       zmf.sum[i] = 2 * ((obs.sum[i] - b) / (b - a))^2
#     } else if (obs.sum[i] >= b) {
#       zmf.sum[i] = 0
#     }
#   }

#   return(zmf.sum)
# }

# # Normalizing Deep Sea Coral Habitat Suitability
# DSC_RH_z_membership$Score.Z_Membership <- z_membership(
#   DSC_RH_z_membership,
#   'gridcode',
#   0.001
# )

# DSC_RH_z_membership_score <- grid_test %>%
#   full_join(DSC_RH_z_membership, by = "CellID_5km") %>%
#   sf::st_transform('+proj=longlat +datum=WGS84') %>%
#   filter(!is.na(gridcode))

# st_write_parquet(
#   DSC_RH_z_membership_score,
#   "C:\\GitHub\\SMORES\\data\\5km\\DSC_RH_z_membership_scored_full_5km.parquet"
# )

# #Surveys fixed
# Surveys.fixed.grid <- sf::st_intersection(Surveys.fixed, grid_test) %>%
#   mutate(Score.Surveys.Fixed = Surveys.fixed.Score) %>%
#   mutate(area.part = st_area(.)) %>%
#   group_by(CellID_5km) %>% #use for 2km grid
#   #group_by(GRID_ID) %>% #use for NCCOS hexagonal grid
#   slice_max(area.part, n = 1) %>%
#   select(CellID_5km, Score.Surveys.Fixed) #use for 2km grid

# Surveys_fixed <- Surveys.fixed.grid %>%
#   st_drop_geometry() %>%
#   group_by(CellID_5km) %>%
#   distinct(CellID_5km, .keep_all = TRUE)
# Surveys_fixed_scored <- grid_test %>%
#   full_join(Surveys_fixed, by = "CellID_5km") %>%
#   filter(Score.Surveys.Fixed == 1) %>%
#   rename("1" = Score.Surveys.Fixed) %>%
#   mutate(
#     "0" = 0,
#     "0.01" = 0.01,
#     "0.001" = 0.001,
#     "0.1" = 0.1,
#     "0.2" = 0.2,
#     "0.3" = 0.3,
#     "0.4" = 0.4,
#     "0.5" = 0.5,
#     "0.6" = 0.6,
#     "0.7" = 0.7,
#     "0.8" = 0.8,
#     "0.9" = 0.9
#   )
# Surveys_fixed_scored_long <- pivot_longer(
#   Surveys_fixed_scored,
#   cols = starts_with(c("0", "1")),
#   names_to = "Surveys_fixed",
#   values_to = "Score.Surveys_fixed"
# ) %>%
#   sf::st_transform('+proj=longlat +datum=WGS84') %>%
#   select(-Surveys_fixed)

# st_write_parquet(
#   Surveys_fixed_scored_long,
#   "C:\\GitHub\\SMORES\\data\\5km\\Surveys_fixed_scored_full.parquet"
# )

# #Surveys periodic grid
# Surveys.per <- sf::st_intersection(Surveys.per, grid_test) %>%
#   mutate(Score.Surveys.Per = Surveys.per.Score) %>%
#   mutate(area.part = st_area(.)) %>%
#   group_by(CellID_5km) %>% #use for 2km grid
#   #group_by(GRID_ID) %>% #use for NCCOS hexagonal grid
#   slice_max(area.part, n = 1) %>%
#   select(CellID_5km, Score.Surveys.Per) #use for 2km grid
# Surveys_periodic <- Surveys.per %>%
#   st_drop_geometry() %>%
#   group_by(CellID_5km) %>%
#   distinct(CellID_5km, .keep_all = TRUE)
# Surveys_periodic_scored <- grid_test %>%
#   full_join(Surveys_periodic, by = "CellID_5km") %>%
#   filter(Score.Surveys.Per == 1) %>%
#   rename("1" = Score.Surveys.Per) %>%
#   mutate(
#     "0" = 0,
#     "0.01" = 0.01,
#     "0.001" = 0.001,
#     "0.1" = 0.1,
#     "0.2" = 0.2,
#     "0.3" = 0.3,
#     "0.4" = 0.4,
#     "0.5" = 0.5,
#     "0.6" = 0.6,
#     "0.7" = 0.7,
#     "0.8" = 0.8,
#     "0.9" = 0.9
#   )
# Surveys_periodic_scored_long <- pivot_longer(
#   Surveys_periodic_scored,
#   cols = starts_with(c("0", "1")),
#   names_to = "Surveys_periodic",
#   values_to = "Score.Surveys_periodic"
# ) %>%
#   sf::st_transform('+proj=longlat +datum=WGS84') %>%
#   select(-Surveys_periodic)

# st_write_parquet(
#   Surveys_periodic_scored_long,
#   "C:\\GitHub\\SMORES\\data\\5km\\Surveys_periodic_scored_full.parquet"
# )

# #Seeps
# Seeps.grid <- sf::st_intersection(Seeps, grid_test) %>%
#   mutate(Score.Seeps = Seeps.Score) %>%
#   mutate(area.part = st_area(.)) %>%
#   group_by(CellID_5km) %>% #use for 2km grid
#   #group_by(GRID_ID) %>% #use for NCCOS hexagonal grid
#   slice_max(area.part, n = 1) %>%
#   select(CellID_5km, Score.Seeps) #use for 2km grid
# Seeps_score <- Seeps.grid %>%
#   st_drop_geometry() %>%
#   group_by(CellID_5km) %>%
#   distinct(CellID_5km, .keep_all = TRUE)
# Seeps_scored <- grid_test %>%
#   full_join(Seeps_score, by = "CellID_5km") %>%
#   filter(Score.Seeps == 1) %>%
#   rename("1" = Score.Seeps) %>%
#   mutate(
#     "0" = 0,
#     "0.01" = 0.01,
#     "0.001" = 0.001,
#     "0.1" = 0.1,
#     "0.2" = 0.2,
#     "0.3" = 0.3,
#     "0.4" = 0.4,
#     "0.5" = 0.5,
#     "0.6" = 0.6,
#     "0.7" = 0.7,
#     "0.8" = 0.8,
#     "0.9" = 0.9
#   )
# Seeps_scored_long <- pivot_longer(
#   Seeps_scored,
#   cols = starts_with(c("0", "1")),
#   names_to = "Seeps",
#   values_to = "Score.Seeps"
# ) %>%
#   sf::st_transform('+proj=longlat +datum=WGS84') %>%
#   select(-Seeps)
# st_crs(DSC_RH_scored_long) == st_crs(Seeps_scored_long)

# st_write_parquet(
#   Seeps_scored_long,
#   "C:\\GitHub\\SMORES\\data\\5km\\Seeps_scored_full_5km.parquet"
# )

# #Shelfbreaks
# ShlfBrk.grid <- sf::st_intersection(ShlfBrk, grid_test) %>%
#   mutate(Score.ShlfBrk = ShlfBrk.Score) %>%
#   mutate(area.part = st_area(.)) %>%
#   group_by(CellID_5km) %>% #use for 2km grid
#   #group_by(GRID_ID) %>% #use for NCCOS hexagonal grid
#   slice_max(area.part, n = 1) %>%
#   select(CellID_5km, Score.ShlfBrk) #use for 2km grid
# ShlfBrk_score <- ShlfBrk.grid %>%
#   st_drop_geometry() %>%
#   group_by(CellID_5km) %>%
#   distinct(CellID_5km, .keep_all = TRUE)
# ShlfBrk_scored <- grid_test %>%
#   full_join(ShlfBrk_score, by = "CellID_5km") %>%
#   filter(Score.ShlfBrk == 1) %>%
#   rename("1" = Score.ShlfBrk) %>%
#   mutate(
#     "0" = 0,
#     "0.01" = 0.01,
#     "0.001" = 0.001,
#     "0.1" = 0.1,
#     "0.2" = 0.2,
#     "0.3" = 0.3,
#     "0.4" = 0.4,
#     "0.5" = 0.5,
#     "0.6" = 0.6,
#     "0.7" = 0.7,
#     "0.8" = 0.8,
#     "0.9" = 0.9
#   )
# ShlfBrk_scored_long <- pivot_longer(
#   ShlfBrk_scored,
#   cols = starts_with(c("0", "1")),
#   names_to = "ShlfBrk",
#   values_to = "Score.ShlfBrk"
# ) %>%
#   sf::st_transform('+proj=longlat +datum=WGS84') %>%
#   select(-ShlfBrk)
# st_crs(DSC_RH_scored_long) == st_crs(ShlfBrk_scored_long)

# st_write_parquet(
#   ShlfBrk_scored_long,
#   "C:\\GitHub\\SMORES\\data\\5km\\ShlfBrk_scored_full_5km.parquet"
# )

# #EFHCA
# EFHCA.grid <- sf::st_intersection(EFHCA, grid_test) %>%
#   mutate(Score.EFHCA = EFHCA.Score) %>%
#   mutate(area.part = st_area(.)) %>%
#   group_by(CellID_5km) %>% #use for 2km grid
#   #group_by(GRID_ID) %>% #use for NCCOS hexagonal grid
#   slice_max(area.part, n = 1) %>%
#   select(CellID_5km, Score.EFHCA) #use for 2km grid
# EFHCA_score <- EFHCA.grid %>%
#   st_drop_geometry() %>%
#   group_by(CellID_5km) %>%
#   distinct(CellID_5km, .keep_all = TRUE)
# EFHCA_scored <- grid_test %>%
#   full_join(EFHCA_score, by = "CellID_5km") %>%
#   filter(Score.EFHCA == 0.01) %>%
#   rename("0.01" = Score.EFHCA) %>%
#   mutate(
#     "0" = 0,
#     "0.001" = 0.001,
#     "0.1" = 0.1,
#     "0.2" = 0.2,
#     "0.3" = 0.3,
#     "0.4" = 0.4,
#     "0.5" = 0.5,
#     "0.6" = 0.6,
#     "0.7" = 0.7,
#     "0.8" = 0.8,
#     "0.9" = 0.9,
#     "1" = 1
#   )
# EFHCA_scored_long <- pivot_longer(
#   EFHCA_scored,
#   cols = starts_with(c("0", "1")),
#   names_to = "EFHCA",
#   values_to = "Score.EFHCA"
# ) %>%
#   sf::st_transform('+proj=longlat +datum=WGS84') %>%
#   select(-EFHCA)

# st_write_parquet(
#   EFHCA_scored_long,
#   "C:\\GitHub\\SMORES\\data\\5km\\EFHCA_scored_full_5km.parquet"
# )

# #EFHCA.700
# EFHCA_700.grid <- sf::st_intersection(EFHCA_700, grid_test) %>%
#   mutate(Score.EFHCA.700 = EFHCA.700.Score) %>%
#   mutate(area.part = st_area(.)) %>%
#   group_by(CellID_5km) %>% #use for 2km grid
#   #group_by(GRID_ID) %>% #use for NCCOS hexagonal grid
#   slice_max(area.part, n = 1) %>%
#   select(CellID_5km, Score.EFHCA.700) #use for 2km grid
# EFHCA_700_score <- EFHCA_700.grid %>%
#   st_drop_geometry() %>%
#   group_by(CellID_5km) %>%
#   distinct(CellID_5km, .keep_all = TRUE)
# EFHCA_700_scored <- grid_test %>%
#   full_join(EFHCA_700_score, by = "CellID_5km") %>%
#   filter(Score.EFHCA.700 == 0.01) %>%
#   rename("0.01" = Score.EFHCA.700) %>%
#   mutate(
#     "0" = 0,
#     "0.001" = 0.001,
#     "0.1" = 0.1,
#     "0.2" = 0.2,
#     "0.3" = 0.3,
#     "0.4" = 0.4,
#     "0.5" = 0.5,
#     "0.6" = 0.6,
#     "0.7" = 0.7,
#     "0.8" = 0.8,
#     "0.9" = 0.9,
#     "1" = 1
#   )
# EFHCA_700_scored_long <- pivot_longer(
#   EFHCA_700_scored,
#   cols = starts_with(c("0", "1")),
#   names_to = "EFHCA_700",
#   values_to = "Score.EFHCA.700"
# ) %>%
#   sf::st_transform('+proj=longlat +datum=WGS84') %>%
#   select(-EFHCA_700)

# st_write_parquet(
#   EFHCA_700_scored_long,
#   "C:\\GitHub\\SMORES\\data\\5km\\EFHCA_700_scored_full_5km.parquet"
# )

# # HAPCAOI
# HAPCaoi.grid <- sf::st_intersection(HAPCaoi, grid_test) %>%
#   mutate(Score.HAPC.AOI = HAPC.AOI.Score) %>%
#   mutate(area.part = st_area(.)) %>%
#   group_by(CellID_5km) %>% #use for 2km grid
#   #group_by(GRID_ID) %>% #use for NCCOS hexagonal grid
#   slice_max(area.part, n = 1) %>%
#   select(CellID_5km, Score.HAPC.AOI) #use for 2km grid
# HAPCaoi_score <- HAPCaoi.grid %>%
#   st_drop_geometry() %>%
#   group_by(CellID_5km) %>%
#   distinct(CellID_5km, .keep_all = TRUE)
# HAPCaoi_scored <- grid_test %>%
#   full_join(HAPCaoi_score, by = "CellID_5km") %>%
#   filter(Score.HAPC.AOI == 0.01) %>%
#   rename("0.01" = Score.HAPC.AOI) %>%
#   mutate(
#     "0" = 0,
#     "0.001" = 0.001,
#     "0.1" = 0.1,
#     "0.2" = 0.2,
#     "0.3" = 0.3,
#     "0.4" = 0.4,
#     "0.5" = 0.5,
#     "0.6" = 0.6,
#     "0.7" = 0.7,
#     "0.8" = 0.8,
#     "0.9" = 0.9,
#     "1" = 1
#   )
# HAPCaoi_scored_long <- pivot_longer(
#   HAPCaoi_scored,
#   cols = starts_with(c("0", "1")),
#   names_to = "HAPCaoi",
#   values_to = "Score.HAPC.AOI"
# ) %>%
#   sf::st_transform('+proj=longlat +datum=WGS84') %>%
#   select(-HAPCaoi)

# st_write_parquet(
#   HAPCaoi_scored_long,
#   "C:\\GitHub\\SMORES\\data\\5km\\HAPCaoi_scored_full_5km.parquet"
# )

# #HAPCReef
# HAPCreef.grid <- sf::st_intersection(HAPCreef, grid_test) %>%
#   mutate(Score.HAPC.Reef = HAPC.RR.Score) %>%
#   mutate(area.part = st_area(.)) %>%
#   group_by(CellID_5km) %>% #use for 2km grid
#   #group_by(GRID_ID) %>% #use for NCCOS hexagonal grid
#   slice_max(area.part, n = 1) %>%
#   select(CellID_5km, Score.HAPC.Reef) #use for 2km grid
# HAPCreef_score <- HAPCreef.grid %>%
#   st_drop_geometry() %>%
#   group_by(CellID_5km) %>%
#   distinct(CellID_5km, .keep_all = TRUE)
# HAPCreef_scored <- grid_test %>%
#   full_join(HAPCreef_score, by = "CellID_5km") %>%
#   filter(Score.HAPC.Reef == 0.001) %>%
#   rename("0.001" = Score.HAPC.Reef) %>%
#   mutate(
#     "0" = 0,
#     "0.01" = 0.01,
#     "0.1" = 0.1,
#     "0.2" = 0.2,
#     "0.3" = 0.3,
#     "0.4" = 0.4,
#     "0.5" = 0.5,
#     "0.6" = 0.6,
#     "0.7" = 0.7,
#     "0.8" = 0.8,
#     "0.9" = 0.9,
#     "1" = 1
#   )
# HAPCreef_scored_long <- pivot_longer(
#   HAPCreef_scored,
#   cols = starts_with(c("0", "1")),
#   names_to = "HAPCreef",
#   values_to = "Score.HAPC.Reef"
# ) %>%
#   sf::st_transform('+proj=longlat +datum=WGS84') %>%
#   select(-HAPCreef)

# st_write_parquet(
#   HAPCreef_scored_long,
#   "C:\\GitHub\\SMORES\\data\\5km\\HAPCreef_scored_full_5km.parquet"
# )

# ## Species Layers
# ESA_Critical_Habitat <- sf::st_read(
#   dsn = "Z:\\ArcGIS\\Projects\\OWEC\\p30\\protected_species_density_layers\\nmfs_combined\\Critical_Habitat_Areas_by_the_National_Marine_Fisheries_Service\\Critical_Habitat_Areas_by_the_National_Marine_Fisheries_Service.shp"
# )

# bia_bw_nms <- "https://services2.arcgis.com/C8EMgrsFcRFL6LrL/ArcGIS/rest/services/BIA_II_WebMerc/FeatureServer/1"
# # contains parent and core - parent = entire range, core = intensified use areas such as breeding ground or feeding site
# bia_bw_nms_layer <- arc_read(
#   bia_bw_nms,
#   where = "Common_Name = 'Blue whale'"
# ) %>%
#   st_transform(crsOut)

# #killer whale
# killer_whale <- ESA_Critical_Habitat %>%
#   filter(commonName == "Whale, killer") %>%
#   select(-c(statusType, scientific, listingSta, federalReg)) %>%
#   st_transform(crsOut)
# killer_whale.grid <- sf::st_intersection(killer_whale, grid_test) %>%
#   mutate(Score.killer_whale = 1) %>%
#   mutate(area.part = st_area(.)) %>%
#   group_by(CellID_5km) %>% #use for 2km grid
#   #group_by(GRID_ID) %>% #use for NCCOS hexagonal grid
#   slice_max(area.part, n = 1) %>%
#   select(CellID_5km, Score.killer_whale) #use for 2km grid
# killer_whale_score <- killer_whale.grid %>% #no duplicates
#   st_drop_geometry() %>%
#   group_by(CellID_5km) %>%
#   distinct(CellID_5km, .keep_all = TRUE)
# killer_whale_scored <- grid_test %>%
#   full_join(killer_whale_score, by = "CellID_5km") %>%
#   filter(Score.killer_whale == 1) %>%
#   rename("1" = Score.killer_whale) %>%
#   mutate(
#     "0" = 0,
#     "0.01" = 0.01,
#     "0.001" = 0.001,
#     "0.1" = 0.1,
#     "0.2" = 0.2,
#     "0.3" = 0.3,
#     "0.4" = 0.4,
#     "0.5" = 0.5,
#     "0.6" = 0.6,
#     "0.7" = 0.7,
#     "0.8" = 0.8,
#     "0.9" = 0.9
#   )
# killer_whale_scored_long <- pivot_longer(
#   killer_whale_scored,
#   cols = starts_with(c("0", "1")),
#   names_to = "killer_whale",
#   values_to = "Score.killer_whale"
# ) %>%
#   sf::st_transform('+proj=longlat +datum=WGS84') %>%
#   select(-killer_whale)

# st_write_parquet(
#   killer_whale_scored_long,
#   "C:\\GitHub\\SMORES\\data\\5km\\killer_whale_scored_full_5km.parquet"
# )

# # leatherback sea turtle
# leatherback_turtle <- ESA_Critical_Habitat %>%
#   filter(commonName == "Sea turtle, leatherback") %>%
#   select(-c(statusType, scientific, listingSta, federalReg)) %>%
#   st_transform(crsOut)
# leatherback_turtle.grid <- sf::st_intersection(
#   leatherback_turtle,
#   grid_test
# ) %>%
#   mutate(Score.leatherback_turtle = 1) %>%
#   mutate(area.part = st_area(.)) %>%
#   group_by(CellID_5km) %>% #use for 2km grid
#   #group_by(GRID_ID) %>% #use for NCCOS hexagonal grid
#   slice_max(area.part, n = 1) %>%
#   select(CellID_5km, Score.leatherback_turtle) #use for 2km grid
# leatherback_turtle_score <- leatherback_turtle.grid %>% #no duplicates
#   st_drop_geometry() %>%
#   group_by(CellID_5km) %>%
#   distinct(CellID_5km, .keep_all = TRUE)
# leatherback_turtle_scored <- grid_test %>%
#   full_join(leatherback_turtle_score, by = "CellID_5km") %>%
#   filter(Score.leatherback_turtle == 1) %>%
#   rename("1" = Score.leatherback_turtle) %>%
#   mutate(
#     "0" = 0,
#     "0.01" = 0.01,
#     "0.001" = 0.001,
#     "0.1" = 0.1,
#     "0.2" = 0.2,
#     "0.3" = 0.3,
#     "0.4" = 0.4,
#     "0.5" = 0.5,
#     "0.6" = 0.6,
#     "0.7" = 0.7,
#     "0.8" = 0.8,
#     "0.9" = 0.9
#   )
# leatherback_turtle_scored_long <- pivot_longer(
#   leatherback_turtle_scored,
#   cols = starts_with(c("0", "1")),
#   names_to = "leatherback_turtle",
#   values_to = "Score.leatherback_turtle"
# ) %>%
#   sf::st_transform('+proj=longlat +datum=WGS84') %>%
#   select(-leatherback_turtle)

# st_write_parquet(
#   leatherback_turtle_scored_long,
#   "C:\\GitHub\\SMORES\\data\\5km\\leatherback_turtle_scored_full_5km.parquet"
# )

# #humpback whale - mexico and central dps
# humpback_whale <- ESA_Critical_Habitat %>%
#   filter(commonName == "Whale, humpback") %>%
#   select(-c(statusType, scientific, listingSta, federalReg)) %>%
#   st_transform(crsOut)
# # humpback_whale.grid <- sf::st_intersection(humpback_whale, grd.norcal) %>%
# #   mutate(Score.humpback_whale = 1) %>%
# #   mutate(area.part = st_area(.)) %>%
# #   group_by(CellID_2km) %>% #use for 2km grid
# #   #group_by(GRID_ID) %>% #use for NCCOS hexagonal grid
# #   slice_max(area.part, n = 1) %>%
# #   select(CellID_2km, Score.humpback_whale) #use for 2km grid

# #Error:
# # Error in scan(text = lst[[length(lst)]], quiet = TRUE) :
# # scan() expected 'a real', got '936246.12987604213.'
# # Error in (function (msg)  :
# # TopologyException: side location conflict at 92746.754168340936 936246.12987604213. This can occur if the input geometry is invalid.

# # returns false for row 5 and 17 of data
# # st_is_valid(humpback_whale)

# # returns that the two false rows have a self-intersection [92746.7541682779 936246.129876046]
# # st_is_valid(humpback_whale, reason = TRUE)

# # suggestion is to apply a buffer of 0 to force the creation of a new geometry -> stack overflow article https://stackoverflow.com/questions/66584191/sfst-intersection-virtually-random-error-action
# humpback_whale_buffer <- humpback_whale %>%
#   st_buffer(0)

# # buffer has made all geometries valid
# st_is_valid(humpback_whale_buffer, reason = TRUE)

# humpback_whale.grid <- sf::st_intersection(humpback_whale_buffer, grid_test) %>%
#   mutate(Score.humpback_whale = 1) %>%
#   mutate(area.part = st_area(.)) %>%
#   group_by(CellID_5km) %>% #use for 2km grid
#   #group_by(GRID_ID) %>% #use for NCCOS hexagonal grid
#   slice_max(area.part, n = 1) %>%
#   select(CellID_5km, Score.humpback_whale) #use for 2km grid

# humpback_whale_score <- humpback_whale.grid %>% #had 16354 duplicates
#   st_drop_geometry() %>%
#   group_by(CellID_5km) %>%
#   distinct(CellID_5km, .keep_all = TRUE)

# humpback_whale_scored <- grid_test %>%
#   full_join(humpback_whale_score, by = "CellID_5km") %>%
#   filter(Score.humpback_whale == 1) %>%
#   rename("1" = Score.humpback_whale) %>%
#   mutate(
#     "0" = 0,
#     "0.01" = 0.01,
#     "0.001" = 0.001,
#     "0.1" = 0.1,
#     "0.2" = 0.2,
#     "0.3" = 0.3,
#     "0.4" = 0.4,
#     "0.5" = 0.5,
#     "0.6" = 0.6,
#     "0.7" = 0.7,
#     "0.8" = 0.8,
#     "0.9" = 0.9
#   )
# humpback_whale_scored_long <- pivot_longer(
#   humpback_whale_scored,
#   cols = starts_with(c("0", "1")),
#   names_to = "humpback_whale",
#   values_to = "Score.humpback_whale"
# ) %>%
#   sf::st_transform('+proj=longlat +datum=WGS84') %>%
#   select(-humpback_whale)

# st_write_parquet(
#   humpback_whale_scored_long,
#   "C:\\GitHub\\SMORES\\data\\5km\\humpback_whale_scored_full.parquet"
# )

# #blue whale
# blue_whale <- bia_bw_nms_layer %>%
#   select(c(Common_Name, Stock, Area_Name, geometry)) %>%
#   st_transform(crsOut)
# blue_whale.grid <- sf::st_intersection(blue_whale, grid_test) %>%
#   mutate(Score.blue_whale = 1) %>%
#   mutate(area.part = st_area(.)) %>%
#   group_by(CellID_5km) %>% #use for 2km grid
#   #group_by(GRID_ID) %>% #use for NCCOS hexagonal grid
#   slice_max(area.part, n = 1) %>%
#   select(CellID_5km, Score.blue_whale) #use for 2km grid

# blue_whale_score <- blue_whale.grid %>% #no duplicates
#   st_drop_geometry() %>%
#   group_by(CellID_5km) %>%
#   distinct(CellID_5km, .keep_all = TRUE)
# blue_whale_scored <- grid_test %>%
#   full_join(blue_whale_score, by = "CellID_5km") %>%
#   filter(Score.blue_whale == 1) %>%
#   rename("1" = Score.blue_whale) %>%
#   mutate(
#     "0" = 0,
#     "0.01" = 0.01,
#     "0.001" = 0.001,
#     "0.1" = 0.1,
#     "0.2" = 0.2,
#     "0.3" = 0.3,
#     "0.4" = 0.4,
#     "0.5" = 0.5,
#     "0.6" = 0.6,
#     "0.7" = 0.7,
#     "0.8" = 0.8,
#     "0.9" = 0.9
#   )
# blue_whale_scored_long <- pivot_longer(
#   blue_whale_scored,
#   cols = starts_with(c("0", "1")),
#   names_to = "blue_whale",
#   values_to = "Score.blue_whale"
# ) %>%
#   sf::st_transform('+proj=longlat +datum=WGS84') %>%
#   select(-blue_whale)

# st_write_parquet(
#   blue_whale_scored_long,
#   "C:\\GitHub\\SMORES\\data\\5km\\blue_whale_scored_full.parquet"
# )

# #submarine cables
# furl_nms <- "https://coast.noaa.gov/arcgis/rest/services/Hosted/SubmarineCables/FeatureServer/0"
# nms_layer <- arc_read(
#   furl_nms,
#   where = "status = 'In Service'
#                 or status = 'Under Construction'"
# ) %>%
#   st_transform(crsOut)

# st_is_valid(nms_layer, reason = TRUE)
# nms_layer2 <- st_make_valid(nms_layer)

# submarine_cable.grid <- sf::st_intersection(nms_layer2, grid_test) %>%
#   mutate(Score.submarine_cable = 0) %>%
#   mutate(area.part = st_area(.)) %>%
#   group_by(CellID_5km) %>% #use for 2km grid
#   #group_by(GRID_ID) %>% #use for NCCOS hexagonal grid
#   slice_max(area.part, n = 1) %>%
#   select(CellID_5km, Score.submarine_cable, status) #use for 2km grid
# submarine_cable_score <- submarine_cable.grid %>%
#   st_drop_geometry() %>%
#   group_by(CellID_5km) %>%
#   distinct(CellID_5km, .keep_all = TRUE)
# submarine_cable_scored <- grid_test %>%
#   full_join(submarine_cable_score, by = "CellID_5km") %>%
#   filter(Score.submarine_cable == 0) %>%
#   rename("0" = Score.submarine_cable) %>%
#   mutate(
#     "1" = 1,
#     "0.01" = 0.01,
#     "0.001" = 0.001,
#     "0.1" = 0.1,
#     "0.2" = 0.2,
#     "0.3" = 0.3,
#     "0.4" = 0.4,
#     "0.5" = 0.5,
#     "0.6" = 0.6,
#     "0.7" = 0.7,
#     "0.8" = 0.8,
#     "0.9" = 0.9
#   )
# submarine_cable_scored_long <- pivot_longer(
#   submarine_cable_scored,
#   cols = starts_with(c("0", "1")),
#   names_to = "submarine_cable",
#   values_to = "Score.submarine_cable"
# ) %>%
#   sf::st_transform('+proj=longlat +datum=WGS84') %>%
#   select(-submarine_cable)

# st_write_parquet(
#   submarine_cable_scored_long,
#   "C:\\GitHub\\SMORES\\data\\5km\\submarine_cable_scored_full.parquet"
# )

# #Fisheries data
# # for just Coos Bay + Brookings

# Fisheries <- sf::st_read(
#   "G:\\My Drive\\SMORES\\NCCOSfisherieslayers20250815081621\\Non-Confidential_NMFS_fisheries_submodel_data_2km_grid.shp"
# ) %>%
#   st_transform(crsOut)

# Trawl_Fisheries <- sf::st_read(
#   "G:\\My Drive\\SMORES\\Scenario_4_trawl_polygon\\Scenario_4_trawl_polygon.shp"
# )
# trawl_fisheries_transformed <- Trawl_Fisheries %>%
#   st_transform(crs = crsOut)
# st_crs(trawl_fisheries_transformed)

# #At-Sea hake mid-water trawl = ASH_RI in fisheries dataset
# ASH <- Fisheries %>%
#   select(ASH_RI)
# ASH.grid_RI <- sf::st_intersection(ASH, grid_test) %>%
#   mutate(Score.ASH_Ranked_Importance = ASH_RI) %>%
#   mutate(area.part = st_area(.)) %>%
#   group_by(CellID_5km) %>% #use for 2km grid
#   slice_max(area.part, n = 1) %>%
#   select(CellID_5km, Score.ASH_Ranked_Importance) %>% #checked and no duplicates present but there are na values present
#   filter(!is.na(Score.ASH_Ranked_Importance)) %>%
#   sf::st_transform('+proj=longlat +datum=WGS84')

# st_write_parquet(
#   ASH.grid_RI,
#   "C:\\GitHub\\SMORES\\data\\5km\\ASH_Ranked_Importance_scored_full.parquet"
# )

# # to make the user selected score options
# ASH.grid <- ASH.grid_RI %>%
#   st_drop_geometry() %>%
#   group_by(CellID_5km) %>%
#   distinct(CellID_5km, .keep_all = TRUE)
# ASH_scored <- grid_test %>%
#   full_join(ASH.grid, by = "CellID_5km") %>%
#   filter(!is.na(Score.ASH_Ranked_Importance)) %>%
#   rename("0" = Score.ASH_Ranked_Importance) %>%
#   mutate("0" = 0, "0.01" = 0.01, "0.001" = 0.001) %>%
#   pivot_longer(
#     cols = starts_with(c("0.", 0)),
#     names_to = "ASH",
#     values_to = "Score.ASH"
#   ) %>%
#   sf::st_transform('+proj=longlat +datum=WGS84') %>%
#   select(-ASH)

# st_write_parquet(
#   ASH_scored,
#   "C:\\GitHub\\SMORES\\data\\5km\\ASH_scored_full.parquet"
# )

# # Shoreside hake mid-water trawl = SSH_RI in fisheries dataset
# SSH <- Fisheries %>%
#   select(SSH_RI)
# SSH.grid_RI <- sf::st_intersection(SSH, grid_test) %>%
#   mutate(Score.SSH_Ranked_Importance = SSH_RI) %>%
#   mutate(area.part = st_area(.)) %>%
#   group_by(CellID_5km) %>% #use for 2km grid
#   slice_max(area.part, n = 1) %>%
#   select(CellID_5km, Score.SSH_Ranked_Importance) %>% #checked and no duplicates present
#   filter(!is.na(Score.SSH_Ranked_Importance)) %>%
#   sf::st_transform('+proj=longlat +datum=WGS84')

# st_write_parquet(
#   SSH.grid_RI,
#   "C:\\GitHub\\SMORES\\data\\5km\\SSH_Ranked_Importance_scored_full.parquet"
# )

# # to make the user selected score options
# SSH.grid <- SSH.grid_RI %>%
#   st_drop_geometry() %>%
#   group_by(CellID_5km) %>%
#   distinct(CellID_5km, .keep_all = TRUE)
# SSH_scored <- grid_test %>%
#   full_join(SSH.grid, by = "CellID_5km") %>%
#   filter(!is.na(Score.SSH_Ranked_Importance)) %>%
#   rename("0" = Score.SSH_Ranked_Importance) %>%
#   mutate("0" = 0, "0.01" = 0.01, "0.001" = 0.001) %>%
#   pivot_longer(
#     cols = starts_with(c("0.", 0)),
#     names_to = "SSH",
#     values_to = "Score.SSH"
#   ) %>%
#   sf::st_transform('+proj=longlat +datum=WGS84') %>%
#   select(-SSH)

# st_write_parquet(
#   SSH_scored,
#   "C:\\GitHub\\SMORES\\data\\5km\\SSH_scored_full.parquet"
# )

# # Groundfish bottom trawl = GFBT_RI in fisheries dataset
# GFBT <- Fisheries %>%
#   select(GFBT_RI)
# GFBT.grid_RI <- sf::st_intersection(GFBT, grid_test) %>%
#   mutate(Score.GFBT_Ranked_Importance = GFBT_RI) %>%
#   mutate(area.part = st_area(.)) %>%
#   group_by(CellID_5km) %>% #use for 2km grid
#   slice_max(area.part, n = 1) %>%
#   select(CellID_5km, Score.GFBT_Ranked_Importance) %>% #checked and no duplicates present
#   filter(!is.na(Score.GFBT_Ranked_Importance)) %>%
#   sf::st_transform('+proj=longlat +datum=WGS84')

# st_write_parquet(
#   GFBT.grid_RI,
#   "C:\\GitHub\\SMORES\\data\\5km\\GFBT_Ranked_Importance_scored_full.parquet"
# )

# # to make the user selected score options
# GFBT.grid <- GFBT.grid_RI %>%
#   st_drop_geometry() %>%
#   group_by(CellID_5km) %>%
#   distinct(CellID_5km, .keep_all = TRUE)
# GFBT_scored <- grid_test %>%
#   full_join(GFBT.grid, by = "CellID_5km") %>%
#   filter(!is.na(Score.GFBT_Ranked_Importance)) %>%
#   rename("0" = Score.GFBT_Ranked_Importance) %>%
#   mutate("0" = 0, "0.01" = 0.01, "0.001" = 0.001) %>%
#   pivot_longer(
#     cols = starts_with(c("0.", 0)),
#     names_to = "GFBT",
#     values_to = "Score.GFBT"
#   ) %>%
#   sf::st_transform('+proj=longlat +datum=WGS84') %>%
#   select(-GFBT)

# st_write_parquet(
#   GFBT_scored,
#   "C:\\GitHub\\SMORES\\data\\5km\\GFBT_scored_full.parquet"
# )

# # Groundfish pot gear = GFP_RI in fisheries dataset
# GFP <- Fisheries %>%
#   select(GFP_RI)
# GFP.grid_RI <- sf::st_intersection(GFP, grid_test) %>%
#   mutate(Score.GFP_Ranked_Importance = GFP_RI) %>%
#   mutate(area.part = st_area(.)) %>%
#   group_by(CellID_5km) %>% #use for 2km grid
#   slice_max(area.part, n = 1) %>%
#   select(CellID_5km, Score.GFP_Ranked_Importance) %>% #checked and no duplicates present
#   filter(!is.na(Score.GFP_Ranked_Importance)) %>%
#   sf::st_transform('+proj=longlat +datum=WGS84')

# st_write_parquet(
#   GFP.grid_RI,
#   "C:\\GitHub\\SMORES\\data\\5km\\GFP_Ranked_Importance_scored_full.parquet"
# )

# # to make the user selected score options
# GFP.grid <- GFP.grid_RI %>%
#   st_drop_geometry() %>%
#   group_by(CellID_5km) %>%
#   distinct(CellID_5km, .keep_all = TRUE)
# GFP_scored <- grid_test %>%
#   full_join(GFP.grid, by = "CellID_5km") %>%
#   filter(!is.na(Score.GFP_Ranked_Importance)) %>%
#   rename("0" = Score.GFP_Ranked_Importance) %>%
#   mutate("0" = 0, "0.01" = 0.01, "0.001" = 0.001) %>%
#   pivot_longer(
#     cols = starts_with(c("0.", 0)),
#     names_to = "GFP",
#     values_to = "Score.GFP"
#   ) %>%
#   sf::st_transform('+proj=longlat +datum=WGS84') %>%
#   select(-GFP)

# st_write_parquet(
#   GFP_scored,
#   "C:\\GitHub\\SMORES\\data\\5km\\GFP_scored_full.parquet"
# )

# # Groundfish longline gear = GFLL_RI in fisheries dataset
# GFLL <- Fisheries %>%
#   select(GFLL_RI)
# GFLL.grid_RI <- sf::st_intersection(GFLL, grid_test) %>%
#   mutate(Score.GFLL_Ranked_Importance = GFLL_RI) %>%
#   mutate(area.part = st_area(.)) %>%
#   group_by(CellID_5km) %>% #use for 2km grid
#   slice_max(area.part, n = 1) %>%
#   select(CellID_5km, Score.GFLL_Ranked_Importance) %>% #checked and no duplicates present
#   filter(!is.na(Score.GFLL_Ranked_Importance)) %>%
#   sf::st_transform('+proj=longlat +datum=WGS84')

# st_write_parquet(
#   GFLL.grid_RI,
#   "C:\\GitHub\\SMORES\\data\\5km\\GFLL_Ranked_Importance_scored_full.parquet"
# )

# # to make the user selected score options
# GFLL.grid <- GFLL.grid_RI %>%
#   st_drop_geometry() %>%
#   group_by(CellID_5km) %>%
#   distinct(CellID_5km, .keep_all = TRUE)
# GFLL_scored <- grid_test %>%
#   full_join(GFLL.grid, by = "CellID_5km") %>%
#   filter(!is.na(Score.GFLL_Ranked_Importance)) %>%
#   rename("0" = Score.GFLL_Ranked_Importance) %>%
#   mutate("0" = 0, "0.01" = 0.01, "0.001" = 0.001) %>%
#   pivot_longer(
#     cols = starts_with(c("0.", 0)),
#     names_to = "GFLL",
#     values_to = "Score.GFLL"
#   ) %>%
#   sf::st_transform('+proj=longlat +datum=WGS84') %>%
#   select(-GFLL)

# st_write_parquet(
#   GFLL_scored,
#   "C:\\GitHub\\SMORES\\data\\5km\\GFLL_scored_full.parquet"
# )

# # Pink shrimp trawl = PS_RI in fisheries dataset
# PS <- Fisheries %>%
#   select(PS_RI)

# PS.grid_RI <- sf::st_intersection(PS, grid_test) %>%
#   mutate(Score.PS_Ranked_Importance = PS_RI) %>%
#   mutate(area.part = st_area(.)) %>%
#   group_by(CellID_5km) %>% #use for 2km grid
#   slice_max(area.part, n = 1) %>%
#   select(CellID_5km, Score.PS_Ranked_Importance) %>% #checked and no duplicates present
#   filter(!is.na(Score.PS_Ranked_Importance)) %>%
#   sf::st_transform('+proj=longlat +datum=WGS84')
# st_crs(DSC_RH_scored_long) == st_crs(PS.grid_RI)

# st_write_parquet(
#   PS.grid_RI,
#   "C:\\GitHub\\SMORES\\data\\5km\\PS_Ranked_Importance_scored_full.parquet"
# )

# # to make the user selected score options
# PS.grid <- PS.grid_RI %>%
#   st_drop_geometry() %>%
#   group_by(CellID_5km) %>%
#   distinct(CellID_5km, .keep_all = TRUE)
# PS_scored <- grid_test %>%
#   full_join(PS.grid, by = "CellID_5km") %>%
#   filter(!is.na(Score.PS_Ranked_Importance)) %>%
#   rename("0" = Score.PS_Ranked_Importance) %>%
#   mutate("0" = 0, "0.01" = 0.01, "0.001" = 0.001) %>%
#   pivot_longer(
#     cols = starts_with(c("0.", 0)),
#     names_to = "PS",
#     values_to = "Score.PS"
#   ) %>%
#   sf::st_transform('+proj=longlat +datum=WGS84') %>%
#   select(-PS)

# st_write_parquet(
#   PS_scored,
#   "C:\\GitHub\\SMORES\\data\\5km\\PS_scored_full.parquet"
# )

# # Dungeness Crab = CRAB_RI in fisheries dataset
# CRAB <- Fisheries %>%
#   select(CRAB_RI)
# CRAB.grid_RI <- sf::st_intersection(CRAB, grid_test) %>%
#   mutate(Score.CRAB_Ranked_Importance = CRAB_RI) %>%
#   mutate(area.part = st_area(.)) %>%
#   group_by(CellID_5km) %>% #use for 2km grid
#   slice_max(area.part, n = 1) %>%
#   select(CellID_5km, Score.CRAB_Ranked_Importance) %>% #checked and no duplicates present
#   filter(!is.na(Score.CRAB_Ranked_Importance)) %>%
#   sf::st_transform('+proj=longlat +datum=WGS84')

# st_write_parquet(
#   CRAB.grid_RI,
#   "C:\\GitHub\\SMORES\\data\\5km\\CRAB_Ranked_Importance_scored_full.parquet"
# )

# # to make the user selected score options
# CRAB.grid <- CRAB.grid_RI %>%
#   st_drop_geometry() %>%
#   group_by(CellID_5km) %>%
#   distinct(CellID_5km, .keep_all = TRUE)
# CRAB_scored <- grid_test %>%
#   full_join(CRAB.grid, by = "CellID_5km") %>%
#   filter(!is.na(Score.CRAB_Ranked_Importance)) %>%
#   rename("0" = Score.CRAB_Ranked_Importance) %>%
#   mutate("0" = 0, "0.01" = 0.01, "0.001" = 0.001) %>%
#   pivot_longer(
#     cols = starts_with(c("0.", 0)),
#     names_to = "CRAB",
#     values_to = "Score.CRAB"
#   ) %>%
#   sf::st_transform('+proj=longlat +datum=WGS84') %>%
#   select(-CRAB)

# st_write_parquet(
#   CRAB_scored,
#   "C:\\GitHub\\SMORES\\data\\5km\\CRAB_scored_full.parquet"
# )

# # Commercial troll/hook and line albacore = ALCO_RI in fisheries dataset
# ALCO <- Fisheries %>%
#   select(ALCO_RI)
# ALCO.grid_RI <- sf::st_intersection(ALCO, grid_test) %>%
#   mutate(Score.ALCO_Ranked_Importance = ALCO_RI) %>%
#   mutate(area.part = st_area(.)) %>%
#   group_by(CellID_5km) %>% #use for 2km grid
#   slice_max(area.part, n = 1) %>%
#   select(CellID_5km, Score.ALCO_Ranked_Importance) %>% #checked and no duplicates present
#   filter(!is.na(Score.ALCO_Ranked_Importance)) %>%
#   sf::st_transform('+proj=longlat +datum=WGS84')

# st_write_parquet(
#   ALCO.grid_RI,
#   "C:\\GitHub\\SMORES\\data\\5km\\ALCO_Ranked_Importance_scored_full.parquet"
# )

# # to make the user selected score options
# ALCO.grid <- ALCO.grid_RI %>%
#   st_drop_geometry() %>%
#   group_by(CellID_5km) %>%
#   distinct(CellID_5km, .keep_all = TRUE)
# ALCO_scored <- grid_test %>%
#   full_join(ALCO.grid, by = "CellID_5km") %>%
#   filter(!is.na(Score.ALCO_Ranked_Importance)) %>%
#   rename("0" = Score.ALCO_Ranked_Importance) %>%
#   mutate("0" = 0, "0.01" = 0.01, "0.001" = 0.001) %>%
#   pivot_longer(
#     cols = starts_with(c("0.", 0)),
#     names_to = "ALCO",
#     values_to = "Score.ALCO"
#   ) %>%
#   sf::st_transform('+proj=longlat +datum=WGS84') %>%
#   select(-ALCO)

# st_write_parquet(
#   ALCO_scored,
#   "C:\\GitHub\\SMORES\\data\\5km\\ALCO_scored_full.parquet"
# )

# # Charter vessel albacore troll/hook and line = ALCH_RI in fisheries dataset
# ALCH <- Fisheries %>%
#   select(ALCH_RI)
# ALCH.grid_RI <- sf::st_intersection(ALCH, grid_test) %>%
#   mutate(Score.ALCH_Ranked_Importance = ALCH_RI) %>%
#   mutate(area.part = st_area(.)) %>%
#   group_by(CellID_5km) %>% #use for 2km grid
#   slice_max(area.part, n = 1) %>%
#   select(CellID_5km, Score.ALCH_Ranked_Importance) %>% #checked and no duplicates present
#   filter(!is.na(Score.ALCH_Ranked_Importance)) %>%
#   sf::st_transform('+proj=longlat +datum=WGS84')

# st_write_parquet(
#   ALCH.grid_RI,
#   "C:\\GitHub\\SMORES\\data\\5km\\ALCH_Ranked_Importance_scored_full.parquet"
# )

# # to make the user selected score options
# ALCH.grid <- ALCH.grid_RI %>%
#   st_drop_geometry() %>%
#   group_by(CellID_5km) %>%
#   distinct(CellID_5km, .keep_all = TRUE)
# ALCH_scored <- grid_test %>%
#   full_join(ALCH.grid, by = "CellID_5km") %>%
#   filter(!is.na(Score.ALCH_Ranked_Importance)) %>%
#   rename("0" = Score.ALCH_Ranked_Importance) %>%
#   mutate("0" = 0, "0.01" = 0.01, "0.001" = 0.001) %>%
#   pivot_longer(
#     cols = starts_with(c("0.", 0)),
#     names_to = "ALCH",
#     values_to = "Score.ALCH"
#   ) %>%
#   sf::st_transform('+proj=longlat +datum=WGS84') %>%
#   select(-ALCH)

# st_write_parquet(
#   ALCH_scored,
#   "C:\\GitHub\\SMORES\\data\\5km\\ALCH_scored_full.parquet"
# )

# # Trawl Fisheries
# trawl_fisheries <- trawl_fisheries_transformed %>%
#   sf::st_intersection(grid_test) %>%
#   mutate(Score.Trawl_Fisheries = FID) %>%
#   mutate(Score.Trawl_Fisheries = 0.001) %>%
#   mutate(area.part = st_area(.)) %>%
#   group_by(CellID_5km) %>% #use for 2km grid
#   slice_max(area.part, n = 1) %>%
#   select(CellID_5km, Score.Trawl_Fisheries) %>% #checked and no duplicates present
#   filter(!is.na(Score.Trawl_Fisheries)) %>%
#   sf::st_transform('+proj=longlat +datum=WGS84')

# # ERROR in GEOMETRYCOLLECTION
# # returns FALSE
# any(st_is_empty(trawl_fisheries))

# # returns [1] GEOMETRY
# st_geometry_type(trawl_fisheries, by_geometry = FALSE)
# # GEOMETRYCOLLECTION at row 310
# geom_type <- st_geometry_type(trawl_fisheries)

# # let's plot it to see what we are looking at for 2km grid
# geometrycollection <- trawl_fisheries[310, ]
# test <- trawl_fisheries[309, ]

# # let's plot it to see what we are looking at for 5km grid
# geometrycollection <- trawl_fisheries[25, ]

# plot(geometrycollection)
# plot(test)
# plot(trawl_fisheries, reset = FALSE)
# plot(geometrycollection, col = "red", add = TRUE)

# geometry_collection_extract <- st_collection_extract(
#   geometrycollection,
#   type = "POLYGON"
# )
# plot(geometry_collection_extract, col = "pink", add = TRUE)

# intersection <- st_intersects(geometrycollection, geometry_collection_extract)
# plot(intersection, col = "red")

# trawl_fisheries[25, ] <- geometry_collection_extract[1, ]

# # check for row 25 to see if index replacement worked; it worked
# st_geometry_type(trawl_fisheries)

# #ERROR in Point (row 485) for 2km grid
# # Point at row 485
# st_geometry_type(trawl_fisheries)

# point <- trawl_fisheries[485, ]
# plot(trawl_fisheries, reset = FALSE)
# plot(point, col = "red", add = TRUE)

# trawl_fisheries <- trawl_fisheries[-485, ]

# #index removal successful
# plot(trawl_fisheries)

# st_geometry_type(trawl_fisheries)

# # returns false for a number of data points
# st_is_valid(trawl_fisheries)

# valid_trawl <- st_make_valid(trawl_fisheries)
# plot(trawl_fisheries)
# plot(valid_trawl)
# st_is_valid(valid_trawl)

# st_write_parquet(
#   trawl_fisheries,
#   "C:\\GitHub\\SMORES\\data\\5km\\trawl_fisheries_scored_full.parquet"
# )

# #WEA's
# BOEM.gdb <- "Z:\\ArcGIS\\Projects\\OWEC\\p30\\boem_offshorewindenergy.gdb"
# WEA <- sf::st_read(dsn = BOEM.gdb, layer = "BOEM_CA_OR_WEAs_merge_new") %>%
#   filter(AreaType == "WEA") %>%
#   sf::st_transform('+proj=longlat +datum=WGS84')

# st_write_parquet(WEA, "C:\\GitHub\\SMORES\\data\\WEA.parquet")

# #OCS Planning Area Polygons for Oil and Gas exploration
# ocs_planning_area_nms <- "https://services7.arcgis.com/G5Ma95RzqJRPKsWL/arcgis/rest/services/OCS_Planning_Area_Polygons/FeatureServer/5"

# ocs_planning_area_layer <- arc_read(
#   ocs_planning_area_nms,
#   ,
#   where = "REGION = 'Pacific'"
# ) %>%
#   sf::st_transform('+proj=longlat +datum=WGS84')

# ocs_planning_area_layer_transformed <- ocs_planning_area_layer %>%
#   mutate(Area_Name = PLANNING_AREA) %>%
#   select(Area_Name, geometry)

# WEA_transformed <- WEA %>%
#   st_set_geometry("geometry") %>%
#   select(Area_Name, geometry)

# wea_ocs_join <- rbind(ocs_planning_area_layer_transformed, WEA_transformed) %>%
#   mutate(
#     Area_Name = case_when(
#       Area_Name == "CEC" ~ "Central California",
#       Area_Name == "NOC" ~ "Northern California",
#       Area_Name == "SOC" ~ "Southern California",
#       Area_Name == "WAO" ~ "Washington/Oregon",
#       TRUE ~ Area_Name
#     )
#   ) %>%
#   sf::st_transform(4326) %>%
#   st_zm()

# # CA Sea Space : Areas of Interest
# CA_SeaSpace_aoi_path <- "G:\\Shared drives\\NMFS NWC OEI GIS\\ArcGIS\\Projects\\AB 525 Sea Space Area of Interest_ea1857\\commondata\\osw_technical_potential_02112022\\SeaSpace_1.shp"
# CA_SeaSpace_layer <- sf::st_read(CA_SeaSpace_aoi_path) |>
#   summarise(geometry = sf::st_union(geometry)) |>
#   sf::st_transform('+proj=longlat +datum=WGS84') |>
#   sf::st_transform(4326) |>
#   mutate(Area_Name = "AB 525 Sea Space Area of Interest") |>
#   st_zm()

# # CA Suitable Sea Space : Sea Space Areas
# CA_SeaSpace_Northern_path <- "G:\\Shared drives\\NMFS NWC OEI GIS\\ArcGIS\\Projects\\OWEC\\shapefiles\\MasterAG_Aggregated_NS_Merge_Export.shp"
# CA_SeaSpace_Northern <- sf::st_read(CA_SeaSpace_Northern_path) |>
#   select(CA_Area_Na, geometry) |>
#   summarise(geometry = sf::st_union(geometry)) |>
#   mutate(Area_Name = "Sea_Space_Northern") |>
#   sf::st_transform('+proj=longlat +datum=WGS84') |>
#   sf::st_transform(4326)
# plot(CA_SeaSpace_Northern)

# CA_SeaSpace_Southern_path <- "G:\\Shared drives\\NMFS NWC OEI GIS\\ArcGIS\\Projects\\OWEC\\shapefiles\\MasterAG_Aggregated_SSC_1.shp"
# CA_SeaSpace_Southern <- sf::st_read(CA_SeaSpace_Southern_path) |>
#   select(PROT_NUM, geometry) |>
#   summarise(geometry = sf::st_union(geometry)) |>
#   mutate(Area_Name = "Sea_Space_Southern") |>
#   sf::st_transform('+proj=longlat +datum=WGS84') |>
#   sf::st_transform(4326)

# SeaSpace_Areas_combined <- CA_SeaSpace_Northern |>
#   bind_rows(CA_SeaSpace_Southern) |>
#   summarise(geometry = sf::st_union(geometry)) |>
#   mutate(Area_Name = "AB 525 Suitable Sea Space")

# # Areas of Interest Full
# Areas_Of_Interest_Full <- wea_ocs_join |>
#   bind_rows(SeaSpace_Areas_combined) |>
#   bind_rows(CA_SeaSpace_layer)

# st_write_parquet(
#   Areas_Of_Interest_Full,
#   "C:\\GitHub\\SMORES\\data\\AOI_Full.parquet"
# )
