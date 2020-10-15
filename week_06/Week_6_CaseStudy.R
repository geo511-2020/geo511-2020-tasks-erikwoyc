library(raster)
library(sp)
library(spData)
library(tidyverse)
library(sf)
library(ncdf4)
# Data Download
download.file("https://crudata.uea.ac.uk/cru/data/temperature/absolute.nc","absolute.nc")
tmean=raster("absolute.nc")
plot(tmean)
# World Filter
data(world)  #load 'world' data from spData package
data_poly <- world %>% 
  filter(continent != 'Antarctica')
sp_world <- as(data_poly, 'Spatial')
plot(sp_world)

#Prep Climate Data
TempC <- raster::extract(tmean, sp_world, fun = max, na.rm = 1, 
                         smal = 1, sp = 1)
TempC_sf <- st_as_sf(TempC)
TempC_sf
# Plot
ggplot() + geom_sf(data = TempC_sf, 
                   aes(fill = TempC_sf$CRU_Global_1961.1990_Mean_Monthly_Surface_Temperature_Climatology)) +
  scale_fill_viridis_c(name="Annual\nMaximum\nTemperature (C)") + 
  theme(legend.position = 'bottom')
# Communicate the Results
library(dplyr)
#view(TempC_sf)
Results <- TempC_sf %>% group_by(continent) %>%
  arrange(desc(CRU_Global_1961.1990_Mean_Monthly_Surface_Temperature_Climatology))

#Worked with group to figure the final results table out

Results_table <- Results %>% 
  top_n(1, CRU_Global_1961.1990_Mean_Monthly_Surface_Temperature_Climatology)

Final_Results_Table <- Results_table %>%
  st_set_geometry(NULL) %>%
  select('name_long', 'continent', 'CRU_Global_1961.1990_Mean_Monthly_Surface_Temperature_Climatology')
view(Final_Results_Table)

  
  

                   
                   