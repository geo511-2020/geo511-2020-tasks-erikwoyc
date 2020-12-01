library(sf)
library(tidyverse)
library(ggmap)
library(rnoaa)
library(spData)
data(world)
data(us_states)

#NOAA Data
dataurl="https://www.ncei.noaa.gov/data/international-best-track-archive-for-climate-stewardship-ibtracs/v04r00/access/shapefile/IBTrACS.NA.list.v04r00.points.zip"
tdir=tempdir()
download.file(dataurl,destfile=file.path(tdir,"temp.zip"))
unzip(file.path(tdir,"temp.zip"),exdir = tdir)
list.files(tdir)

#Storm_SHP
storm_data <- read_sf(list.files(tdir,pattern=".shp",full.names = T))

#Wrangle the Data
storms <- storm_data %>%
  filter(SEASON >= 1950) %>%
  mutate_if(is.numeric, function(x) ifelse(x==-999.0,NA,x)) %>%
  mutate(decade=(floor(year/10)*10))

region <- st_bbox(storms)

#Make First Plot
plot1 <- ggplot(world) + geom_sf() + facet_wrap(~decade) +
  stat_bin2d(data=storms, aes(y=st_coordinates(storms)[,2], x=st_coordinates(storms)[,1]),bins=100) +
  scale_fill_distiller(palette="YlOrRd", trans="log", direction=-1, breaks = c(1,10,100,1000)) +
  coord_sf(ylim=region[c(2,4)], xlim=region[c(1,3)]) + 
  labs(title = "Storms 1950 - Present", x = element_blank(), y = element_blank())
print(plot1)

#US States with Most Storms
transform_us <- st_transform(us_states, crs = st_crs(storms))
states <- transform_us %>%
  select(state = NAME)
storm_states <- st_join(storms, states, join = st_intersects,left = F)

#States with Most Storms Table
Top_5 <- storm_states %>%
  group_by(state) %>%
  summarize(storms=length(unique(NAME))) %>%
  arrange(desc(storms)) %>%
  slice(1:5) %>% st_set_geometry(NULL)

knitr::kable(Top_5, "pipe")


