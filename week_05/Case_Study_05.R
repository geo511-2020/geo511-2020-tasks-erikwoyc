library(spData)
library(sf)
library(tidyverse)
library(units)
# Data
data(world)
data(us_states)

# Transformation
st_crs(world)
albers="+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 
+x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
world_albers <- st_transform(world, albers)

#Filter Canada
Canada <- world_albers %>% 
  filter(name_long == "Canada")
# Buffer
Canada_Buffer <- st_buffer(Canada, dist = 10000)

#States Object
us_states_albers <- st_transform(us_states, albers)
NY <- us_states_albers %>%
  filter(NAME == "New York")

# Create Border Object
Border <- st_intersection(Canada_Buffer, NY)
# Plot
Final_Plot <- ggplot() + geom_sf(data = NY) + 
  geom_sf(data = Border, aes(fill = 'red')) +
  labs(title = "New York Land within 10km")
Final_Plot
#Area Calculation
Border_area <- st_area(Border)
set_units(Border_area, km^2)
# Save Plot
ggsave(filename = "Final_Plot.png")