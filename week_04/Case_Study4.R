library(tidyverse)
library(nycflights13)
library(dplyr)
# Exploring Data
str(airports)
view(airports)
str(flights)
view(flights)
## Table Join
Airport_join <- left_join(flights, airports, by = c("dest" = "faa"))
##Farthest Airport
Farthest_Airport <- Airport_join %>%
  arrange(desc(distance)) %>%
  slice(1)
view(Farthest_Airport$name)
## Answer
## Honolulu Intl