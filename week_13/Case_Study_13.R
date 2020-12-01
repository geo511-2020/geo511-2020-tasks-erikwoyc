library(dplyr)
library(ggplot2)
library(ggmap)
library(htmlwidgets)
library(tidyverse)
library(rnoaa)
library(xts)
library(dygraphs)

d=meteo_tidy_ghcnd("USW00014733",
                   date_min = "2016-01-01", 
                   var = c("TMAX"),
                   keep_flags=T) %>% 
  mutate(date=as.Date(date),
         tmax=as.numeric(tmax)/10) #Divide the tmax data by 10 to convert to degrees.

summary(d)
head(d)

weather_xts <- xts(d$tmax, order.by=d$date)

plot <- dygraph(weather_xts, main = "Daily Maximum Temperature in Buffalo, NY") %>%
  dyRangeSelector(dateWindow = c("2020-01-01", "2020-10-31"))
plot

a=meteo_tidy_ghcnd("USW00014733",
                   date_min = "2016-01-01", 
                   var = c("prcp"),
                   keep_flags=T) %>% 
  mutate(date=as.Date(date))

precip <- xts(a$prcp, order.by=a$date)

# Creating Two Dygraphs was aided by https://stackoverflow.com/questions/32502506/dygraph-in-r-multiple-plots-at-once
# create a list of dygraphs objects
library(htmltools)
dy_graph <- list(
  dygraphs::dygraph(weather_xts, group="temp_rain", main="Daily Maximum Temperature in Buffalo, NY") %>%
    dyRangeSelector(dateWindow = c("2020-01-01", "2020-10-31")),
  dygraphs::dygraph(precip, group="temp_rain", main="Daily Precipitation in Buffalo, NY")%>%
    dyRangeSelector(dateWindow = c("2020-01-01", "2020-10-31"))
)  

# render the dygraphs objects using htmltools
htmltools::browsable(htmltools::tagList(dy_graph))