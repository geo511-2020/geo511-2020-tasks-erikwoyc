install.packages("reprex")
reprex::reprex({
library(tidyverse)
library(reprex)
library(sf)
library(spData)
library(ggplot2)
data(world)

# Run Reprex
ggplot(world,aes(x=gdpPercap, color=continent)) +
  geom_density(alpha=0.5,color=F)}, venue = "html")

#Current Code with error
ggplot(world,aes(x=gdpPercap, y=continent, color=continent))+
  geom_density(alpha=0.5,color=F)
