Buffalo House Price Analysis
================
Erik Woyciesjes
October 15, 2020

# Introduction to problem/question

# Problem / Question

I would like to run an analysis on housing prices in the City of
Buffalo. I want to display my results with map visuals and provide a
multiple linear regression to illustrate what variables had the greatest
effect on housing prices. Additionally, I would like to compare the
change in housing prices across Buffalo neighborhoods. \# Inspiring
Examples

## Example 1

![](http://urbanspatialanalysis.com/wp-content/uploads/2017/02/plot4_point-map-e1487598425346-792x508.png)

![](http://urbanspatialanalysis.com/wp-content/uploads/2017/02/plot10_time-series-792x1056.png)
These types of graphics are what I hope to produce in my project. The
first using a Buffalo basemap and the Tax Assessor’s location data. The
facet function was used here to visualize the change in prices overtime,
which is one of the main goals of my project. The second, to show change
in housing prices based on neighborhood using ggplot. For more
information on this project see link.
<http://urbanspatialanalysis.com/dataviz-tutorial-mapping-san-francisco-home-prices-using-r/>

## Example 2

<https://rpubs.com/ablythe/520912>

This project aimed to predict the housing prices in California in 1990
based on a number of possible location-based predictors. Although this
project is a higher level than what I intend to do for my project, I
felt that this was an interesting example to explore the possibilities
within a housing price analysis. Examples of some graphics produced from
this project include Predicted Price Over/Under Actual Price.
![](https://github.com/geo511-2020/geo511-2020-tasks-erikwoyc/blob/master/predictive%20model.png?raw=true)

## Example 3

<https://github.com/vincnardelli/bayes-house-price> Like Example 2, this
project is a little more complicated than what I intend to do but a
similar data set (City Assessor’s data) was used to build this model,
and I found it helpful to understand how other people tackled this kind
of project.

# Proposed data sources

2017 – 2018
<https://data.buffalony.gov/Government/2017-2018-Assessment-Roll/bxmp-ux8w>

2019 – 2020
<https://data.buffalony.gov/Government/2019-2020-Assessment-Roll/kckn-jafw>

2020 – 2021 (Current)
<https://data.buffalony.gov/Government/Current-2020-2021-Assessment-Roll/4t8s-9yih>

Erie County Parcel Shapefiles (From Erie County GIS Services)

Neighborhood Shapefile
<https://data.buffalony.gov/Economic-Neighborhood-Development/Neighborhoods/q9bk-zu3p>

# Proposed methods

Proposed Packages: Ggplot2, ggmap, dplyr I intend to clean my data and
view a few variables using ggplot. I then intend to run a linear
regression (<http://r-statistics.co/Linear-Regression.html>), create a
summary table of results and some associated scatter plots to
investigate and visualize possible indicators of price. Using ggmap,
buffalo housing will be mapped based on price or potentially change in
price over time. Another possible avenue of investigation could be
looking at prices by year built and mapping changes in price based on
the decade a house was built. One possible way of doing this could be to
group houses (year built) in decades and run an ANOVA to examine
variance in housing prices by decade. Finally, I hope to create map that
shows a comparison of these results across neighborhoods using the
neighborhood shapefile.

# Expected results

I hope to successfully run some statistical analysis as outlined in
methods and produce some plots to visualize the data from that analysis.
In addition, maps will be created similar to example one as well as
possible a map showing prices or price change across neighborhoods. To
provide further detail of the data used in this analysis, plots of a
couple variables and summary tables of the linear regression will be
created as well.
