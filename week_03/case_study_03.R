#Plot 1
library(ggplot2); library(gapminder); library(dplyr)
gapminder1 <- gapminder %>%
  filter(!country == "Kuwait")
Plot1 <- ggplot(gapminder1, aes(x = lifeExp, y = gdpPercap, color = continent,
                       size = pop/100000)) + geom_point() +
  facet_wrap(~year,nrow=1) + scale_y_continuous(trans = "sqrt") +
  theme_bw() + labs(title = "Wealth & Life Expectancy Through Time", 
                    x = "Life Expectancy", y = "GDP per Capita",
                    size = "Population (100k)", color = "Continent")
Plot1
##ggsave("Plot1.png", width = 15, units = "in")
#Plot 2 Data Prep
gapminder1 <- gapminder %>%
  filter(!country == "Kuwait")
gapminder_continent <- gapminder %>%
  filter(!country == "Kuwait") %>%
  group_by(continent,year) %>%
  summarize(gdpPercapweighted = weighted.mean(x = gdpPercap, w = pop),
            pop = sum(as.numeric(pop)))
#Plot 2
Plot2 <- ggplot(gapminder1, aes(x = year, y = gdpPercap, color = continent, 
                                group = country)) + 
  geom_line() + geom_point() +
  geom_line(gapminder_continent, mapping = aes(x = year, y = gdpPercapweighted),
            inherit.aes = FALSE) +
  geom_point(gapminder_continent, mapping = aes(x = year, y = gdpPercapweighted,
             size = pop/100000),inherit.aes = FALSE) +
  facet_wrap(~continent,nrow=1) + theme_bw() +
  labs(x="Year", y = "GDP per Capita", size = "Population (100k)", color = "Continent")
Plot2
##Collin & Kyle helped with idea for placement of inherit.aes = FALSE 
##ggsave("Plot2.png", width = 15, units = "in")