library(tidyverse)
library(ggplot2)
library(ggthemes)
#Was having trouble with url so I changed it to the second one provided
dataurl="https://raw.githubusercontent.com/AdamWilsonLab/GEO511/master/CS_02.csv"
temp=read_csv(dataurl,
              skip=1, #skip the first line which has column names
              na="999.90", # tell R that 999.90 means missing in this dataset
              col_names = c("YEAR","JAN","FEB","MAR", # define column names 
                            "APR","MAY","JUN","JUL",  
                            "AUG","SEP","OCT","NOV",  
                            "DEC","DJF","MAM","JJA",  
                            "SON","metANN"))
#Got the idea to create the data frame from Shruti in Group 5 Meeting
#Wanted to try to isolate data this way, rather than just using the JJA data set
Summer_temps <- data.frame(Year=temp[,1], Means=rowMeans(temp[7:9]))
view(Summer_temps)
# Create Graphic
casestudy2 <- ggplot(Summer_temps, aes(YEAR,Means)) +
  geom_line() +
  geom_smooth(color="red") + xlab("Year") + ylab("Mean Summer Temperatures (C)") +
  ggtitle("Mean Summer Temperature in Buffalo NY") +
  labs(subtitle = "Summer Includes June, July and August", 
       caption = ~ atop(paste("Data from the Global Historical Climate Network"),
                        paste("Red Line is a LOESS Line"))) +
  theme_economist()
casestudy2
ggsave(filename = "casestudy2.png")