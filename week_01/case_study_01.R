data(iris) #Load data set
library(ggplot2)
#Calculate Mean and Save it as an Object
petal_length_mean <- mean(iris$Petal.Length) 
petal_length_mean
#Plot the Distribution of the Petal Length Column as a Histogram
ggplot(data=iris, aes(x = iris$Petal.Length, fill = Species)) +
  geom_histogram(breaks=seq(0,7, by=1)) +
  labs(Title="Histogram for Petal Length", x="Petal Length", y="Count") + 
  scale_x_continuous(breaks=c(0,1,2,3,4,5,6,7))