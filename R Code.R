library(car)
Carsdata <- read.csv("C:\\D2WAP\\Training\\Analytics Accelerator\\Introduction to Statistics - Session 1\\cars.csv")
boxplot(Carsdata$Accelerate)
qqPlot(Carsdata$Accelerate)