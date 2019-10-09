### Setting Working Directory
setwd("C:/Users/rpand/Desktop/Documents/Classes/Classes/Linear_Regression_Accelerator")
library(lmtest)
library(car)


##Read the data in the file
DefaultData <-read.csv("Linear_Regression.csv")

#Check what is there in the file

View(DefaultData)

#Check if the data is populated/imported properly
head(DefaultData)
tail(DefaultData)

#Check the summary of the file
summary(DefaultData)

#Generate plot of Dependent variable (Losses)
plot(DefaultData$Losses)

#Check the quantile to find out the outlier limit
quantile(DefaultData$Losses, c(0,0.05,0.1,0.25,0.5,0.75,0.90,0.95,0.99,0.995,1))

#Creating the Capped Losses column with 1200 cap
DefaultData$CappedLosses<-ifelse(DefaultData$Losses>1200,1200,DefaultData$Losses)

#Check if Capped Losses column has been created properly or not
summary(DefaultData)
names(DefaultData)

#Create new object deleting Losses and S.no.
DefaultData3<-DefaultData[,-c(1,9)]

#Check the headings of the new object
names(DefaultData3)

#Generate plots to see the relation between the independent variables and the dependent variable
plot(DefaultData3$Age,DefaultData3$CappedLosses)
plot(DefaultData3$Years.of.Driving.Experience,DefaultData3$CappedLosses)
plot(DefaultData3$Number.of.Vehicles,DefaultData3$CappedLosses)
plot(DefaultData3$Gender,DefaultData3$CappedLosses)
plot(DefaultData3$Married,DefaultData3$CappedLosses)
plot(DefaultData3$Vehicle.Age,DefaultData3$CappedLosses)
plot(DefaultData3$Fuel.Type,DefaultData3$CappedLosses)

#We will use the data which has been converted into bands and dummy variables. Read the final Data
DefaultData4<-read.csv("Linear_Reg_Sample_Data.csv")

#Look at the column Headings
names(DefaultData4)


#Create linear function for vif
vif_data<-lm(Capped_Losses~Years_Drv_Exp+Number_Vehicles+Average_Age+Gender_Dummy+Married_Dummy+Avg_Veh_Age+Fuel_Type_Dummy,data=DefaultData4)

#Check Vif, vif>2 means presence of multicollinearity
vif(vif_data)

#Compare R-square of Average_Age and Years_Drv_Exp to check which performs better
age1<-lm(Capped_Losses~Average_Age,data=DefaultData4)
drv1<-lm(Capped_Losses~Years_Drv_Exp,data=DefaultData4)
summary(age1)
summary(drv1)
#keep Average_Age and remove Years_Drv_Exp
#In same way we can decide to keep Age band as compared to Age and Vehicle Age Band as compared to Vehicle Age

#Run Linear Regression w/o Years_Drv_Exp
lin_r1<-lm(Capped_Losses~Number_Vehicles+Average_Age+Gender_Dummy+Married_Dummy+Avg_Veh_Age+Fuel_Type_Dummy,data=DefaultData4)

#Let's look at the results
summary(lin_r1)
#Remove Number_Vehicles
lin_r<-lm(Capped_Losses~Age+Average_Age+Gender_Dummy+Married_Dummy+Avg_Veh_Age+Fuel_Type_Dummy,data=DefaultData4)


#Run Linear Regression w/o Number_Vehicles
lin_r2<-lm(Capped_Losses~Average_Age+Gender_Dummy+Married_Dummy+Avg_Veh_Age+Fuel_Type_Dummy,data=DefaultData4)
summary(lin_r2)

#Let's look at the results
summary(lin_r2)

bptest(lin_r2)

#Variance Covariance Matrix

library("sandwich")
vcovHC(lin_r2,omega=NULL, type="HC4")

#Fixing Heteroskedasticity using "Variance-Covariance" matrix

library("lmtest")
coeftest(lin_r2,df=Inf,vcov=vcovHC(lin_r2,type="HC4"))


