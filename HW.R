#Setting working directory
setwd("C:/Users/rpand/Desktop/Documents/Classes/Classes/Time_Series_Accelerator")

library("forecast")

#Reading the data
sales <- read.csv("sales.csv")

#What kind of data we have
sales
head(sales)
tail(sales)

#Create time series from the input data, [,1] is for first column and all rows. freq = 12, is for 12 months. For quarters it will be freq = 4
sales <- ts(sales[,1],start=1995,freq=12)

#Let's view, what is the output
sales

#Plot Time Series
plot(sales)

#Divides into Seasonal, Trend and Remainder. S.Window controls how rapidly the seasonal component can change

apts <- decompose(sales)

plot (apts)

autoexp <- ets(sales)

autoexp

error_estimate_Exp <- accuracy(autoexp)

forecast(autoexp, 5)

plot(forecast(autoexp, 5))

sales.pred <- predict(autoexp,5,prediction.interval=TRUE)

sales.pred

write.csv(sales.pred, "sales_pred.csv")


# ME: Mean Error
# RMSE: Root Mean Squared Error
# MAE: Mean Absolute Error
# MPE: Mean Percentage Error
# MAPE: Mean Absolute Percentage Error
# MASE: Mean Absolute Scaled Error
# ACF1: Autocorrelation of errors at lag 1.
