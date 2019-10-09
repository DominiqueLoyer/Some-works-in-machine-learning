##Step 1 - setwd

setwd("C:/Users/rpand/Desktop/Documents/Classes/Classes/Linear_Logistic_Regression_R")

##Step 2 - Load Data

logdata <- read.csv("Default_On_Payment.csv")

##Step 3 - Explore Data- where are the missing values? why some are NAs and others no values?

dim(logdata)
names(logdata)
head(logdata)
tail(logdata)
str(logdata)
summary(as.factor(logdata$Default_On_Payment))
summary_logdata = summary(logdata)
write.csv(summary_logdata,"summary_logdata.csv",row.names=F)

##Step 4 - Remove any rows with missing values and removed customer_id

logdata <- logdata[complete.cases(logdata),]

logdata <- logdata[,c(-1)]

# Step 5- Bivariate Analyis 

library(gmodels)

CrossTable(logdata$Status_Checking_Acc,logdata$Default_On_Payment) 
CrossTable(logdata$Status_Checking_Acc,logdata$Default_On_Payment,expected=FALSE,prop.r=FALSE, prop.c=FALSE,prop.t=FALSE, prop.chisq=FALSE,chisq=FALSE) 
CrossTable(logdata$Status_Checking_Acc,logdata$Default_On_Payment,expected=FALSE, prop.c=FALSE,prop.t=FALSE, prop.chisq=FALSE,chisq=FALSE) 


# Step 6- WOE & IV

library(Information)
library(riv)
library(devtools)
library(woe)
library(gridExtra)


# Step 6a Generate IV of each independent factor- what variables we want to select for building model?

stat <- create_infotables(data=logdata, y = "Default_On_Payment")
grid.table(stat$Summary, rows=NULL)
write.csv(stat$Summary,"IV_summary.csv",row.names=F)


# Step 6b- subset the data to select only significant variables

newdata <- subset(logdata, select = c(Status_Checking_Acc, Duration_in_Months, Credit_History, Savings_Acc, Purposre_Credit_Taken, Age, Property, Years_At_Present_Employment, Housing, Other_Inst_Plans, Default_On_Payment))


# 6c Generate WOE table for each independed factor

stat <- create_infotables(data=newdata, y = "Default_On_Payment")

grid.table(stat$Tables$Status_Checking_Acc, rows=NULL)



# Step 7 - Build Linear Reg model

library(car)

linreg = lm(Default_On_Payment~., data = newdata)
summary(linreg)
plot(predict(linreg)) #predicted values lie outside 0 to 1 range - hence improper model fit
plot(linreg)


# Step 8- Transformmation/Dummy coding of variables for getting the best model
write.csv(stat$Tables$Duration_in_Months,"Duration_summary.csv",row.names=F)
newdata$Duration_Category <- ifelse(newdata$Duration_in_Months %in% c("4","5","6", "7", "8", "9", "10", "11","12", "13", "14", "15", "16", "17", "18"),"lessthan20", ifelse(newdata$Duration_in_Months %in% c("20","21","22", "24", "26", "27", "28", "30","33", "36", "39"), "20to40", "morethan40"))
table(newdata$Duration_in_Months, newdata$Duration_Category)
stat <- create_infotables(data=newdata, y = "Default_On_Payment")
newdata$Duration_Dummy_20 <- ifelse(newdata$Duration_Category == "lessthan20", 1,0)
newdata$Duration_Dummy_40 <- ifelse(newdata$Duration_Category == "20to40", 1,0)
newdata$Status_Checking_Acc_A11 <- ifelse(newdata$Status_Checking_Acc == "A11", 1,0)
newdata$Status_Checking_Acc_A12 <- ifelse(newdata$Status_Checking_Acc == "A12", 1,0)
newdata$Status_Checking_Acc_A13 <- ifelse(newdata$Status_Checking_Acc == "A13", 1,0)
newdata$Default_On_Payment1 <- as.factor(ifelse(newdata$Default_On_Payment == 1,"1","0"))


#Step 9 - Build logistic regression

library(ROCR)

logreg <- glm(Default_On_Payment ~ Duration_Dummy_20+Duration_Dummy_40+Status_Checking_Acc_A11+Status_Checking_Acc_A12+Status_Checking_Acc_A13+Age, family = binomial("logit"),data = newdata)
summary(logreg)
plot(predict(logreg,type="response")) #note plot option has type to get inverse of log_odds

newdata$predicted = predict(logreg,type="response")
write.csv(newdata,"output_logreg.csv", row.names = F)

capture.output(summary(logreg), file = "summary_logreg.csv")
summary_residuals_model.csv<-residuals(logreg, type="deviance")
write.csv(summary_residuals_model.csv, "summary_residuals_model.csv")

#Step 10 - Model Diagnostics

# ROCR

newdata$predicted = predict(logreg,type="response")
pred<-prediction(newdata$predicted,newdata$Default_On_Payment)
perf <- performance(pred,"tpr","fpr")
plot(perf)
abline(a=0, b=1, col="Red")

# AUC
auc.perf = performance(pred, measure = "auc")
auc.perf@y.values

# Create Decile by scorebands

library(dplyr)
newdata$decile <- ntile(-newdata$predicted,10)
write.csv(newdata, "newdata.csv")

# KS

max(attr(perf,'y.values')[[1]]-attr(perf,'x.values')[[1]])

# Lift
lift.obj <- performance(pred, "lift", x.measure = "rpp")
plot(lift.obj,
     main="Cross-Sell - Lift Chart",
     xlab="% Populations",
     ylab="Lift",
     col="blue")
abline(1,0,col="grey")

# Lorenz and Gini
library(ineq)
# Gini Index
ineq(newdata$predicted,type="Gini")

## Lorenz Curve
plot(Lc(newdata$predicted),col="darkred",lwd=2)

##Get Concordance/Pairs Stats

# High-Low Ratio of bad rate by decile in Excel. 
# Confidence interval for ROC, KS, Gini
#  mydata[ which(mydata$gender=='F' & mydata$age > 65), ]



Concordance = function(y,yhat) 
{
  outcome_and_fitted_col<-data.frame(y, yhat)
  colnames(outcome_and_fitted_col)<-c("Responder","fitted.values")
  # get a subset of outcomes where the event actually happened
  ones = outcome_and_fitted_col[outcome_and_fitted_col[,1] == 1,]
  # get a subset of outcomes where the event didn't actually happen
  zeros = outcome_and_fitted_col[outcome_and_fitted_col[,1] == 0,]
  # Equate the length of the event and non-event tables
  if (length(ones[,1])>length(zeros[,1])) {ones = ones[1:length(zeros[,1]),]}
  else {zeros = zeros[1:length(ones[,1]),]}
  # Following will be c(ones_outcome, ones_fitted, zeros_outcome, zeros_fitted)
  ones_and_zeros = data.frame(ones, zeros)
  # initiate columns to store concordant, discordant, and tie pair evaluations
  conc = rep(NA, length(ones_and_zeros[,1]))
  disc = rep(NA, length(ones_and_zeros[,1]))
  ties = rep(NA, length(ones_and_zeros[,1]))
  for (i in 1:length(ones_and_zeros[,1])) {
    # This tests for concordance
    if (ones_and_zeros[i,2] > ones_and_zeros[i,4])
    {conc[i] = 1
    disc[i] = 0
    ties[i] = 0
    }
    # This tests for a tie
    else if (ones_and_zeros[i,2] == ones_and_zeros[i,4])
    {
      conc[i] = 0
      disc[i] = 0
      ties[i] = 1
    }
    # This should catch discordant pairs.
    else if (ones_and_zeros[i,2] < ones_and_zeros[i,4])
    {
      conc[i] = 0
      disc[i] = 1
      ties[i] = 0 
    }
  }
  # Here we save the various rates
  conc_rate = mean(conc, na.rm=TRUE)
  disc_rate = mean(disc, na.rm=TRUE)
  tie_rate = mean(ties, na.rm=TRUE)
  return(list(concordance=conc_rate, num_concordant=sum(conc), discordance=disc_rate, num_discordant=sum(disc), tie_rate=tie_rate,num_tied=sum(ties)))
}

Concordance_test<-Concordance(newdata$Default_On_Payment,newdata$predicted)

Concordance_test
