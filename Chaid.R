### Setting Working Directory
setwd("C:/Users/rpand/Desktop/Documents/Classes/Classes/Decision_Tree_Accelerator")

## Load library "Party" for running the Decision Tree
library(partykit)

##Read the data in the file
cust_data<-read.csv("Default_On_Payment_CHAID.csv")

## Create a new Factor variable "Default_Payment" in the table
cust_data$Default_Payment <- factor(ifelse(cust_data$Default_On_Payment==1,"Default", "Non Default"))

## Check the distribution on new variable "Default_Payment"
table(cust_data$Default_Payment)
pie(table(cust_data$Default_Payment))

# Quick checks and exploration of the data
dim(cust_data)
str(cust_data)
head(cust_data)
tail(cust_data)
summary(cust_data)

# Conditional Inference Tree for Default_On_Payment
ctrl<- ctree_control(mincriterion = 0.95, minsplit = 100, minbucket = 100)
fit <- ctree(Default_Payment~ Housing+Status_Checking_Acc, 
             data=cust_data, control=ctrl)
plot(fit,main="Conditional Inference Tree for Default_Payment ")
print(fit)

###detailed results including splits
summary(fit)


