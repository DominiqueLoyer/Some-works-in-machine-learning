###
setwd("C:/Users/rpand/Desktop/Documents/Classes/Classes/Decision_Tree_Accelerator")
library(rpart)
##Read the data in the file
cust_data<-read.csv("Default_On_Payment_CHAID.csv")

###rpart(formula, data=, method=,control=) 
# fit<-rpart(Default_On_Payment~Status_Checking_Acc+Duration_in_Months+Credit_History+Purposre_Credit_Taken+
#              Credit_Amount+Savings_Acc+Years_At_Present_Employment+Inst_Rt_Income+Marital_Status_Gender+
#              Other_Debtors_Guarantors+Current_Address_Yrs+Property+Age+Other_Inst_Plans+Housing+
#              Num_CC+Job+Dependents+Telephone+Foreign_Worker,
#             data=cust_data, method="class", 
#            control=rpart.control(minsplit=20, cp=0.01))


fit<-rpart(Default_On_Payment~Status_Checking_Acc+Credit_History, data=cust_data, method="class", 
           control=rpart.control(minsplit=50, cp=0.001))


##display complexity parameter table
printcp(fit)

###plot cross-validation results
plotcp(fit)

###detailed results including splits
summary(fit)
printcp(fit)

###Prune the tree to the desired size ..at min error cp
pfit<- prune(fit, cp = 0.001)

###plot decision tree 
plot(pfit, uniform=TRUE, main="Classification Tree for Default_on_payment")

###label the decision tree plot 
text(pfit,splits = TRUE, use.n=TRUE, all=TRUE, cex=0.5, pretty=1)
labels(pfit)
library(rpart.plot)

prp(pfit, type=4, extra=4, under=TRUE)


