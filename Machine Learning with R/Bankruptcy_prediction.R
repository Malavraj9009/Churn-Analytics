## bankrupt
## all ratio variables
## target = 0 if bankrupt,1 is solvent

library(ROCR)
library(caTools)

bankruptcy = read.csv(file.choose(),header = T)
attach(bankruptcy)
View(bankruptcy)

## checking NA
summary(bankruptcy)
is.na(bankruptcy)

## transforming variables
logx1=log(X1)
logx1

## logistic regression
bank_model = glm(Y~X1+X2+X3,data = bankruptcy,family = 'binomial')
summary(bank_model)

# removing X3 as p-value is greater
bank_model12 = glm(Y~X1+X2,data = bankruptcy,family = 'binomial')
summary(bank_model12)

# removing X2 as p-value is greater
bank_model123 = glm(Y~X1,data = bankruptcy,family = 'binomial')
summary(bank_model123)

## prediction for table
pred = predict(bank_model123,type = 'response')

## classification table
class_tab = table(Y,pred>0.5)
class_tab

##TAN = TN+FP , TAP = TP+FN
##SENSITIVITY = TP/(FN+TP)
##SPECIFICITY = TN/(TN+FP)


ROCRpred = prediction(pred,Y)
ROCRperf = performance(ROCRpred,'tpr','fpr')
plot(ROCRperf)
## graph closer to borders are good graph
## closer to diagonal is not a good result

##AUC CURVE
auc = performance(ROCRpred,measure = "auc")
auc = auc@y.values
auc
