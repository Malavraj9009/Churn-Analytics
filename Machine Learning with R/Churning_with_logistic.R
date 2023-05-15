##CHURN ANALYTICS USING LOGISTIC REGRESSION

##Importing required libraries

library(ggplot2)
library(scales)

#reading the file
churn <- read.csv(file.choose(),header = T)
attach(churn)
cnnt <- plot(Churn,col=c('red','blue'))

##FINDING PROPORTION OF Churners and International Plans
cnt <- table(Churn,Int.l..Plan)
barplot(cnt,col =c('blue','red') )

##FINDING PROPORTION OF Churners and Customer Service Calls
cnts <- table(Churn,CustServ.Calls)
hist(CustServ.Calls,xlim = c(0,10),col = 'cyan',ylab = 'cnts',xlab = 'Cust_ser_Call',
     main = 'histogram of Cust_service Calls')


#overlayed Barchart
ggplot()+geom_bar(data=churn,aes(x=factor(CustServ.Calls),fill=factor(Churn)),
                  position='stack')+scale_x_discrete("Cust_serv_call")+
                  scale_y_continuous('Percent')+guides(fill=guide_legend(title='churn'))+
                  scale_fill_manual(values=c('blue','red'))


##normalied hist
ggplot()+geom_bar(data=churn,aes(x=factor(CustServ.Calls),fill=factor(Churn)),
                  position='fill')+scale_x_discrete("Cust_serv_call")+
                  scale_y_continuous('Percent')+guides(fill=guide_legend(title='churn'))+
                  scale_fill_manual(values=c('blue','red'))


##finding Correlation among day min, day call and day charge.
pairs(~Day.Mins+Day.Calls+Day.Charge)
days <- cbind(Day.Mins,Day.Calls,Day.Charge)

MinCAll <- cor.test(Day.Mins,Day.Calls) #testing correlation
MinCha <- cor.test(Day.Mins,Day.Charge)
callcha <- cor.test(Day.Calls,Day.Charge)

round(cor(days),4)
MinCAll$p.value
MinCha$p.value
callcha$p.value

## Dividing the dataset into training and testing using 75-25% split.
smp_size = floor(0.75*nrow(churn))
set.seed(125)

training_churn = sample(seq_len(nrow(churn)),size = smp_size)
training = churn[training_churn,]
testing = churn[-training_churn,]

table(training$Churn)
table(testing$Churn)

#Logistic regression for a dichotomous predictor,CONSIDERING ONLY Voice mail plan(VMP)
#as predictor.

cnts1 = table(Churn,VMail.Plan,dnn=c("Churn","Voice mail plan"))
sumtab = addmargins(cnts1,FUN = sum)
sumtab

# create dummy for voice mail plan(vmp)
VMP.ind = ifelse(VMail.Plan=="yes",1,0)

# run logistic regression
lr=glm(Churn~VMP.ind,data = churn,family = "binomial")
summary(lr)

## conclusion: as p-value of voice mail plan is less than 0.05 it is
#significant variable.


##CREATING LEVELS OF CUSTOMER CALLS
churnCSC = factor(CustServ.Calls)
levels(churnCSC)
levels(churnCSC)[0:1] = "Low"
levels(churnCSC)[2:3] = "Medium"
levels(churnCSC)[4:9] = "High"


##MAKING DUMMY VARIABLE
churn_med=ifelse(churnCSC=="Medium",1,0)
churn_hi=ifelse(churnCSC=="High",1,0)
table(Churn,churnCSC)


Cust_GLM = glm(Churn~churn_med+churn_hi,family = 'binomial',data = Churn)
summary(Cust_GLM)

##FROM THE SUMMARY WE CAN SEE THAT,THE CUSTOMER WHO HAD MADE HIGHER NUMBER OF CALLS,
##TEND TO CHURN MORE THAN OTHER CUSTOMERS WHO HAVE MADE MEDIUM NUMBER OF CALLS.
