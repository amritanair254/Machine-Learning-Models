#Load Packages and data----------------------------
library(e1071)
library(gmodels)
library(readr)
source('C:/Users/amrit/Downloads/BabsonAnalytics.R')         
df = read_csv("C:/Users/amrit/Downloads/UniversalBank(1).csv")    #Read the file and save it to dataframe df

#Prepping the data---------------------------------
df$`Personal Loan`=as.factor(df$`Personal Loan`)    #Converting categorical vars to factors
df$`Securities Account`=as.factor(df$`Securities Account`)
df$`CD Account`=as.factor(df$`CD Account`)
df$Online=as.factor(df$Online)
df$CreditCard=as.factor(df$CreditCard)
df$ID = NULL                                        #Removing numerical variables
df$Age = NULL
df$Experience = NULL
df$Income = NULL
df$`ZIP Code` = NULL
df$Family = NULL
df$CCAvg = NULL
df$Education = NULL
df$Mortgage = NULL

#Partitioning data---------------------------------
N = nrow(df)                     #No of rows
trainingCases = sample(N,round(N*0.6))  #indices of training cases
train = df[ trainingCases,]      #60% of data used for training
test  = df[-trainingCases,]      #40% of data used for testing

#Build model to predict----------------------------
model       = naiveBayes(`Personal Loan` ~ ., data = train) #our Bayes model
predictions = predict(model, test, type = "class")
actuals     = test$`Personal Loan`                          #Find the actuals to compare with predictions

#performance of our model--------------------------
error_rate = sum(actuals != predictions)/nrow(test)     #10.1% error rate
CrossTable(predictions, actuals, expected = F, prop.r = F, prop.c = F, prop.t = F, prop.chisq = F)
# Confusion matrix shows that there are 200 false negatives and 2 false positives
benchmark_error  = benchmarkErrorRate(train$`Personal Loan` , test$`Personal Loan`) #10.05%
sensitivity_rate = 1/201     #0.5% we cannot accurately predict how many will borrow
specificity_rate = 1797/1799  #99.8% we can very accurately predict how many will not borrow!!


#Exploring probabilities------------------------------
model$apriori   #2721 out of all customers didn't borrow and  279 did borrow 
#Prior probability without any additional info = 279/(279+2721) would borrow



model$tables$`Securities Account` 
#Given customers borrowed, 87.1% did not open securities account and 0.13% did
#Given customers didn't borrow, 89.5% did not open securities account and 10.4% did
model$tables$`CD Account`
#Given customers borrowed, 70.9% did not open CD account and 29.03% did
#Given customers didn't borrow, 96.47% did not open CD account and 3.5% did
model$tables$Online
#Given customers borrowed, 42.6% did not engage in online banking and 57.3% did
#Given customers didn't borrow, 39.9% did not engage in online banking and 60.05% did
model$tables$CreditCard
#Given customers borrowed, 71.32% did not use UB credit cards and 28.67% did
#Given customers didn't borrow, 71.37% did not use UB credit cards and 28.63% did


limited = df[df$`Securities Account` == "1",] 
likely_securities=sum(limited$`Personal Loan`=="1")/sum(limited$`Personal Loan`=="0")
#If people opened securities account, it is 0.12 times more likely that people would borrow than not

limited = df[df$`CD Account` == "1",] 
likely_CD=sum(limited$`Personal Loan`=="1")/sum(limited$`Personal Loan`=="0")
#If people opened CD account, it is 0.86 times more likely that people would borrow than not

limited = df[df$Online == "1",] 
likely_Online=sum(limited$`Personal Loan`=="1")/sum(limited$`Personal Loan`=="0")
#If people engaged in Online banking, it is 0.108 times more likely that people would borrow than not

limited = df[df$CreditCard == "1",] 
likely_CC=sum(limited$`Personal Loan`=="1")/sum(limited$`Personal Loan`=="0")
#If people used UB credit crads, it is 0.107 times more likely that people would borrow than not

