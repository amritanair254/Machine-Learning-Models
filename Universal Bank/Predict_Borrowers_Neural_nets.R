#Load Packages and data----------------------------
library(readr)
library(nnet)
library(caret)
library(NeuralNetTools)
library(e1071)
library(gmodels)

source('C:/Users/amrit/Downloads/BabsonAnalytics.R')         
df = read_csv("C:/Users/amrit/Downloads/UniversalBank(1).csv")

#Prepping the data---------------------------------
df$`Personal Loan`=as.factor(df$`Personal Loan`)    #Converting categorical vars to factors
df$Education=as.factor(df$Education)
df$`Securities Account`=as.factor(df$`Securities Account`)
df$`CD Account`=as.factor(df$`CD Account`)
df$Online=as.factor(df$Online)
df$CreditCard=as.factor(df$CreditCard)

df$ID = NULL
df$`ZIP Code` = NULL

normalizer = preProcess(df, method=c("range")) #Normalize dataframe using caret package functions
df = predict(normalizer, df)

#Partitioning data-----------------------------------------
N = nrow(df)                     #No of rows
trainingCases = sample(N,round(N*0.6))  #indices of training cases
train = df[ trainingCases,]      #60% of data used for training
test  = df[-trainingCases,]      #40% of data used for testing

#Benchmark error rate---------------------------------------
error_bench = benchmarkErrorRate(train$`Personal Loan`, test$`Personal Loan`) 
# bench error = 9.2% for this random sample



#Neural Nets===========================================================
#Build Neural Net model to predict-------------------------
model = nnet(`Personal Loan` ~ ., data=train, size = 4)
plotnet(model)
predNum = predict(model, test)
pred = as.factor(predict(model, test, type="class"))

#Performance of neural net model----------------------------
error_nn = sum(pred != test$`Personal Loan`)/nrow(test)   
# model error = 2.75% for this random sample, error rate is significantly improved using neural nets
CrossTable(pred, test$`Personal Loan`, expected = F, prop.r = F, prop.c = F, prop.t = F, prop.chisq = F)
# Confusion matrix shows that there are 22 false negatives and 33 false positives
#sensitivity_rate: 88% accuracy in predicting how many will borrow
#specificity_rate: 98.2% we can very accurately predict how many will not borrow!!




#Naive Bayes=============================================================
#Build Naive Bayes model to predict----------------------------
model       = naiveBayes(`Personal Loan` ~ ., data = train) #our Bayes model
predictions = predict(model, test, type = "class")
actuals     = test$`Personal Loan`                          #Find the actuals to compare with predictions

#performance of our Naive Bayes model--------------------------
error_nb = sum(actuals != predictions)/nrow(test)     #9.35% error rate
CrossTable(predictions, actuals, expected = F, prop.r = F, prop.c = F, prop.t = F, prop.chisq = F)
# Confusion matrix shows that there are 62 false negatives and 125 false positives
#sensitivity_rate: 66.3% accuracy in predicting how many will borrow
#specificity_rate: 93.1% we can very accurately predict how many will not borrow!!




