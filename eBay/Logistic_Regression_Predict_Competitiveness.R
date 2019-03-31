#Data Prep---------------------------------------
library(readr)
library(gmodels)

df = read_csv("C:/Users/amrit/Downloads/eBayAuctions.csv")
source('C:/Users/amrit/Downloads/BabsonAnalytics.R')         
df$ClosePrice = NULL     #Removing from df
df$Competitive=as.logical(df$Competitive)
df$Category=as.factor(df$Category)
df$EndDay=as.factor(df$EndDay)
df$Currency=as.factor(df$Currency)

#Partitioning data---------------------------------
N = nrow(df)                     #No of rows
trainingCases = sample(N,round(N*0.6))  #indices of training cases
train = df[ trainingCases,]      #60% of data used for training
test  = df[-trainingCases,]      #40% of data used for testing
errorBench = benchmarkErrorRate(train$Competitive,test$Competitive) #Benachmark error = 46.38% for a random sample

#Logistic Regression Model-------------------------------------
model = glm(Competitive ~ ., data = train, family=binomial) #logistic regression model
summary(model)
pred = predict(model,test, type ="response")
predTF = (pred >0.5)                                   #Only 50% sure to predict true 
errorRate = sum(predTF != test$Competitive)/nrow(test) #36.5% error rate for the same random sample

model = step(model)    #Removed Duration from the model because it is very insignificant
summary(model)

#Model Performance-------------------------------------
pred = predict(model,test, type ="response")
predTF = (pred >0.5)
errorRate = sum(predTF != test$Competitive)/nrow(test) #36.37% error rate for the same random sample
table(predTF,test$Competitive)

ROCChart(test$Competitive,pred)
liftChart(test$Competitive, pred)
