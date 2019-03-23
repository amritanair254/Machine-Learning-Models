#Load Packages and data--------------------------
library(readr)    #import the readr package
library(gmodels)  #functions to check out errors
library(class)    #functions for knn
library(ggplot2)  #functions for graphing
df = read_csv("C:/Users/amrit/Downloads/UniversalBank(1).csv")    #Read the file and save it to dataframe df
source('C:/Users/amrit/Downloads/BabsonAnalytics.R')              #For kNN function

#Prepping metadata-------------------------------
#Deleting all the other categorical variables and irrelevant variables
df$ID = NULL     
df$Family = NULL
df$Education = NULL
df$`Securities Account` = NULL
df$`CD Account` = NULL
df$Online = NULL
df$CreditCard = NULL
df$`ZIP Code` = NULL
df$`Personal Loan`=as.factor(df$`Personal Loan`)

#Exploring the data--------------------------------
#Checking how experience, avg spending on credit card, age, mortgage and income affects borrowing
qplot(Experience,Income, color = `Personal Loan`, data=df, ylim=c(50,200))
qplot(CCAvg,     Income, color = `Personal Loan`, data=df, ylim=c(50,200))
qplot(Age,       Income, color = `Personal Loan`, data=df, ylim=c(50,200))
qplot(Mortgage, Experience, color = `Personal Loan`, data=df, xlim=c(100,600))
qplot(CCAvg,    Experience, color = `Personal Loan`, data=df)

#Partitioning data---------------------------------
N = nrow(df)
trainingCases = sample(N,round(N*0.6))  
train = df[ trainingCases,]      #60% of data used for training
test  = df[-trainingCases,]      #40% of data used for testing
 
#Build model to predict----------------------------
k_best = kNNCrossVal(`Personal Loan` ~ ., train)           #Finding the best k
predictions = kNN(`Personal Loan` ~ ., train, test, k=5)   #Model that predicts knn
actuals     = test$`Personal Loan`                         #Find the actuals to compare with predictions

#performance of our model--------------------------
error_rate = sum(actuals != predictions)/nrow(test)     #8.9% error rate
Confusion_Matrix = CrossTable(predictions, actuals, expected = F, prop.r = F, prop.c = F, prop.t = F, prop.chisq = F)
# Confusion matrix shows that there are 134 false negatives and 44 false positives
benchmark_error = benchmarkErrorRate(train$`Personal Loan` , test$`Personal Loan`) #9.95%
sensitivity_rate = 65/199     #25.4%
specificity_rate = 1757/1801  #98.3%

#Our model without Normalization--------------------
predictions = kNN(`Personal Loan` ~ ., train, test, k=9, norm = F)   #Bad idea to turn it off
error_rateNoNorm = sum(actuals != predictions)/nrow(test)            #9.6% error rate. 
#By turning off normalization, the error rate went up by 0.7%


