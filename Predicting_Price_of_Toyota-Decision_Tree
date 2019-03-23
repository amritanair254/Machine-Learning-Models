#Data Prep---------------------------------------
library(readr)
library(rpart)
library(rpart.plot)
library(gmodels)

df = read_csv("C:/Users/amrit/Downloads/ToyotaCorolla(1).csv")
source('C:/Users/amrit/Downloads/BabsonAnalytics.R')         
df$Model = NULL     #We don't want to chase nise...we dont want our model to split by id numbers
df$Automatic=as.factor(df$Automatic)
df$Met_Color=as.factor(df$Met_Color)
df$Fuel_Type=as.factor(df$Fuel_Type)
df$Doors=as.factor(df$Doors)

#Partitioning data---------------------------------
N = nrow(df)                     #No of rows
trainingCases = sample(N,round(N*0.6))  #indices of training cases
train = df[ trainingCases,]      #60% of data used for training
test  = df[-trainingCases,]      #40% of data used for testing

#Benchmarking--------------------------------------
prediction_bench = mean(train$Price)        
errors_bench = observations - prediction_bench
#Performance and Errors of benchmark
mape_bench = mean(abs(errors_bench/observations))   #The difference between observed and averge of prices was off by 25.7%
rmse_bench = sqrt(mean(errors_bench^2))             #The average is $10862 off from observed



#Regular tree Model-------------------------------------
model = rpart(Price ~ ., data = train)
rpart.plot(model)
#pred = predict(model, test, type = "class")
#Performance and Errors
predictions = predict(model,test)
observations = test$Price
errors = observations - predictions
hist(errors)
mape = mean(abs(errors/observations))   #On average this model is 10.53% off
rmse = sqrt(mean(errors^2))             #On average this model is $1395.45 off


#Tree with default stopping rules-----------
stoppingRules = rpart.control(minsplit = 20, minbucket = round(minsplit/3), cp = 0.01)
modelStopped = rpart( Price ~ ., data = train, control = stoppingRules)
rpart.plot(modelStopped)
#Performance and Errors
predictions = predict(modelStopped,test)
observations = test$Price
errors = observations - predictions
hist(errors)
mape = mean(abs(errors/observations))   #On average, this model is 12.3% off
rmse = sqrt(mean(errors^2))   #On average this model is $1618.07 off


#Build a tree with pruned branches-----------
stoppingRules = rpart.control(minsplit = 2, minbucket = 1, cp = 0)
modelOverfit = rpart( Price ~ ., data = train, control = stoppingRules)
pruned = easyPrune(modelOverfit)
rpart.plot(pruned)
#Performance and Errors
predictions = predict(pruned,test)
observations = test$Price
errors = observations - predictions
hist(errors)
mape = mean(abs(errors/observations))   #On average, this model is 9.08% off
rmse = sqrt(mean(errors^2))   #On average this model is $1173 off


#Linear Regression model----------------------------
lm_model = lm(Price ~ . , data = train)   # use lm() to run a linear regression of Price on all 11 predictors in the # training set. # use . after ~ to include all the remaining columns in train.df as predictors. 
lm_model = step(lm_model)
summary(lm_model)
#Performance and Errors
predictions = predict(lm_model,test)
observations = test$Price
errors = observations - predictions
hist(errors)
mape = mean(abs(errors/observations))   #On average, this model is 9.6% off
rmse = sqrt(mean(errors^2))   #On average this model is $1317.74 off

#What I learned---------------------------------
#On average the linear regression model is $1317.74 off, ie $1317.74 off
#On average the pruned tree model is 9.08% off, ie $1173 off
#On average, tree model with default stopping rules is 12.3% off, ie $1618.07 off
#Benchmark error was 25% or $10862 off

#Out of the models tried here, the best predictor is the pruned tree model, and it surpassed the benchmark errors the most
