#load the data---------------
library(readr)    #import the readr package
df=read_csv("C:/Users/amrit/Downloads/ToyotaCorolla(1).csv")    #Read the file and save it to dataframe df
#You can remove data/variable using remove(df)
#Manage the data------------
df$Model     = as.factor(df$Model)
df$Fuel_Type =  as.factor(df$Fuel_Type)
df$Met_Color = as.logical(df$Met_Color)
df$Automatic =as.logical(df$Automatic)
df$Model     =NULL


# partition data -----------
n             = nrow(df)                    #No of observations
training_size = round(n*0.6)                #training size is 60% of total observations
training_rows = sample(n, training_size)    #simple random sample
training      = df[training_rows,]          #create new df
testing       = df[-training_rows,]         



#Build the model----------
my_model = lm(Price ~ . , data = training)   # use lm() to run a linear regression of Price on all 11 predictors in the # training set. # use . after ~ to include all the remaining columns in train.df as predictors. 
my_model = step(my_model)
summary(my_model)

#my_model = lm(Price ~ ., data = training)
#summary(my_model)

#make prediction-------
predictions = predict(my_model,testing)
observations = testing$Price
errors = observations - predictions


#evaluate performance-----
hist(errors)
mape = mean(abs(errors/observations))   #On average I am 11% off
rmse = sqrt(mean(errors^2))   #On average I am $1651 off

#benchmarking
prediction_bench = mean(training$Price)
errors_bench = observations - prediction_bench

mape_bench = mean(abs(errors_bench/observations))   #On average I am 11% off
rmse_bench = sqrt(mean(errors_bench^2))   #On average I am $1651 off


