#Load Packages and data----------------------------
library(e1071)
library(gmodels)
library(readr)
#source('C:/Users/amrit/Downloads/BabsonAnalytics.R')    
df = read_csv("C:/Users/amrit/OneDrive/Desktop/Wayfair/Wayfair-Babson_HackathonData 2019.csv") 
df2 = df

#Functions used in main program-------------------
benchmarkErrorRate = function(training, test){
  prop_train = as.data.frame(prop.table(table(training)))
  prop_train = prop_train[order(-prop_train$Freq),]
  dominant_class=prop_train[1,1]
  guess = as.character(dominant_class)
  percent_correct_simple=sum(guess == as.character(test))/length(test)
  return(1 - percent_correct_simple)
}

#Naive Bayes Linear Regression to predict Customer Purchase behavior on site========================
#Prepping the data---------------------------------
df$Purchased=as.factor(df$Purchased)            # Converting binary categorical vars to factors
df$ViewedProductInVisit=as.factor(df$ViewedProductInVisit)
df$ViewedSaleInVisit=as.factor(df$ViewedSaleInVisit)
df$PlacedSearch=as.factor(df$PlacedSearch)
df$ClickedBanner=as.factor(df$ClickedBanner)
df$AddedToBasket=as.factor(df$AddedToBasket)
df$VisitDate = NULL                             # Removing numerical or multifactored variables
df$UniqueVisitID = NULL
df$VisitorGroup = NULL
df$PlatformUsed = NULL                                        
df$VisitSource = NULL
df$VisitorGroup = NULL
df$BrowserName = NULL                                        
df$OSName = NULL
df$State = NULL
df$Gender = NULL
df$IncomeRange= NULL
df$SecondsOnSite= NULL
df$TotalPageViews= NULL

#Partitioning data---------------------------------
N = nrow(df)                       #No of observations
trainingCases = sample(N,round(N*0.6))  #indices of training cases
train = df[ trainingCases,]        #60% of data used for training
test  = df[-trainingCases,]        #40% of data used for testing


#Build model to predict----------------------------
model       = naiveBayes(Purchased ~ ., data = train)   #our Bayes model
predictions = predict(model, test, type = "class")
actuals     = test$Purchased   

#performance of our model--------------------------
benchmark_error  = benchmarkErrorRate(train$Purchased , test$Purchased) #2.5%
error_rate = sum(actuals != predictions)/nrow(test)     #our model's error rate is 3.4% error rate, 0.9% above benchmark
CrossTable(predictions, actuals, expected = F, prop.r = F, prop.c = F, prop.t = F, prop.chisq = F)

  #              |        actuals 
  #  predictions |         0 |         1 | Row Total | 
  # -------------|-----------|-----------|-----------|
  #            0 |    381699 |      8415 |    390114 | 
  # -------------|-----------|-----------|-----------|
  #            1 |      5372 |      1705 |      7077 | 
  # -------------|-----------|-----------|-----------|
  # Column Total |    387071 |     10120 |    397191 | 
  # -------------|-----------|-----------|-----------|

# Confusion matrix above shows that there are 8415/10120 false negatives(83%) and 5372/387071 false positives (1.4%)
#It can accurately predict people who don't buy

sensitivity_rate = 1705/10120       #16.8% we cannot successfully predict how many will buy
specificity_rate = 381699/387071  #98.6% we can very accurately predict how many will not buy!!


model  #View our model. Results given below:
# A-priori probabilities: 97.4% people probably won't purchase; 2.55% will    
# Conditional probabilities:
# Given that people don't purchase, 50% would probably view product during their visit
# Given that people don't purchase, 13.6% would probably view sale in visit
# Given that people don't purchase, 35.9% would probably place search
# Given that people don't purchase, 4.6% would probably click banner
# Given that people don't purchase, 8.6% would probably not add to basket




#Run above code separately
#Naive Bayes Regression to predict Customer Behavior====================================================================
#Prepping the data---------------------------------
df2$Purchased=as.factor(df2$Purchased)            # Converting binary categorical vars to factors
df2$VisitDate=as.factor(df2$VisitDate)
df2$VisitorGroup=as.factor(df2$VisitorGroup)
df2$PlatformUsed=as.factor(df2$PlatformUsed)
df2$VisitSource=as.factor(df2$VisitSource)
df2$BrowserName=as.factor(df2$BrowserName)
df2$OSName=as.factor(df2$OSName)

df2$State=NULL
df2$Gender=NULL
df2$IncomeRange=NULL
df2$UniqueVisitID = NULL
df2$ViewedProductInVisit = NULL                                        
df2$ViewedSaleInVisit = NULL
df2$PlacedSearch = NULL
df2$ClickedBanner = NULL
df2$AddedToBasket= NULL
df2$SecondsOnSite= NULL
df2$TotalPageViews= NULL
df2 <- na.omit(df2)

#Partitioning data---------------------------------
N = nrow(df2)                       #No of observations
trainingCases = sample(N,round(N*0.6))  #indices of training cases
train = df2[ trainingCases,]        #60% of data used for training
test  = df2[-trainingCases,]        #40% of data used for testing

#Build model to predict----------------------------
model       = naiveBayes(Purchased ~ ., data = train)   #our Bayes model
predictions = predict(model, test, type = "class")
actuals     = test$Purchased   

#performance of our model--------------------------
benchmark_error  = benchmarkErrorRate(train$Purchased , test$Purchased) #2.5%
error_rate = sum(actuals != predictions)/nrow(test)     #our model's error rate is 2.5% error rate, 0.9% above benchmark


model #Returns the following:
# Conditional probabilities:
#   VisitDate
# Y  7/1/2017  7/2/2017  7/3/2017  7/4/2017  7/5/2017  7/6/2017  7/7/2017  7/8/2017
# 0 0.1153053 0.1218521 0.1213819 0.1343773 0.1249576 0.1267282 0.1214990 0.1338985
# 1 0.1237744 0.1281832 0.1254195 0.1650984 0.1224584 0.1071922 0.1064684 0.1214055
# 
# Visitor Group
# Y   New Visitor   Prior Visitor     Prior Visitor with Purchase
# 0  24.4%          21.1%               54.5%
# 1  42.9%          14.4%               81.2%
# 
# Platform Used
# Y       Phone       Web
# 0       51.4%       48.5%
# 1       38.9%       61%
# 
# Visit Source
# Y   EmailFromWayfair ThirdPartyAds    WebSearch
# 0        17.4%          31.1%            51.5%
# 1        39.9%          24.5%            35.6%
# 
# BrowserName
# Y     Chrome   Microsoft Edge     Safari
# 0     45.6%        5.5%             48.8%
# 1     47.8%        5.3%             46.8%
# 
# OSName
# Y   Android   Chrome    iOS     Mac OS X    Windows
# 0   21.4%     0.8%      42.7%   11.1%       23.7%
# 1   12.5%     0.8%      36.3%   20.9%       29.3%

#-------------------------------------




