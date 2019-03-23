# ?2014-2015 by Davit Khachatryan
# Scale just some of the columns of a data frame.
# 
# INPUTS
#   mydata    Data frame.
#   col_nums  Column numbers (not names!) that you want standardized 
#             -- you can get these using str()
#
# OUTPUT
#   A new data frame with the desired columns standardized.
#
# EXAMPLE
#   standardized = easyStandardize(training,c(1,2,3))


easyStandardize <- function(mydata, col_nums){
  cols_for_standard=as.matrix(mydata[,col_nums])
  standardized=scale(cols_for_standard)
  all_col_nums=c(1:length(names(mydata)))
  remaining_cols=as.vector(all_col_nums[is.na(pmatch(all_col_nums, col_nums))])
  remaining_data=subset(mydata,select=remaining_cols)
  mydata=cbind(remaining_data, standardized)
  return(mydata)
}


benchmarkErrorRate <- function(training, test){
  prop_train = as.data.frame(prop.table(table(training)))
  prop_train=prop_train[order(-prop_train$Freq),]
  dominant_class=prop_train[1,1]
  guess = as.character(dominant_class)
  percent_correct_simple=sum(guess == as.character(test))/length(test)
  return(1 - percent_correct_simple)
}

# ?2014-2015 by Davit Khachatryan
# Create an set of indices for a training set 
# whose proportions of the categorical target variable are as close as possible to 
# the proportions observed in the full data frame. 
# 
# INPUTS
#   mydata      Data frame
#   myresponse  Categorical response variable 
#               
# OUTPUT
#   A vector of indices defining a training set whose myresponse values occur
#   in the same proportion as the full dataframe, mydata.
#
# EXAMPLE
#   idices   = proportionalPartition(df,df$target)
#   training = df[idx,]
#   test     = df[-idx]
proportionalPartition <- function(mydata, myresponse, p=0.8){
nobs=dim(mydata)[1]
set.seed(1) #sets the seed for random sampling

prop = prop.table(table(myresponse))
length.vector = round(p*nobs*prop)
train_size=sum(length.vector)
test_size=nobs-train_size
class.names = as.data.frame(prop)[,1]
numb.class = length(class.names)
train_index = c()

for(i in 1:numb.class){
  index_temp = which(myresponse==class.names[i])
  train_index_temp = sample(index_temp, length.vector[i], replace = F)
  train_index = c(train_index, train_index_temp)
}

return(train_index)
}


# ?2014-2015 by Nathan Karst
# Creates a lift chart from  
# 
# INPUTS
#   obs         Binary (0/1 or TRUE/FALSE) observations
#   pred        Numerical predictions in [0,1]
#               
# OUTPUT
#   A lift chart with both 
#
# EXAMPLE
#df = read.csv("C:/users/nkarst/Dropbox/MyQTM2000/Rawdata/gender_height_hope_college.csv")
#df$IsMaleTF = (df$IsMale == 1)
#model = glm(IsMaleTF ~ Height, data=df, family = binomial(logit))
#pred    = predict(model, df, type = "response")
#liftChart(df$IsMale, pred)
liftChart <- function(obs, pred){ 
out <- tryCatch(
{
  library(ggplot2)
},
error=function(cond) {
  install.packages("ggplot2")
  library(ggplot2)
})  

df = data.frame(obs,pred)  
df = df[order(-pred),]

y = c(0, cumsum(df[,1]))
df1 = data.frame("Index"=0:length(pred),"y"=y,"type"="Observed")
df2 = data.frame("Index"=0:length(pred),"y"=0:length(pred)*sum(df[,1])/length(pred),"type"="Benchmark")
df3 = data.frame("Index"=c(0:sum(df[,1]),length(obs)),"y"=c(0:sum(df[,1]),sum(df[,1])),"type"="Ideal")

df = rbind(df2,df3,df1)

#plot(cumsum(df[,1]),ylab='Cumulative observations of TRUE')
#lines(c(0,length(pred)), c(0,sum(df[,1])))
qplot(Index,y,data=df,color=type,geom=c("line","point"),ylab="Cumulative Observations of TRUE",main="Lift Chart")

}


# ?2014-2015 by Nathan Karst
# Creates a ROC chart from  
# 
# INPUTS
#   obs         Binary (0/1 or TRUE/FALSE) observations
#   pred        Numerical predictions in [0,1]
#               
# OUTPUT
#   A lift chart with both 
#
# EXAMPLE
#df = read.csv("C:/users/nkarst/Dropbox/MyQTM2000/Rawdata/gender_height_hope_college.csv")
#df$IsMaleTF = (df$IsMale == 1)
#model = glm(IsMaleTF ~ Height, data=df, family = binomial(logit))
#pred    = predict(model, df, type = "response")
#ROCChart(df$IsMale, pred)
ROCChart <- function(obs, pred){ 
  out <- tryCatch(
{
  library(ggplot2)
},
error=function(cond) {
  install.packages("ggplot2")
  library(ggplot2)
})  

se = c()
sp = c()
P = c()
for(i in 1:101){
  p = (i-1)/100
  
  predTF = (pred > p)
  
  sp[i] = sum(predTF == FALSE & obs == FALSE)/sum(obs == FALSE)
  se[i] = sum(predTF == TRUE & obs == TRUE)/sum(obs == TRUE)
  P[i] = p
} 

df1 = data.frame(x=1-sp,y=se,"type"="Observed")
df1 = df1[order(df1$x,df1$y),]

df2 = data.frame(x=1-sp,y=1-sp,"type"="Benchmark")
df3 = data.frame(x=c(0,0,1),y=c(0,1,1),"type"="Ideal")


df = rbind(df2,df3,df1)

qplot(x,y,data=df,color=type,geom=c("point","line"),ylab="Sensitivity = True Positive Rate",xlab="1 - Specificity = False Positive Rate",main="ROC Chart")

}

easyPrune <- function(model){ 
  return(prune(model, cp = model$cptable[which.min(model$cptable[ , "xerror"]), "CP"]))
}


elbowChart <- function(df){
  x = c()
  for(i in 2:10){
    model = kmeans(df, i)
    x[i] = mean(model$withinss)
  }

  plot(1:10,x,type='b',ylab='Average Within Cluster Sum of Squares',xlab='Cluster Size')
}

removeOutliers <- function(df){
  for(i in 1:ncol(df)){
    if(is.integer(df[,i])){
      df[,i] = as.numeric(df[,i])
    }
    
    
    if(is.numeric(df[,i])){
      x = scale(df[,i])
      df = df[abs(x) < 3,]
    }
  
  }
  return(df)
}


kNNCrossVal <- function(form,train,norm=T){
  library(class)
  out <- tryCatch(
    {
      library(caret)
    },
    error=function(cond) {
      install.packages("caret")
      library(caret)
    })    
  
  tgtCol <- which(colnames(train) == as.character(form[[2]]))
  
  set.seed(1)
  idx <- createFolds(unlist(train[,tgtCol]), k=10)

  ks <- 1:12
  res <- sapply(ks, function(k) {
    ##try out each version of k from 1 to 12
    res.k <- sapply(seq_along(idx), function(i) {
      ##loop over each of the 10 cross-validation folds
      ##predict the held-out samples using k nearest neighbors
      pred <- kNN(form, train[-idx[[i]],], train[idx[[i]],], norm=norm, k=k)
      ##the ratio of misclassified samples
      sum(unlist(train[ idx[[i]],tgtCol ]) != pred)/length(pred)
    })
    ##average over the 10 folds
    mean(res.k)
  })
  plot(res,ylab="Average Cross-Validation Error",xlab="k")
  
  return(which.min(res))
}

# based on kNN from DMwR -- was having problem with mismatched vector size
# fixed with 'unlist(train[, tgtCol])' which just expands data frame to simple vector
kNN = function (form, train, test, norm = T, norm.stats = NULL, ...) 
{
  tgtCol <- which(colnames(train) == as.character(form[[2]]))
  if (norm) {
    if (is.null(norm.stats)) 
      tmp <- scale(train[, -tgtCol], center = T, scale = T)
    else tmp <- scale(train[, -tgtCol], center = norm.stats[[1]], 
                      scale = norm.stats[[2]])
    train[, -tgtCol] <- tmp
    ms <- attr(tmp, "scaled:center")
    ss <- attr(tmp, "scaled:scale")
    test[, -tgtCol] <- scale(test[, -tgtCol], center = ms, 
                             scale = ss)
  }
  return(knn(train[, -tgtCol], test[, -tgtCol], unlist(train[, tgtCol]), ...))
}


