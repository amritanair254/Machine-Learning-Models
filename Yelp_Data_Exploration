#This file was saved as .txt instead of .R to avoid blackboard upload issues



library(readr) #import library readr
Yelp <- read_csv("C:/Users/amrit/Downloads/Yelp.csv") #read Yelp data using function read_csv from library readr
str(Yelp) #inspect data struction. ID is loaded as characters and rest of data is numeric



library("ggplot2", lib.loc="~/R/win-library/3.5")  #import library ggplot2 to use function qplot
qplot(Yelp$'Average Review',geom="histogram",binwidth=0.5,main="Histogram of Avg Review",xlab="Avg Review",fill=I("blue"),col=I("red"),alpha=I(0.2))
#Plots histogram of Average Review. Please check attached pdf for notes. Geom detemines type of graph.
#highest number of people have given an average of around 4 star review.



qplot(Yelp$'Reviews',Yelp$'Months Active',main="Scatter Plot",xlab="Reviews",ylab="Months Active",col=I("red"),alpha=I(0.2))
#creates a scatter plot of Reviews and Months Active
qplot(Yelp$'Reviews',Yelp$'Months Active',main="Scatter Plot",xlab="Reviews",ylab="Months Active",col=I("red"),alpha=I(0.2),geom=c("point","smooth"),method="lm",formula=y~x)
#creates a scatterplot with linear regression (best-fit) line that shows positive correlation between the two variables



x=Yelp$Reviews
#Creates a new variable x that represents the number of reviews per month written by each user
qplot(x,geom="histogram",binwidth=10,main="Histogram of Review",xlab="Review",fill=I("blue"),col=I("red"),alpha=I(0.2))
#Plots a histogram of x. Looks like the most frequent number of reviews that users post on Yelp is between 0-10.



y=Yelp[Yelp$`Average Review`<=2,] #filter observations where Average Review is <=2 and feed the result into a new dataframe y
z=y$Reviews
qplot(z,geom="histogram",binwidth=10,main="Histogram of Review",xlab="Review",fill=I("blue"),col=I("red"),alpha=I(0.2))
#plot a histogram of their reviews per month
#Among the set of users whose Average Review is less than or equal to 2 (892 users who give poor reviews on average), most of them (>800 users) post 0-10 reviews per month. Around 50 users post 10-20 reviews per month. 
#Maximum number of reviews for this subset of users is less than 40. 






