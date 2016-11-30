setwd("c:\\INFM600/")
OriginalData=read.csv("OnlineNewsPopularity-R.csv")
NewData <- OriginalData[c(2,3,48:51,62)] #create a subset of the data to be used for Q3
head(NewData) #see first few values of data

tapply(NewData$shares,NewData$month,mean) #mean no. of shares across months for both pos. and neg. news
min(NewData$shares) #descriptive stats to get idea of range
max(NewData$shares) #descriptive stats to get idea of range

mean(NewData$global_rate_positive_words) #avg. % of positive words in an article
pos_news<-ifelse(NewData$global_rate_positive_words>=0.08,1,0) #news with more than 8% of the total words being pos. is considered pos. news, other methods can also be conisdered
mean(NewData$shares[pos_news==1])#avg. shares of pos. news vs other news
NewData$mpos <- pos_news #add new pos. news column to subset
library(dplyr)#library to use filter function
fil = filter(NewData,mpos==1)#filter only pos. news rows
nrow(fil)#no. of positive news articles
tapply(fil$shares,fil$month,mean)#avg. shares of pos. news month wise
m = aov(shares~month,data = fil) #anova to check if difference in #shared is significant, assumptions of anova need to be checked to see if this is valid
summary(m)

#same as above but for neg. news
mean(NewData$global_rate_negative_words) 
neg_news<-ifelse(NewData$global_rate_negative_words>=0.04,1,0) #news with more than 4% of the total words being neg. is considered neg. news, other methods can also be conisdered
mean(NewData$shares[neg_news==1])
NewData$mneg <- neg_news 
filn = filter(NewData,mneg==1)
nrow(filn)

n = aov(shares~month,data = filn)
summary(n)




#R plots for Q3. Does the rate of sharing positive and negative news vary with the time of the year?






library(ggplot2)
library(xkcd)
vignette("xkcd-intro")
qplot(month, data = NewData, geom = "bar",main = "Month wise distribution of articles published",xlab = "Month",ylab = "Number of Articles")+scale_x_discrete(limits=c("1", "2","3","4","5","6","7","8","9","10","11","12"))
#This is the month wise distribution of number of articles published. Just a basic graph to get things started.

#overall news
barplot(tapply(NewData$shares,NewData$month,mean),ylim = c(0,5000),main = "Month wise sharing of News")
axis(1, at=c(0,18), labels=c("",""), lwd.ticks=0)
tapply(NewData$shares,NewData$month,mean)
#This bar plot shows the month wise mean number of shares. We can use this to see how different rate of sharing of positive and negative news is from the general mean.

#negative news
barplot(tapply(filn$shares,filn$month,mean),ylim = c(0,5000),main = "Month wise sharing of Negative News")
axis(1, at=c(0,18), labels=c("",""), lwd.ticks=0)
#axis(1, at=seq(1 , 12, by=1), lwd=0, lwd.ticks=1)
#using this plot we can identify the months where negative news is shared maximum and minimum
tapply(filn$shares,filn$month,mean)

#positive news
barplot(tapply(fil$shares,fil$month,mean),ylim = c(0,5000),main = "Month wise sharing of Positive News")
axis(1, at=c(0,18), labels=c("",""), lwd.ticks=0)
#usinf this plot we can identify the months where positive news is shared maximum and minimum
tapply(fil$shares,fil$month,mean)

qplot(OriginalData$timedelta,OriginalData$shares,geom="line")#This plot shows how the number of shares for a particular article vary with the time since which it has been released. We can see articles which have been online for 260-450 days have the max. number of shares

qplot(filn$timedelta,filn$shares,geom="line")#This plot shows how the number of shares for a particular article vary with the time since which it has been released. We can see articles which have been online for 260-450 days have the max. number of shares
qplot(fil$timedelta,fil$shares,geom="line")#This plot shows how the number of shares for a particular article vary with the time since which it has been released. We can see articles which have been online for 260-450 days have the max. number of shares
