#INFM600-0201
#TeamAwesome - Mashable Online News Popularity
#There are three main research questions
#Q1- Which is the best day to release a new article? Are people less likely to read about business on the weekend?
#Q2- How does text and image quantity in articles relate to the number of shares?
#Q3- Does the rate of sharing positive and negative news vary with the time of the year?

# Preliminary Step: Read the file into R
#This data set has 62 columns and 39,645 rows
OriginalData <- read.csv("F:/MIM/Team Awesome/OnlineNewsPopularity.csv")


###################
# Research Question 1 - R Script
### Q1- Which is the best day to release a new article? Are people less likely to read about business on the weekend?###

# Check frequency of business news
table(OriginalData$data_channel_is_bus)
freq_bus <- table(OriginalData$data_channel_is_bus)/length(OriginalData$data_channel_is_bus) 
freq_bus #This gives us the frequency of business articles in the dataset

# Descriptive Statistics of no. of shares
min(OriginalData$shares) #Minimum number of shares in an article
max(OriginalData$shares) #Maximum number of shares in an article
mean(OriginalData$shares) #Average number of shares in an article
sd(OriginalData$shares) #standard deviation of shares in an article

# Descriptive Statistics of business news
mean(OriginalData$data_channel_is_bus) #Average number of business news published
sd(OriginalData$data_channel_is_bus) #standard deviation of business news published
min(OriginalData$data_channel_is_bus) #Minimum number of business news published
max(OriginalData$data_channel_is_bus) #MAximum number of busniess news published

# Create a subset with only business related columns published
# The columns needed for this question are index, weekday_is_monday, weekday_is_tuesday,weekday_is_wednesday, weekday_is_thursday, 
# weekday_is_friday, weekday_is_saturday, weekday_is_sunday, is_weekend, shares for those articles where data_channel_is_bus is equal to 1
Q1_newdata <- subset(OriginalData, data_channel_is_bus == 1, select = c(index, weekday_is_monday, weekday_is_tuesday, weekday_is_wednesday, weekday_is_thursday, weekday_is_friday, weekday_is_saturday, weekday_is_sunday, is_weekend, shares))
View(Q1_newdata)
attach(Q1_newdata)

# To check whether there is a correlation beween different days of the week
# Correlation test between different weekdays and no. of shares
cor.test(weekday_is_monday, shares)
cor.test(weekday_is_tuesday, shares) 
cor.test(weekday_is_wednesday, shares) 
cor.test(weekday_is_thursday, shares) 
cor.test(weekday_is_friday, shares) 
cor.test(weekday_is_saturday, shares) 
cor.test(weekday_is_sunday, shares)

# Create another subset that only focuses on the business related articles published on the weekend
channel_weekend <- subset(OriginalData, is_weekend == 1, select = c(index, data_channel_is_lifestyle, data_channel_is_entertainment, data_channel_is_bus, data_channel_is_socmed, data_channel_is_tech, data_channel_is_world, is_weekend, shares))
View(channel_weekend)
attach(channel_weekend)

# To check whether there is a relation between the other genres of article and shares on the weekend
# Correlation test between channels and no. of shares
cor.test(data_channel_is_lifestyle, shares)
cor.test(data_channel_is_entertainment, shares) 
cor.test(data_channel_is_bus, shares) 
cor.test(data_channel_is_socmed, shares) 
cor.test(data_channel_is_tech, shares)
cor.test(data_channel_is_world, shares)
# End of Question 1
###################


# Research Question 2 - R Script
### Q2- How does text and image quantity in articles relate to the number of shares? ###

# Making a subset of the columns needed for our analysis
# The columns required for this question are n_tokens_content (number_of_words), num_imgs (numb_images), shares
# These columns were extracted to a seperate csv file and an extra column was added called 'image_there'
# image_there has two values - 0 or 1
# 0 - There are no images in the article 
# 1 - There is at least one image in the article

# Read the data set into R Studio
Q2_data <- read.csv("F:/MIM/Team Awesome/Q2_data.csv")

# Attaching the data set
attach(Q2_data)

# Descriptive Statistics of the Independent Variable - number of words
min(number_of_words) #Minimum number of Words in an article
max(number_of_words) #Maximum number of words in an article
mean(number_of_words) #Average number of words in an article
sd(number_of_words) #standard deviation of words in an article

# Descriptive Statistics of the Independent Variable - number of images
min(num_images) #Minimum number of images in an article
max(num_images) #Maximum number of images in an article
mean(num_images) #Average number of images in an article
sd(num_images) #standard deviation of images in an article

# Descriptive Statistics of the Dependent Variable - number of shares
min(shares) #Minimum number of shares in an article
max(shares) #Maximum number of shares in an article
mean(shares) #Average number of shares in an article
sd(shares) #standard deviation of shares in an article

# Distributions of the Independent Variables
hist(number_of_words, main = "Histogram of Number of Words", xlab = "Number of words in an article")
hist(num_images, main = "Histogram of Number of Images", xlab = "Number of images in an article")

# Distribution of Dependent Variable
hist(shares, main = "Histogram of Number of Shares", xlab = "Number of Shares")

# Average number of shares of an article with resepect to whether there is an image or not (0 - No image; 1 - At least one image)
tapply(shares, image_there, mean)

# Test for Non-Linearity
plot(number_of_words,shares)

# Test for Outliers
m = lm(shares~number_of_words,data = Q2_data) 
pred = m$fitted.values 
resid = m$residuals
resid.sd = sd(resid)
resid[abs(resid)>=3*resid.sd]

m2 = lm(shares~number_of_words,data = Q2_data)
pred2 = m2$fitted.values 
resid2 = m2$residuals
resid2.sd = sd(resid2)
resid2[abs(resid2)>=3*resid2.sd]

# Test for Constant Error
plot(pred,resid)
plot(pred2,resid2)

# Testing for Normality
# load library 'car' for using qqplot
library(car) 
# Q-Q Plot for Independent Variables
qqnorm(number_of_words)
qqline(number_of_words, col = "red")
qqnorm(num_images)
qqline(num_images, col = "red")
# Q-Q Plot for Dependent Variable
qqnorm(shares)
qqline(shares, col = "red")

# Performing Multiple Regression on the two IV's
summary(lm(shares~number_of_words+num_images, d = Q2_data))

# End of Question 2
###################


###################
# Research Question 3 - R Script
### Q3- Does the rate of sharing positive and negative news vary with the time of the year?

# Create a subset of the data to be used for Q3
# The columns being used for this research question are timedelta, n_tokens_title, 
# global_rate_positive_words,global_rate_negative_words,rate_postive_words, rate_negative_words, shares
Q3_newdata <- OriginalData[c(2,3,48:51,62)] 

# See first few values of data
head(Q3_newdata) 

#attaching the dataset
attach(Q3_newdata)

# Mean no. of shares across months for both pos. and neg. news
tapply(shares,month,mean) 

#Descriptive Statistics
min(shares) #descriptive stats to get idea of range
max(shares) #descriptive stats to get idea of range

# Descriptive Statistics of positive news
mean(global_rate_positive_words) #avg. % of positive words in an article
pos_news<-ifelse(global_rate_positive_words>=0.08,1,0) #news with more than 8% of the total words being pos. is considered pos. news, other methods can also be conisdered
mean(shares[pos_news==1]) #avg. shares of pos. news vs other news

# Add new positive news column to subset
Q3_newdata$mpos <- pos_news 

# Library to use filter function
library(dplyr) 
fil = filter(Q3_newdata,mpos==1) #filter only positive news rows
nrow(fil) #no. of positive news articles
tapply(fil$shares,fil$month,mean) #avg. shares of pos. news month wise
m = aov(shares~month,data = fil) #anova to check if difference in #shared is significant, assumptions of anova need to be checked to see if this is valid
summary(m)

# Descriptive Statistics of negative news
mean(global_rate_negative_words) 
neg_news<-ifelse(global_rate_negative_words>=0.04,1,0) #news with more than 4% of the total words being neg. is considered neg. news, other methods can also be conisdered
mean(shares[neg_news==1])

# Add new negative news column to subset
Q3_newdata$mneg <- neg_news 
filn = filter(Q3_newdata,mneg==1) #filter only negative news rows
nrow(filn) #no. of negative news articles
tapply(filn$shares,filn$month,mean)

# ANOVA test
# Dependent Variable - shares
# Independent Variable - Month (has 12 levels)
n = aov(shares~month,data = filn)
summary(n)

# End of Question 3
###################