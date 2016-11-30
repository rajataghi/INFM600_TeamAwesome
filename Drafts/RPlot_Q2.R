#Q2- How does text and image quantity in articles relate to the number of shares?

#Read the data set into R Studio
Q2_data <- read.csv("c:/INFM600/Q2_data.csv")
View(Q2_data)
attach(Q2_data)

#Frequency Distribution of the total number of words in an article. This shows that histogram is right skewed
hist(number_of_words, xlab = "Total Number of Words in an article", main = "Frequency Distribution of Number of Words")
#Frequency Distribution of the total number of images in an article. This shows that histogram is right skewed
hist(num_images, main = "Total Number of Images in an article", xlab = "Frequency Distribution of Number of Images")
#Frequency Distribution of the total number of shares in an article. This shows that histogram is right skewed
hist(shares, main = "Total Number of Shares for an Article", xlab = "Frequency Distribution of Number of Shares")

#Load library 'ggplot2' and 'xkcd' for visualizations
library(ggplot2)
library(xkcd)
library(extrafont)

#Since the number of shares is very large, we have divided it by 10000 and saved it as new column called 'numshare'
Q2_data$numshare<- shares/10000
#Distribution of number of images and number or words, respectively, in an article with respect to number of shares (x10000) of that article
#The parameter alpha is used to show the density of the articles at that point
#The line is made to view the trend of the data since we have a lot of data points.
qplot(num_images, numshare,data = Q2_data, xlab = "Number of Images in an Article", ylab = "Number of Shares(x10000)", main = "Distribution of number of images in an article with respect to number of shares of that article", xlim = c(0, 100), ylim = c(0,20), alpha = I(1/10), geom = c("point", "smooth"), span = 0)
qplot(number_of_words, numshare, data = Q2_data, xlab = "Number of Words in an Article", ylab = "Number of Shares (x10000)", main = "Distribution of number of words in an article with respect to number of shares of that article", xlim = c(0, 7000), ylim = c(0,15), alpha = I(1/10), geom = c("point", "smooth"), span = 0)


#This bar graph shows the number of articles with an image and the number of articles without an image respectively
ggplot(data = Q2_data, aes(x = image_there)) +
  geom_bar(aes(fill = image_there), stat="count", width = 0.5)+
  xlab("Is there an Image in the Article?")+ ylab("Number of Articles") +
  ggtitle("Number of Articles with and without images") + 
  theme(legend.position = "none")


#This plot shows the average number of shares for articles with an image and without an image respectively
res <- aggregate(shares ~ image_there, data = Q2_data, FUN = mean)
ggplot(res, aes(x = image_there, y = shares)) + 
  geom_bar(aes(fill = image_there), stat = "identity") +
  xlab(NULL) + ylab("Average Number of Shares")


#This plot shows the total number of shares of articles with an image and without and image respectively
ggplot(data = Q2_data, aes(x = image_there, y = numshare)) +
  geom_bar(aes(fill = image_there),stat="identity", width = 0.5) +
  xlab("Is there an Image in the Article?")+ ylab("Number of Shares") +
  ggtitle("Total number of shares of articles with and without images")+
  theme(legend.position = "none") 

#This plot shows the average number of words in articles with and without images respectively
res1 <- aggregate(number_of_words ~ image_there, data = Q2_data, FUN = mean)
ggplot(data = res1, aes(x = image_there, y = number_of_words)) +
  geom_bar(aes(fill = image_there),stat="identity", width = 0.5) +
  xlab("Is there an Image in the Article?")+ ylab("Average Number of Words") +
  ggtitle("Average number of words of articles with and without images")+
  theme(legend.position = "none") 