### Q1- Which is the best day to release a new article? Are people less likely to read about business on the weekend?###

## Barplot, Distribution of Weekdays by shares 
Q1.1_Data <- subset(OnlineNewsPopularity, select = c(weekday_is_monday, weekday_is_tuesday, weekday_is_wednesday, weekday_is_thursday, weekday_is_friday, weekday_is_saturday, weekday_is_sunday, is_weekend, shares))
# get average no. of shares Monday ~ Sunday
m1 <- mean(subset(OnlineNewsPopularity, weekday_is_monday == 1, select = shares)$shares)
m2 <- mean(subset(OnlineNewsPopularity, weekday_is_tuesday == 1, select = shares)$shares)
m3 <- mean(subset(OnlineNewsPopularity, weekday_is_wednesday == 1, select = shares)$shares)
m4 <- mean(subset(OnlineNewsPopularity, weekday_is_thursday == 1, select = shares)$shares)
m5 <- mean(subset(OnlineNewsPopularity, weekday_is_friday == 1, select = shares)$shares)
m6 <- mean(subset(OnlineNewsPopularity, weekday_is_saturday == 1, select = shares)$shares)
m7 <- mean(subset(OnlineNewsPopularity, weekday_is_sunday == 1, select = shares)$shares)
weekday_mean <- c(m1, m2, m3, m4, m5, m6, m7)
weekday_name <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
weekday_dc <- data.frame(weekday_name, weekday_mean) # put means and weekday labels in a dataframe
weekday_dc <- weekday_dc[order(weekday_dc$weekday_mean),] # sort by means, ascending
weekday_mean <- sort(weekday_mean) # sort means

# barplot
barplot(weekday_mean,
        main = "Distribution of Weekdays",
        xlab = "Weekday",
        ylab = "Average No. of Shares",
        ylim = c(0,4000),
        col = colors()[237],
        names.arg = c("Thur", "Tues", "Fri", "Wed", "Mon", "Sun", "Sat")
)

## Plot, Distribution of shares 
plot(Q1.1_Data$shares,
     main = "Distribution of Shares",
     xlab = "Article Index",
     ylab = "No. of Shares",
     col = colors()[166])

## Average no. of shares of business articles, weekday vs. weekend
Q1.2.1_Data <- subset(OnlineNewsPopularity, data_channel_is_bus == 1, select = c(weekday_is_monday, weekday_is_tuesday, weekday_is_wednesday, weekday_is_thursday, weekday_is_friday, weekday_is_saturday, weekday_is_sunday, is_weekend, shares))
# get average no. of shares on weekday and weekend
m_wd <- mean(subset(Q1.2.1_Data, is_weekend == 0, select = shares)$shares)
m_we <- mean(subset(Q1.2.1_Data, is_weekend == 1, select = shares)$shares)
m_bus_wd_we <- c(m_wd, m_we)
# barplot
barplot(m_bus_wd_we,
        main = "Average Shares of Business Articles on Weekday and Weekend",
        ylab = "Average No. of Shares",
        ylim = c(0,4000),
        col = colors()[237],
        names.arg = c("Weekday", "Weekend"))

## On weekend, average no. of shares: business articles vs. non-business articles
Q1.2.2_Data <- subset(OnlineNewsPopularity, is_weekend == 1, select = c(data_channel_is_bus, shares))
# get average no. of shares of business and non-business articles
m_wd_b <- mean(subset(Q1.2.2_Data, data_channel_is_bus == 1, select = shares)$shares)
m_wd_nb <- mean(subset(Q1.2.2_Data, data_channel_is_bus == 0, select = shares)$shares)
m_wd_b_nb <- c(m_wd_b, m_wd_nb)
# bar plot
barplot(m_wd_b_nb,
        main = "Average Shares on Weekend: Business vs. Non-Business Articles",
        ylab = "Average No. of Shares",
        ylim = c(0,4000),
        col = colors()[237],
        names.arg = c("Business", "Non-Business"))

