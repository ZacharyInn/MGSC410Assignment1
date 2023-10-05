library(ggplot2)
library(dplyr)

tweets_clean <- subset(Tweets, select = -c(airline_sentiment_gold, negativereason_gold))
head(tweets_clean)

# airline sentiments
slide4<-ggplot(tweets_clean, aes(x=airline, fill = airline_sentiment)) +
  geom_bar(stat="count") +
  labs(y= "Number of Tweets", x = "Airline") +
  theme_minimal()

slide4

#negative reasonings
slide5 <- subset(tweets_clean, select = c(negativereason, negativereason_confidence, airline_sentiment, airline_sentiment_confidence))
slide5clean <- na.omit(slide5)

slide5mod <- slide5clean %>%
  mutate(
    negreasonmod = case_when(
      negativereason %in% c("Lost Luggage", "Damaged Luggage") ~ "Luggage Problems",
      negativereason %in% c("Late Flight", "Cancelled Flight") ~ "Late/Cancelled Flight",
      negativereason == "longlines" ~ "Long Lines",
      TRUE ~ as.character(negativereason)
    )
  )

slide5<-ggplot(slide5mod, aes(x=negreasonmod, fill = negreasonmod)) +
  geom_bar(stat="count") +
  labs(y= "Number of Tweets", x = "Reason") +
  theme(axis.text.x = element_text(angle = 45)) +
  theme(axis.text.x = element_text(vjust = 0.5))
 

slide5


mean_badflight <- mean(slide5mod$negativereason_confidence[slide5mod$negreasonmod == "Bad Flight"])
mean_canttell <- mean(slide5mod$negativereason_confidence[slide5mod$negreasonmod == "Can't Tell"])
mean_customerserv <- mean(slide5mod$negativereason_confidence[slide5mod$negreasonmod == "Customer Service Issue"])
mean_flightatt <- mean(slide5mod$negativereason_confidence[slide5mod$negreasonmod == "Flight Attendant Complaints"])
mean_flightbook <- mean(slide5mod$negativereason_confidence[slide5mod$negreasonmod == "Flight Booking Problems"])
mean_latecan <- mean(slide5mod$negativereason_confidence[slide5mod$negreasonmod == "Late/Cancelled Flight"])
mean_ll <- mean(slide5mod$negativereason_confidence[slide5mod$negreasonmod == "Long Lines"])
mean_luggage <- mean(slide5mod$negativereason_confidence[slide5mod$negreasonmod == "Luggage Problems"])