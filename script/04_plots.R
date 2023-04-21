
### PLOTS 

#Number of articles by diet altogether
#Create count of articles by year for plotting
count_by_diet <- articles %>%
  group_by(diet_found) %>%
  tally()

count_by_diet$color <- NA
count_by_diet <- coloriser(count_by_diet)

# Arrange margins
par(mar = c(5.1, 4.1, 6.1, 1.1))
#Plot Articles published by topic
art_by_topic <- barplot(height=count_by_diet$n, names=count_by_diet$diet_found, 
        col = count_by_diet$color,
        horiz = TRUE,
        ylab="Topics", 
        xlab="Count", 
        main="Articles published by topic", 
        xlim=c(0,5500)
) %>% 
  text(x = 500 , paste(count_by_diet$n, sep="", cex = 1) )


#Articles by topic and year

#Create empty dataframe with columns year, diet topic and count
#Count topic occurrences by year
count_year_by_topic <- articles %>%
  group_by(year, diet_found) %>%
  tally()

count_year_by_topic$color <- NA
count_year_by_topic <- coloriser(count_year_by_topic)

# Arrange margins
par(mar = c(5.1, 4.1, 6.1, 1.1))
#Plot and legend articles by year and topic
act_by_year_topic <- barplot(height=count_year_by_topic$n, names=count_year_by_topic$year, 
        col = count_year_by_topic$color,
        horiz = FALSE,
        ylab="Count", 
        xlab="Years", 
        main="Articles published by year and topic", 
        ylim=c(0,800)
) %>% 
  text(count_year_by_topic$n+15 , paste(count_year_by_topic$n, sep="") ,cex=1) 

legend("topleft",
       c("Keto","Paleo", "Vegan"),
       fill = c("red","cyan", "chartreuse3")
)

######## Sentiment analysis ##########

# Clean and preprocess data for SA
text_clean <- articles_clean %>%
  mutate(body_text = str_to_lower(body_text)) %>%
  unnest_tokens(word, body_text) %>% 
  anti_join(stop_words) %>%
  group_by(id) %>%
  summarize(cleaned_text = paste(word, collapse = " "))

# Perform sentiment analysis
text_clean$sentiment <- get_sentiment(text_clean$cleaned_text) 

#Save result into main df articles_clean
articles_clean$sentiment <-NA
articles_clean$sentiment<- text_clean$sentiment

#Create plot with SA by diets
sentiment_plot <- articles_clean %>%
  filter(str_detect(diet_found, "vegan|keto|paleo")) %>%
  group_by(diet_found) %>%
  summarize(sentiment = mean(sentiment, na.rm = TRUE)) %>%
  ggplot(aes(x = diet_found, y = sentiment, fill = diet_found)) +
  geom_col() +
  ggtitle("Sentiment by diet") +
  xlab("Diet") +
  ylab("Sentiment") +
  labs(fill = "Diets") +
  theme_bw()
