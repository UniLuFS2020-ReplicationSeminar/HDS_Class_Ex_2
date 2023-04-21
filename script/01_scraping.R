library(guardianapi)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(here)

#set API key
api_key <- rstudioapi::askForPassword()
Sys.setenv(GU_API_KEY = api_key) 
gu_api_key(check_env = TRUE)

#Query parameters
query<- c("vegan", "paleo", "keto")
from_date <- "2000-01-01"
to_date <- "2023-04-19"

#Get articles for each query
articles <- list()
for (q in query) {
  res <- gu_content(apiKey = api_key,
                    query = q,
                    from = from_date,
                    to = to_date)
#Add a column indicating the diet type
  res$diet_found <- q
  articles[[q]] <- res
}

#Column names
col_names <- lapply(articles, colnames)

#Checking common columns
common_cols <- Reduce(intersect, col_names)
for (df_name in names(articles)) {
  extra_cols <- setdiff(col_names[[df_name]], common_cols)
  if (length(extra_cols) > 0) {
    message(paste0("The following columns are in ", df_name, " but not in the other data frames: ", 
                   paste(extra_cols, collapse = ", ")))
  }
}

#Create df with common columns
articles_df <- articles %>%
  lapply(subset, select = common_cols) %>%
  setNames(seq_along(.)) %>%
  bind_rows()

#Create new column diet_check
articles_df$diet_check <- NA

#Check body text for query
for (i in 1:nrow(articles_df)){
  if(any(str_detect(articles_df$body_text[i], query))) {
    articles_df$diet_check[i]<- TRUE
  } 
  else {
    articles_df$diet_check[i]<- FALSE
  }
} 

#Cleaning data
 articles_clean <- articles_df %>%
    filter(diet_check == TRUE) %>%
    group_by(id) %>%
    filter(n() == 1) %>%
    ungroup()

### CLEANING UP

#Add year and month separately
articles <- articles_clean %>%
  mutate(year = format(as.Date(articles_clean$web_publication_date, format = "%Y-%m-%d"), "%Y"))

articles <- articles %>%
  mutate(month = format(as.Date(articles_clean$web_publication_date, format = "%Y-%m-%d"), "%m"))

## Arranging data for visualisation
articles$year <- as.factor(articles$year)
articles$diet_found <- as.factor(articles$diet_found)
articles$section_name <- as.factor(articles$section_name)
articles$byline <- as.factor(articles$byline)
articles$wordcount <- as.numeric(articles$wordcount)
articles$production_office <- as.factor(articles$production_office)
articles$publication <- as.factor(articles$publication)
articles$legally_sensitive <- as.factor(articles$legally_sensitive)


#Adding colors to dataframe (labels to colors) function
coloriser <- function(df) { 
  for( i in 1:nrow(df)) {
   if (df[i, ]$diet_found == 'vegan'  ) {
     df[i, ]$color <- "chartreuse3"
   } 
   else { if (df[i, ]$diet_found == 'keto' ) {
     df[i, ]$color <- "red"
   }
      else { if (df[i, ]$diet_found == 'paleo' ) {
       df[i, ]$color <- "cyan"
     }
      }} }
  return(df)
}


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
barplot(height=count_by_diet$n, names=count_by_diet$diet_found, 
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
barplot(height=count_year_by_topic$n, names=count_year_by_topic$year, 
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
