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

#Create new list with common columns
newArticles <- list()
for (i in 1:length(articles)) {
  new_list = subset(articles[[i]],TRUE, common_cols)
  newArticles[[i]]= new_list
}

#Create df from list
articles_df <- do.call(rbind, newArticles)

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
articles_clean<- articles_df %>% 
  filter(diet_check == TRUE)

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


#Create 5-year intervals, for that a new dataframe is used
count_year_by_topic$year <- count_year_by_topic$year %>% as.character() %>% as.numeric()
count_year_by_topic$n <- count_year_by_topic$n %>% as.numeric()
count_year_by_topic$diet_found <- count_year_by_topic$diet_found %>% as.character()

columns = c("year_group", "topic", "total_count") 
five_year_by_topic = data.frame(matrix(nrow = 0, ncol = length(columns))) 
colnames(five_year_by_topic) = columns

count_year_by_topic$year_group <- cut(count_year_by_topic$year, breaks = seq(2000, 2025, by = 5))
five_year_by_topic <- count_year_by_topic %>%
  group_by(year_group, diet_found) %>%
  summarize(total_count = sum(n))
five_year_by_topic$year_group[15] <- five_year_by_topic$year_group[1]
five_year_by_topic  <- five_year_by_topic %>%
  arrange(year_group)


five_year_by_topic$color <- NA
five_year_by_topic <- coloriser(five_year_by_topic)
# Arrange margins
par(mar = c(5.1, 4.1, 6.1, 1.1))
#Plot and legend articles by year and topic
barplot(height=five_year_by_topic$total_count, names=five_year_by_topic$year_group, 
        col = five_year_by_topic$color,
        horiz = TRUE,
        ylab="Years", 
        xlab="Count", 
        main="Articles published by year and topic", 
        xlim=c(0,2500)
) %>% 
  text(x = 500 , paste(five_year_by_topic$total_count, sep="") ,cex=1) 

legend("bottomright",
       c("Keto","Paleo", "Vegan"),
       fill = c("red","cyan", "chartreuse3")
)
  