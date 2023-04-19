library(guardianapi)
library(tidyverse)

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

### FILTERING 

#Add year and month separately
articles <- articles_clean %>%
  mutate(year = format(as.Date(articles_clean$web_publication_date, format = "%Y-%m-%d"), "%Y"))

articles <- articles %>%
  mutate(month = format(as.Date(articles_clean$web_publication_date, format = "%Y-%m-%d"), "%m"))

