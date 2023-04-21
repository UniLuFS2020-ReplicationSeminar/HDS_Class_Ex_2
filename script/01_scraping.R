library(guardianapi)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(here)
library(tidytext)
library(syuzhet)

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
