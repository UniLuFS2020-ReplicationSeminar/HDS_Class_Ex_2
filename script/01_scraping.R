library(guardianapi)
library(stringr)

# set API key
api_key <- rstudioapi::askForPassword()
Sys.setenv(GU_API_KEY = api_key) 
gu_api_key(check_env = TRUE)

# Query parameters
from_date <- "2000-01-01"
to_date <- "2023-04-19"

# Get articles for each query

res_vegan<- gu_content(apiKey = api_key,
                      query = "vegan",
                      from = from_date,
                      to = to_date)
#Create add column
res_vegan$diet<- NA


