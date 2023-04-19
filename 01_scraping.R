library(guardianapi)

# set API key
api_key <- rstudioapi::askForPassword()
Sys.setenv(GU_API_KEY = api_key) 
gu_api_key(check_env = TRUE)

# Query parameters
query<- c("vegan", "paleo", "keto")
from_date <- "2000-01-01"
to_date <- "2023-04-19"