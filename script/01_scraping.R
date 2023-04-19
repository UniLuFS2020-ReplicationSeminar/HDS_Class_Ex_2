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

### VEGAN
res_vegan<- gu_content(apiKey = api_key,
                      query = "vegan",
                      from = from_date,
                      to = to_date)
#Create add column
res_vegan$diet<- NA

#Checking body text
for (i in 1:nrow(res_vegan)){
  if(any(str_detect(res_vegan$body_text[i], "vegan"))) {
    res_vegan$diet[i]<- TRUE
  } 
  else {
    res_vegan$diet[i]<- FALSE
  }
}

### PALEO

res_paleo<- gu_content(apiKey = api_key,
                       query = "paleo",
                       from = from_date,
                       to = to_date)
#Create add column
res_paleo$diet<- NA

#Checking body text
for (i in 1:nrow(res_paleo)){
  if(any(str_detect(res_vegan$body_text[i], "paleo"))) {
    res_paleo$diet[i]<- TRUE
  } 
  else {
    res_paleo$diet[i]<- FALSE
  }
}

### Keto

res_keto <- gu_content(apiKey = api_key,
                       query = "keto",
                       from = from_date,
                       to = to_date)
#Create add column
res_keto$diet<- NA

#Checking body text
for (i in 1:nrow(res_keto)){
  if(any(str_detect(res_keto$body_text[i], "keto"))) {
    res_keto$diet[i]<- TRUE
  } 
  else {
    res_keto$diet[i]<- FALSE
  }
}