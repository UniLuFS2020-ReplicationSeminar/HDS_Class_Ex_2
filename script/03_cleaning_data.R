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

