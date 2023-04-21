
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

