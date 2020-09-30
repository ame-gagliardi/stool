df <- as_tibble(readRDS("C:/Users/amedeo/Desktop/R_Projects/stool/data/clinical/merged_cleaned.rds"))

smoker <- df %>% 
  dplyr::filter(smoke == "fumatore")

smoker$curr_day <- droplevels(smoker$curr_day)
levels(smoker$curr_day) <- c("2", "6", "16", "21", "26", "34", "11", "25")
smoker$curr_day_cat <- as.numeric(as.character(smoker$curr_day))
smoker$ncigs <- ifelse(smoker$curr_day_cat <16, "0", "1")

df[,"ncigs"] <- smoker[match(df$id, smoker$id, nomatch=NA),"ncigs"]
df <- dplyr::relocate(df, "ncigs", .after = "smoke")
