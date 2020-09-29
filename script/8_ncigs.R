smoker <- coldata %>% 
  dplyr::filter(smoke == "fumatore")
smoker$curr_day <- droplevels(smoker$curr_day)
levels(smoker$curr_day) <- c("2", "6", "16", "21", "26", "34", "11", "25")
smoker$curr_day_cat <- as.numeric(as.character(smoker$curr_day))
smoker$ncigs <- ifelse(smoker$curr_day_cat <16, "0", "1")

coldata[,"ncigs"] <- smoker[match(coldata$id, smoker$id, nomatch=NA),"ncigs"]
coldata <- dplyr::relocate(coldata, "ncigs", .after = "smoke")
