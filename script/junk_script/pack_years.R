library(tidyverse)

df1 <- df %>% 
  dplyr::select(idpaziente,age_cat, sex, smoke, contains("curr")) %>%
  dplyr::filter(smoke == "fumatore")
  
  
to_remove <- grep("no_filtro", colnames(df1))
to_remove2 <- grep("both_filtro", colnames(df1))  
  
to_remove <- c(to_remove, to_remove2)  

df1 <- df1[,-(to_remove)]  
  

levels(df1$curr_day_20_filtro) <- c(NA,"0","2","16","21","26","34","6","11")
levels(df1$curr_day_30_filtro) <- c(NA,"0","2","16","21","26","34","6","11")
levels(df1$curr_day_40_filtro) <- c(NA,"0","2","16","21","26","34","6","11")
levels(df1$curr_day_50_filtro) <- c(NA,"0","2","16","21","34","6","11")

df1$curr_day_20_filtro <- replace_na(df1$curr_day_20_filtro, 0)
df1$curr_day_30_filtro <- replace_na(df1$curr_day_30_filtro, 0)
df1$curr_day_40_filtro <- replace_na(df1$curr_day_40_filtro, 0)
df1$curr_day_50_filtro <- replace_na(df1$curr_day_50_filtro, 0)

df1$curr_day_20_filtro <- as.numeric(as.character(df1$curr_day_20_filtro))
df1$curr_day_30_filtro <- as.numeric(as.character(df1$curr_day_30_filtro))
df1$curr_day_40_filtro <- as.numeric(as.character(df1$curr_day_40_filtro))
df1$curr_day_50_filtro <- as.numeric(as.character(df1$curr_day_50_filtro))




df1$pack_year <- ((df1$curr_day_20_filtro*10) + (df1$curr_day_30_filtro*10) + (df1$curr_day_40_filtro*10) + (df1$curr_day_50_filtro*10))/20


#n di sigarette
df1$curr_day_cat = df1$curr_day
levels(df1$curr_day_cat) <- c(NA, "2", "16", "21","26","26","34","6","11")
df1$curr_day_cat <- as.numeric(as.character(df1$curr_day_cat))
df1$curr_day_cat <- cut(df1$curr_day_cat, breaks=c(0,15,34), labels=c("0","1"))
saveRDS(df1, "C:/Users/amedeo/Desktop/R_Projects/sdv/data/clinical/subset_analyses/smoker_subset.rds")
