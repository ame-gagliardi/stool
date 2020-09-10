
## N° sigarette al giorno

df <- readRDS("C:/Users/amedeo/Desktop/R_Projects/sdv_redo/data_redo/clinical/merged_cleaned.rds")

df1 <- df %>% 
  dplyr::select(id,library,age_cat, sex, smoke, contains("curr")) %>%
  dplyr::filter(smoke == "fumatore")

to_remove <- grep("no_filtro", colnames(df1))
to_remove2 <- grep("both_filtro", colnames(df1))  

to_remove <- c(to_remove, to_remove2)  

df1 <- df1[,-(to_remove)]

df1$curr_day_cat = df1$curr_day
levels(df1$curr_day_cat) <- c(NA, "2", "6", "16", "21", "26", "31", "34+", "11", "26")
df1$curr_day_cat <- droplevels(df1$curr_day_cat)
df1$curr_day_cat <- as.numeric(as.character(df1$curr_day_cat))
df1$curr_day_cat <- cut(df1$curr_day_cat, breaks=c(0,15,34), labels=c("0","1"))
saveRDS(df1, "C:/Users/amedeo/Desktop/R_Projects/sdv_redo/data_redo/clinical/smoker.rds")
