library(tidyverse)

donna <- as_tibble(readRDS("C:/Users/amedeo/Desktop/R_Projects/stool/data/clinical/donna.rds")) %>% 
  dplyr::select(idpaziente, mestr_now) %>% 
  dplyr::rename(id_old = idpaziente)

df <- readRDS("C:/Users/amedeo/Desktop/R_Projects/stool/data/clinical/merged_cleaned.rds")

i <- df$id_old %in% donna$id_old

df <-  as_tibble(merge(df, donna, by = "id_old", all.x = TRUE))

saveRDS(df, "C:/Users/amedeo/Desktop/R_Projects/stool/data/clinical/merged_cleaned.rds")
