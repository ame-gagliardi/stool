library(tidyverse)

df <- readRDS("data/clinical/de_all_merged_cleaned.rds")
pa_men <- read.delim("data/clinical/PA_INDEX_man_corrected.csv", sep = ";")
pa_women <- read.delim("data/clinical/PA_INDEX_women_corrected.csv", sep =";") %>% 
  dplyr::select(-sex)
 pa <- bind_rows(pa_men, pa_women)

rownames(df) <- df$id_old
rownames(pa) <- pa$idpaziente

i <- intersect(rownames(df), rownames(pa))
df <- df[i,]
pa <- pa[i,]
all.equal(rownames(df), rownames(pa))

df[,"phys_act_2"] <- pa$PA_total
df[,"met"] <- pa$MET_total
