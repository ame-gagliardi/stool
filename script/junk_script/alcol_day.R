library(tidyverse)
library(stringr)
alco <- read.csv("C:/Users/amedeo/Desktop/R_projects/sdv/data/acque/epicdieta_gday.csv", sep = ";")
df <- readRDS("C:/Users/amedeo/Desktop/R_projects/sdv/data/clinical/merged_study_cleaned_samples.rds")

which(alco$idpaziente == "G06") #elimino un duplicato
alco <- alco[-162,]

tmp_alco <- alco[,c(1,236,237,238,239)]
tmp_alco$idpaziente <- tolower(tmp_alco$idpaziente)

tmp_df <- df[,c(1,2)]
tmp_df$old_id <- tolower(tmp_df$old_id)

rownames(tmp_alco) <- tmp_alco$idpaziente
xx <- rownames(tmp_alco)
xx[164:233] <- stringr::str_pad(xx[164:233], 3, side="left", pad="0") ## Aggiungo gli 0 agli ID per arrivare a 3 cifre come nel dataset originale
tmp_alco$id <- xx


setdiff(df$old_id, tmp_alco$id)
tmp_alco[234:236,] <- NA
tmp_alco[234:236,3] <- c("109", "277", "vov016") ## Metto questi campioni come NA per non eliminarli dal dataset originale

rownames(df) <- df$old_id
rownames(tmp_alco) <- tmp_alco$id
i <- intersect(rownames(df), rownames(tmp_alco))

df <- df[i,]
tmp_alco <- tmp_alco[i,]
df$alc_gr_day <- tmp_alco$TOT.alcool_g.day
df$alc_wine_gr_day <- tmp_alco$ToT.wine_gday
df$alc_terz <- tmp_alco$TOT.alcool_g.day.terz
df$alc_wine_terz <- tmp_alco$ToT.wine_gday.terz
rownames(df) <- df$idpaziente

saveRDS(df, "C:/Users/amedeo/Desktop/R_projects/sdv/data/clinical/merged_study_cleaned_samples.rds")


