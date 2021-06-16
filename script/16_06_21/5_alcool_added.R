libraries <- c("stringi", "stringr", "tidyverse", "memisc")
lapply(libraries, require, character.only = TRUE)

# Script per aggiungere l'alcool calcolato da Anto ##

df <- readRDS("C:/Users/amedeo/Desktop/R_Projects/sdv_redo/data_redo/cleaned_samples.rds")
diet <- read.table("C:/Users/amedeo/Desktop/R_Projects/sdv_redo/data/acque/epicdieta_gday.csv", header = TRUE, sep = ";")


xx <- which(diet$idpaziente %in% df$idpaziente == FALSE)
diet$idpaziente[xx] <- stringr::str_pad(diet$idpaziente[xx], 3, side="left", pad="0")
which(diet$idpaziente == "G06")

diet <- diet[-162,] # Elimino uno dei duplicati di G06

rownames(diet) <- diet$idpaziente
i <- intersect(rownames(diet), rownames(df))
df <- df[i,]
diet <- diet[i,]
all.equal(rownames(df), rownames(diet))

df[,c(128,129,130,131)] <- diet[,236:239]
colnames(diet)[236:239]

df <- dplyr::relocate(df, "TOT.alcool_g.day.terz", .after = "PA_total")

# Modifiche ai nomi delle colonne #

colnames(df)[1] <- c("id_old")
colnames(df)[4] <- c("library")
colnames(df)[14] <- c("phys_act")
colnames(df)[15] <- c("alcool")

# Salvo il dataset #

saveRDS(df, "C:/Users/amedeo/Desktop/R_Projects/sdv_redo/data_redo/cleaned_samples.rds")
