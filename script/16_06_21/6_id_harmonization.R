libraries <- c("tidyverse", "stringi")
lapply(libraries, require, character.only = TRUE)
rm(libraries)

# Armonizzazione degli ID tra la matrice delle conte e il dataset delle covariate #

# Data load #

cel_cts <- read.table("C:/Users/amedeo/Desktop/R_Projects/sdv_redo/data/ngs_count/original_count_giulio/raw_count_celiachia.txt", header = TRUE, row.names = 1)
crc_cts <- read.table("C:/Users/amedeo/Desktop/R_Projects/sdv_redo/data/ngs_count/original_count_giulio/raw_count_crc.txt", header = TRUE, row.names = 1)
vov_cts <- read.table("C:/Users/amedeo/Desktop/R_Projects/sdv_redo/data/ngs_count/original_count_giulio/raw_count_vov.txt", header = TRUE, row.names = 1)

df <- readRDS("C:/Users/amedeo/Desktop/R_Projects/sdv_redo/data_redo/cleaned_samples.rds")

cel <- df[which(df$study == "Celiac"),]
crc <- df[which(df$study == "CRC"),]
vov <- df[which(df$study == "VOV"),]

# Celiaci # 
# Va inserito un _ tra la parte numerica e quella alfabetica nella matrice delle conte

stri_sub(colnames(cel_cts), 4, 3) <- "_"

i <- intersect(rownames(cel), colnames(cel_cts))
cel <- cel[i,]
cel_cts <- cel_cts[,i]
xx <- all.equal(rownames(cel), colnames(cel_cts))

cel$id <- cel$id_old
cel <- dplyr::relocate(cel, "id", .before = "id_old")

if(xx == TRUE){
  saveRDS(cel, "C:/Users/amedeo/Desktop/R_Projects/sdv_redo/data_redo/clinical/celiac_cleaned.rds")
  saveRDS(cel_cts, "C:/Users/amedeo/Desktop/R_Projects/sdv_redo/data_redo/ngs/celiac_harmonized_raw.rds")
  rm(i,xx,cel_cts, cel)
}


# CRC #

# Nel dataset clinico ho eliminato il campione "00TL" perché non c'è nella matrice delle conte.
# Poi ho diviso in due il dataset, uno con quelli di Vercelli (che avevano solo il codice numerico) e uno con quelli del Gradenigo (G).
# Ho aggiunto i caratteri "F0" a quelli del Gradenigo e "VF" a quelli di Vercelli in modo che combaciassero con quelli della matrice conte.
# 
# Nella matrice delle conte ho diviso i due dataset come sopra.
# In entrambe ho rimosso l'identificativo GF o VF, ho reso tutte gli ID a 3 cifre (es. 04 -> 004) e poi ho rimesso "GF" o "VF".
# Infine ho riunito i dataset in uno solo CRC.

## MODIFICA NOMI SU DATASET CLINICO ##
crc <- crc[which(rownames(crc) != "00TL"),]
xx <- str_detect(rownames(crc), "G")
g_crc <- crc[xx,]
num_crc <- crc[!xx,]
stri_sub(g_crc$id_old, 2, 1) <- "F0"
stri_sub(num_crc$id_old, 1, 0) <- "VF"
crc_tmp <- rbind(g_crc, num_crc)
crc$id <- crc_tmp$id_old
crc <- dplyr::relocate(crc, "id", .before = "id_old")
rownames(crc) <- crc$id

## MODIFICA NOMI SU MATRICE CONTE ##

gg <- str_detect(colnames(crc_cts), "GF")
vv <- str_detect(colnames(crc_cts), "VF")
g_crc <- crc_cts[,gg]
v_crc <- crc_cts[,vv]

colnames(g_crc) <- stringr::str_remove(colnames(g_crc), "GF")
colnames(g_crc) <-  stringr::str_pad(colnames(g_crc), 3, side="left", pad="0")
colnames(g_crc) <- stringr::str_c("GF",colnames(g_crc))

colnames(v_crc) <- stringr::str_remove(colnames(v_crc), "VF")
colnames(v_crc) <-  stringr::str_pad(colnames(v_crc), 3, side="left", pad="0")
colnames(v_crc) <- stringr::str_c("VF",colnames(v_crc))

crc_cts <- cbind(g_crc, v_crc)

i <- intersect(rownames(crc), colnames(crc_cts))
crc <- crc[i,]
crc_cts <- crc_cts[,i]
xx <- all.equal(rownames(crc), colnames(crc_cts))

if(xx == TRUE){
  saveRDS(crc, "C:/Users/amedeo/Desktop/R_Projects/sdv_redo/data_redo/clinical/crc_cleaned.rds")
  saveRDS(crc_cts, "C:/Users/amedeo/Desktop/R_Projects/sdv_redo/data_redo/ngs/crc_harmonized_raw.rds")
  rm(list=ls())
}

## VOV ##

# Nella matrice delle conte VOV modifico la parte numerica dell'ID perché diventi sempre a 3 cifre (es. VOV4 -> VOV004)

colnames(vov_cts) <- stringr::str_remove(colnames(vov_cts), "VOV")
colnames(vov_cts) <-  stringr::str_pad(colnames(vov_cts), 3, side="left", pad="0")
colnames(vov_cts) <- stringr::str_c("VOV",colnames(vov_cts))

i <- intersect(rownames(vov), colnames(vov_cts))
vov <- vov[i,]
vov_cts <- vov_cts[,i]
xx <- all.equal(rownames(vov), colnames(vov_cts))

vov$id <- vov$id_old
vov <- dplyr::relocate(vov, "id", .before = "id_old")
if(xx == TRUE){
  saveRDS(vov, "C:/Users/amedeo/Desktop/R_Projects/sdv_redo/data_redo/clinical/vov_cleaned.rds")
  saveRDS(vov_cts, "C:/Users/amedeo/Desktop/R_Projects/sdv_redo/data_redo/ngs/vov_harmonized_raw.rds")
  rm(i,xx,vov,vov_cts)
}

## Creo i dataset unici di conte e covariate ##

crc_cts <-readRDS("C:/Users/amedeo/Desktop/R_Projects/sdv_redo/data_redo/ngs/crc_harmonized_raw.rds")
vov_cts <-readRDS("C:/Users/amedeo/Desktop/R_Projects/sdv_redo/data_redo/ngs/vov_harmonized_raw.rds")
cel_cts <-readRDS("C:/Users/amedeo/Desktop/R_Projects/sdv_redo/data_redo/ngs/celiac_harmonized_raw.rds")

crc <- readRDS("C:/Users/amedeo/Desktop/R_Projects/sdv_redo/data_redo/clinical/crc_cleaned.rds")
vov <- readRDS("C:/Users/amedeo/Desktop/R_Projects/sdv_redo/data_redo/clinical/vov_cleaned.rds")
cel <- readRDS("C:/Users/amedeo/Desktop/R_Projects/sdv_redo/data_redo/clinical/celiac_cleaned.rds")

xx <- all.equal(rownames(vov_cts), rownames(cel_cts))
yy <- all.equal(rownames(crc_cts), rownames(cel_cts))

if(xx == TRUE & yy == TRUE){
  df <- cbind(crc_cts, vov_cts, cel_cts)
  df2 <- rbind(crc, vov, cel)
  saveRDS(df, "C:/Users/amedeo/Desktop/R_Projects/sdv_redo/data_redo/ngs/merged_harmonized.rds")
  saveRDS(df2, "C:/Users/amedeo/Desktop/R_Projects/sdv_redo/data_redo/clinical/merged_cleaned.rds")
  rm(list=ls())
}
