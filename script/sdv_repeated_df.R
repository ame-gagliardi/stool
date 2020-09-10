library(tidyverse)
library(DESeq2)
library(stringi)
library(stringr)

df.path <- c("C:/Users/amedeo/Desktop/R_Projects/stool/stool_vita/data/clinical/merged_study_whole_samples.rds")
df <- as_tibble(readRDS(df.path))

## Dataset separati per studio ##

vov <- df %>% 
  dplyr::filter(Study == "VOV")

cel <- df %>% 
  dplyr::filter(Study == "Celiac")


## Codifico e etichetto l'ID delle patologie ##

vov$id_pat <- factor(vov$id_pat,
                     levels = c(0,1,2),
                     labels = c("Onnivori","Vegetariani","Vegani"))

cel$id_pat <- factor(cel$id_pat,
                     levels = c(0,1,2),
                     labels = c("Celiaco_dieta","Nuova_diagnosi","Controllo"))  

## Selezione solo i ripetuti

vov$idpaziente <- as.character(vov$idpaziente)

vov_rep <- vov %>% 
  dplyr::filter(idpaziente == "VOV085" |
                  idpaziente == "VOV107" | 
                  idpaziente == "VOV116" | 
                  idpaziente == "VOV119" | 
                  idpaziente == "VOV094" | 
                  idpaziente == "VOV139" )


cel$idpaziente <- as.character(cel$idpaziente)

cel_rep <- cel %>% 
  dplyr::filter(idpaziente == "Cii_008" |
                  idpaziente == "Cii_011" | 
                  idpaziente == "Cii_014" | 
                  idpaziente == "Cii_021" | 
                  idpaziente == "Cii_026" | 
                  idpaziente == "Cii_035" )

df_rep <- bind_rows(vov_rep, cel_rep)

### Armonizzazione con la matrice delle conte ##
vov.count.path <- c("C:/Users/amedeo/Desktop/R_Projects/stool/stool_vita/data/ngs_count/original_count_giulio/raw_count_vov.txt")
cel.count.path <- c("C:/Users/amedeo/Desktop/R_Projects/stool/stool_vita/data/ngs_count/original_count_giulio/raw_count_celiachia.txt")

vov_cts <- read.table(vov.count.path, row.names = 1, header = TRUE)
cel_cts <- read.table(cel.count.path, row.names = 1, header = TRUE)
vov_cts$mirna <- rownames(vov_cts)
cel_cts$mirna <- rownames(cel_cts)

colnames(vov_cts) <- stringr::str_remove(colnames(vov_cts), "VOV")
colnames(vov_cts) <-  stringr::str_pad(colnames(vov_cts), 3, side="left", pad="0")
colnames(vov_cts) <- stringr::str_c("VOV",colnames(vov_cts))

rownames(vov_rep) <- vov_rep$idpaziente

i <- intersect(rownames(vov_rep), colnames(vov_cts))
vov_rep <- vov_rep[i,]
vov_cts <- vov_cts[,i]
rownames(vov_rep) <- vov_rep$idpaziente
xx <- all.equal(rownames(vov_rep), colnames(vov_cts))



stri_sub(colnames(cel_cts), 4, 3) <- "_"
rownames(cel_rep) <- cel_rep$idpaziente

i <- intersect(rownames(cel_rep), colnames(cel_cts))
cel_rep <- cel_rep[i,]
cel_cts <- cel_cts[,i]
rownames(cel_rep) <- cel_rep$idpaziente
xx <- all.equal(rownames(cel_rep), colnames(cel_cts))

vov_cts$mirna <- rownames(vov_cts)
cel_cts$mirna <- rownames(cel_cts)

df <- bind_rows(cel_rep, vov_rep)
rownames(df) <- df$idpaziente
df.count <- bind_cols(cel_cts, vov_cts)
rownames(df.count) <- df.count$mirna
df.count$mirna <- NULL
df.count$mirna1 <- NULL

all.equal(colnames(df.count), rownames(df))

xx <- c("VOV107", "VOV116", "VOV094", "VOV139","VOV085", "VOV119", "Cii_026", "Cii_011", "Cii_014", "Cii_035", "Cii_008", "Cii_021")
df$cross_ID <- xx
df <- moveMe(df, c("cross_ID"), "after", "idpaziente")
rownames(df) <- df$idpaziente



saveRDS(df, "C:/Users/amedeo/Desktop/R_Projects/stool/stool_vita/data/clinical/repetead_df.rds")
saveRDS(df.count, "C:/Users/amedeo/Desktop/R_Projects/stool/stool_vita/data/ngs_count/repetead_harmonized.rds")
