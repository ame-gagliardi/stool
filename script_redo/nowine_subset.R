## Adding other types of alcohol ##

library(tidyverse)
library(stringi)
library(memisc)


df <- readRDS("C:/Users/amedeo/Desktop/R_Projects/stool/data_redo/clinical/merged_cleaned.rds") # Carico il df finale
epicdieta <- read.table("C:/Users/amedeo/Desktop/R_Projects/stool/data_redo/acque/epicdieta_gday.csv", sep =";", header = TRUE)  # Carico il df con l'alcool

## ID 

change <- setdiff(epicdieta$idpaziente,df$id_old) # Controllo gli id diversi                                  

tmp <- numeric()
for(i in 1:length(change)){
  tmp <- append(tmp, which(epicdieta$idpaziente == change[i]))
}

epicdieta$idpaziente[tmp] <- str_pad(epicdieta$idpaziente[tmp], 3, side="left", pad="0") # Aggiungo gli 0 che mancano perché siano uguali

## Consistency

rm <- which(duplicated(epicdieta$idpaziente)) # Rimuovo il duplicato G06
epicdieta <- epicdieta[-162,]

rownames(df) <- df$id_old
rownames(epicdieta) <- epicdieta$idpaziente

i <- intersect(rownames(df), rownames(epicdieta))
df <- as_tibble(df[i,])
epicdieta <- as_tibble(epicdieta[i,])
epicdieta$id <- df$id
epicdieta <- dplyr::relocate(epicdieta, "id", .before = "idpaziente")

keep <- colnames(epicdieta)[c(1,173:177)]

dieta <- epicdieta %>% 
  dplyr::select(keep) %>% 
  dplyr::rename(red = 2, white = 3, aperitif = 4, spirits = 5, beer = 6) %>% 
  dplyr::mutate(red = as.numeric(gsub(",", ".", red)), white = as.numeric(sub(",", ".", white)),
                aperitif = as.numeric(gsub(",", ".", aperitif)), spirits = as.numeric(gsub(",", ".", spirits)), 
                beer = as.numeric(gsub(",", ".", beer))) %>% 
  dplyr::mutate(tot_alc = red+white+aperitif+spirits+beer, tot_vino = red+white,
                astemio = tot_alc == 0, no_vino = tot_vino == 0 & tot_alc !=0)

dieta$alcoclass <- with(dieta, cases("0" = astemio == TRUE,
                                     "1" = astemio == FALSE & no_vino == FALSE,
                                     "2" = astemio == FALSE & no_vino == TRUE))


# saveRDS(dieta, "C:/Users/amedeo/Desktop/R_Projects/stool/data_redo/clinical/drinker.rds")


  
  
  
dieta <- dieta %>% 
  dplyr::filter(alcoclass != 0)

beer <- dieta %>% 
  dplyr::filter(alcoclass == 1)
wine <- dieta %>% 
  dplyr::filter(alcoclass == 2)


