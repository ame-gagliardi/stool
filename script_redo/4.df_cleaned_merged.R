libraries <- c("stringi", "tidyverse", "memisc")
lapply(libraries, require, character.only = TRUE)

## Script per unire i dataset uomo e donna ##

# uomo <- readRDS("C:/Users/amedeo/Desktop/R_Projects/sdv_redo/data_redo/uomo.rds")
# donna <- readRDS("C:/Users/amedeo/Desktop/R_Projects/sdv_redo/data_redo/donna.rds")

uomo <- readRDS("C:/Users/amedeo/Desktop/check/uomo.rds")
donna <- readRDS("C:/Users/amedeo/Desktop/check/donna.rds")


# Seleziono le colonne in comune e le metto nello stesso ordine

i <- intersect(colnames(uomo), colnames(donna))
uomo <- uomo[,i]
donna <- donna[,i]
all.equal(colnames(uomo), colnames(donna))

# Correzione dell'età della donna 56

donna$dis_tumors_age <- as.numeric(as.character(donna$dis_tumors_age))
donna$dis_tumors_age[56] <- c("73")
donna$dis_tumors_age <- as.numeric(donna$dis_tumors_age)

# Merge dei dataset

df <- bind_rows(uomo, donna)

# Levels delle variabili

df$sex <- factor((df$sex),
                 levels = c(0,1),
                 labels = c("donna","uomo"))

df$smk_smoke_status <- factor((df$smk_smoke_status),
                              levels = c(0,1,2),
                              labels = c("non_fumatore","ex_fumatore", "fumatore"))

df$bmi_cat <- factor((df$bmi_cat),
                     levels = c(1,0,2,3),
                     labels = c("underweight", "normal", "overweight", "obese"))

df$PA_total <- tolower(gsub(" ","_", df$PA_total))
df$PA_total <- as.factor(df$PA_total)
df$PA_total <- factor((df$PA_total),
                      levels = c("inactive", "moderately_inactive", "moderately_active", "active"))


colnames(df)[13] <- c("smoke")

df <- dplyr::relocate(df, "PA_total", .after = "smoke")
df <- dplyr::relocate(df, "idfogliorisposte", .after = "idpaziente")

## Dataset separati per studio ##

crc <- df[which(df$study == "CRC"),]
vov <- df[which(df$study == "VOV"),]
cel <- df[which(df$study == "Celiac"),]

crc$id_pat <- factor(crc$id_pat,
                     levels = c(0,1,2,3,4),
                     labels = c("controllo","polipi","infiammazione","crc","emorroidi"))

vov$id_pat <- factor(vov$id_pat,
                     levels = c(0,1,2),
                     labels = c("onnivori","vegetariani","vegani"))

cel$id_pat <- factor(cel$id_pat,
                     levels = c(0,1,2),
                     labels = c("dieta","nuova","controllo"))  

## Elimino polipi/infiammazioni/tumori da crc##

crc <- crc[which(crc$id_pat == "controllo" | crc$id_pat == "emorroidi"),]
crc$id_pat <- droplevels(crc$id_pat)

## Elimino i ripetuti e le nuove diagnosi dai celiaci ##

cel <- cel[which(cel$idpaziente != "Cii_008"),]
cel <- cel[which(cel$idpaziente != "Cii_011"),]
cel <- cel[which(cel$idpaziente != "Cii_014"),]
cel <- cel[which(cel$idpaziente != "Cii_021"),]
cel <- cel[which(cel$idpaziente != "Cii_026"),]
cel <- cel[which(cel$idpaziente != "Cii_035"),]
cel <- cel[which(cel$idpaziente != "CMa_010"),]
cel <- cel[which(cel$idpaziente != "CMa_009"),]
cel <- cel[which(cel$id_pat == "dieta" | cel$id_pat == "controllo"),]
cel$id_pat <- droplevels(cel$id_pat)

rownames(crc) <- crc$idpaziente
rownames(vov) <- vov$idpaziente
rownames(cel) <- cel$idpaziente

## Salvo i singoli dataset e il nuovo dataset completo

# save(crc, vov, cel, file = "C:/Users/amedeo/Desktop/R_Projects/sdv_redo/data_redo/studies.rda")

save(crc, vov, cel, file = "C:/Users/amedeo/Desktop/check/studies.rda")

dfmerged <- bind_rows(crc, vov, cel)

# saveRDS(dfmerged, file = "C:/Users/amedeo/Desktop/R_Projects/sdv_redo/data_redo/cleaned_samples.rds")
saveRDS(dfmerged, file = "C:/Users/amedeo/Desktop/check/cleaned_samples.rds")

