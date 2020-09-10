
## Correzioni ai dataset che non richiedono uno script dedicato

## Errore nel nome delle library 27.08.2020 ##

## Le library dei celiaci hanno nomi diversi anche sono in realtà la stessa. 'CEL 2' e 'Cel 2' sono la stessa library. 
## CMA_11 è segnato erroneamente come 'Cel 3' ma fa parte della 'Cel 2'.


df <- readRDS("C:/Users/amedeo/Desktop/R_Projects/sdv_redo/data_redo/clinical/merged_cleaned.rds")
df$library <- as.factor(df$library)
levels(df$library)[1] <- c("Library CEL stool 1")
levels(df$library)[2] <- c("Library CEL stool 2")
levels(df$library)[3] <- c("Library CEL stool 2")

# Rename columns

colnames(df)[131] <- c("tot_gr_wine")
colnames(df)[132] <- c("wine")

# Factorizing

df$study <- as.factor(df$study)
df$library <-  as.factor(df$library)
df$alcool <- as.factor(df$alcool)
df$wine <- as.factor(df$wine)

# Change " " in library to "_"

df$library <- gsub(" ", "_", df$library)

# Level changes

levels(df$phys_act) <- c("2", "1", "0", "0")
df$phys_act <- relevel(df$phys_act, "0")

saveRDS(df, "C:/Users/amedeo/Desktop/R_Projects/sdv_redo/data_redo/clinical/merged_cleaned.rds")
