library(tidyverse)
library(DESeq2)

##
cts <- readRDS("C:/Users/amedeo/Desktop/R_Projects/stool/data/ngs/merged_harmonized_converted.rds")          # Matrice conte
rm <- which(colnames(cts) == "VOV114")                                                                       # Elimino VOV114
cts <- cts[,-rm]

df <- as.data.frame(readRDS("C:/Users/amedeo/Desktop/R_Projects/stool/data/clinical/de_merged_cleaned.rds")) # Dataset covariate
rownames(df) <- df$id

i <- intersect(rownames(df), colnames(cts))
cts <- cts[,i]
df <- df[i,]

dds <- DESeqDataSetFromMatrix(countData = cts,                        # Creo oggetto dds
                              colData = df,
                              design = ~library + sex + age_cat)


dds <- estimateSizeFactors(dds)                                      # Size factor per la normalizzazione
normalized <- counts(dds, normalized = TRUE)                         # Conte normalizzate

write.table(normalized, file="data/normalized_counts.txt", sep="\t", quote = FALSE) # Salvo il file
normalized <- read.table("C:/Users/amedeo/Desktop/R_Projects/stool/data/normalized_counts.txt", sep = "\t")

# Variability

variability <- as_tibble(data.frame("miRNA" = rownames(normalized), "min" = apply(normalized, 1, min, na.rm = TRUE), "max" = apply(normalized, 1, max, na.rm = TRUE),
                                    "mean" = round(apply(normalized, 1, mean, na.rm = TRUE), digits = 2), "median" = apply(normalized, 1, median, na.rm = TRUE), 
                                    "sd" = apply(normalized, 1, sd, na.rm = TRUE), "mad" = apply(normalized, 1, mad, na.rm = TRUE),
                                    "age_40" = apply(normalized[,which(df$age_cat == "_40")], 1, median, na.rm = TRUE),
                                    "age40_60" = apply(normalized[,which(df$age_cat == "40_60")], 1, median, na.rm = TRUE),
                                    "age60_" = apply(normalized[,which(df$age_cat == "60_")], 1, median, na.rm = TRUE),
                                    "sexM" = apply(normalized[,which(df$sex == "male")], 1, median, na.rm = TRUE),
                                    "sexF" = apply(normalized[,which(df$sex == "female")], 1, median, na.rm = TRUE),
                                    "smokeNe" = apply(normalized[,which(df$smoke == "never")], 1, median, na.rm = TRUE),
                                    "smokeFo" = apply(normalized[,which(df$smoke == "former")], 1, median, na.rm = TRUE),
                                    "smokeCu" = apply(normalized[,which(df$smoke == "current")], 1, median, na.rm = TRUE),
                                    "ncigs_16" = apply(normalized[,which(df$ncigs == "_16")], 1, median, na.rm = TRUE),
                                    "ncigs16_" = apply(normalized[,which(df$ncigs == "16_")], 1, median, na.rm = TRUE),
                                    "alcoAbst" = apply(normalized[,which(df$alcool == "abstemious")], 1, median, na.rm = TRUE),
                                    "alcoLight" = apply(normalized[,which(df$alcool == "light")], 1, median, na.rm = TRUE),
                                    "alcoHeavy" = apply(normalized[,which(df$alcool == "heavy")], 1, median, na.rm = TRUE),
                                    "wineAll" = apply(normalized[,which(df$wine_consumption == "all")], 1, median, na.rm = TRUE),
                                    "wineNo" = apply(normalized[,which(df$wine_consumption == "nowine")], 1, median, na.rm = TRUE),
                                    "physModAct" = apply(normalized[,which(df$phys_act == "modAct")], 1, median, na.rm = TRUE),
                                    "physModInact" = apply(normalized[,which(df$phys_act == "modInact")], 1, median, na.rm = TRUE),
                                    "physInact" = apply(normalized[,which(df$phys_act == "inactive")], 1, median, na.rm = TRUE),
                                    "coffeeNo" = apply(normalized[,which(df$coffee_cat == "no_coffee")], 1, median, na.rm = TRUE),
                                    "coffeeLight" = apply(normalized[,which(df$coffee_cat == "light_drinker")], 1, median, na.rm = TRUE),
                                    "coffeeHeavy" = apply(normalized[,which(df$coffee_cat == "heavy_drinker")], 1, median, na.rm = TRUE),
                                    "menstrYes" = apply(normalized[,which(df$mestr_now == "yes")], 1, median, na.rm = TRUE),
                                    "menstrNo" = apply(normalized[,which(df$mestr_now == "no")], 1, median, na.rm = TRUE),
                                    "bmiNormal" = apply(normalized[,which(df$bmi_cat == "normal")], 1, median, na.rm = TRUE),
                                    "bmiOver" = apply(normalized[,which(df$bmi_cat == "overweight")], 1, median, na.rm = TRUE),
                                    "age_40_mean" = apply(normalized[,which(df$age_cat == "_40")], 1, mean, na.rm = TRUE),
                                    "age40_60_mean" = apply(normalized[,which(df$age_cat == "40_60")], 1, mean, na.rm = TRUE),
                                    "age60__mean" = apply(normalized[,which(df$age_cat == "60_")], 1, mean, na.rm = TRUE),
                                    "sexM_mean" = apply(normalized[,which(df$sex == "male")], 1, mean, na.rm = TRUE),
                                    "sexF_mean" = apply(normalized[,which(df$sex == "female")], 1, mean, na.rm = TRUE),
                                    "smokeNe_mean" = apply(normalized[,which(df$smoke == "never")], 1, mean, na.rm = TRUE),
                                    "smokeFo_mean" = apply(normalized[,which(df$smoke == "former")], 1, mean, na.rm = TRUE),
                                    "smokeCu_mean" = apply(normalized[,which(df$smoke == "current")], 1, mean, na.rm = TRUE),
                                    "ncigs_16_mean" = apply(normalized[,which(df$ncigs == "_16")], 1, mean, na.rm = TRUE),
                                    "ncigs16__mean" = apply(normalized[,which(df$ncigs == "16_")], 1, mean, na.rm = TRUE),
                                    "alcoAbst_mean" = apply(normalized[,which(df$alcool == "abstemious")], 1, mean, na.rm = TRUE),
                                    "alcoLight_mean" = apply(normalized[,which(df$alcool == "light")], 1, mean, na.rm = TRUE),
                                    "alcoHeavy_mean" = apply(normalized[,which(df$alcool == "heavy")], 1, mean, na.rm = TRUE),
                                    "wineAll_mean" = apply(normalized[,which(df$wine_consumption == "all")], 1, mean, na.rm = TRUE),
                                    "wineNo_mean" = apply(normalized[,which(df$wine_consumption == "nowine")], 1, mean, na.rm = TRUE),
                                    "physModAct_mean" = apply(normalized[,which(df$phys_act == "modAct")], 1, mean, na.rm = TRUE),
                                    "physModInact_mean" = apply(normalized[,which(df$phys_act == "modInact")], 1, mean, na.rm = TRUE),
                                    "physInact_mean" = apply(normalized[,which(df$phys_act == "inactive")], 1, mean, na.rm = TRUE),
                                    "coffeeNo_mean" = apply(normalized[,which(df$coffee_cat == "no_coffee")], 1, mean, na.rm = TRUE),
                                    "coffeeLight_mean" = apply(normalized[,which(df$coffee_cat == "light_drinker")], 1, mean, na.rm = TRUE),
                                    "coffeeHeavy_mean" = apply(normalized[,which(df$coffee_cat == "heavy_drinker")], 1, mean, na.rm = TRUE),
                                    "menstrYes_mean" = apply(normalized[,which(df$mestr_now == "yes")], 1, mean, na.rm = TRUE),
                                    "menstrNo_mean" = apply(normalized[,which(df$mestr_now == "no")], 1, mean, na.rm = TRUE),
                                    "bmiNormal_mean" = apply(normalized[,which(df$bmi_cat == "normal")], 1, mean, na.rm = TRUE),
                                    "bmiOver_mean" = apply(normalized[,which(df$bmi_cat == "overweight")], 1, mean, na.rm = TRUE)))

saveRDS(variability, "data/variability.rds")
