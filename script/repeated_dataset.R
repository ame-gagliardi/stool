library(tidyverse)
library(DESeq2)

# Dataset creation

df <- readRDS("data/clinical/de_all_merged_cleaned.rds") %>% 
  dplyr::filter(id == "VOV085" | id == "VOV107" | id == "VOV116" | id == "VOV119" | id == "VOV094" | id == "VOV139") %>% 
  dplyr::select(id, age, age_terz, sex, bmi, bmi_cat, alcool_28, wine_consumption, coffee_cat, phys_act_2, met)

df[, "person"] <- c("LA", "AR", "SG", "BP", "AN", "ST")
df[, "cel"] <- c("Cii_008", "Cii_026", "Cii_011", "Cii_014", "Cii_021", "Cii_035")
df[, "vov_date"] <- c("23-11-2017", "05-12-2017", "11-01-2018", "06-02-2018", "07-02-2018", "19-06-2018")
df[, "cel_date"] <- c("14-02-2019", "18-06-2019", "21-02-2019", "15-04-2019", "11-06-2019", "02-07-2019")
df$vov_date <- as.Date(df$vov_date, format = c("%d-%m-%Y"))
df$cel_date <- as.Date(df$cel_date, format = c("%d-%m-%Y"))
df[, "days"] <- df$cel_date - df$vov_date

df <- df[,c("id", "cel", "person","vov_date", "cel_date", "days", "sex", "age", "age_terz", 
            "bmi", "bmi_cat", "alcool_28", "wine_consumption", "coffee_cat", "phys_act_2", "met")]

colnames(df) <- c("vov", "cel", "name", "vov_date", "cel_date", "interval", "sex", "age", "age_cat", "bmi", "bmi_cat", "alcool", "wine", "coffee", "phys_act", "met")

df$phys_act <- as.factor(df$phys_act)
levels(df$alcool) <- c("abstemious", "habitual", "light")
levels(df$coffee) <- c("heavy", "light", "no")

df[7:12,] <- df[1:6,]
df[7:12, "vov"] <- df[1:6, "cel"]
df[7:12, "cel"] <- df[1:6, "vov"]

rownames(df) <- df$vov

## Count matrix

# Raw counts
vov_cts <- readRDS("data/ngs/single_others/vov_harmonized_converted.rds")
i <- intersect(rownames(df)[1:6], colnames(vov_cts))
vov_cts <- vov_cts[,i]


cel_cts <- readRDS("data/ngs/converted/celiac_repeated_converted.rds")
colnames(cel_cts) <- stringr::str_replace_all(colnames(cel_cts), "Cii", "Cii_")
i <- intersect(rownames(df)[7:12], colnames(cel_cts))
cel_cts <- cel_cts[,i]

rep_cts <- bind_cols(vov_cts, cel_cts)

all.equal(rownames(df), colnames(rep_cts))

# Normalized counts (by deseq)

dds <- DESeqDataSetFromMatrix(countData = rep_cts,
                              colData = df,
                              design = ~age_cat + sex + coffee)

dds <- estimateSizeFactors(dds)
rep_norm <- as.data.frame(counts(dds, normalized = TRUE))

saveRDS(df, file = c("data/repeated_samples/repeated_df.rds"))
saveRDS(cel_cts, file = c("data/repeated_samples/cel_cts_raw.rds"))
saveRDS(vov_cts, file = c("data/repeated_samples/vov_cts_raw.rds"))
saveRDS(rep_cts, file = c("data/repeated_samples/repeated_cts_raw.rds"))
saveRDS(rep_norm, file = c("data/repeated_samples/repeated_cts_norm.rds"))



