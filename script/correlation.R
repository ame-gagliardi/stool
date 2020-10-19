library(tidyverse)
library(ggpubr)
library(DESeq2)

## Data load

sig_tot <- as.data.frame(readRDS("results/sig_unique.rds"))
rownames(sig_tot) <- sig_tot$miRNA
df <- readRDS("data/clinical/de_merged_cleaned.rds")

cts <- readRDS("data/ngs/merged_harmonized_converted.rds") %>% 
  dplyr::select(-VOV114)
i <-  intersect(rownames(sig_tot), rownames(cts))
cts <- cts[i,]
sig_tot <- sig_tot[i,]

df <- as.data.frame(df)
rownames(df) <- df$id
cts <- as.data.frame(cts)

i <- intersect(rownames(df), colnames(cts))
df <- df[i,]
cts <- cts[,i]
all.equal(rownames(df), colnames(cts))

## Testing normal distribution of BMI and Age

# Age
ggdensity(df$age,
          main = c("Density plot of age distribution"),
          xlab = c("Age (years)"))

shapiro.test(df$age) # p.value is lower than 0.05, age is not normally ditrubuted

ggpubr::ggqqplot(df$age, col = "steelblue", size = 1, title = c("qqPlot of age"))

## Correlation test for age
df <- as.data.frame(df)
cts <- as.data.frame(t(cts))

corrAge <- as.data.frame(matrix(data = NA, nrow = length(sig_tot$miRNA), ncol = 5))
colnames(corrAge) <- c("miRNA", "rho", "spear_pvalue", "tau", "ken_pvalue")

for(i in 1:length(sig_tot$miRNA)){
  df[,17+i] <- cts[,i]
  resKen <- cor.test(df$age, df[,17+i], method = "kendall")
  resSpear <- cor.test(df$age, df[,17+i], method = "spearman")
  corrAge[i,1] <- colnames(cts)[i]
  corrAge[i,2] <- resSpear$estimate
  corrAge[i,3] <- resSpear$p.value
  corrAge[i,4] <- resKen$estimate
  corrAge[i,5] <- resKen$p.value
}

corrAge <- merge(corrAge, sig_tot, by = "miRNA")
# Bmi
# I'll delete the underweight category
df <- readRDS("data/clinical/de_merged_cleaned.rds")
rm <- which(df$bmi_cat == "underweight")
df <- df[-rm,]

cts <- readRDS("data/ngs/merged_harmonized_converted.rds") %>% 
  dplyr::select(-VOV114)
i <-  intersect(rownames(sig_tot), rownames(cts))
cts <- cts[i,]
sig_tot <- sig_tot[i,]


ggdensity(df$bmi,
          main = c("Density plot of bmi distribution"),
          xlab = c("BMI"))

shapiro.test(df$bmi) # p.value is lower than 0.05, bmi is not normally distributed

ggpubr::ggqqplot(df$bmi, col = "steelblue", size = 1, title = c("qqPlot of age"))

df <- as.data.frame(df)
rownames(df) <- df$id
cts <- as.data.frame(t(cts))

i <- intersect(rownames(cts), rownames(df))
cts <- cts[i,]
df <- df[i,]
all.equal(rownames(df), rownames(cts))

corrBMI <- as.data.frame(matrix(data = NA, nrow = length(sig_tot$miRNA), ncol = 5))
colnames(corrBMI) <- c("miRNA", "rho", "spear_pvalue", "tau", "ken_pvalue")

for(i in 1:length(sig_tot$miRNA)){
  df[,17+i] <- cts[,i]
  resKen <- cor.test(df$bmi, df[,17+i], method = "kendall")
  resSpear <- cor.test(df$bmi, df[,17+i], method = "spearman")
  corrBMI[i,1] <- colnames(cts)[i]
  corrBMI[i,2] <- resSpear$estimate
  corrBMI[i,3] <- resSpear$p.value
  corrBMI[i,4] <- resKen$estimate
  corrBMI[i,5] <- resKen$p.value
}

corrBMI <-  merge(corrBMI, sig_tot, by = "miRNA")


write.table(corrAge, file = "results/age_correlation.txt", sep = "\t", row.names = FALSE, quote = FALSE)
write.table(corrBMI, file = "results/bmi_correlation.txt", sep = "\t", row.names = FALSE, quote = FALSE)
