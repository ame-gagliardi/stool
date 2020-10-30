library(tidyverse)
library(ggpubr)
library(DESeq2)

## Data load

cts <- readRDS("data/ngs/merged_harmonized_converted.rds")

df <- readRDS("data/clinical/de_all_merged_cleaned.rds")
rownames(df) <- df$id

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
cts <- as.data.frame(t(cts))
corrAge <- as.data.frame(matrix(data = NA, nrow = length(rownames(cts)), ncol = 3))
colnames(corrAge) <- c("miRNA", "kendall_correlation", "pvalue")

for(i in 1:length(colnames(cts))){
  df[,22] <- cts[,i]
  colnames(df)[22] <- colnames(cts)[i]
  resKen <- cor.test(df$age, df[,last(colnames(df))], method = "kendall")
  corrAge[i,1] <- colnames(cts)[i]
  corrAge[i,2] <- resKen$estimate
  corrAge[i,3] <- resKen$p.value
}

corrAge <- merge(corrAge, sig_tot, by = "miRNA")
# Bmi
# I'll delete the underweight category
df <- readRDS("data/clinical/de_all_merged_cleaned.rds")
rownames(df) <- df$id
rm <- which(df$bmi_cat == "underweight")
df <- df[-rm,]

cts <- readRDS("data/ngs/merged_harmonized_converted.rds") %>% 
  dplyr::select(-VOV114)

i <- intersect(rownames(df), colnames(cts))
df <- df[i,]
cts <- cts[,i]
all.equal(rownames(df), colnames(cts))

ggdensity(df$bmi,
          main = c("Density plot of bmi distribution"),
          xlab = c("BMI"))

shapiro.test(df$bmi) # p.value is lower than 0.05, bmi is not normally distributed

ggpubr::ggqqplot(df$bmi, col = "steelblue", size = 1, title = c("qqPlot of age"))

cts <- as.data.frame(t(cts))

i <- intersect(rownames(cts), rownames(df))
cts <- cts[i,]
df <- df[i,]
all.equal(rownames(df), rownames(cts))

corrBMI <- as.data.frame(matrix(data = NA, nrow = length(colnames(cts)), ncol = 3))
colnames(corrBMI) <- c("miRNA", "kendall_correlation", "pvalue")

for(i in 1:length(colnames(cts))){
  df[,22] <- cts[,i]
  colnames(df)[22] <- colnames(cts)[i]
  resKen <- cor.test(df$bmi, df[,22], method = "kendall")
  corrBMI[i,1] <- colnames(cts)[i]
  corrBMI[i,2] <- resKen$estimate
  corrBMI[i,3] <- resKen$p.value
}

corrBMI <-  merge(corrBMI, sig_tot, by = "miRNA")


write.table(corrAge, file = "results/age_correlation.txt", sep = "\t", row.names = FALSE, quote = FALSE)
write.table(corrBMI, file = "results/bmi_correlation.txt", sep = "\t", row.names = FALSE, quote = FALSE)
