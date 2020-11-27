source("C:/Users/amedeo/Desktop/R_Projects/general_script/useful_functions.R")
library(ggpubr)

# Data loading

df <- readRDS("data/clinical/de_all_merged_cleaned.rds") %>% 
  dplyr::filter(sex == "male")
rownames(df) <- df$id

cts <- readRDS("data/ngs/raw_counts.rds")
norm <- readRDS("data/ngs/normalized_counts.rds")

i <- intersect(rownames(df), colnames(norm))
df <- df[i,]
norm <- norm[,i]



# Age and BMI correlation
t.norm <- as.data.frame(t(norm))

db <- df %>% 
  dplyr::select(id, age, bmi)

corrMat <- as.data.frame(matrix(NA, nrow = length(colnames(t.norm)), ncol = 7))
colnames(corrMat) <- c("miRNA", "age", "age.p", "age.fdr", "bmi", "bmi.p", "bmi.fdr")


for(i in 1:length(colnames(t.norm))){
  
  db[,4] <- t.norm[,i]
  ken.age <- cor.test(db$age, db$V4, method = "kendall")
  ken.bmi <- cor.test(db$bmi, db$V4, method = "kendall")
  corrMat[i, "miRNA"] <- colnames(t.norm)[i]
  corrMat[i, "age"] <- round(ken.age$estimate, 3)
  corrMat[i, "age.p"] <- round(ken.age$p.value, 3)
  corrMat[i, "bmi"] <- round(ken.bmi$estimate, 3)
  corrMat[i, "bmi.p"] <- round(ken.bmi$p.value, 3)
}

corrMat$age.fdr <- p.adjust(corrMat$age.p, method = "fdr")
corrMat$bmi.fdr <- p.adjust(corrMat$bmi.p, method = "fdr")


write.table(corrMat, file = c("results/age_bmi_correlation_male.txt"), sep = "\t", row.names = FALSE, quote = FALSE)
