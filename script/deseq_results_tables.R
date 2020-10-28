library(tidyverse)
library(DESeq2)

refCov <- c("age_terz")
refDb <- c("male")

## Dataset load
df <- read.delim(paste0("data/clinical/de_",refDb,"_merged_cleaned.txt"))
df[,refCov] <- as.factor(df[,refCov])
levName <- levels(df[,refCov])
## DEseq analysis results
dds <- readRDS(paste0("results/full_model/dds/",refCov,".rds"))
results(dds)
## Mean and median loading
mean <- readRDS("data/miRNA_mean.rds")
mean$ID <- rownames(mean)
median <- readRDS("data/miRNA_median.rds")
median$ID <- rownames(median)

## Insert here case-by-case modification to the datasets - DELETE BEFORE CLOSING
##

## Comparison
comp <- combn(levName, m=2)
check <- rownames(results(dds))

for(i in 1:ncol(comp)){
  cov1 <- comp[1,i] # Confronti delle variabili nelle colonne di comp
  cov2 <- comp[2,i]
  
  keep <- c(levName, "ID") # Select the mean and median of the refCov
  mean <- mean[,keep]
  median <- median[,keep]
  if(all.equal(check, rownames(mean))){
    res <- results(dds, contrast = c(refCov, cov2, cov1))
    res[,"ID"] <- rownames(res)
    res[,"comparison"] <- paste0(cov2, "_vs_", cov1)
    res[,"treshold"] <- res$padj <0.05
    res <- as.data.frame(res)
    res <- merge(res, mean, by = "ID")
    res <- merge(res, median, by = "ID")
    write.table(res, file = paste0("results/full_model/tables/DE_results_", cov2,"_vs_", cov1,".txt", sep = ""), sep = "\t", quote = F, row.names = F)
  }else{
    print("Rownames no equal")
  }
    
}

rm(list=ls())

