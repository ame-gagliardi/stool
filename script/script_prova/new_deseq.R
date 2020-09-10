# Library load
library(DESeq2)
library(tidyverse)
library(biobroom)
library(pheatmap)
library(RColorBrewer)
library(ComplexHeatmap)



## DATA & PARAMETERS LOADING

count.path <- c("C:/Users/amedeo/Desktop/R_Projects/sdv/data/ngs_count/merged_count_harmonized_converted.rds") # Inserisci qua il path della matrice delle conte 
covar.path <- c("C:/Users/amedeo/Desktop/R_Projects/sdv/data/clinical/merged_study_cleaned_samples.rds") # Inserisci qua il path della tabella delle covariate

rawCount <- readRDS(count.path)
coldata <- readRDS(covar.path)

check <- all(colnames(rawCount) == rownames(coldata))
if(check == FALSE){
  i <- intersect(rownames(coldata), colnames(rawCount))
  coldata <- coldata[i,]
  rawCount <- rawCount[,i]
}
all(colnames(rawCount) == rownames(coldata))

## Model specification and drop of NA from covariate of interest

covars <- c("age_cat", "study", "sex")

m <- length(coldata[,last(covars)])
coldata <- tidyr::drop_na(coldata, last(covars))
n <- length(coldata[,last(covars)])
n.na <- m-n                                          # Ti dice quanti campioni ha eliminato

if(n.na>0){
  i <- intersect(colnames(rawCount), rownames(coldata)) # Risistema i dataframe in modo che siano uguali
  rawCount <- rawCount[,i]                              
  coldata <- coldata[i,]
  all.equal(colnames(rawCount), rownames(coldata))
}

model <- as.formula(paste("~", paste(covars,collapse = "+")))

rm(covar.path, count.path, i, m, n, check, covars, n.na)

## DEseq object

dds <- DESeqDataSetFromMatrix(countData = rawCount,
                              colData = coldata,
                              design = model)

## Exploratory data analysis

vst <- varianceStabilizingTransformation(dds, blind=TRUE) # Transformation for data visualization

plotPCA(vst, intgroup = "from") # Plot PCA

vst_mat <- assay(vst) # Extract the vst matrix from PCA

vst_cor <- cor(vst_mat) # Compute pairwise correlation values

pheatmap(vst_cor,fontsize_row = 5, fontsize_col = 5) # Plot heatmap


## Deseq analysis

dds <- DESeq(dds)

resultsNames(dds)

res <- results(dds, contrast = c("sex", "uomo", "donna"), independentFiltering = TRUE, alpha = 0.05) ## Contrast -> 1. Var d'interesse 2 vs 3
summary(res)

res <- broom::tidy(res)%>% 
  dplyr::arrange(p.adjusted) %>% 
  dplyr::mutate(comparison = "M_F")







########

remove <- c("VF213", "VOV072", "VF210", "VF061", "VF124", "VF078", "VF063", "VOV038", "VOV036", "VOV114")



xx <- which(colnames(rawCount) == c("VF213") |
            colnames(rawCount) == c("VOV072")|
            colnames(rawCount) == c("VF210") |
            colnames(rawCount) == c("VF061") |
            colnames(rawCount) == c("VF124") |
            colnames(rawCount) == c("VF078") |
            colnames(rawCount) == c("VF063") |
            colnames(rawCount) == c("VOV038")|
            colnames(rawCount) == c("VOV036")|
            colnames(rawCount) == c("VOV114"))

rawCount <- rawCount[,-xx]
