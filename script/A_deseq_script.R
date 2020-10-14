library(DESeq2)
library(tidyverse)

cts <- as.data.frame(readRDS("C:/Users/amedeo/Desktop/R_Projects/stool/data/ngs/merged_harmonized_converted.rds"))
rm <- which(colnames(cts) == "VOV114")
cts <- cts[,-rm]

df <- as.data.frame(readRDS("C:/Users/amedeo/Desktop/R_Projects/stool/data/clinical/merged_cleaned.rds"))
rownames(df) <- df$id

#variables <- c("age_cat", "sex", "smoke", "ncigs", "alcool", "alco_class", "phys_act", "coffee_cat")
variables <- c("phys_act")
for(i in 1:length(variables)){
print(paste("Deseq on", variables[i]))
  covars <- c("library", "sex", "age_cat", variables[i])

check <- all(colnames(cts) == rownames(df))
if(check == FALSE){
  i <- intersect(rownames(df), colnames(cts))
  df <- df[i,]
  cts <- cts[,i]
}

m <- length(df[,last(covars)])
df <- tidyr::drop_na(df, last(covars))
n <- length(df[,last(covars)])
n.na <- m-n  

if(n.na>0){
  i <- intersect(colnames(cts), rownames(df)) # Risistema i dataframe in modo che siano uguali
  cts <- cts[,i]                              
  df <- df[i,]
  all.equal(colnames(cts), rownames(df))
}

model <- as.formula(paste("~", paste(covars,collapse = "+")))
dds <- DESeqDataSetFromMatrix(countData = cts,
                              colData = df,
                              design = model)

dds <- DESeq(dds)
print(paste("Saving deseq results of", variables[i]))
saveRDS(dds, paste0("C:/Users/amedeo/Desktop/R_Projects/",last(covars),".rds"))
}

