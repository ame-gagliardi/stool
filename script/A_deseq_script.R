library(DESeq2)
library(tidyverse)

# Con questo script mi fa le analisi per ogni variabili e le salva come dds

cts <- as.data.frame(readRDS("data/ngs/merged_harmonized_converted.rds"))
rm <- which(colnames(cts) == "VOV114") # Campione da rimuovere
cts <- cts[,-rm]

variables <- c("age_cat", "sex", "smoke", "ncigs", "alcool", "wine_consumption", "phys_act", "coffee_cat", "mestr_now", "bmi_cat") # Variabili da analizzare


for(i in 1:length(variables)){
print("Loading a fresh dataset")

df <- as.data.frame(readRDS("data/clinical/de_merged_cleaned.rds"))
rownames(df) <- df$id
levels(df$bmi_cat) <- c("under", "normal", "over", "over")
rm <- which(df$bmi_cat == "under")
df <- df[-rm,]  

print(paste("Deseq on", variables[i]))
covars <- c("library", "sex", "age_cat", variables[i])

check <- all(colnames(cts) == rownames(df))
if(check == FALSE){
  print(paste0("Tidying count matrix and dataset on ", last(covars)))
  i <- intersect(rownames(df), colnames(cts))
  df <- df[i,]
  cts <- cts[,i]
}

m <- length(df[,last(covars)])
xx <- rownames(df)[which(is.na(df[,last(covars)]))]
df <- tidyr::drop_na(df, last(covars))
n <- length(df[,last(covars)])
n.na <- m-n  
print(paste("I've dropped",n.na,"samples for NA: ", paste0(xx, collapse = " ,")))
line <- paste0("Samples excluded from ", last(covars), " deseq analyses (n = ",n.na,"): ", paste0(xx, collapse = " ,"))
write(line,file="results/de_full_output.txt",append=TRUE)

if(n.na>0){
  print(paste0("Tidying count matrix and dataset after NA dropping on ", last(covars)))
  i <- intersect(colnames(cts), rownames(df)) 
  cts <- cts[,i]                              
  df <- df[i,]
}
new.check <- all.equal(colnames(cts), rownames(df))

if(new.check == TRUE){
  print("I'll proceed to the deseq analysis")
  model <- as.formula(paste("~", paste(covars,collapse = "+")))
  dds <- DESeqDataSetFromMatrix(countData = cts,
                                colData = df,
                                design = model)
  
  dds <- DESeq(dds)
  print(paste("Saving deseq results of", last(covars)))
  saveRDS(dds, paste0("results/full_model/dds/",last(covars),".rds"))
} else {
  print(paste0("Houston we have a problem on", last(covars)))
  }
}
