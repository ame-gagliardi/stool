# Script per l'analisi di espressione differenziale dei miRNA + Boxplot#

# Library load
library(DESeq2)
library(dplyr)
library(biobroom)


## DATA & PARAMETERS LOADING

count.path <- c("C:/Users/amedeo/Desktop/R_Projects/sdv/data/ngs_count/merged_count_harmonized_converted.rds") # Inserisci qua il path della matrice delle conte 
covar.path <- c("C:/Users/amedeo/Desktop/R_Projects/sdv/data/clinical/merged_study_cleaned_samples.rds") # Inserisci qua il path della tabella delle covariate

rawCount <- readRDS(count.path)
coldata <- readRDS(covar.path)

## FILTERING (choose only one)

median_filter <- sapply(1:nrow(rawCount), FUN = function(i){median(as.numeric(rawCount[i,])) > 20})          # Filtro -> La feature viene tenuta se ha una mediana >20
#percentage_filter <- sapply(1:nrow(rawCount), FUN = function(i){sum(rawCount[i,]>0)/length(rawCount[i,]) > 0.1}) # Filtro -> Almeno x% dei campioni deve avere la conta >0

rawCount <- rawCount[median_filter,] # Inserisci il nome del filtro che hai scelto
covars <- c("age_cat","sex") # Scrivi qua i nomi delle covariate che vuoi usare nel modello: la variabile d'interesse DEVE ESSERE L'ULTIMA

#levels(coldata$bmi_cat) <- c("0","1","2","2") ## solo per analisi bmi
#coldata <- coldata[which(coldata$bmi_cat != "1"),]

i <- intersect(colnames(rawCount), rownames(coldata)) # Controlla che i campioni siano gli stessi e ordinati nello stesso modo
rawCount <- rawCount[,i]                              # nella matrice delle conte e nella tabella delle covariate                          
coldata <- coldata[i,]
all.equal(colnames(rawCount), rownames(coldata))      # DEVE essere TRUE


# Elimino i campioni con NA nella covariata d'interesse (DEseq non le accetta)

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

rm(covar.path,count.path,i,m,n,median_filter)

## MODELLO PER L'ANALISI

model <- as.formula(paste("~", paste(covars,collapse = "+")))

## ANALISI ##

# Deseq restituisce una tabella dei risultati con l'ultimo livello contro il primo. Per ottenere tutti i confronti bisogna usare contrast
# e inserire i nomi dei livelli desiderati (il reference level è sempre l'ultimo inserito). Crea una variabile per ogni possibile 
# risultato e poi fai un unione dei dataset per averne uno finale con tutti i miRNA significativi in base alla soglie che dai.
# A partire da quest'ultimo df si faranno poi i vari plot. 

dds <- DESeqDataSetFromMatrix(countData = rawCount,
                              colData = coldata,
                              design = model)
dds <- DESeq(dds)
resultsNames(dds)

res_1 <- results(dds, contrast = c("sex","uomo", "donna"), alpha = 0.01) ## Contrast -> 1. Var d'interesse 2 vs 3
res_1 <- broom::tidy(res_1)%>% 
  dplyr::arrange(p.adjusted) %>% 
  dplyr::mutate(comparison = "M_F")


res_2 <- results(dds, contrast = c("alc_terz","2", "0"), alpha = 0.01)
res_2 <- broom::tidy(res_2) %>% 
  dplyr::filter(p.adjusted < 0.01) %>% 
  dplyr::arrange(p.adjusted) %>% 
  dplyr::mutate(comparison = "2_0") 
  
  
res_3 <- results(dds, contrast = c("alc_terz","2", "1"), alpha = 0.01)
res_3 <- tidy(res_3) %>% 
  dplyr::filter(p.adjusted < 0.01) %>% 
  dplyr::arrange(p.adjusted) %>% 
  dplyr::mutate(comparison = "2_1")

res_4 <- results(dds, contrast = c("alc_cat","over", "abstemious"))
res_4$miRNA <- rownames(res_4)
res_4 <- as_tibble(res_4) %>% 
  dplyr::filter(padj < 0.01) %>% 
  dplyr::arrange(padj) %>% 
  dplyr::mutate(comparison = "4_0") %>% 
  moveMe(c("miRNA"), "first")

res <- bind_rows(res_1, res_2) %>% 
  distinct(gene, .keep_all = TRUE ) %>% 
  dplyr::arrange(p.adjusted)

## SAVE DATA IN RDA

save(res, file = "C:/Users/amedeo/Desktop/R_Projects/sdv/results/ncig_results.rda")


ggplot(res_1, aes(p.value)) +
  geom_histogram()
  
  

