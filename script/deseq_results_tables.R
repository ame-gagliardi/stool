source("C:/Users/amedeo/Desktop/R_Projects/general_script/useful_functions.R")

refCov <- c("wine_consumption")                                  # Variable to study
refDb <- c("female")                                      # Dataset with the variable
cts <- c("normalized")                                  # Count matrix (normalized or raw)
dds.path <- paste0("results/by_sex/", refDb,"/dds/")    # Dds object path
result.path <- c("results/by_sex/female/")                # Path to save results


## Dataset load
df <- readRDS(paste0("data/clinical/de_",refDb,"_merged_cleaned.rds"))
cts <- readRDS(paste0("data/ngs/",cts,"_counts.rds"))

i <- intersect(rownames(df), colnames(cts))
cts <- cts[,i] 
df <- df[i,]
all.equal(rownames(df), colnames(cts))
## DEseq analysis results

dds <- readRDS(paste0(dds.path,refCov,".rds"))
results(dds)

## Insert here case-by-case modification to the datasets - DELETE BEFORE CLOSING
# levels(df$bmi_cat) <- c("normal", "over", "over", NA)
##

## Comparison
levName <- covarLevels(refCov, df)
comp <- combn(levName, m=2)
check <- rownames(results(dds))
##

for(i in 1:ncol(comp)){
  cov1 <- comp[1,i]                                # Confronti delle variabili nelle colonne di comp
  cov2 <- comp[2,i]
  mean <- miRNA_average_class(refCov, df, cts)     # Creo una matrice con le media dei miRNA per ogni livello della variabile
  mean$ID <- rownames(mean)
  median <- miRNA_median_class(refCov, df, cts)    # Creo una matrice con le mediane dei miRNA per ogni livello della variabile
  median$ID <- rownames(median)
  
  if(all.equal(check, rownames(mean))){
    res <- results(dds, contrast = c(refCov, cov2, cov1)) # Carico il dds con i risultati del contrast specificato
    res[,"ID"] <- rownames(res)                           # Aggiungo il nome dei miRNA
    res[,"comparison"] <- paste0(cov2, "_vs_", cov1)      # Aggiungo il nome del confronto
    res[,"treshold"] <- res$padj <0.05                    # Aggiungo un V/F sul treshold di significatività
    res <- as.data.frame(res)                
    ctrl <- all.equal(rownames(mean), rownames(median))
    ctrl2 <- all.equal(rownames(mean), rownames(dds))
      if(ctrl == TRUE & ctrl2 == TRUE){
        res <- merge(res, mean, by = "ID")                # Merge della matrice dei risultati con media e mediana
        res <- merge(res, median, by = "ID")
      }else{
        print("Rownames non equal: res and mean not merged")
      }
    
  dir.create(paste0(result.path, "tables/",refCov), recursive = TRUE) # Creo la cartella dove salvare i risultati
  write.table(res, file = paste0(result.path,"/tables/",refCov, "/DE_results_", cov2,"_vs_", cov1,".txt", sep = ""), sep = "\t", quote = F, row.names = F)
  }else{
    print("Rownames no equal")
  }
}

rm(list = setdiff(ls(), lsf.str()))




