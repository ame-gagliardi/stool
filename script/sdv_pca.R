library(umap)
library(factoextra)

count.path <- c("C:/Users/amedeo/Desktop/R_Projects/sdv/data/ngs_count/merged_count_harmonized_converted.rds") # Inserisci qua il path della matrice delle conte 
covar.path <- c("C:/Users/amedeo/Desktop/R_Projects/sdv/data/clinical/merged_study_cleaned_samples.rds") # Inserisci qua il path della tabella delle covariate

rawCount <- as.data.frame(readRDS(count.path))
coldata <- as.data.frame(readRDS(covar.path))

i <- intersect(rownames(coldata), colnames(rawCount))
coldata <- coldata[i,]
rawCount <- rawCount[,i]

select_rows <- sapply(1:nrow(rawCount), FUN = function(i){sum(rawCount[i,]>0)/length(rawCount[i,]) > 0.1}) # Filtro -> Almeno x% dei campioni deve avere la conta >0
select_rows <- sapply(1:nrow(rawCount), FUN = function(i){median(as.numeric(rawCount[i,])) > 20})          # Filtro -> La feature viene tenuta se ha una mediana >20

select_count <- rawCount[select_rows,] 

count.pca <- prcomp(log(rawCount+1))                     # PCA con prcomp
tmp <- as.data.frame(count.pca$rotation[,c(1,2)]) # Risultati PCA con prcomp

count.umap.raw <- umap(t(log(select_count+1)))               # PCA con umap
tmp <- as.data.frame(count.umap.raw$layout)       # Risultati PCA con umap

colnames(tmp) <- c("x","y")

# Aggiungere le variabili per visualizzare i gruppi nel plot PCA

tmp[,"smoke"] <- coldata[match(rownames(tmp), rownames(coldata), nomatch=0),"smoke"]
tmp[,"age_cat"] <- coldata[match(rownames(tmp), rownames(coldata), nomatch=0),"age_cat"]
tmp[,"sex"] <- coldata[match(rownames(tmp), rownames(coldata), nomatch=0),"sex"]
tmp[,"study"] <- coldata[match(rownames(tmp), rownames(coldata), nomatch=0),"study"]
tmp[,"from"] <- crc_coldata[match(rownames(tmp), rownames(crc_coldata), nomatch=0),"from"]


ggplot2::ggplot(data = tmp, ggplot2::aes(x=PC1, y=PC2)) +
  geom_point(ggplot2::aes(color=from))

# Grafici vari sulla PCA

fviz_eig(count.pca) # % di varianza spiegata dalle dimensioni

