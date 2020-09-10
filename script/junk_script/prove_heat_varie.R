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

dds <- DESeqDataSetFromMatrix(countData = rawCount,
                              colData = coldata,
                              design = model)


vst1 <- varianceStabilizingTransformation(dds, blind=TRUE) # Transformation for data visualization

vst_mat1 <- assay(vst1) # Extract the vst matrix from PCA

vst_cor1 <- cor(vst_mat1) # Compute pairwise correlation values


breaksList = seq(0, 1, by = 0.1)
pheatmap(vst_cor,
         color = colorRampPalette(rev(brewer.pal(n = 7, name = "RdYlBu")))(length(breaksList)), # Defines the vector of colors for the legend (it has to be of the same lenght of breaksList)
         breaks = breaksList,
         fontsize_row = 5,
         fontsize_col = 5,
         main = "complete")

rm(list=ls())

##### Sample removed

count.path <- c("C:/Users/amedeo/Desktop/R_Projects/sdv/data/ngs_count/merged_count_harmonized_converted.rds") # Inserisci qua il path della matrice delle conte 
covar.path <- c("C:/Users/amedeo/Desktop/R_Projects/sdv/data/clinical/merged_study_cleaned_samples.rds") # Inserisci qua il path della tabella delle covariate

rawCount2 <- readRDS(count.path)
coldata2 <- readRDS(covar.path)

torm <- c("VF213","VF210","VF061","VF124","VF078","VOV114","VOV036","VOV038","VOV072", "VF081", "VF063")

torm <- c("VOV114")

torm <- which(rownames(coldata2) %in% torm)
coldata2 <- coldata2[-torm,]

check <- all(colnames(rawCount2) == rownames(coldata2))
if(check == FALSE){
  i <- intersect(rownames(coldata2), colnames(rawCount2))
  coldata2 <- coldata2[i,]
  rawCount2 <- rawCount2[,i]
}
all(colnames(rawCount2) == rownames(coldata2))

covars <- c("age_cat", "study", "sex")

m <- length(coldata2[,last(covars)])
coldata2 <- tidyr::drop_na(coldata2, last(covars))
n <- length(coldata2[,last(covars)])
n.na <- m-n                                          # Ti dice quanti campioni ha eliminato

if(n.na>0){
  i <- intersect(colnames(rawCount2), rownames(coldata2)) # Risistema i dataframe in modo che siano uguali
  rawCount2 <- rawCount2[,i]                              
  coldata2 <- coldata2[i,]
  all.equal(colnames(rawCount2), rownames(coldata2))
}

model <- as.formula(paste("~", paste(covars,collapse = "+")))

rm(covar.path, count.path, i, m, n, check, covars, n.na, torm)

dds <- DESeqDataSetFromMatrix(countData = rawCount2,
                              colData = coldata2,
                              design = model)


vst2 <- varianceStabilizingTransformation(dds, blind=TRUE) # Transformation for data visualization

vst_mat2 <- assay(vst2) # Extract the vst matrix from PCA

vst_cor2 <- cor(vst_mat2) # Compute pairwise correlation values


breaksList = seq(0, 1, by = 0.1)
pheatmap(vst_cor,
         color = colorRampPalette(rev(brewer.pal(n = 7, name = "RdYlBu")))(length(breaksList)), # Defines the vector of colors for the legend (it has to be of the same lenght of breaksList)
         breaks = breaksList,
         fontsize_row = 5,
         fontsize_col = 5,
         main = "All samples",
         annotation_row = my_sample,
         cutree_rows = 3,
         show_colnames = FALSE)


rm(list=ls())

col_fun <- colorRamp2(c(0, 0.2, 0.4, 0.6, 0.8, 1), c("green","blue", "white", "orange", "pink", "red"))
col_fun(seq(0,1))

col_fun <- colorRamp2(c(0, 0.5, 1), c("green", "blue", "red"))
Heatmap(vst_cor, column_title = "All samples", col = col_fun, show_row_names = FALSE, column_names_gp = gpar(fontsize=5))
Heatmap(vst_cor2, column_title = "Filtered samples", col = col_fun)



my_sample <- data.frame(coldata$study)
colnames(my_sample) <- c("Study")




