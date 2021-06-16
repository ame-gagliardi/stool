source("C:/Users/amedeo/Desktop/R_Projects/general_script/libraries_functions.R")
source("C:/Users/amedeo/Desktop/R_Projects/general_script/libraries_graph.R")

# DATA LOADING

r_folder <- getwd()
folder <- c("/data/")
project <- c("/sv_") 
tissue <- c("stool_")
biospecimen <- c("stool_")
sex <- c("female_")
ctsType <- c("normalized_")
species <- c("mirna_")
cohort <- c("pooled")

df.path <- paste0(r_folder, folder, "clinical", project, tissue, biospecimen, sex, "samples_", cohort, ".rds")
cts.path <- paste0(r_folder, folder, "ngs", project, tissue, biospecimen, sex ,ctsType, "counts_", species, cohort, ".rds")
result.path <- paste0(r_folder, "/results/differential")

df <- readRDS(df.path)
cts <- readRDS(cts.path)

## Filter on median at least 10
median <- apply(cts, 1, median, na.rm = TRUE)
cts <- cts[which(median >= 10),]

##

i <- intersect(rownames(df), colnames(cts))
df <- df[i,]
cts <- cts[,i]
all.equal(rownames(df), colnames(cts))

# Correlation

varCor <- c("age")
t.cts <- as.data.frame(t(cts))

db <- df %>% 
  rownames_to_column(var = "row") %>% 
  dplyr::select(row, all_of(varCor)) %>% 
  dplyr::rename(id = row)
rownames(db) <- db$id

db <- db[complete.cases(db[,varCor]),]

i <- intersect(rownames(db), rownames(t.cts))
db <- db[i,]
t.cts <- t.cts[i,]
all.equal(rownames(db), rownames(t.cts))

corr <- as.data.frame(matrix(NA, nrow = length(colnames(t.cts)), ncol = 8))
colnames(corr) <- c("miRNA", "SC", "SC.p", "SC.fdr", "PC", "PC.p", "PC.fdr", "DC")


for(i in 1:length(colnames(t.cts))){
  
  db[,3] <- t.cts[,i]
  spearman <- cor.test(db[,varCor], db$V3, method = "spearman", exact = FALSE)
  pearson <- cor.test(db[,varCor], db$V3, method = "pearson")
  distance <- energy::dcor(db[,varCor], db$V3)
  corr[i, "miRNA"] <- colnames(t.cts)[i]
  corr[i, "SC"] <- round(spearman$estimate, 4)
  corr[i, "SC.p"] <- round(spearman$p.value, 4)
  corr[i, "PC"] <- round(pearson$estimate, 4)
  corr[i, "PC.p"] <- round(pearson$p.value, 4)
  corr[i, "DC"] <- round(distance, 4)
}

corr$SC.fdr <- p.adjust(corr$SC.p, method = "fdr")
corr$PC.fdr <- p.adjust(corr$PC.p, method = "fdr")

saveRDS(corr, file = paste0("C:/Users/amedeo/Desktop/R_Projects/stool/results/correlation/",biospecimen, tissue, sex, varCor,"_", cohort, "_correlation.rds"))
write.table(corr, file = paste0("C:/Users/amedeo/Desktop/R_Projects/stool/results/correlation/",biospecimen, tissue, sex, varCor,"_", cohort, "_correlation.txt"),
            quote = FALSE, row.names = FALSE, sep = "\t")

rm(list = setdiff(ls(), lsf.str()))
