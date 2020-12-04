source("C:/Users/amedeo/Desktop/R_Projects/general_script/functions.R")
source("script/libraries.R")


# Data loading

refDB <- "all"
refCTS <- "normalized"


df.path <- paste0("data/clinical/", refDB, "_samples.rds")
cts.path <- paste0("data/ngs/", refCTS, "_counts.rds")

df <- readRDS(df.path)
cts <- readRDS("data/ngs/raw_counts.rds")

i <- intersect(rownames(df), colnames(cts))
df <- df[i,]
cts <- cts[,i]
all.equal(rownames(df), colnames(cts))

# Age correlation

t.cts <- as.data.frame(t(cts))

db <- df %>% 
  dplyr::select(age, bmi)

i <- intersect(rownames(db), rownames(t.cts))
db <- db[i,]
t.cts <- t.cts[i,]
all.equal(rownames(db), rownames(t.cts))

corr <- as.data.frame(matrix(NA, nrow = length(colnames(t.cts)), ncol = 8))
colnames(corr) <- c("miRNA", "SC", "SC.p", "SC.fdr", "PC", "PC.p", "PC.fdr", "DC")


for(i in 1:length(colnames(t.cts))){
  
  db[,3] <- t.cts[,i]
  spearman <- cor.test(db$age, db$V3, method = "spearman", exact = FALSE)
  pearson <- cor.test(db$age, db$V3, method = "pearson")
  distance <- energy::dcor(db$age, db$V3)
  corr[i, "miRNA"] <- colnames(t.cts)[i]
  corr[i, "SC"] <- round(spearman$estimate, 3)
  corr[i, "SC.p"] <- round(spearman$p.value, 3)
  corr[i, "PC"] <- round(pearson$estimate, 3)
  corr[i, "PC.p"] <- round(pearson$p.value, 3)
  corr[i, "DC"] <- round(distance, 3)
}

corr$SC.fdr <- p.adjust(corr$SC.p, method = "fdr")
corr$PC.fdr <- p.adjust(corr$PC.p, method = "fdr")


