##PCA script: DESeq2 e prcomp
library(tidyverse)
library(DESeq2)

refDB <- "all"
refCTS <- "raw"
df.path <- paste0("data/clinical/de_",refDB, "_merged_cleaned.rds")
cts.path <- paste0("data/ngs/", refCTS, "_counts.rds")

df <- readRDS(df.path)
cts <- readRDS(cts.path)

i <- intersect(rownames(df), colnames(cts))
df <- df[i,]
cts <- cts[,i]

## Prcomp PCA ##

count.pca <- prcomp(log(cts+1))
tmp <- as.data.frame(count.pca$rotation[,c(1,2)])
colnames(tmp) <- c("PC1","PC2")

check <- all.equal(rownames(tmp), rownames(df))
if(check == FALSE){
  i <- intersect(rownames(tmp), rownames(df))
  tmp <- tmp[i,]
  coldata <- coldata[i,]
}

varPCA <- "ncigs"

tmp[,varPCA] <- df[match(rownames(tmp), rownames(df), nomatch=0),varPCA]
ggplot2::ggplot(data = tmp, ggplot2::aes(x=PC1, y=PC2)) +
  geom_point(aes(color=get(varPCA)))+
  labs(color = "Number of cigs/day") +
  scale_color_discrete(labels = c("<16", ">16", "Former", "Never"))

## DESeq PCA

covToModel <- c("age_cat", "sex", "ncigs")

df <- drop_na(df, smoke)
i <- intersect(rownames(df), colnames(cts))
df <- df[i,]
cts <- cts[,i]

model <- as.formula(paste("~", paste(covToModel,collapse = "+")))

dds <- DESeqDataSetFromMatrix(countData = cts,
                              colData = df,
                              design = model)

vsd <- varianceStabilizingTransformation(dds, blind = TRUE, fitType = "local")

plotPCA(vsd, intgroup = "ncigs")


# Solo sui significativi

cigs <- read.delim("C:/Users/amedeo/Desktop/sig_ncigs.txt", header = FALSE)
cigs <- unique(cigs$V1)

keep <- which(rownames(cts) %in% cigs)

cts <- cts[keep,]
i <- intersect(colnames(cts), rownames(df))
df <- df[i,]
cts <- cts[,i]

zeroes <- rowSums(cts)
table(zeroes == 0)

pseudoCTS <- cts+1

df <- drop_na(df, smoke)
i <- intersect(rownames(df), colnames(pseudoCTS))
df <- df[i,]
pseudoCTS <- pseudoCTS[,i]

model <- as.formula(paste("~", paste(covToModel,collapse = "+")))

dds <- DESeqDataSetFromMatrix(countData = pseudoCTS,
                              colData = df,
                              design = model)

vsd <- varianceStabilizingTransformation(dds, blind = TRUE, fitType = "local")

plotPCA(vsd, intgroup = "ncigs")



##prcomp
count.pca <- prcomp(log(cts+1))
tmp <- as.data.frame(count.pca$rotation[,c(1,2)])
colnames(tmp) <- c("PC1","PC2")

check <- all.equal(rownames(tmp), rownames(df))
if(check == FALSE){
  i <- intersect(rownames(tmp), rownames(df))
  tmp <- tmp[i,]
  coldata <- coldata[i,]
}

varPCA <- "ncigs"

tmp[,varPCA] <- df[match(rownames(tmp), rownames(df), nomatch=0),varPCA]
ggplot2::ggplot(data = tmp, ggplot2::aes(x=PC1, y=PC2)) +
  geom_point(aes(color=get(varPCA))) +
  labs(color = "Number of cigs/day") +
  scale_color_discrete(labels = c("<16", ">16", "Former", "Never"))
