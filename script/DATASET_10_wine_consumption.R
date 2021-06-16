## Wine consumption {.tabset}

```{r wine, include=FALSE}
count.path <- c("C:/Users/amedeo/Desktop/R_projects/stool/data_redo/ngs/merged_harmonized_converted.rds")
covar.path <- c("C:/Users/amedeo/Desktop/R_projects/stool/data_redo/clinical/merged_cleaned.rds")

rawCount <- readRDS(count.path)
coldata <- readRDS(covar.path)

rm <- c("VOV114")
check <- rm %in% rownames(coldata)
if(check == TRUE){
  xx <- which(coldata$id == "VOV114")
  coldata <- coldata[-xx,]
}

covars <- c("age_cat","sex", "library", "wine")

i <- intersect(colnames(rawCount), rownames(coldata)) # Controlla che i campioni siano gli stessi e ordinati nello stesso modo
rawCount <- rawCount[,i]                              # nella matrice delle conte e nella tabella delle covariate                          
coldata <- coldata[i,]
all.equal(colnames(rawCount), rownames(coldata))

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

rm(covar.path,count.path,i,m,n,check, rm, n.na, xx)

model <- as.formula(paste("~", paste(covars,collapse = "+")))

dds <- DESeqDataSetFromMatrix(countData = rawCount,
                              colData = coldata,
                              design = model)
dds <- DESeq(dds)

res_1 <- results(dds, contrast = c("wine","1", "0"), alpha = 0.01)
res_1$miRNA <- rownames(res_1)
res_1 <- as_tibble(res_1) %>% 
  dplyr::arrange(padj) %>% 
  dplyr::mutate(comparison = "1_0") %>% 
  dplyr::relocate("miRNA", .before = "baseMean")

res_2 <- results(dds, contrast = c("wine","2", "0"), alpha = 0.01)
res_2$miRNA <- rownames(res_2)
res_2 <- as_tibble(res_2) %>% 
  dplyr::arrange(padj) %>% 
  dplyr::mutate(comparison = "2_0") %>% 
  dplyr::relocate("miRNA", .before = "baseMean")

res_3 <- results(dds, contrast = c("wine","2", "1"), alpha = 0.01)
res_3$miRNA <- rownames(res_3)
res_3 <- as_tibble(res_3) %>% 
  dplyr::arrange(padj) %>% 
  dplyr::mutate(comparison = "2_1") %>% 
  dplyr::relocate("miRNA", .before = "baseMean")

res <- bind_rows(res_1, res_2, res_3) %>% 
  dplyr::arrange(padj) %>% 
  distinct(miRNA, .keep_all = TRUE )

```

```{r wine table, echo=FALSE}
kable(res) %>% kable_styling(bootstrap_options = c("striped", "hover"), position = "center") %>% scroll_box(height = "300px")
```

```{r wine volcano, warning=FALSE}

# Volcano plot data #

df <- as_tibble(data.frame(miRNA = res$miRNA, baseMean = round(res$baseMean, digits = 2), logFC = round(res$log2FoldChange, digits = 2), negLogPval = -log10(res$pvalue), padj = round(res$padj, digits = 4)))
treshold <- df$padj <0.05
df$treshold <- treshold
df <- df[complete.cases(df$treshold),]

# Volcano plot 

plot_ly(df, x=~logFC, y=~negLogPval, type="scatter", mode = "markers", color=~treshold,
        hoverinfo = 'text',
        text = ~paste(miRNA))

# '\n Padj: ', padj,
# '\n Log2FC: ', logFC)

```

```{r wine boxplot, echo=FALSE, warning=FALSE}
# Boxplot data

sign_mir <- df %>% 
  dplyr::filter(baseMean >=20 & padj < 0.05)
sign_mir <- as.data.frame(sign_mir)
rownames(sign_mir) <- sign_mir$miRNA

countData <- rawCount[rownames(sign_mir),]
countData <- as.data.frame(t(countData))
countData <- countData[!seq_len(nrow(countData)) %in% sapply(countData, which.max),]
countData$id <- rownames(countData)
# colnames(coldata)[1] <- "id"
countData <- merge(countData, coldata, by= "id", all.x = TRUE)
xx <- grepl(paste0("(?=.*", "hsa|id_pat", ")", collapse=""), colnames(countData), perl=TRUE)
countData <- countData[,xx]
countData.m <- pivot_longer(countData, c(1:length(colnames(countData)) - 1), names_to = "miRNA", values_to = "count")

# Boxplot

ggplot(data = countData.m, aes(x=miRNA, y=count)) + 
  geom_boxplot(aes(fill=id_pat)) +
  facet_wrap( ~ miRNA, scales="free")

rm(list=ls())

```