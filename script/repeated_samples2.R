source("C:/Users/UappaSguappa/Desktop/R_projects/general_script/useful_functions.R")

df <- readRDS("data/clinical/repeated_samples.rds")
cts <- readRDS("data/ngs/repeated_counts.rds")

rownames(df) <- df$vov_id
rownames(df) %in% colnames(cts)

i <- intersect(rownames(df), colnames(cts))
cts <- cts[,i]
df <- df[i,]

df$sex <- as.factor(df$sex)
df$libraries <- as.factor(df$libraries)
df$libraries <- str_replace_all(df$libraries, " ", "_")
## Count normalization
covars <- c("libraries", "sex")
model <- as.formula(paste("~", paste(covars,collapse = "+")))
#VOV
vov <- df[1:6,]
cts_vov <- cts[,1:6]

dds <- DESeqDataSetFromMatrix(countData = cts_vov,
                              colData = vov,
                              design = model)

dds <- estimateSizeFactors(dds)
vov_norm <- as.data.frame(counts(dds, normalized = TRUE))
#CEL
cel <- df[7:12,]
cts_cel <- cts[,7:12]

dds <- DESeqDataSetFromMatrix(countData = cts_cel,
                              colData = cel,
                              design = model)

dds <- estimateSizeFactors(dds)
cel_norm <- as.data.frame(counts(dds, normalized = TRUE))
#Merge
repeated_normalized <- bind_cols(vov_norm, cel_norm)


saveRDS(repeated_normalized, file = "data/ngs/repeated_normalized_counts.rds")
saveRDS(df, "data/clinical/repeated_samples.rds")


######
df <- readRDS("data/clinical/repeated_samples.rds")
dispersion <- readRDS("results/repeated/rep_dispersion.rds")
norm_cts <- as.data.frame(t(readRDS("data/ngs/repeated_normalized_counts.rds")))

vov <- df[1:6,]
cel <- df[7:12,]

vov_cts <- norm_cts[1:6,]
cel_cts <- norm_cts[7:12,]


prova <- as.data.frame(matrix(NA, nrow = 3524, ncol = 1))
rownames(prova) <- colnames(norm_cts)

for(i in 1:length(colnames(norm_cts))){
  vov[,16] <- vov_cts[,i]
  colnames(vov)[16] <- colnames(vov_cts)[i]
  cel[,16] <- cel_cts[,i]
  colnames(cel)[16] <- colnames(cel_cts)[i]
  res <- t.test(vov[,16], cel[,16], paired = TRUE)
  prova[i,1] <- res$p.value
}

all.equal(rownames(dispersion), rownames(prova))
dispersion$p.value <- prova$V1

saveRDS(dispersion, file = "results/repeated/rep_dispersion.rds")
write.table(dispersion, file = "results/repeated/rep_dispersion.txt", sep = "\t", quote = FALSE, row.names = TRUE)

