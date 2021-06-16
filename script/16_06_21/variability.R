library(tidyverse)
library(DESeq2)


df <- readRDS("data/clinical/de_all_merged_cleaned.rds")
norm_cts <- readRDS("data/ngs/normalized_counts.rds")
raw_cts <- readRDS("data/ngs/raw_counts.rds")

i <- intersect(rownames(df), colnames(norm_cts))
norm_cts <- norm_cts[,i]
df <- df[i,]

dispersion <- data.frame(mean = apply(norm_cts, 1, mean, na.rm = TRUE),
                         median = apply(norm_cts, 1, median, na.rm = TRUE),
                         st.dev = apply(norm_cts, 1, sd, na.rm = TRUE),
                         min = apply(norm_cts, 1, min, na.rm = TRUE),
                         max = apply(norm_cts, 1, max, na.rm = TRUE))

dispersion[,"range"] <- dispersion$max - dispersion$min
dispersion[,"mad"] <- apply(norm_cts, 1, mad, na.rm = TRUE)

## MAD plotting

mad <- dispersion %>% 
  dplyr::filter(mad > 20)

mad$miRNA <- rownames(mad)
mad$index <- seq(1:length(rownames(mad)))


ggplot(mad, aes(x = index, y = mad)) +
  geom_point() +
  geom_text(aes(label= miRNA ,hjust=0, vjust=0))

