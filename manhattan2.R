# Manhattan plot

library(qqman)
library(tidyverse)

# SNP = nome del mirna
# P = cosa rappresentare sull'asse Y, in questo caso log della mediana
# CHR = cromosoma del mirna
# BP = posizione sul cromosoma

# Data

a <- read.delim("data/downstream/mirna_location.csv", sep = ";")
colnames(a) <- c("SNP", "n.samples", "P", "CHR", "BP", "end", "strand", "X")
a <- a %>% 
  dplyr::select(SNP, P, CHR, BP)

# Manipulation

a$P <- log10(a$P +1) # Log10 della mediana 
# Rinomino chrX e chrY in numerical
X <- which(a$CHR == "X")
Y <- which(a$CHR == "Y")
a$CHR[X] <- 23
a$CHR[Y] <- 24
a$CHR <- as.numeric(a$CHR)
rownames(a) <- a$SNP

# miRNA da evidenziare nel plot
hl <- c("hsa-miR-1246-3p:Novel", "hsa-miR-1302", "hsa-miR-3125", "hsa-miR-320e-5p:Novel", "hsa-miR-5698",
        "hsa-miR-607", "hsa-miR-6075", "hsa-miR-647-3p:Novel", "hsa-miR-6777-5p")

manhattan(a, 
          chr = "CHR",
          bp = "BP",
          snp = "SNP",
          p = "P",
          ylim = c(0,4),
          logp = F,
          suggestiveline = 1,
          highlight = hl,
          chrlabs = c(1:22, "X", "Y"),
          ylab = "log10(Median Expression)",
          cex.axis = 0.75)


