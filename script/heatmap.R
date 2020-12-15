source("C:/Users/amedeo/Desktop/R_Projects/general_script/functions.R")
source("C:/Users/amedeo/Desktop/R_Projects/general_script/graphics_libraries.R")

# Data #

refDB <- c("both")  # Both, male, female
refCTS <- c("normalized") # Raw, Normalized
refVar <- c("sex") # Variable of interest
annoVar <- c("sex") # Variable for annotation
annoLeg <- c("Sex") # Variable for legend

df <- readRDS(paste0("data/clinical/", refDB, "_samples.rds"))
cts <- readRDS(paste0("data/ngs/", refCTS, "_counts.rds"))
miR_sig <- read.delim(paste0("results/sig_mirna/", refDB, "/",refVar, ".txt"), sep = "\t", header = FALSE, col.names = c("miRNA"))

# Modifiche temporanee al df, commentare o cancellare prima di chiudere #


# Seleziono i miRNA significativi

cts <- as.data.frame(cts[rownames(cts) %in% miR_sig$miRNA,])

i <- intersect(rownames(df), colnames(cts))
df <- df[i,]
cts <- cts[,i]
all.equal(rownames(df), colnames(cts))

# Trasformo in matrice per la heatmap trasformando le normalizzate in log

mat <- as.matrix(cts)
mat <- log2(mat+1)
mat <- t(mat)
mat <- scale(mat)
mat <- t(mat)

# Annotation

col_fun <- colorRamp2(c(-4, -2, 0, 2, 4), c("blue", "green", "white", "yellow", "red"))

column_ha <- HeatmapAnnotation("Sex" = df[, annoVar], "Library" = df[,"library"], "Study" = df[,"study"])

# , col = list("Age (Tertiles)" = col_fun))
# row_dend <- hclust(dist(mat))
# col_dend <- hclust(dist(t(mat)))

Heatmap(mat, 
        heatmap_legend_param = list(
          title = "Z score"),
        show_column_names = FALSE,
        top_annotation = column_ha,
        clustering_distance_columns = "euclidean",
        row_names_gp = gpar(fontsize = 6.5),
        column_title = paste0("Heatmap of ", refVar, " expression"),
        column_title_gp = gpar(fill = "white", border = "black"),
        )

rm(list=ls())
dev.off()



