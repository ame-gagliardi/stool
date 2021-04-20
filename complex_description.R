source("C:/Users/amedeo/Desktop/R_Projects/general_script/libraries_functions.R")
source("C:/Users/amedeo/Desktop/R_Projects/general_script/libraries_graph.R")

cts <- readRDS("data/ngs/sv_stool_stool_both_normalized_counts_mirna_pooled.rds")
df <- readRDS("data/clinical/sv_stool_stool_both_samples_pooled.rds")

# cts <- cts[1:200,]

## Description ##

by_mirna <- as.data.frame(matrix(NA, nrow = length(rownames(cts)), ncol = 4))
rownames(by_mirna) <- rownames(cts)
colnames(by_mirna) <- c("n_subjects", "least_one", "fifty", "all")
cts_check <- cts >= 10

for(i in 1:length(rownames(cts))){
  by_mirna[i, "n_subjects"] <- sum(cts_check[i,])
  by_mirna[i, "least_one"] <- by_mirna[i,"n_subjects"] > 0
  by_mirna[i, "fifty"] <- by_mirna[i,"n_subjects"] >= length(rownames(df))/2
  by_mirna[i, "all"] <- by_mirna[i,"n_subjects"] == length(rownames(df))
}


by_samples <- as.data.frame(matrix(NA, nrow = length(rownames(df)), ncol = 1))
rownames(by_samples) <- rownames(df)
colnames(by_samples) <- c("Total_miRNA")

for(i in 1:length(rownames(by_samples))){
  by_samples[i,"Total_miRNA"] <- sum(cts_check[,i])
}

rm <- which(by_mirna$least_one == F)
rm <- rownames(by_mirna)[rm]
cts <- cts[!(rownames(cts) %in% rm),]
by_mirna <- by_mirna[!(rownames(by_mirna) %in% rm),]

keep <- which(by_mirna$fifty == T)
keep <- rownames(by_mirna)[keep]

# cts <- cts[keep,]
# by_mirna <- by_mirna[keep,]
## Annotation

mirna_annotation <- as.data.frame(matrix(NA, ncol = 1, nrow = length(rownames(cts))))
rownames(mirna_annotation) <- rownames(cts)
colnames(mirna_annotation) <- c("n_samples")
samples_annotation <- as.data.frame(matrix(NA, ncol = 1, nrow = length(rownames(df))))
colnames(samples_annotation) <- c("n_mirna")
rownames(samples_annotation) <- rownames(df)

mirna <- read.delim("C:/Users/amedeo/Desktop/Filexame.csv", header = F)
colnames(mirna) <- c("mirna")
mirna$check <- TRUE

mirna_annotation$n_samples <- by_mirna$n_subjects
samples_annotation$n_mirna <- by_samples$Total_miRNA

mirna_annotation[,"color"] <- mirna[match(rownames(mirna_annotation), mirna$mirna, nomatch = NA),"check"]
mirna_annotation[,"check"] <- ifelse(is.na(mirna_annotation$color), F, T)



mat <- as.matrix(log10(cts+1))

column_ha <- HeatmapAnnotation(miRNAs = anno_barplot(samples_annotation$n_mirna,
                                                     gp = gpar(fill = "black", col = "black")), annotation_label = c(""))
row_ha <- rowAnnotation(barplot = anno_barplot(mirna_annotation$n_samples, 
                                                   gp = gpar(col = ifelse(mirna_annotation$check == T, "red", "grey"),
                                                             fill = ifelse(mirna_annotation$check == T, "red", "grey")),
                                               bar_width = 1), annotation_label = c(""))

colcor <- colorRampPalette(brewer.pal(9,"Blues"))(100)

Heatmap(mat, 
        name = "mat", 
        top_annotation = column_ha,
        right_annotation = row_ha,
        column_title = c("Numbers of detected miRNAs"),
        row_title = c("Numbers of samples for each miRNA"),
        row_title_side = "right",
        show_row_names = F,
        show_column_names = F,
        show_row_dend = F,
        show_column_dend = F,
        col=colorRamp2(breaks=c(0,2,4,6), colors=c("white", colcor[50], colcor[75], colcor[100])),
        heatmap_legend_param = list(title = "log10 \n", title_position = "topcenter"))
