source("C:/Users/UappaSguappa/Desktop/R_projects/general_script/libraries_functions.R")
source("C:/Users/UappaSguappa/Desktop/R_projects/general_script/libraries_graph.R")


## Heatmap data ##
## Creo un dds per ogni variabile con tutti i mirna e i corrispetivi lfc##

var <- c("phys_act")

fileName <- list.files(paste0("results/differential/tables/pooled/",var))
fileName <- fileName[grep("both", fileName)]

for(i in 1:length(fileName)){
        nam <- paste0("A_", i)
        assign(nam,
               read.delim(paste0("D:/R_Projects/stool/results/differential/tables/pooled/", var, "/", fileName[i])))
}


A_1 <- A_1 %>% 
        dplyr::select(ID, log2FoldChange, comparison) %>% 
        dplyr::mutate(comparison = as.factor(comparison)) 
colnames(A_1)[2] <- levels(A_1$comparison)
A_1 <- A_1[,c(1,2)]

A_2 <- A_2 %>% 
        dplyr::select(ID, log2FoldChange, comparison) %>% 
        dplyr::mutate(comparison = as.factor(comparison)) 
colnames(A_2)[2] <- levels(A_2$comparison)
A_2 <- A_2[,c(1,2)]

A_3 <- A_3 %>% 
        dplyr::select(ID, log2FoldChange, comparison) %>% 
        dplyr::mutate(comparison = as.factor(comparison)) 
colnames(A_3)[2] <- levels(A_3$comparison)
A_3 <- A_3[,c(1,2)]

A <- cbind(A_1, A_2, A_3)

saveRDS(A, file = paste0("data/downstream/lfcHM_", var,".dds"))
rm(list=ls())

## Carico tutti i dds in modo da avere una matrice con tutti gli lfc per ogni confronto ##

fileNames <- list.files("data/downstream/")
fileNames <- fileNames[grep("LFC", fileNames)]

for(i in 1:length(fileNames)){
        nam <- paste0("A_", i)
        assign(nam,
               readRDS(paste0("D:/R_Projects/stool/data/downstream/", fileNames[i])))
}

lfc <- cbind(A_1, A_2, A_3, A_4, A_5, A_6, A_7)
rownames(lfc) <- lfc$ID
torm <- grep("ID", colnames(lfc))
lfc <- lfc[,-torm]
lfc <- lfc %>% mutate_if(is.numeric, ~round(.,3))

saveRDS(lfc, file = "data/downstream/lfc_dataset.dds")

## Filtro per tenere solamente i miRNA significativi (non ripetuti)
lfc <- readRDS("data/downstream/heatmap_LFC_dataset.dds")

filter <- read.delim("data/downstream/heatmap_LFC_filter_mirna.txt", sep ="\n", header = F)
colnames(filter) <- "miRNA"
filter$miRNA <- trimws(filter$miRNA)
tokeep <- filter %>% dplyr::distinct(miRNA)
tokeep$miRNA[!(tokeep$miRNA %in% rownames(lfc))] <- paste0(tokeep$miRNA[!(tokeep$miRNA %in% rownames(lfc))], ":Novel")
tokeep <- tokeep %>% dplyr::distinct(miRNA)
rownames(tokeep) <- tokeep$miRNA

lfc <- lfc[rownames(lfc) %in% rownames(tokeep),]

# ## Filtro per tenere i miRNA significativi in almeno due confronti
# lfc <- readRDS("data/downstream/heatmap_LFC_dataset.dds")
# filter <- read.delim("data/downstream/heatmap_LFC_filter_mirna.txt", sep =";", header = F)
# colnames(filter) <- "miRNA"
# filter$miRNA <- trimws(filter$miRNA)
# 
# # mirna_rep <- data.frame(table(filter$miRNA))
# # mirna_rep <- mirna_rep[mirna_rep$Freq>1,]
# # colnames(mirna_rep) <- c("mirna", "freq")
# # rownames(mirna_rep) <- mirna_rep$mirna
# 
# rownames(mirna_rep)[!(rownames(mirna_rep) %in% rownames(lfc))] <- paste0(rownames(mirna_rep)[!(rownames(mirna_rep) %in% rownames(lfc))], ":Novel")
# lfc <- lfc[rownames(lfc) %in% rownames(mirna_rep),]

lfc$normal_vs_underweight <- lfc$normal_vs_underweight * -1

###########################################
################ Heatmap 1 ################
###########################################

# Rename delle colonne #

colnames(lfc) <- c("Age [>53 vs <37]", "Age [>53 vs 37-53]", "Age [37-53 vs <37]",
                   "Alcohol [High vs No]", "Alcohol [Low vs No]",
                   "BMI [Underweight vs Normal]", "BMI [Obese vs Normal]", "BMI [Overweight vs Normal]",
                   "Smoke [<16 cigs/day vs Never]", "Smoke [>16 cigs/day vs never]", "Smoke [Former vs Never]", 
                   "Coffee [High vs No]", "Coffee [Low vs No]", 
                   "Physical activity [Inactive vs Active]", "Physical activity [Moderately Active vs Active]", "Physical Activity [Moderately Inactive vs Active]",
                   "Sex [Male vs Female]")

# Heatmap ##

mat <- as.matrix(lfc)
cn = colnames(mat)
rn = as.data.frame(matrix(NA, nrow = length(rownames(mat)), ncol = 2))
rn[,1] <- rownames(mat)
rn[,2] <- str_detect(rn$V1, ":Novel")
rn$V1 <- str_remove_all(rn$V1, "hsa-")

colcor <- rev(colorRampPalette(brewer.pal(11, "RdBu"))(256))

anno_df <- data.frame(var=colnames(mat), Covariate=NA, Class=NA)
anno_df[,"Covariate"] <- c("Age", "Age", "Age", "Alcohol", "Alcohol", "BMI", "BMI", "BMI", "Smoke", "Smoke", "Smoke",
                           "Coffee", "Coffee", "Physical Activity", "Physical Activity", "Physical Activity", "Sex")

anno_df[,"Class"] <-  c("Antropometric", "Antropometric", "Antropometric", "Lifestyle", "Lifestyle", "Antropometric", "Antropometric",
                        "Antropometric", "Lifestyle", "Lifestyle", "Lifestyle", "Lifestyle",
                        "Lifestyle", "Lifestyle", "Lifestyle", "Lifestyle", "Antropometric")

col_ha <- HeatmapAnnotation(df = anno_df,
                            col = list(Covariate = c("Age" = "#3182BD", 
                                                     "Smoke" = "#969696", 
                                                     "Alcohol" = "#DD1C77",
                                                     "Coffee" = "#F6E8C3", 
                                                     "Physical Activity" = "#C2A5CF", 
                                                     "BMI" = "#238B45", 
                                                     "Sex" = "#E41A1C"),
                                       
                                       Class = c("Antropometric" = "#f0fc03", "Lifestyle" = "#1c03fc")))

hm1 <- Heatmap(mat,
        column_names_rot = 30,
        column_names_side = "top",
        column_dend_side = "bottom",
        column_names_gp = gpar(fontsize = 11),
        row_labels = ifelse(str_detect(rn$V1, ":Novel"), str_remove_all(rn$V1, ":Novel"), rn$V1),
        row_names_gp = gpar(fontsize = 10, fontface = ifelse(rn$V2 == TRUE, "italic", "plain")),
        name = "Log2FC",
        col=colorRamp2(breaks=c(-4,-2,0,2,4), colors=c(colcor[1], colcor[64], "white", colcor[192], colcor[256])), 
        top_annotation = col_ha[,c(2,3)],
        show_row_dend = F)



