source("D:/R_Projects/general/libraries_functions.R")
source("D:/R_Projects/general/libraries_graph.R")


## Heatmap data ##
## Creo un dds per ogni variabile con tutti i mirna e i corrispetivi lfc##

var <- c("sex")

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

saveRDS(A_1, file = paste0("data/downstream/lfcHM_", var,".dds"))
rm(list=ls())

## Carico tutti i dds in modo da avere una matrice con tutti gli lfc per ogni confronto ##

fileNames <- list.files("data/downstream/")
fileNames <- fileNames[grep("lfcHM", fileNames)]

for(i in 1:length(fileNames)){
        nam <- paste0("A_", i)
        assign(nam,
               readRDS(paste0("D:/R_Projects/stool/data/downstream/", fileNames[i])))
}

lfc <- cbind(A_1, A_2, A_3, A_4, A_5, A_6, A_7, A_8)
rownames(lfc) <- lfc$ID
torm <- grep("ID", colnames(lfc))
lfc <- lfc[,-torm]

## Filtro per tenere solamente i miRNA significativi (non ripetuti)

filter <- read.delim("data/downstream/hm_filter.txt", sep ="\t", header = F)
colnames(filter) <- c("mirna", "covariate")
lfc <- lfc[rownames(lfc) %in% filter$mirna,]


################ Heatmap 1 ################

# Rename delle colonne #

colnames(lfc) <- c("Age[>53 vs <37]", "Age[>53 vs 37-53", "Age[37-53 vs <37",
                   "Alcohol[High vs No]", "Alcohol[Low vs No]",
                   "BMI[Normal vs Underweight", "BMI[Obese vs Normal]", "BMI[Overweight vs Normal]",
                   "Smoke[<16 cigs/day vs Former", "Smoke[<16 cigs/day vs never]", "Smoke[>16 cigs/day vs <16 cigs/day]", 
                   "Smoke[>16 cigs/day vs Former", "Smoke[>16 cigs/day vs Never","Smoke[Former vs Never]",
                   "Coffee[High vs No]", "Coffee[Low vs No]", "Menopausal[Yes vs No]", 
                   "Physical activity[Inactive vs Active]", "Physical activity[Moderately Active vs Active]", "Physical Activity[Moderately Inactive vs Active]",
                   "Sex[Male vs Female")

# Heatmap ##

mat <- as.matrix(lfc)
cn = colnames(mat)
rn = as.data.frame(matrix(NA, nrow = length(rownames(mat)), ncol = 2))
rn[,1] <- rownames(mat)
rn[,2] <- str_detect(rn$V1, ":Novel")
rn$V1 <- str_remove_all(rn$V1, "hsa-")

colcor <- rev(colorRampPalette(brewer.pal(11, "RdBu"))(256))

anno_df <- data.frame(var=colnames(mat), Covariate=NA, Class=NA)
anno_df[,"Covariate"] <- c("Age", "Age", "Age", "Alcohol", "Alcohol", "BMI", "BMI", "BMI", "Smoke", "Smoke", "Smoke", "Smoke",
                           "Smoke", "Smoke", "Coffee", "Coffee", "Menopausal", "Physical Activity", "Physical Activity", 
                           "Physical Activity", "Sex")

anno_df[,"Class"] <-  c("Antropometric", "Antropometric", "Antropometric", "Lifestyle", "Lifestyle", "Antropometric", "Antropometric",
                        "Antropometric", "Lifestyle", "Lifestyle", "Lifestyle", "Lifestyle", "Lifestyle", "Lifestyle", "Lifestyle",
                        "Lifestyle", "Antropometric", "Lifestyle", "Lifestyle", "Lifestyle", "Antropometric")

col_ha <- HeatmapAnnotation(df = anno_df,
                            col = list(Covariate = c("Age" = "#3182BD", 
                                                     "Smoke" = "#969696", 
                                                     "Alcohol" = "#DD1C77",
                                                     "Coffee" = "#F6E8C3", 
                                                     "Physical Activity" = "#C2A5CF", 
                                                     "BMI" = "#238B45", 
                                                     "Sex" = "#E41A1C",
                                                     "Menopausal" = "#FF99FF"),
                                       
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
        top_annotation = col_ha[,c(2,3)])


#