source("C:/Users/amedeo/Desktop/R_Projects/general_script/libraries_functions.R")
source("C:/Users/amedeo/Desktop/R_Projects/general_script/libraries_graph.R")


lfc <- read.table("C:/Users/amedeo/Desktop/df_lfc.csv", sep =";", row.names = 1, header = TRUE)
lfc[,25:28] <- NULL
colnames(lfc)[24] <- "bmi3"
lfc[,"bmi3"] <- lfc$bmi3*(-1)
mirna <- read.delim("data/hm_filter.txt", sep ="\t", header = FALSE)
colnames(mirna) <- c("ID", "Var")


################ Heatmap 1 ################

mirna[,"dup"] <- duplicated(mirna$ID)  
mirna <- mirna[which(mirna$dup == TRUE),]

lfc$ID <- rownames(lfc)
lfc$ID <- str_remove_all(lfc$ID, ":Novel")

match <- lfc$ID %in% mirna$ID
lfc <- lfc[match,]

mirna[,3:10] <- c("age","sex","smoke","cigs","alcohol","coffee","physact","bmi")
colnames(mirna)[3:10] <- c("age","sex","smoke","cigs","alcohol","coffee","physact","bmi")
mirna[,3:10] <- NA

lfc$menopausal <- NULL
lfc$ID <- NULL
colnames(lfc) <- c("Age [37-53 vs <18 years]", "Age [>53 vs <18 years]", "Age [>53 vs 37-53 years]", 
                   "Sex [Male vs Female]",
                   "Smoking habits [Former vs Never]", "Smoking habits [Current vs Never]", "Smoking habits [Current vs Former]",
                   "Smoking intensity [>16 vs <16 cigs/day]", "Smoking intensity [<16 vs 0 cigs/day]", "Smoking intensity [>16 vs 0 cigs/day]",
                   "Alcohol intake [Low vs None]", "Alcohol intake [High vs None]", "Alcohol intake [High vs Low]",
                   "Coffee intake [Low vs None]", "Coffee intake [High vs None]", "Coffee intake [High vs Low]", "Coffee intake [Drinker vs None]",
                   "Physical activity [Inactive vs Mod. active]", "Physical activity [Mod. inactive vs Mod. active]", "Physical activity [Inactive vs Mod. inactive]",
                   "BMI [Obese vs Normal]", "BMI [Overweight vs Normal]", "BMI [Normal vs Underweight]")



## Heatmap
colnames(lfc)
mat <- as.matrix(lfc)
mat <- t(mat)
#mat <- scale(mat)
mat <- t(mat)

cn = colnames(mat)
rn = as.data.frame(matrix(NA, nrow = length(rownames(mat)), ncol = 2))
rn[,1] <- rownames(mat)
rn[,2] <- str_detect(rn$V1, ":Novel")
rn$V1 <- str_remove_all(rn$V1, "hsa-")

colcor <- rev(colorRampPalette(brewer.pal(11, "RdBu"))(256))

anno_df <- data.frame(var=colnames(mat), Covariate=NA, Class=NA)
anno_df[,"Covariate"] <- c("Age", "Age", "Age", "Sex", "Smoke", "Smoke", "Smoke", "Smoke", "Smoke", "Smoke", "Alcohol", "Alcohol", "Alcohol", 
                       "Coffee", "Coffee" , "Coffee", "Coffee", "Physical activity", "Physical activity", "Physical activity", "BMI", "BMI", "BMI")
anno_df[,"Class"] <-  c("Antropometric", "Antropometric", "Antropometric", "Antropometric", "Lifestyle", "Lifestyle","Lifestyle","Lifestyle","Lifestyle",
                        "Lifestyle", "Lifestyle","Lifestyle","Lifestyle","Lifestyle","Lifestyle","Lifestyle","Lifestyle","Lifestyle","Lifestyle","Lifestyle",
                        "Antropometric","Antropometric","Antropometric")
        
col_ha <- HeatmapAnnotation(df = anno_df,
                            col = list(Covariate = c("Age" = "#3182BD", "Smoke" = "#969696", "Alcohol" = "#DD1C77",
                                                     "Coffee" = "#F6E8C3", "Physical activity" = "#C2A5CF", "BMI" = "#238B45", "Sex" = "#E41A1C"),
                                       Class = c("Antropometric" = "#f0fc03", "Lifestyle" = "#1c03fc")))

hm1 <- Heatmap(mat,
        column_names_rot = 30,
        column_names_side = "top",
        column_dend_side = "bottom",
        column_names_gp = gpar(fontsize = 11),
        row_labels = ifelse(str_detect(rn$V1, ":Novel"), str_remove_all(rn$V1, ":Novel"), rn$V1),
        row_names_gp = gpar(fontsize = 15, fontface = ifelse(rn$V2 == TRUE, "italic", "plain")),
        name = "Log2FC",
        col=colorRamp2(breaks=c(-4,-2,0,2,4), colors=c(colcor[1], colcor[64], "white", colcor[192], colcor[256])), 
        top_annotation = col_ha[,c(2,3)])


################ Heatmap 2 ################

lfc <- read.table("C:/Users/amedeo/Desktop/df_lfc.csv", sep =";", row.names = 1, header = TRUE)
lfc[,25:28] <- NULL
colnames(lfc)[24] <- "bmi3"
lfc[,"bmi3"] <- lfc$bmi3*(-1)
mirna <- read.delim("data/hm_filter.txt", sep ="\t", header = FALSE)
colnames(mirna) <- c("ID", "Var")

lfc$ID <- rownames(lfc)
lfc$ID <- str_remove_all(lfc$ID, ":Novel")

match <- lfc$ID %in% mirna$ID
lfc <- lfc[match,]

mirna[,3:10] <- c("age","sex","smoke","cigs","alcohol","coffee","physact","bmi")
colnames(mirna)[3:10] <- c("age","sex","smoke","cigs","alcohol","coffee","physact","bmi")
mirna[,3:10] <- NA

lfc$menopausal <- NULL
lfc$ID <- NULL
colnames(lfc) <- c("Age [37-53 vs <18 years]", "Age [>53 vs <18 years]", "Age [>53 vs 37-53 years]", 
                   "Sex [Male vs Female]",
                   "Smoking habits [Former vs Never]", "Smoking habits [Current vs Never]", "Smoking habits [Current vs Former]",
                   "Smoking intensity [>16 vs <16 cigs/day]", "Smoking intensity [<16 vs 0 cigs/day]", "Smoking intensity [>16 vs 0 cigs/day]",
                   "Alcohol intake [Low vs None]", "Alcohol intake [High vs None]", "Alcohol intake [High vs Low]",
                   "Coffee intake [Low vs None]", "Coffee intake [High vs None]", "Coffee intake [High vs Low]", "Coffee intake [Drinker vs None]",
                   "Physical activity [Inactive vs Mod. active]", "Physical activity [Mod. inactive vs Mod. active]", "Physical activity [Inactive vs Mod. inactive]",
                   "BMI [Obese vs Normal]", "BMI [Overweight vs Normal]", "BMI [Normal vs Underweight]")

## Heatmap
colnames(lfc)
mat <- as.matrix(lfc)
mat <- t(mat)
#mat <- scale(mat)
mat <- t(mat)

cn = colnames(mat)
rn = as.data.frame(matrix(NA, nrow = length(rownames(mat)), ncol = 2))
rn[,1] <- rownames(mat)
rn[,2] <- str_detect(rn$V1, ":Novel")
rn$V1 <- str_remove_all(rn$V1, "hsa-")

colcor <- rev(colorRampPalette(brewer.pal(11, "RdBu"))(256))

anno_df <- data.frame(var=colnames(mat), Covariate=NA, Class=NA)
anno_df[,"Covariate"] <- c("Age", "Age", "Age", "Sex", "Smoke", "Smoke", "Smoke", "Smoke", "Smoke", "Smoke", "Alcohol", "Alcohol", "Alcohol", 
                           "Coffee", "Coffee" , "Coffee", "Coffee", "Physical activity", "Physical activity", "Physical activity", "BMI", "BMI", "BMI")
anno_df[,"Class"] <-  c("Antropometric", "Antropometric", "Antropometric", "Antropometric", "Lifestyle", "Lifestyle","Lifestyle","Lifestyle","Lifestyle",
                        "Lifestyle", "Lifestyle","Lifestyle","Lifestyle","Lifestyle","Lifestyle","Lifestyle","Lifestyle","Lifestyle","Lifestyle","Lifestyle",
                        "Antropometric","Antropometric","Antropometric")

col_ha <- HeatmapAnnotation(df = anno_df,
                            col = list(Covariate = c("Age" = "#3182BD", "Smoke" = "#969696", "Alcohol" = "#DD1C77",
                                                     "Coffee" = "#F6E8C3", "Physical activity" = "#C2A5CF", "BMI" = "#238B45", "Sex" = "#E41A1C"),
                                       Class = c("Antropometric" = "#f0fc03", "Lifestyle" = "#1c03fc")))

hm2 <- Heatmap(mat,
        column_names_rot = 30,
        column_names_side = "top",
        column_dend_side = "bottom",
        column_names_gp = gpar(fontsize = 10),
        row_labels = ifelse(str_detect(rn$V1, ":Novel"), str_remove_all(rn$V1, ":Novel"), rn$V1),
        row_names_gp = gpar(fontsize = 8, fontface = ifelse(rn$V2 == TRUE, "italic", "plain")),
        name = "Log2FC",
        col=colorRamp2(breaks=c(-4,-2,0,2,4), colors=c(colcor[1], colcor[64], "white", colcor[192], colcor[256])), top_annotation = col_ha[,c(2,3)])


png("hm_comb.png",width = 30, height = 40, units="cm", res = 500)
htlist <- hm2 %v% hm1
draw(htlist)
dev.off()

png("hm_full.png",width = 30, height = 40, units="cm", res = 500)
hm2
dev.off()

png("hm_sub.png",width = 30, height = 40, units="cm", res = 500)
hm1
dev.off()
