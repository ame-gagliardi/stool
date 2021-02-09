source("C:/Users/amedeo/Desktop/R_Projects/general_script/libraries_functions.R")
source("C:/Users/amedeo/Desktop/R_Projects/general_script/libraries_graph.R")


lfc <- read.table("C:/Users/amedeo/Desktop/df_lfc.csv", sep =";", row.names = 1, header = TRUE)
lfc[,25:28] <- NULL
colnames(lfc)[24] <- "bmi3"
lfc[,"bmi3"] <- lfc$bmi3*(-1)
mirna <- read.delim("data/sig_mirna_list_both.txt", sep ="\t")
mirna <- mirna[!duplicated(mirna$ID),]
  
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

# rownames(lfc) <- str_remove_all(rownames(lfc), "hsa-")
# rownames(lfc) <- str_remove_all(rownames(lfc), ":Novel")

# # Antropometric LFC
# 
# antro1 <- grep("Age", colnames(lfc))
# antro2 <- grep("Sex", colnames(lfc))
# antro <- c(antro1, antro2)
# lfc <- lfc[,antro]
# 
# # Lifestyle LFC
# 
# lfc <- lfc[,-antro]


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

Heatmap(mat,
        column_names_rot = 30,
        column_names_side = "top",
        column_dend_side = "bottom",
        column_names_gp = gpar(fontsize = 10),
        row_labels = ifelse(str_detect(rn$V1, ":Novel"), str_remove_all(rn$V1, ":Novel"), rn$V1),
        row_names_gp = gpar(fontsize = 7, fontface = ifelse(rn$V2 == TRUE, "italic", "plain")),
        name = "Log2FC"
        )


