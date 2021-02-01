source("C:/Users/amedeo/Desktop/R_Projects/general_script/libraries_functions.R")
source("C:/Users/amedeo/Desktop/R_Projects/general_script/libraries_graph.R")


lfc <- read.table("C:/Users/amedeo/Desktop/df_lfc.csv", sep =";", row.names = 1, header = TRUE)
lfc[,25:28] <- NULL
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
colnames(lfc) <- c("Age [37-53 vs <18]", "Age [>53 vs<18]", "Age [>53 vs 37-53]", 
                   "Sex",
                   "Smoking status [Former vs Never]", "Smoking status [Current vs Never", "Smoking status [Current vs Former",
                   "Cigs/day [>16 vs <16]", "Cigs/day [<16 vs Never]", "Cigs/day [>16 vs Never]",
                   "Alcohol intake [Low vs Non drinker", "Alcohol intake [High vs Non drinker", "Alcohol intake [High vs Low",
                   "Coffee intake [Low vs Non drinker", "Coffee intake [High vs Non drinker", "Coffee intake [High vs Low", "Coffee intake [Drinker vs Non drinker",
                   "Physical activity [Inactive vs Moderately active", "Physical activity [Moderately inactive vs Moderately active", "Physical activity [Inactive vs Moderately inactive",
                   "BMI [Obese vs Normal]", "BMI [Overweight vs Normal]", "BMI [Underweight vs Normal")

# Antropometric LFC

antro1 <- grep("Age", colnames(lfc))
antro2 <- grep("Sex", colnames(lfc))
antro <- c(antro1, antro2)
lfc <- lfc[,antro]

# Lifestyle LFC

lfc <- lfc[,-antro]


## Heatmap
colnames(lfc)
mat <- as.matrix(lfc)
mat <- t(mat)
#mat <- scale(mat)
mat <- t(mat)

cn = colnames(mat)
Heatmap(mat, 
        show_column_names = FALSE, 
        row_names_gp = gpar(fontsize = 5),
        column_names_gp  = gpar(fontsize = 3),
        bottom_annotation = HeatmapAnnotation(
          text = anno_text(cn, rot = 45, location = unit(1, "npc"), just = "right"),
          annotation_height = max_text_width(cn)
        )
)

