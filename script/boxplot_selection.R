source("C:/Users/amedeo/Desktop/R_Projects/general_script/libraries_functions.R")
source("C:/Users/amedeo/Desktop/R_Projects/general_script/libraries_graph.R")

# DATA LOADING

r_folder <- c("C:/Users/amedeo/Desktop/R_Projects/stool/") 
folder <- c("data/")
project <- c("sv_")
biospecimen <- c("stool_")
sex <- c("both_")
ctsType <- c("normalized_")

df.path <- paste0(r_folder, folder, "clinical/", project, biospecimen, sex, "samples.rds")
cts.path <- paste0(r_folder, folder, "ngs/", project, biospecimen, "both_",ctsType, "counts.rds")

df <- readRDS(df.path)
cts <- readRDS(cts.path)

i <- intersect(rownames(df), colnames(cts))
df <- df[i,]
cts <- cts[,i]
all.equal(rownames(df), colnames(cts))

# External data

box_mir <- read.delim("C:/Users/amedeo/Desktop/R_Projects/stool/data/mirna_box.txt", quote="")

# Boxplot

## Data
t.cts <- as.data.frame(t(cts))
## Legend

color <- brewer.pal(9, "BuGn")[4:8]
var <- c("bmi")
name <- c("BMI")
labels <- c("Normal", "Obese", "Overweight", "Underweight")
tmp <- length(box_mir[,var]) - sum(is.na(box_mir[,var]))

for(i in 1:tmp){
  mirX <- box_mir[i,var]
  xx <- which(colnames(t.cts) == mirX)
  df[,"mirna"] <- t.cts[,xx]
  
  title <- mirX
  
  p <- ggplot(df, aes(x=bmi_cat, y=log(mirna), fill=bmi_cat)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 2) +
  geom_jitter(width = 0.1, size = 1) +
  stat_summary(fun = median, geom = "smooth", aes(group = 1), color = "red") +
  scale_fill_manual(name = name, labels = labels, values = color) +
  ylab(bquote(~Log[10]~ 'expression level')) +
  theme_classic() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  ggtitle(title)
  
  mirX <- str_replace(mirX, ":Novel", "-Novel")
  ggsave(p, filename = paste0("C:/Users/amedeo/Desktop/R_Projects/stool/results/figures/boxplot/final/", mirX, ".png"))
}


