source("D:/R_Projects/general/libraries_functions.R")
source("D:/R_Projects/general/libraries_graph.R")


# DATA LOADING

r_folder <- getwd()
folder <- c("/data/")
project <- c("/sv_") 
tissue <- c("stool_")
biospecimen <- c("stool_")
sex <- c("both_")
sex_cts <- c("both_")
ctsType <- c("normalized_")
species <- c("mirna_")
cohort <- c("pooled")

df.path <- paste0(r_folder, folder, "clinical", project, tissue, biospecimen, sex, "samples_", cohort, ".rds")
cts.path <- paste0(r_folder, folder, "ngs", project, tissue, biospecimen, sex_cts ,ctsType, "counts_", species, cohort, ".rds")
result.path <- paste0(r_folder, "/results/differential")

df <- readRDS(df.path)
cts <- readRDS(cts.path)


i <- intersect(rownames(df), colnames(cts))
cts <- cts[,i]
df <- df[i,]
all.equal(rownames(df), colnames(cts))

# External data

box_mir <- read.delim("data/downstream/boxplot_mirna_to_select.csv", sep = ";")


# Boxplot

## Data
t.cts <- as.data.frame(t(cts))
# colnames(t.cts) <- str_remove_all(colnames(t.cts), ":Novel")

## Legend
sex_color <- brewer.pal(3, "Set1")[1:2]
age_color <- brewer.pal(3, "Blues") 
meno_color <- brewer.pal(3, "Pastel2")[1:2]
bmi_color <- brewer.pal(4, "Greens")
smoke_color <- brewer.pal(3, "OrRd")
cigs_color <- brewer.pal(4, "OrRd")
coffee_color <- brewer.pal(10, "BrBG")[c(2,3,5)]
phys_color <- brewer.pal(4, "YlOrRd")
alcohol_color <- brewer.pal(3, "PuRd")
##
var <- c("phys_act")
name <- c("Physical activity")
labels <- levels(df[,var])
tmp <- length(box_mir[,var]) - sum(is.na(box_mir[,var]))
#
db <- df
xx <- which(is.na(db[,var]))
if(length(xx) > 0){
  db <- db[-xx,]
  k <- intersect(rownames(db), rownames(t.cts))
  n.cts <- t.cts[k,]
  db <- db[k,]
  all.equal(rownames(db), rownames(n.cts))
}else{
  k <- intersect(rownames(db), rownames(t.cts))
  n.cts <- t.cts[k,]
  db <- db[k,]
}
all.equal(rownames(db), rownames(n.cts))
#

for(i in 1:tmp){
  mirX <- box_mir[i,var]
  xx <- which(colnames(t.cts) == mirX)
  db[,"mirna"] <- n.cts[,xx]
  
  title <- mirX
  
  p <- ggplot(db, aes(x=phys_act, y=log(mirna), fill=phys_act)) +
  geom_boxplot(outlier.shape = 1) +
  geom_jitter(width = 0.1, size = 1) +
  stat_summary(fun = median, geom = "smooth", aes(group = 1), color = "red") +
  scale_fill_manual(name = name, labels = labels, values = phys_color) +
  ylab(bquote(~Log[10]~ 'expression level')) +
  theme_classic() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  ggtitle(title)
  
  mirX <- str_replace(mirX, ":Novel", "-Novel")
  ggsave(p, filename = paste0("D:/R_Projects/stool/results/figures/pooled/to_select/",var,"/", mirX, ".png"))
}


