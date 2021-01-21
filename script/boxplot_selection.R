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

# Df mods

levels(df$bmi_cat)
df$bmi_cat <- factor(df$bmi_cat, levels = c("underweight", "normal", "overweight", "obese"))
levels(df$smoke)
levels(df$cigs)
df$smoke <- factor(df$smoke, levels = c("never", "former", "current"))
df$cigs <- factor(df$cigs, levels = c("never", "former", "_16", "16_"))
levels(df$coffee)
df$coffee <- factor(df$coffee, levels = c("no_coffee", "light_drinker", "heavy_drinker"))
df$phys_act <- as.factor(df$phys_act)
levels(df$phys_act)
df$phys_act <- factor(df$phys_act, levels = c("Inactive", "Moderately inactive", "Moderately active", "Active"))
levels(df$phys_act) <- c("Inactive", "Moderately inactive", "Moderately active", "Moderately active")
levels(df$alcool_ua)

colnames(box_mir)[3] <- "menstruation"
colnames(box_mir)[4] <- "bmi_cat"
colnames(box_mir)[6] <- "cigs"
colnames(box_mir)[8] <- "phys_act"
colnames(box_mir)[9] <- "alcool_ua"

# Boxplot

## Data
t.cts <- as.data.frame(t(cts))
colnames(t.cts) <- str_remove_all(colnames(t.cts), ":Novel")

## Legend
sex_color <- brewer.pal(3, "Set1")[1:2]
age_color <- brewer.pal(3, "Blues") 
meno_color <- brewer.pal(3, "Pastel2")[1:2]
bmi_color <- brewer.pal(4, "Greens")
smoke_color <- brewer.pal(3, "OrRd")
cigs_color <- brewer.pal(4, "OrRd")
coffee_color <- brewer.pal(10, "BrBG")[c(2,3,5)]
phys_color <- brewer.pal(3, "YlOrRd")
alcohol_color <- brewer.pal(3, "PuRd")

var <- c("alcool_ua")
name <- c("Alcohol consumption")
labels <- c("Abstemious", "Moderate", "Heavy")
tmp <- length(box_mir[,var]) - sum(is.na(box_mir[,var]))

for(i in 1:tmp){
  mirX <- box_mir[i,var]
  xx <- which(colnames(t.cts) == mirX)
  db[,"mirna"] <- n.cts[,xx]
  
  title <- mirX
  
  p <- ggplot(db, aes(x=alcool_ua, y=log(mirna), fill=alcool_ua)) +
  geom_boxplot(outlier.shape = 1) +
  geom_jitter(width = 0.1, size = 1) +
  stat_summary(fun = median, geom = "smooth", aes(group = 1), color = "red") +
  scale_fill_manual(name = name, labels = labels, values = alcohol_color) +
  ylab(bquote(~Log[10]~ 'expression level')) +
  theme_classic() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  ggtitle(title)
  
  mirX <- str_replace(mirX, ":Novel", "-Novel")
  ggsave(p, filename = paste0("C:/Users/amedeo/Desktop/R_Projects/stool/results/figures/boxplot/final/", mirX, ".png"))
}

## Menopausal
db <- df
xx <- which(is.na(db$menstruation))
db <- db[-xx,]
k <- intersect(rownames(db), rownames(t.cts))
n.cts <- t.cts[k,]
db <- db[k,]
all.equal(rownames(db), rownames(n.cts))

## BMI
xx <- which(is.na(df$bmi_cat))
db <- df
db <- db[-xx,]
k <- intersect(rownames(db), rownames(t.cts))
n.cts <- t.cts[k,]
db <- db[k,]
all.equal(rownames(db), rownames(n.cts))

## Smoke
xx <- which(is.na(df$smoke))
db <- df
db <- db[-xx,]
k <- intersect(rownames(db), rownames(t.cts))
n.cts <- t.cts[k,]
db <- db[k,]
all.equal(rownames(db), rownames(n.cts))

## Cigs
xx <- which(is.na(df$cigs))
db <- df
db <- db[-xx,]
k <- intersect(rownames(db), rownames(t.cts))
n.cts <- t.cts[k,]
db <- db[k,]
all.equal(rownames(db), rownames(n.cts))

## P
xx <- which(is.na(df$phys_act))
db <- df
db <- db[-xx,]
k <- intersect(rownames(db), rownames(t.cts))
n.cts <- t.cts[k,]
db <- db[k,]
all.equal(rownames(db), rownames(n.cts))
