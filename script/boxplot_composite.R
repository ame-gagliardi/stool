source("C:/Users/amedeo/Desktop/R_Projects/general_script/libraries_functions.R")
source("C:/Users/amedeo/Desktop/R_Projects/general_script/libraries_graph.R")
library(memisc)

## BOXPLOT ARRANGE

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

# miRNA selection

mirna <- read.delim("data/facet_grid_boxplot.txt", sep ="\t")

mirna$psymb <- with(mirna, cases("*" = padj <0.05 & padj > 0.01,
                                 "**" = padj <0.01 & padj > 0.001,
                                 "***" = padj <0.001 & padj > 0.0001,
                                 "****" = padj <= 0.0001))

mirna$mirna <- str_replace_all(mirna$mirna, ":Novel", "-N")
mirna$mirna <- str_remove_all(mirna$mirna, "hsa-")
rownames(cts) <- str_replace_all(rownames(cts), ":Novel", "-N")
rownames(cts) <- str_remove_all(rownames(cts), "hsa-")

####################################################################################################################################################
####################################################################################################################################################
##############################################    ANTROPOMETRIC COMBINED PLOT    ###################################################################
####################################################################################################################################################
####################################################################################################################################################

#################
### AGE MIRNA ###
#################

age_color <- brewer.pal(3, "Blues") 
age_mirna <- mirna[mirna$var=="age",]
age_cts <- as.data.frame(t(cts[rownames(cts) %in% age_mirna$mirna,]))
all.equal(age_mirna$mirna, colnames(age_cts))

dt <- df
levels(dt$age_cat) <- c("<37", "37-53", ">53")
age_mirna$group1 <- as.factor(age_mirna$group1)
levels(age_mirna$group1) <- c("37-53", ">53")
age_mirna$group2 <- as.factor(age_mirna$group2)
levels(age_mirna$group2) <- c("<37")

legend <- c("Age")
labels <- c("<37", "37-53", ">53")


## hsa-miR-12127 ##

age1 <- age_mirna[1,1]
db <- dt
db[,age1] <- age_cts[,age1]
colnames(db)[24] <- "mirna"
data <- age_mirna[age_mirna$mirna == age1,]

p1 <- ggplot(db, aes(x = age_cat, y = log(mirna))) +
  geom_boxplot(aes(fill = age_cat), outlier.shape = 1) +
  geom_jitter(width = 0.1, size = 1, alpha = 0.5) +
  scale_fill_manual(name = "Age class \n  (years)", labels = labels, values = age_color) +
  ylab(bquote(~Log[10]~ 'expression levels')) +
  theme_classic() +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  ggtitle(str_remove(age1, "-N")) +
  stat_pvalue_manual(data, y.position = max(log(db$mirna)+0.3), step.increase = 0.1, tip.length = 0.01, label = "psymb")

## hsa-miR-4276-3p ##

age2 <- age_mirna[2,1]
db <- dt
db[,age2] <- age_cts[,age2]
colnames(db)[24] <- "mirna"
data <- age_mirna[age_mirna$mirna == age2,]
age2 <- str_remove(age2, "-N")
p2 <- ggplot(db, aes(x = age_cat, y = log(mirna))) +
  geom_boxplot(aes(fill = age_cat), outlier.shape = 1) +
  geom_jitter(width = 0.1, size = 1, alpha = 0.5) +
  scale_fill_manual(name = "Age class \n  (years)", labels = labels, values = age_color) +
  xlab("Age class") +
  theme_classic() +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  ggtitle(str_remove(age2, "-N")) +
  stat_pvalue_manual(data, y.position = max(log(db$mirna)+0.3), step.increase = 0.1, tip.length = 0.01, label = "psymb")

## hsa-miR-922 ##

age3 <- age_mirna[3,1]
db <- dt
db[,age3] <- age_cts[,age3]
colnames(db)[24] <- "mirna"
data <- age_mirna[age_mirna$mirna == age3,]

p3 <- ggplot(db, aes(x = age_cat, y = log(mirna))) +
  geom_boxplot(aes(fill = age_cat), outlier.shape = 1) +
  geom_jitter(width = 0.1, size = 1, alpha = 0.5) +
  scale_fill_manual(name = "Age class \n  (years)", labels = labels, values = age_color) +
  theme_classic() +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  ggtitle(str_remove(age3, "-N")) +
  stat_pvalue_manual(data, y.position = max(log(db$mirna)+0.3), step.increase = 0.1, tip.length = 0.01, label = "psymb")
#
pAge <- ggarrange(p3,p2,p1, nrow = 1, common.legend = TRUE, legend = "right")
pAge <- annotate_figure(pAge,
                        left = text_grob(bquote(~Log[10]~ '(expression levels)'), rot = 90, size = 8))

#################
### SEX MIRNA ###
#################

sex_color <- brewer.pal(3, "Set1")[1:2]
sex_mirna <- mirna[mirna$var=="sex",]
sex_cts <- as.data.frame(t(cts[rownames(cts) %in% sex_mirna$mirna,]))
all.equal(sex_mirna$mirna, colnames(sex_cts))

legend <- c("Sex")
labels <- c("Females", "Males")

##  hsa-miR-192-5p ##

sex1 <- sex_mirna[1,1]
db <- df
db[,sex1] <- sex_cts[,sex1]
colnames(db)[24] <- "mirna"
data <- sex_mirna[sex_mirna$mirna == sex1,]

p1 <- ggplot(db, aes(x = sex, y = log(mirna))) +
  geom_boxplot(aes(fill = sex), outlier.shape = 1) +
  geom_jitter(width = 0.1, size = 1, alpha = 0.5) +
  scale_fill_manual(name = "Sex", labels = labels, values = sex_color) +
  theme_classic() +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  ggtitle(str_remove(sex1, "-N")) +
  stat_pvalue_manual(data, y.position = max(log(db$mirna)+0.3), step.increase = 0.1, tip.length = 0.01, label = "psymb")

## hsa-miR-21-5p ##

sex2 <- sex_mirna[2,1]
db <- df
db[,sex2] <- sex_cts[,sex2]
colnames(db)[24] <- "mirna"
data <- sex_mirna[sex_mirna$mirna == sex2,]

p2 <- ggplot(db, aes(x = sex, y = log(mirna))) +
  geom_boxplot(aes(fill = sex), outlier.shape = 1) +
  geom_jitter(width = 0.1, size = 1, alpha = 0.5) +
  scale_fill_manual(name = "Sex", labels = labels, values = sex_color) +
  theme_classic() +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  ggtitle(str_remove(sex2, "-N")) +
  stat_pvalue_manual(data, y.position = max(log(db$mirna)+0.3), step.increase = 0.1, tip.length = 0.01, label = "psymb")

## hsa-miR-324-5p ##

sex3 <- sex_mirna[3,1]
db <- df
db[,sex3] <- sex_cts[,sex3]
colnames(db)[24] <- "mirna"
data <- sex_mirna[sex_mirna$mirna == sex3,]

p3 <- ggplot(db, aes(x = sex, y = log(mirna))) +
  geom_boxplot(aes(fill = sex), outlier.shape = 1) +
  geom_jitter(width = 0.1, size = 1, alpha = 0.5) +
  scale_fill_manual(name = "Sex", labels = labels, values = sex_color) +
  theme_classic() +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  ggtitle(str_remove(sex3, "-N")) +
  stat_pvalue_manual(data, y.position = max(log(db$mirna)+0.3), step.increase = 0.1, tip.length = 0.01, label = "psymb")

#
pSex <- ggarrange(p2,p1,p3, nrow = 1, common.legend = TRUE, legend = "right")
pSex <- annotate_figure(pSex,
                        left = text_grob(bquote(~Log[10]~ '(expression levels)'), rot = 90, size = 8))

rm(p1,p2,p3,sex_cts,sex_mirna,db,dt,sex_color,sex1,sex2,sex3)

########################
### MENOPAUSAL MIRNA ###
########################

menopausal_color_1 <- brewer.pal(10, "Set3")[6]
menopausal_color_2 <- brewer.pal(8, "Accent")[6]
menopausal_color <- c(menopausal_color_1, menopausal_color_2)
menopausal_mirna <- mirna[mirna$var=="menopausal",]
menopausal_cts <- as.data.frame(t(cts[rownames(cts) %in% menopausal_mirna$mirna,]))
all.equal(menopausal_mirna$mirna, colnames(menopausal_cts))

dt <- df
levels(dt$menstruation) <- c("post", "pre") # Ho cambiato i livelli della variabile: Menstruation no -> post(Menopausa), Menstruation yes -> pre(meno)
menopausal_mirna$group1 <- as.factor(menopausal_mirna$group1)
levels(menopausal_mirna$group1) <- c("post")
menopausal_mirna$group2 <- as.factor(menopausal_mirna$group2)
levels(menopausal_mirna$group2) <- c("pre")

legend <- c("Menopausal Status")
labels <- c("Post", "Pre")

torm <- which(is.na(dt$menstruation))
dt <- dt[-torm,]
k <- intersect(rownames(dt), rownames(menopausal_cts))
dt <- dt[k,]
menopausal_cts <- menopausal_cts[k,]
all.equal(rownames(menopausal_cts), rownames(dt))

## hsa-miR-12126-5p ##

meno1 <- menopausal_mirna[1,1]
db <- dt
db[,meno1] <- menopausal_cts[,meno1]
colnames(db)[24] <- "mirna"
data <- menopausal_mirna[menopausal_mirna$mirna == meno1,]

p1 <- ggplot(db, aes(x = menstruation, y = log(mirna))) +
  geom_boxplot(aes(fill = menstruation), outlier.shape = 1) +
  geom_jitter(width = 0.1, size = 1, alpha = 0.5) +
  scale_fill_manual(name = "Menopausal \n status", labels = labels, values = menopausal_color) +
  theme_classic() +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  ggtitle(str_remove(meno1, "-N")) +
  stat_pvalue_manual(data, y.position = max(log(db$mirna)+0.3), step.increase = 0.1, tip.length = 0.01, label = "psymb")


## hsa-miR-192-5p ##

meno2 <- menopausal_mirna[2,1]
db <- dt
db[,meno2] <- menopausal_cts[,meno2]
colnames(db)[24] <- "mirna"
data <- menopausal_mirna[menopausal_mirna$mirna == meno2,]

p2 <- ggplot(db, aes(x = menstruation, y = log(mirna))) +
  geom_boxplot(aes(fill = menstruation), outlier.shape = 1) +
  geom_jitter(width = 0.1, size = 1, alpha = 0.5) +
  scale_fill_manual(name = "Menopausal \n status", labels = labels, values = menopausal_color) +
  theme_classic() +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  ggtitle(str_remove(meno2, "-N")) +
  stat_pvalue_manual(data, y.position = max(log(db$mirna)+0.3), step.increase = 0.1, tip.length = 0.01, label = "psymb")

## hsa-miR-320d ##

meno3 <- menopausal_mirna[3,1]
db <- dt
db[,meno3] <- menopausal_cts[,meno3]
colnames(db)[24] <- "mirna"
data <- menopausal_mirna[menopausal_mirna$mirna == meno3,]

p3 <- ggplot(db, aes(x = menstruation, y = log(mirna))) +
  geom_boxplot(aes(fill = menstruation), outlier.shape = 1) +
  geom_jitter(width = 0.1, size = 1, alpha = 0.5) +
  scale_fill_manual(name = "Menopausal \n status", labels = labels, values = menopausal_color) +
  theme_classic() +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  ggtitle(str_remove(meno3, "-N")) +
  stat_pvalue_manual(data, y.position = max(log(db$mirna)+0.3), step.increase = 0.1, tip.length = 0.01, label = "psymb")
#
pMeno <- ggarrange(p2,p3,p1, nrow = 1, common.legend = TRUE, legend = "right")
pMeno <- annotate_figure(pMeno,
                        left = text_grob(bquote(~Log[10]~ '(expression levels)'), rot = 90, size = 8))

#################
### BMI miRNA ###
#################

bmi_color <- brewer.pal(4, "Greens")
bmi_mirna <- mirna[mirna$var=="bmi",]
bmi_cts <- as.data.frame(t(cts[rownames(cts) %in% bmi_mirna$mirna,]))
all.equal(bmi_mirna$mirna, colnames(bmi_cts))

dt <- df
bmi_mirna$group1 <- as.factor(bmi_mirna$group1)
bmi_mirna$group2 <- as.factor(bmi_mirna$group2)

legend <- c("BMI (Class)")
labels <- c("Underweight", "Normal", "Overweight", "Obese")

torm <- which(is.na(dt$bmi_cat))
if(length(torm) >0){
  dt <- dt[-torm,]
  k <- intersect(rownames(dt), rownames(bmi_cts))
  dt <- dt[k,]
  bmi_cts <- bmi_cts[k,]
  all.equal(rownames(bmi_cts), rownames(dt))
}


## hsa-miR-765-5p ##

bmi1 <- bmi_mirna[1,1]
db <- dt
db[,bmi1] <- bmi_cts[,bmi1]
colnames(db)[24] <- "mirna"
data <- bmi_mirna[bmi_mirna$mirna == bmi1,]

p1 <- ggplot(db, aes(x = bmi_cat, y = log(mirna))) +
  geom_boxplot(aes(fill = bmi_cat), outlier.shape = 1) +
  geom_jitter(width = 0.1, size = 1, alpha = 0.5) +
  scale_fill_manual(name = "BMI class", labels = labels, values = bmi_color) +
  ylab(bquote(~Log[10]~ 'expression levels')) +
  theme_classic() +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  ggtitle(str_remove(bmi1, "-N")) +
  stat_pvalue_manual(data, y.position = max(log(db$mirna)+0.3), step.increase = 0.1, tip.length = 0.01, label = "psymb")

## hsa-miR-182-5p ##

bmi2 <- bmi_mirna[2,1]
db <- dt
db[,bmi2] <- bmi_cts[,bmi2]
colnames(db)[24] <- "mirna"
data <- bmi_mirna[bmi_mirna$mirna == bmi2,]

p2 <- ggplot(db, aes(x = bmi_cat, y = log(mirna))) +
  geom_boxplot(aes(fill = bmi_cat), outlier.shape = 1) +
  geom_jitter(width = 0.1, size = 1, alpha = 0.5) +
  scale_fill_manual(name = "BMI class", labels = labels, values = bmi_color) +
  ylab(bquote(~Log[10]~ 'expression levels')) +
  theme_classic() +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  ggtitle(str_remove(bmi2, "-N")) +
  stat_pvalue_manual(data, y.position = max(log(db$mirna)+0.3), step.increase = 0.1, tip.length = 0.01, label = "psymb")

## hsa-miR-664a-5p ##♦

bmi3 <- bmi_mirna[3,1]
db <- dt
db[,bmi3] <- bmi_cts[,bmi3]
colnames(db)[24] <- "mirna"
data <- bmi_mirna[bmi_mirna$mirna == bmi3,]

p3 <- ggplot(db, aes(x = bmi_cat, y = log(mirna))) +
  geom_boxplot(aes(fill = bmi_cat), outlier.shape = 1) +
  geom_jitter(width = 0.1, size = 1, alpha = 0.5) +
  scale_fill_manual(name = "BMI class", labels = labels, values = bmi_color) +
  ylab(bquote(~Log[10]~ 'expression levels')) +
  theme_classic() +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  ggtitle(str_remove(bmi3, "-N")) +
  stat_pvalue_manual(data, y.position = max(log(db$mirna)+0.3), step.increase = 0.1, tip.length = 0.01, label = "psymb")
##
pBMI <- ggarrange(p2,p3,p1, nrow = 1, common.legend = TRUE, legend = "right")
pBMI <- annotate_figure(pBMI,
                         left = text_grob(bquote(~Log[10]~ '(expression levels)'), rot = 90, size = 8))
##

pAntro <-ggarrange(pSex, pAge, pMeno, pBMI, nrow = 4, align = "h")
ggsave(filename = "C:/Users/amedeo/Desktop/R_Projects/stool/results/figures/antropometric_boxplot.png", pAntro, width = 20, height = 19, units = "cm", dpi = 500)
rm(data, db, dt, meno1, meno2, meno3, menopausal_color, menopausal_cts, menopausal_mirna, p1,p2,p3,pAge,pMeno,pSex,bmi1,bmi2,bmi3,bmi_color,bmi_cts,
   bmi_mirna,age1,age2,age3,age_cts,age_mirna,pBMI)

####################################################################################################################################################
####################################################################################################################################################
##############################################    LIFESTYLE COMBINED PLOT    #######################################################################
####################################################################################################################################################
####################################################################################################################################################

#####################
### ALCOHOL MIRNA ###
#####################

alco_color <- brewer.pal(3, "PuRd")
alco_mirna <- mirna[mirna$var == "alcohol",]
alco_cts <- as.data.frame(t(cts[rownames(cts) %in% alco_mirna$mirna,]))
all.equal(alco_mirna$mirna, colnames(alco_cts))

dt <- df
levels(dt$alcool_ua) <- c("non_drinker", "low", "high")

alco_mirna$group1 <- as.factor(alco_mirna$group1)
levels(alco_mirna$group1) <- c("high", "low")
alco_mirna$group2 <- as.factor(alco_mirna$group2)
levels(alco_mirna$group2) <- c("non_drinker", "low")


legend <- c("Alcohol consumption")
labels <- c("Non drinker", "Low intake", "High intake")

torm <- which(is.na(dt$alcool_ua))

if(length(torm) >0){
  dt <- dt[-torm,]
  k <- intersect(rownames(dt), rownames(alco_cts))
  dt <- dt[k,]
  alco_cts <- alco_cts[k,]
  all.equal(rownames(alco_cts), rownames(dt))
}

## hsa-miR-324-3p

alco1 <- alco_mirna[1,1]
db <- dt
db[,"mirna"] <- alco_cts[,alco1]
colnames(db)[24] <- "mirna"
data <- alco_mirna[alco_mirna$mirna == alco1,]

p1 <- ggplot(db, aes(x = alcool_ua, y = log(mirna))) +
  geom_boxplot(aes(fill = alcool_ua), outlier.shape = 1) +
  geom_jitter(width = 0.1, size = 1, alpha = 0.5) +
  scale_fill_manual(name = "    Alcohol \n     intake", labels = labels, values = alco_color) +
  ylab(bquote(~Log[10]~ 'expression levels')) +
  theme_classic() +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  ggtitle(str_remove(alco1, "-N")) +
  stat_pvalue_manual(data, y.position = max(log(db$mirna)+0.3), step.increase = 0.05, tip.length = 0.01, label = "psymb")


## hsa-miR-608-3p

alco2 <- alco_mirna[2,1]
db <- dt
db[,"mirna"] <- alco_cts[,alco2]
colnames(db)[24] <- "mirna"
data <- alco_mirna[alco_mirna$mirna == alco2,]

p2 <- ggplot(db, aes(x = alcool_ua, y = log(mirna))) +
  geom_boxplot(aes(fill = alcool_ua), outlier.shape = 1) +
  geom_jitter(width = 0.1, size = 1, alpha = 0.5) +
  scale_fill_manual(name = "    Alcohol \n     intake", labels = labels, values = alco_color) +
  ylab(bquote(~Log[10]~ 'expression levels')) +
  theme_classic() +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  ggtitle(str_remove(alco2, "-N")) +
  stat_pvalue_manual(data, y.position = max(log(db$mirna)+0.3), step.increase = 0.05, tip.length = 0.01, label = "psymb")

## hsa-miR-6741-5p

alco3 <- alco_mirna[3,1]
db <- dt
db[,"mirna"] <- alco_cts[,alco3]
colnames(db)[24] <- "mirna"
data <- alco_mirna[alco_mirna$mirna == alco3,]

p3 <- ggplot(db, aes(x = alcool_ua, y = log(mirna))) +
  geom_boxplot(aes(fill = alcool_ua), outlier.shape = 1) +
  geom_jitter(width = 0.1, size = 1, alpha = 0.5) +
  scale_fill_manual(name = "    Alcohol \n     intake", labels = labels, values = alco_color) +
  ylab(bquote(~Log[10]~ 'expression levels')) +
  theme_classic() +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  ggtitle(str_remove(alco3, "-N")) +
  stat_pvalue_manual(data, y.position = max(log(db$mirna)+0.3), step.increase = 0.05, tip.length = 0.01, label = "psymb")

pAlco <- ggarrange(p1,p2,p3, nrow = 1, common.legend = TRUE, legend = "right")
pAlco <- annotate_figure(pAlco,
                        left = text_grob(bquote(~Log[10]~ '(expression levels)'), rot = 90, size = 8))

###################
### NCIGS miRNA ###
###################

ncigs_color <- brewer.pal(9, "Greys")[c(1,3,4,5)]
ncigs_mirna <- mirna[mirna$var=="ncigs",]
ncigs_cts <- as.data.frame(t(cts[rownames(cts) %in% ncigs_mirna$mirna,]))
all.equal(ncigs_mirna$mirna, colnames(ncigs_cts))

dt <- df
levels(dt$cigs) <- c("never", "former", "<16", ">16")
ncigs_mirna$group1 <- as.factor(ncigs_mirna$group1)
ncigs_mirna$group2 <- as.factor(ncigs_mirna$group2)

legend <- c("Smoking status")
labels <- c("Never", "Former", "<16 cigs/day", ">16 cigs/day")

torm <- which(is.na(dt$cigs))

if(length(torm) >0){
  dt <- dt[-torm,]
  k <- intersect(rownames(dt), rownames(ncigs_cts))
  dt <- dt[k,]
  ncigs_cts <- ncigs_cts[k,]
  all.equal(rownames(ncigs_cts), rownames(dt))
}

## hsa-miR-3127-5p ##

ncigs1 <- ncigs_mirna[1,1]
db <- dt
db[,ncigs1] <- ncigs_cts[,ncigs1]
colnames(db)[24] <- "mirna"
data <- ncigs_mirna[ncigs_mirna$mirna == ncigs1,]

p1 <- ggplot(db, aes(x = cigs, y = log(mirna))) +
  geom_boxplot(aes(fill = cigs), outlier.shape = 1) +
  geom_jitter(width = 0.1, size = 1, alpha = 0.5) +
  scale_fill_manual(name = "   Smoking \n    status", labels = labels, values = ncigs_color) +
  ylab(bquote(~Log[10]~ 'expression levels')) +
  theme_classic() +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  ggtitle(str_remove(ncigs1, "-N")) +
  stat_pvalue_manual(data, y.position = max(log(db$mirna)+0.3), step.increase = 0.1, tip.length = 0.01, label = "psymb")


## hsa-miR-4533 ##

ncigs2 <- ncigs_mirna[2,1]
db <- dt
db[,ncigs2] <- ncigs_cts[,ncigs2]
colnames(db)[24] <- "mirna"
data <- ncigs_mirna[ncigs_mirna$mirna == ncigs2,]

p2 <- ggplot(db, aes(x = cigs, y = log(mirna))) +
  geom_boxplot(aes(fill = cigs), outlier.shape = 1) +
  geom_jitter(width = 0.1, size = 1, alpha = 0.5) +
  scale_fill_manual(name = "   Smoking \n    status", labels = labels, values = ncigs_color) +
  ylab(bquote(~Log[10]~ 'expression levels')) +
  theme_classic() +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  ggtitle(str_remove(ncigs2, "-N")) +
  stat_pvalue_manual(data, y.position = max(log(db$mirna)+0.3), step.increase = 0.1, tip.length = 0.01, label = "psymb")

## hsa-miR-8075 ##

ncigs3 <- ncigs_mirna[4,1]
db <- dt
db[,ncigs3] <- ncigs_cts[,ncigs3]
colnames(db)[24] <- "mirna"
data <- ncigs_mirna[ncigs_mirna$mirna == ncigs3,]

p3 <- ggplot(db, aes(x = cigs, y = log(mirna))) +
  geom_boxplot(aes(fill = cigs), outlier.shape = 1) +
  geom_jitter(width = 0.1, size = 1, alpha = 0.5) +
  scale_fill_manual(name = "   Smoking \n    status", labels = labels, values = ncigs_color) +
  ylab(bquote(~Log[10]~ 'expression levels')) +
  theme_classic() +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  ggtitle(str_remove(ncigs3, "-N")) +
  stat_pvalue_manual(data, y.position = max(log(db$mirna)+0.3), step.increase = 0.1, tip.length = 0.01, label = "psymb")

pCigs <- ggarrange(p1,p2,p3, nrow = 1, common.legend = TRUE, legend = "right")
pCigs <- annotate_figure(pCigs,
                         left = text_grob(bquote(~Log[10]~ '(expression levels)'), rot = 90, size = 8))

##############################
### COFFEE STRATIFIED MIRNA ##
##############################

coffee_color <- rev(brewer.pal(10, "BrBG")[c(2,3,5)])
coffee_mirna <- mirna[mirna$group1=="heavy" & mirna$var == "coffee",]
coffee_cts <- as.data.frame(t(cts[rownames(cts) %in% coffee_mirna$mirna,]))
all.equal(coffee_mirna$mirna, colnames(coffee_cts))

dt <- df
levels(dt$coffee) <- c("no_drinker", "light_drinker", "heavy_drinker")
coffee_mirna$group1 <- as.factor(coffee_mirna$group1)
levels(coffee_mirna$group1) <- c("heavy_drinker")
coffee_mirna$group2 <- as.factor(coffee_mirna$group2)
levels(coffee_mirna$group2) <- c("no_drinker")

legend <- c("Coffee consumption")
labels <- c("Non drinker", "Low intake", "High intake")

torm <- which(is.na(dt$coffee))

if(length(torm) >0){
  dt <- dt[-torm,]
  k <- intersect(rownames(dt), rownames(coffee_cts))
  dt <- dt[k,]
  coffee_cts <- coffee_cts[k,]
  all.equal(rownames(coffee_cts), rownames(dt))
}


## hsa-miR-125b-5p ##

coffee1 <- coffee_mirna[1,1]
db <- dt
db[,coffee1] <- coffee_cts[,coffee1]
colnames(db)[24] <- "mirna"
data <- coffee_mirna[coffee_mirna$mirna == coffee1,]

check(coffee1,db,coffee_cts,data)

p1 <- ggplot(db, aes(x = coffee, y = log(mirna))) +
  geom_boxplot(aes(fill = coffee), outlier.shape = 1) +
  geom_jitter(width = 0.1, size = 1, alpha = 0.5) +
  scale_fill_manual(name ="     Coffee \n consumption", labels = labels, values = coffee_color) +
  ylab(bquote(~Log[10]~ 'expression levels')) +
  theme_classic() +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  ggtitle(str_remove(coffee1, "-N")) +
  stat_pvalue_manual(data, y.position = max(log(db$mirna)+0.3), step.increase = 0.05, tip.length = 0.01, label = "psymb")

## hsa-miR-8053 ##

coffee2 <- coffee_mirna[2,1]
db <- dt
db[,coffee2] <- coffee_cts[,coffee2]
colnames(db)[24] <- "mirna"
data <- coffee_mirna[coffee_mirna$mirna == coffee2,]

check(coffee2,db,coffee_cts,data)

p2 <- ggplot(db, aes(x = coffee, y = log(mirna))) +
  geom_boxplot(aes(fill = coffee), outlier.shape = 1) +
  geom_jitter(width = 0.1, size = 1, alpha = 0.5) +
  scale_fill_manual(name ="     Coffee \n consumption", labels = labels, values = coffee_color) +
  ylab(bquote(~Log[10]~ 'expression levels')) +
  theme_classic() +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  ggtitle(str_remove(coffee2, "-N")) +
  stat_pvalue_manual(data, y.position = max(log(db$mirna)+0.3), step.increase = 0.05, tip.length = 0.01, label = "psymb")

## hsa-miR-622 ##

coffee3 <- coffee_mirna[3,1]
db <- dt
db[,coffee3] <- coffee_cts[,coffee3]
colnames(db)[24] <- "mirna"
data <- coffee_mirna[coffee_mirna$mirna == coffee3,]

check(coffee3,db,coffee_cts,data)

p3 <- ggplot(db, aes(x = coffee, y = log(mirna))) +
  geom_boxplot(aes(fill = coffee), outlier.shape = 1) +
  geom_jitter(width = 0.1, size = 1, alpha = 0.5) +
  scale_fill_manual(name ="     Coffee \n consumption", labels = labels, values = coffee_color) +
  ylab(bquote(~Log[10]~ 'expression levels')) +
  theme_classic() +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  ggtitle(str_remove(coffee3, "-N")) +
  stat_pvalue_manual(data, y.position = max(log(db$mirna)+0.3), step.increase = 0.05, tip.length = 0.01, label = "psymb")

pCoffee <- ggarrange(p1,p3,p2, nrow = 1, common.legend = TRUE, legend = "right")
pCoffee <- annotate_figure(pCoffee,
                         left = text_grob(bquote(~Log[10]~ '(expression levels)'), rot = 90, size = 8))

###############################
### PHYSICAL ACTIVITY MIRNA ###
###############################

phys_color <- rev(brewer.pal(11, "PRGn")[2:4])
phys_mirna <- mirna[mirna$var == "physact",]
phys_cts <- as.data.frame(t(cts[rownames(cts) %in% phys_mirna$mirna,]))
all.equal(phys_mirna$mirna, colnames(phys_cts))

dt <- df
levels(dt$phys_act) <- c("inactive", "modInact", "modAct")
phys_mirna$group1 <- as.factor(phys_mirna$group1)
phys_mirna$group2 <- as.factor(phys_mirna$group2)

legend <- c("Physical activity")
labels <- c("Inactive", "Moderately \n Inactive", "Moderately \n Active/Active")

torm <- which(is.na(dt$phys_act))

if(length(torm) >0){
  dt <- dt[-torm,]
  k <- intersect(rownames(dt), rownames(phys_cts))
  dt <- dt[k,]
  phys_cts <- phys_cts[k,]
  all.equal(rownames(phys_cts), rownames(dt))
}

## hsa-mir-2110

phys1 <- phys_mirna[1,1]
db <- dt
db[,"mirna"] <- phys_cts[,phys1]
colnames(db)[24] <- "mirna"
data <- phys_mirna[phys_mirna$mirna == phys1,]

p1 <- ggplot(db, aes(x = phys_act, y = log(mirna))) +
  geom_boxplot(aes(fill = phys_act), outlier.shape = 1) +
  geom_jitter(width = 0.1, size = 1, alpha = 0.5) +
  scale_fill_manual(name = "    Physical \n     activity", labels = labels, values = phys_color) +
  ylab(bquote(~Log[10]~ 'expression levels')) +
  theme_classic() +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  ggtitle(str_remove(phys1, "-N")) +
  stat_pvalue_manual(data, y.position = max(log(db$mirna)+0.3), step.increase = 0.05, tip.length = 0.01, label = "psymb")

## hsa-mir-4452

phys2 <- phys_mirna[3,1]
db <- dt
db[,"mirna"] <- phys_cts[,phys2]
colnames(db)[24] <- "mirna"
data <- phys_mirna[phys_mirna$mirna == phys2,]

p2 <- ggplot(db, aes(x = phys_act, y = log(mirna))) +
  geom_boxplot(aes(fill = phys_act), outlier.shape = 1) +
  geom_jitter(width = 0.1, size = 1, alpha = 0.5) +
  scale_fill_manual(name = "    Physical \n     activity", labels = labels, values = phys_color) +
  ylab(bquote(~Log[10]~ 'expression levels')) +
  theme_classic() +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  ggtitle(str_remove(phys2, "-N")) +
  stat_pvalue_manual(data, y.position = max(log(db$mirna)+0.3), step.increase = 0.05, tip.length = 0.01, label = "psymb")

## hsa-mir-944-5p

phys3 <- phys_mirna[5,1]
db <- dt
db[,"mirna"] <- phys_cts[,phys3]
colnames(db)[24] <- "mirna"
data <- phys_mirna[phys_mirna$mirna == phys3,]

p3 <- ggplot(db, aes(x = phys_act, y = log(mirna))) +
  geom_boxplot(aes(fill = phys_act), outlier.shape = 1) +
  geom_jitter(width = 0.1, size = 1, alpha = 0.5) +
  scale_fill_manual(name = "    Physical \n     activity", labels = labels, values = phys_color) +
  ylab(bquote(~Log[10]~ 'expression levels')) +
  theme_classic() +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  ggtitle(str_remove(phys3, "-N")) +
  stat_pvalue_manual(data, y.position = max(log(db$mirna)+0.3), step.increase = 0.05, tip.length = 0.01, label = "psymb")
#
pPhys <- ggarrange(p3,p1,p2, nrow = 1, common.legend = TRUE, legend = "right")
pPhys <- annotate_figure(pPhys,
                           left = text_grob(bquote(~Log[10]~ '(expression levels)'), rot = 90, size = 8))
##
pLife <-ggarrange(pAlco, pCigs, pCoffee, pPhys, nrow = 4, align = "v")
ggsave(filename = "C:/Users/amedeo/Desktop/R_Projects/stool/results/figures/lifestyle_boxplot.png", pLife, width = 20, height = 19, units = "cm", dpi = 500)