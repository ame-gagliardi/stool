source("C:/Users/amedeo/Desktop/R_Projects/general_script/libraries_functions.R")
source("C:/Users/amedeo/Desktop/R_Projects/general_script/libraries_graph.R")
library(scales)
library(viridis)

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


# Filter

tmp <- as.data.frame(matrix(NA, nrow = length(rownames(cts)), ncol = 4))
colnames(tmp) <- c("miRNA", "median", "med_treshold" ,"mad")
tmp[,1] <- rownames(cts)
tmp[,2] <- apply(cts, 1, median, na.rm = TRUE)
tmp[,3] <- tmp[,2] > 10
tmp <- subset(tmp, tmp$med_treshold==TRUE)
cts <- subset(cts, rownames(cts) %in% tmp$miRNA)
all.equal(rownames(cts), tmp$miRNA)
rownames(tmp) <- tmp$miRNA

# Dispersion data

disp <- tmp %>% 
  dplyr::mutate(mad = apply(cts,1,mad), cv = mad/median) %>% 
  dplyr::arrange(cv) %>% 
  dplyr::mutate(cv_pos = seq(1:length(miRNA)))

disp[,"cv_lab"] <- NA
disp[,"cv_lab"][c(1:10,303:312)] <- TRUE
disp[,"cv_lab"][11:302] <- FALSE
disp[,"cv_lab"][1:10] <- c("Top")
disp[,"cv_lab"][303:312] <- c("Bottom")
disp[,"cv_lab"] <- as.factor(disp$cv_lab)
disp[,"circle"] <- ifelse(disp$miRNA == "hsa-miR-3125" | disp$miRNA == "hsa-miR-1246-3p:Novel" | disp$miRNA == "hsa-miR-6075" | disp$miRNA == "hsa-miR-607"
                          | disp$miRNA == "hsa-miR-1302" | disp$miRNA == "hsa-miR-320e-5p:Novel" | disp$miRNA == "hsa-miR-647-3p:Novel", TRUE, FALSE)
disp[,"italic1"] <- ifelse(str_detect(disp$miRNA, ":Novel") & disp$cv_lab == "Top" |
                           str_detect(disp$miRNA, ":Novel") & disp$cv_lab == "Bottom" |
                           str_detect(disp$miRNA, ":Novel") & disp$circle == TRUE, TRUE, FALSE)
disp[,"italic2"] <- ifelse(str_detect(disp$miRNA, ":Novel") & disp$cv_lab == TRUE, TRUE, FALSE)
disp[,"name"] <- ifelse(disp$cv_lab == "Top" | disp$cv_lab == "Bottom" | disp$circle == TRUE, TRUE, FALSE)
disp[,"miRNA"] <- str_remove_all(disp$miRNA, "hsa-")
disp[,"log10"] <- log10(disp$median)

## Versione no elenco ##

cv_lab_col <- c("#ee0038", "#C0C0C0", "#041cea")

p <- ggplot() +
  geom_point(data=disp[disp$circle == TRUE,], pch = 21, size = 4, colour = "red", stroke = 1.2, aes(x=cv, y=log10, fill = circle)) +
  geom_point(data=disp, aes(x=cv, y=log10, color=cv_lab), size = 2.3) +
  scale_color_manual(name="Coefficient of variation", labels=c("Lowest", "Highest"),breaks=c("Top", "Bottom"), values = cv_lab_col) +
  scale_fill_manual(name="", labels=c("miRNAs expressed \n in all samples"), values = NA) +
  geom_text_repel(data=disp, aes(x=cv, y=log10,label=ifelse(name==TRUE & italic1==TRUE, str_remove_all(miRNA, ":Novel"), "")),fontface =3, size = 4.8,force = 1.7) +
  geom_text_repel(data=disp, aes(x=cv, y=log10,label=ifelse(name==TRUE & italic1==FALSE, miRNA, "")),fontface = 1, size = 4.8,force = 1.7) +
  xlab("Coefficient of variation") +
  ylab(bquote(~log[10]~ "(Median expression)")) +
  theme_classic() +
  theme(legend.title = element_text(size = 17),
        legend.text = element_text(size = 16),
        legend.position = c(0.9,0.7),
        axis.title.x = element_text(size = 19),
        axis.title.y = element_text(size = 19),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16)) +
  guides(color = guide_legend(order = 1),
         fill = guide_legend(order = 2))
  
p

ggsave(p, file = "finalmad.png", width = 44, height = 32, units = "cm", dpi = 500)
  





## Versione elenco ##

top10title <- "Most stable miRNAs"
top10 <- c("mir-647-3p \n mir-6075 \n mir-320e-5p \n mir-501-3p \n mir-3125 \n mir-181b-5p \n mir-1275-3p \n mir-151a-3p \n mir-4520-3p \n mir-4487")
bottom10title <- "Most unstable miRNAs"
bottom10 <- "miR-3619-3p \n miR-378i \n miR-4254-5p \n miR-548d-5p \n miR-371a-3p \n miR-1203 \n miR-4739-3p \n miR-646 \n miR-200c-3p \n miR-645"

p <- ggplot() +
  geom_point(data=disp, aes(x=cv, y=log(median), color=cv_lab)) +
  geom_point(data=disp[disp$circle == TRUE,], pch = 21, size = 5, colour = "red", stroke = 1, aes(x=cv, y=log(median), fill = circle)) +
  scale_fill_manual(name="", labels=c("miRNA expressed \n in all samples"), values = NA) +
  scale_color_manual(name="Coefficient of variation", labels=c("Lowest", "Highest"),breaks=c("Top", "Bottom"), values = cv_lab_col) +
  annotate("text", x=1.22, y=7.5, label = top10title, fontface = 2, color = "green") +
  annotate("text", x=1.22, y=6.8, label = top10, size = 3) +
  annotate("text", x=1.35, y=7.5, label = bottom10title, fontface = 2, color = "red") +
  annotate("text", x=1.35, y=6.8, label = bottom10, size = 3, fontface = 3) +
  geom_text_repel(data=disp, aes(x=cv, y=log(median),label=ifelse(circle==TRUE & cv_lab==FALSE, str_remove_all(miRNA, "hsa-"), "")), size = 2.5) +
  xlab("Coefficient of variation") +
  ylab(bquote(~log[10]~ "(Median of exression level)")) +
  guides(color=FALSE) +
  theme_classic()

p

ggsave(p, file = "elenco.png", width = 40, height = 30, units = "cm", dpi = 500)