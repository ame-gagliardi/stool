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


# Filter

tmp <- as.data.frame(matrix(NA, nrow = length(rownames(cts)), ncol = 4))
colnames(tmp) <- c("miRNA", "median", "med_treshold" ,"mad")
tmp[,1] <- rownames(cts)
tmp[,2] <- apply(cts, 1, median, na.rm = TRUE)
tmp[,3] <- tmp[,2] > 10
tmp <- subset(tmp, tmp$med_treshold==TRUE)
cts <- subset(cts, rownames(cts) %in% tmp$miRNA)
all.equal(rownames(cts), tmp$miRNA)

# Dispersion data

tmp[,"mad"] <- apply(cts, 1, mad) # Mad
tmp <- tmp[order(tmp$mad),] # Order by MAD
tmp[,"order"] <- seq(1:length(rownames(tmp))) 
tmp[,"mad_position"] <- rep(c("top","middle", "bottom"), times = c(20,279,13)) # legend of MAD position
tmp[,"position_legend"] <- ifelse(tmp$mad_position == "top" | tmp$mad_position == "bottom", TRUE, FALSE) # label for plot
tmp[,"cv"] <- tmp$mad/tmp$median # MAD/Median ratio
tmp <- tmp[order(tmp$cv),] # Order by MAD/median ratio
tmp[,"cv_order"] <- seq(1:length(rownames(tmp))) 
tmp[,"cv_position"] <- rep(c("top", "middle", "bottom"), times = c(20,279,13)) # legend of MAD/Median ratio
tmp[,"cv_label"] <- ifelse(tmp$cv_position == "top" | tmp$cv_position == "bottom", TRUE, FALSE) # label for plot
rownames(tmp) <- tmp$miRNA

top <- tmp[1:10,"miRNA"]
bottom <- tmp[303:312, "miRNA"]
tolabel <- c(top, bottom)
tmp[,"top"] <- ifelse()
# tmp[,"top"] <- ifelse(tmp$miRNA == "hsa-miR-647-3p:Novel" | tmp$miRNA == "hsa-miR-6075" | 
#                       tmp$miRNA == "hsa-miR-3125" | tmp$miRNA == "hsa-miR-320e-5p:Novel" |
#                       tmp$miRNA == "hsa-miR-1246-3p" | tmp$miRNA == "hsa-miR-607" | tmp$miRNA == "hsa-miR-1302", TRUE, FALSE)




tmp[,"italic"] <- ifelse(tmp$top == TRUE & str_detect(tmp$miRNA, ":Novel"),1,FALSE)

toellipse <- tmp[c(1,2,3,5),]

# Plot

top10title <- "Most stable miRNAs"
top10 <- as.data.frame(matrix(NA, nrow = 10, ncol = 2))
top10[,1] <- tmp$miRNA[1:10]
t
bottom10title <- "Most unstable miRNAs"
bottom10 <- "hsa-miR-3619-3p \n hsa-miR-378i \n hsa-miR-4254-5p \n hsa-miR-548d-5p \n hsa-miR-371a-3p \n hsa-miR-1203 \n hsa-miR-4739-3p \n hsa-miR-646 \n hsa-miR-200c-3p \n hsa-miR-645"

p <- ggplot(tmp, aes(x=cv, y=log(median))) +
  geom_point(aes(color=top)) +
  ylab(bquote(~log[10]~ "(median)")) +
  xlab(expression(frac(MAD,Median), cex = 1.5)) +
  theme_classic() +
  annotate("text", x=1.2, y=7.5, label = top10title, fontface = 2) +
  annotate("text", x=1.2, y=6.8, label = prova, size = 3, parse = TRUE) +
  annotate("text", x=1.3, y=7.5, label = bottom10title, fontface = 2) +
  annotate("text", x=1.3, y=6.8, label = bottom10, size = 3, fontface = 3) +
  geom_text_repel(aes(label=ifelse(italic==1, str_remove_all(miRNA, ":Novel"), "")), size = 3.5, fontface="bold.italic") +
  geom_text_repel(aes(label=ifelse(italic==0 & top == TRUE, miRNA, "")), size=3.5, fontface="bold") +
  guides(color=FALSE)


prova <- bquote(atop("italic('hsa-miR-647-3p')", "hsa-miR-6075"))

p

ggsave(p, filename = "mad_median_log.png", width = 30, height = 30, units = "cm", dpi = 300)



prova <- c(atop("italic('hsa-miR-647-3p')", 
                "hsa-miR-6075"))
#####