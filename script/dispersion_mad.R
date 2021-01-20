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
tmp[,"coefficient"] <- tmp$mad/tmp$median # MAD/Median ratio
tmp <- tmp[order(tmp$coefficient),] # Order by MAD/median ratio
tmp[,"coefficient_order"] <- seq(1:length(rownames(tmp))) 
tmp[,"coefficient_position"] <- rep(c("top", "middle", "bottom"), times = c(20,279,13)) # legend of MAD/Median ratio
tmp[,"coefficient_label"] <- ifelse(tmp$coefficient_position == "top" | tmp$coefficient_position == "bottom", TRUE, FALSE) # label for plot
rownames(tmp) <- tmp$miRNA
# Plot

p1 <- ggplot(tmp, aes(x=log(mad), y=log(median))) +
  geom_point(aes(color=mad_position)) +
  ylab(bquote(~log[10]~ "(median)")) +
  xlab(bquote(~log[10]~ "(median absolute deviation)")) +
  theme_classic() +
  scale_color_branded() +
  geom_text_repel(aes(label = ifelse(position_legend == TRUE, miRNA, "")), size = 2.5)

p2 <- ggplot(tmp, aes(x=log(mad), y=log(median))) +
  geom_point(aes(color=mad_position)) +
  ylab(bquote(~log[10]~ "(median)")) +
  xlab(bquote(~log[10]~ "(median absolute deviation)")) +
  theme_classic() +
  scale_color_branded()

p3 <- ggplot(tmp, aes(x=coefficient, y=median)) +
  geom_point(aes(color=coefficient_position)) +
  ylab("Median") +
  xlab(expression(frac(MAD,Median), cex = 1.5)) +
  theme_classic() +
  scale_color_branded() +
  geom_text_repel(aes(label = ifelse(coefficient_label == TRUE, miRNA, "")), size = 2.5)

p4 <- ggplot(tmp, aes(x=coefficient, y=median)) +
  geom_point(aes(color=coefficient_position)) +
  ylab("Median") +
  xlab(expression(frac(MAD,Median), cex = 1.5)) +
  theme_classic() +
  scale_color_branded()





ggsave(p1, filename = "mad_plot_names.png", width = 30, height = 30, units = "cm", dpi = 300)
ggsave(p2, filename = "mad_plot_no_names.png", width = 30, height = 30, units = "cm", dpi = 300)
ggsave(p3, filename = "mad_median_ratio_names.png", width = 30, height = 30, units = "cm", dpi = 300)
ggsave(p4, filename = "mad_median_ratio_no_names.png", width = 30, height = 30, units = "cm", dpi = 300)




#####