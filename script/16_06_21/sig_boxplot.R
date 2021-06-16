## Boxplot of significative miRNAs with clear trend
source("C:/Users/amedeo/Desktop/R_Projects/general_script/functions.R")
# Data

refDB <- "both"
refVar <- "age_cat"
refCTS <- "normalized"
miR <- c("hsa-miR-12127")

mirnas <- read.delim(paste0("results/sig_mirna/", refDB, "/", refVar, ".txt"), sep ="\t", header = T, row.names = 1)
df <- readRDS(paste0("data/clinical/", refDB, "_samples.rds"))
cts <- readRDS(paste0("data/ngs/", refCTS, "_counts.rds"))
rownames(cts) <- str_replace_all(rownames(cts), ":Novel", "-Novel")
# Filtering
mirnas <- mirnas %>% 
  dplyr::filter(Boxplot == "Yes")
rownames(mirnas) <- str_replace_all(rownames(mirnas), ":Novel", "-Novel")

i <- intersect(rownames(mirnas), rownames(cts))
cts <- as.data.frame(t(cts[i,]))

all.equal(rownames(df), rownames(cts))

df <- bind_cols(df, cts)

miR <- rownames(mirnas)


for(i in 1:length(miR)){

  cols <- grep("hsa-miR", colnames(df))
  db <- tidyr::pivot_longer(df, cols = grep("hsa-miR", colnames(df)), names_to = "miRNA", values_to = "count") %>% 
    dplyr::select(id, age_cat, miRNA, count) %>% 
    dplyr::filter(miRNA == miR[i])
  # Plot
  fact <- covarLevels(refVar, df)
  # fact <- c("18-37", "37-53", "53-81")
  p <- ggplot(db, aes(x = age_cat, y = log(count), fill = age_cat)) +
    geom_boxplot() +
    labs(title = paste0("Boxplot of transformed counts of: ", miR[i]),
         x = firstup(refVar), y = "Log2") +
    scale_fill_discrete(name = firstup(refVar), labels = fact)

  ggsave(p, filename = paste0("results/figures/boxplot/", miR[i], "_" ,refVar, "_", refDB, "_boxplot.png"))
}
