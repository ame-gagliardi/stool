library(tidyverse)

# Load count

cts <- readRDS("data/ngs/merged_harmonized_converted.rds")

df <- as_tibble(data.frame("miRNA" = rownames(cts), "min" = apply(cts, 1, min), "max" = apply(cts, 1, max),
                           "mean" = round(apply(cts, 1, mean), digits = 2), "median" = apply(cts, 1, median), "sd" = apply(cts, 1, sd)))

write.table(df, "C:/Users/amedeo/Desktop/R_Projects/stool/results/variability.txt", quote = FALSE, row.names = FALSE, sep ="\t")

df_1 <- df %>% 
  dplyr::filter(mean >= 11) %>% 
  dplyr::mutate(sample = seq(length(sd))) %>% 
  dplyr::mutate(check = sd > 50) %>% 
  dplyr::filter(mean < 2000)
  

cts_1 <- cts[which(rownames(cts) %in% df_1$miRNA),]
cts_1 <- as.data.frame(t(cts_1))

for(i in 1:length(rownames(cts_1))){
  cts_1[i,] <- as.numeric(cts_1[i,])
}

cts_1$sample <- seq(length(rownames(cts_1)))
cts_1 <-  tidyr::pivot_longer(cts_1, c(1:867), names_to = "miRNA", values_to = "count")


ggplot(df_1, aes(x = mean, y = sd)) +
  geom_point(aes(color = check))
