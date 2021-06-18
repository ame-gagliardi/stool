candiolo <- c("D:/R_Projects/general/")
casa <- c("C:/Users/UappaSguappa/Desktop/R_projects/general")

source(paste0(candiolo, "FUNCTION_custom.R"))
source(paste0(candiolo, "GRAPH_libraries.R"))

## Repeated plot ##

cts <- readRDS("D:/R_Projects/stool/results/repeated_samples/repeated_cts_norm.rds")
df <- readRDS("D:/R_Projects/stool/results/repeated_samples/repeated_df.rds")

vov_cts <- cts[,1:6]
cel_cts <- cts[,7:12]


# filter <- as.data.frame(matrix(NA, nrow = length(rownames(whole_cts)), ncol = length(colnames(whole_cts))))
# rownames(filter) <- rownames(whole_cts)
# colnames(filter) <- colnames(whole_cts)
# 
# for(i in 1:length(rownames(whole_cts))){
#   filter[i,] <- whole_cts[i,] > 10
# }
# 
# filter[,"passed"] <- apply(filter,1,any)
# 
# all.equal(rownames(whole_cts), rownames(filter))
# whole_cts[,"passed"] <- filter$passed

vov_cts[,"vov.mean"] <- apply(vov_cts, 1, mean, na.rm = TRUE)
cel_cts[,"cel.mean"] <- apply(cel_cts, 1, mean, na.rm = TRUE)

whole_cts <- bind_cols(vov_cts, cel_cts)
xx <- cor.test(vov_cts$vov.mean, cel_cts$cel.mean)


whole_cts$x <- log10(whole_cts$vov.mean + 1)
whole_cts$y <- log10(whole_cts$cel.mean + 1)

p <- ggplot(whole_cts, aes(x,y)) +
  geom_point(aes(alpha=0.6)) +
  geom_smooth(method = "lm", se = TRUE) +
  stat_cor(method = "pearson", size = 10) +
  labs(x=bquote(~log[10]~ '(Average miRNA expression at '~T[0]~')'),
       y=bquote(~log[10]~ '(Average miRNA expression at '~T[1]~')')) +
  theme_classic() +
  theme(axis.title.x = element_text(size = 30),
        axis.title.y = element_text(size = 30),
        axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 25)) +
  guides(alpha=FALSE)
  
ggsave(p, filename = "results/figures/repeated_correlation.png", width = 30, height = 30, units = "cm", dpi = 500)


## Single plot to combine

for(i in 1:6){
  
  cts_plot <- whole_cts %>% 
    rownames_to_column() %>%
    dplyr::rename(mirna = rowname) %>% 
    dplyr::select(mirna, df[i, "vov"], df[i, "cel"]) %>% 
    dplyr::mutate(name = df[i, "name"]) %>% 
    dplyr::rename(x = 2, y = 3) %>% 
    dplyr::mutate(z = log10(x+1), k = log10(y+1))
  
  nam <- paste("A",i,sep="_")
  
  assign(nam, p <- ggplot(cts_plot, aes(z,k)) +
           geom_point(aes(alpha=0.6)) +
           geom_smooth(method = "lm", se = TRUE) +
           stat_cor(method = "pearson", size = 2.5) +
           labs(x=paste0("Subject ", i), y=bquote(~log[10]~ '(Average miRNA expression at '~T[1]~')')) +
           theme_classic() +
           theme(axis.title.y = element_blank()) +
           scale_y_continuous(expand = expansion(mult = c(0, .1))) +
           guides(alpha=FALSE))
}

p <- annotate_figure(ggarrange(A_2, A_5, A_6, A_4, ncol = 4, nrow = 1),
                bottom = text_grob(bquote(~log[10]~ '(Average miRNA expression at '~T[0]~')')), 
                left = text_grob(bquote(~log[10]~ '(Average miRNA expression at '~T[1]~')'), rot = 90))



ggsave(p, filename = "C:/Users/amega/Desktop/scatter.png", width = 40, height = 10, units = "cm", dpi = 500)
ggsave(p, filename = "C:/Users/amega/Desktop/scatter.svg", width = 40, height = 10, units = "cm", dpi = 500)
