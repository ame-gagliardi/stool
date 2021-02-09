source("C:/Users/amedeo/Desktop/R_Projects/general_script/libraries_functions.R")
source("C:/Users/amedeo/Desktop/R_Projects/general_script/libraries_graph.R")

## Repeated plot ##

cts <- readRDS("C:/Users/amedeo/Desktop/R_Projects/stool/results/repeated_samples/repeated_cts_norm.rds")
df <- readRDS("C:/Users/amedeo/Desktop/R_Projects/stool/results/repeated_samples/repeated_df.rds")

vov_cts <- cts[,1:6]
cel_cts <- cts[,7:12]

vov_cts[,"vov.mean"] <- apply(vov_cts, 1, mean, na.rm = TRUE)
cel_cts[,"cel.mean"] <- apply(cel_cts, 1, mean, na.rm = TRUE)

xx <- cor.test(vov_cts$vov.mean, cel_cts$cel.mean)

whole_cts <- bind_cols(vov_cts, cel_cts)

whole_cts$x <- log(whole_cts$vov.mean + 1)
whole_cts$y <- log(whole_cts$cel.mean + 1)

p <- ggplot(whole_cts, aes(x,y)) +
  geom_point(aes(alpha=0.6)) +
  geom_smooth(method = "lm", se = FALSE) +
  stat_cor(method = "pearson", size = 10) +
  labs(x=bquote(~log[10]~ '(Average miRNA expression at '~T[0]~')'),
       y=bquote(~log[10]~ '(Average miRNA expression at '~T[1]~')')) +
  theme_classic() +
  theme(axis.title.x = element_text(size = 30),
        axis.title.y = element_text(size = 30)) +
  guides(alpha=FALSE)
  
ggsave(p, filename = "results/figures/repeated_correlation.png", width = 30, height = 30, units = "cm", dpi = 500)
 


