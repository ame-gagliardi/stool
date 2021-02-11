library(corrplot)
library(RColorBrewer)

# DATA LOADING

cts <- readRDS("C:/Users/amedeo/Desktop/R_Projects/stool/results/repeated_samples/repeated_cts_norm.rds")
df <- readRDS("C:/Users/amedeo/Desktop/R_Projects/stool/results/repeated_samples/repeated_df.rds")

keep <- apply(cts,1,median, na.rm=TRUE) > 10
cts <- cts[keep,]
colnames(cts) <- c("LA", "AR", "SG", "BP", "AN", "ST", "la", "ar", "sg", "bp", "an", "st")

##

cormat <- cor(cts, method="spearman")
res1 <- cor.mtest(cts, conf.level = .95)

colcor <- rev(colorRampPalette(brewer.pal(11, "RdBu"))(256))

corrplot(cormat, tl.cex = 1.5,
         hclust.method = "ward.D2",
         order="hclust",
         type="lower",
         tl.col="black",
         col=colcor,
         p.mat = res1$p,
         sig.level = .05,
         insig = "blank")


