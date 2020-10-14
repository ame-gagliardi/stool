library(tidyverse)
library(qqman)
library(circlize)


load("C:/Users/amedeo/Desktop/R_Projects/stool/results/full_model/age/age.rda")
df <- as_tibble(res)
rm(coldata, res, res_1, res_2, res_3)

mirname <- df$miRNA
mirname <- stringr::str_remove_all(mirname, ":Novel")
mirname <- stringr::str_replace_all(mirname, "-miR", "-mir")
mirname <- stringr::str_remove_all(mirname, "-5p")
mirname <- stringr::str_remove_all(mirname, "-3p")
mirname <- stringr::str_replace(mirname,"\\,.*","")
#write.table(mirname, "C:/Users/amedeo/Desktop/mirname.txt", row.names = FALSE, col.names = FALSE, quote = FALSE, sep = "\n")

pos <- rio::import("C:/Users/amedeo/Desktop/mirpos.txt")

df$miRNA <- mirname
df <- df %>% 
  dplyr::distinct(miRNA, .keep_all = TRUE)

anno <- as_tibble(merge(df, pos, by.x ="miRNA", by.y = "name")) %>% 
  dplyr::select(miRNA, baseMean, log2FoldChange,padj,comparison, chrom) %>% 
  dplyr::arrange(chrom) %>% 
  dplyr::relocate(chrom, .before = "baseMean") %>% 
  dplyr::mutate(chrom = stringr::str_remove_all(chrom, "chr")) %>% 
  dplyr::mutate(chrom = stringr::str_replace_all(chrom, "X", "23")) %>% 
  dplyr::mutate(chrom = stringr::str_replace_all(chrom, "Y", "24")) %>% 
  dplyr::mutate(orChrom = "chr", chrom = as.numeric(chrom)) %>% 
  dplyr::mutate(orChrom = paste0(orChrom,chrom)) %>% 
  dplyr::mutate(sig = padj < 0.05 & baseMean >=15) %>% 
  dplyr::mutate(sig = as.character(ifelse(sig == TRUE, "red","black")))

##

circos.par("track.height" = 0.3, "gap.degree" = 3.5)
circos.initialize(factors = anno$orChrom, x = anno$log2FoldChange)
circos.track(factors = anno$orChrom, ylim = c(0,2014),
             panel.fun = function(x, y) {
               circos.text(CELL_META$xcenter, CELL_META$cell.ylim[2] + mm_y(5), 
                           CELL_META$sector.index)
               circos.axis(labels.cex = 0.6)
             })
circos.trackPoints(anno$orChrom, anno$log2FoldChange, anno$baseMean, col = anno$sig, pch = 16)
circos.yaxis(side = "left", at = c(0,1), tick = FALSE, labels.niceFacing = TRUE, labels.cex = 0.8)
circos.clear()
