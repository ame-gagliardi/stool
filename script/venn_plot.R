library(VennDiagram)

set1 <- res_1$gene
set2 <- res_2$gene
set3 <- res_3$gene

myCol <- brewer.pal(3, "Dark2")

venn.diagram(
  x = list(set1, set2, set3),
  category.names = c("Ex_NoSmk" , "Smk_NoSmk " , "Smk_ExSmk"),
  filename = '#14_venn_diagramm.png',
  output=TRUE,
  
  # Output features
  imagetype="png" ,
  height = 700 , 
  width = 700 , 
  resolution = 300,
  compression = "lzw",
  
  # Circles
  lwd = 2,
  lty = 'blank',
  fill = myCol,
  
  # Numbers
  cex = .6,
  fontface = "bold",
  fontfamily = "sans",
  
  # Set names
  cat.cex = 0.6,
  cat.fontface = "bold",
  cat.default.pos = "outer",
  cat.pos = c(-27, 27, 135),
  cat.dist = c(0.055, 0.055, 0.085),
  cat.fontfamily = "sans",
  rotation = 1
)
