source("C:/Users/amedeo/Desktop/R_Projects/r2excel/R/r2excel.r")
library(xlsx)

refCov <- "age_cat"
files <- list.files(paste0("results/prova/tables/", refCov))

for(i in 1:length(files)){
  assign(paste0(refCov,i), read.delim(paste0("results/prova/tables/",refCov,"/",files[i])))
}

xx <- grep(refCov, ls())
cat <- vector(length = length(xx))
for(i in 1:length(xx)){
  cat[i] <- ls()[i]
}

comb <- combn(cat, m = 1)

cov1 <- get(comb[1,1])
cov2 <- get(comb[1,2])
cov3 <- get(comb[1,3])

tmp.1 <- merge(cov1, cov2, by = "ID")
tmp.2 <- merge(tmp.1, cov3, by = "ID")
assign(refCov, tmp.2)

filename <- "prova_results.xlsx"
wb <- createWorkbook(type = "xlsx")

for(i in 1:length(refCov)){
  sheet <- createSheet(wb, sheetName = refCov[i])
  xlsx.addTable(wb, sheet, refCov, startCol = 1, startRow = 2)
xlsx::saveWorkbook(wb, filename)

}