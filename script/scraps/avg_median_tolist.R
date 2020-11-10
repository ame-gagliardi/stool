source("C:/Users/amedeo/Desktop/R_Projects/general_script/useful_functions.R")

df <- readRDS("data/clinical/de_all_merged_cleaned.rds")
cts <- readRDS("data/ngs/normalized_counts.rds")

covars <- colnames(df)
covars <- covars[-c(1,2,3,4,5,6,10)]

avg_list <- vector(mode = "list", length(covars))

for(i in 1:length(covars)){
  refCov <- covars[i]
  avg_list[i] <- list(miRNA_average_class(refCov, df, cts))
  names(avg_list)[i] <- covars[i]
}

median_list <- vector(mode = "list", length(covars))

for(i in 1:length(covars)){
  refCov <- covars[i]
  median_list[i] <- list(miRNA_median_class(refCov, df, cts))
  names(median_list)[i] <- covars[i]
}

wb <- createWorkbook()
datas <- avg_list
sheetnames <- names(datas) # or names(datas) if provided
sheets <- lapply(sheetnames, createSheet, wb = wb)
void <- Map(addDataFrame, datas, sheets)
saveWorkbook(wb, file = "C:/Users/amedeo/Desktop/prova.xlsx")
