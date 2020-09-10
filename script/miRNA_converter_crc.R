library(dplyr)

## DATA LOAD ## Change the filename only here ##
countData <- readRDS("C:/Users/amedeo/Desktop/R_Projects/sdv_redo/data_redo/ngs/celiac_harmonized_raw.rds")
rosetta <- read.delim("C:/Users/amedeo/Desktop/R_Projects/sdv_redo/script_redo/ID_conversion_table_crc.txt")
converted <- c("C:/Users/amedeo/Desktop/R_Projects/sdv_redo/data_redo/ngs/celiac_harmonized_converted.rds")


## miRNA name conversion ##

countData <- countData[as.character(rosetta$Initial_ID),]
countData <- cbind(countData, ID = rosetta$Final_ID)
newCount <- countData %>% group_by(ID) %>% summarise_each(sum)
newCount <- as.data.frame(newCount)
rownames(newCount) <- newCount$ID
newCount$ID <- NULL

cel <- newCount

saveRDS(newCount, file = converted)

all.equal(rownames(vov_cts), rownames(crc_cts))
all.equal(rownames(vov_cts), rownames(cel_cts))

dfmerged <- cbind(cel_cts, vov_cts, crc_cts)
saveRDS(dfmerged, "C:/Users/amedeo/Desktop/R_Projects/sdv_redo/data_redo/ngs/merged_harmonized_converted.rds")



