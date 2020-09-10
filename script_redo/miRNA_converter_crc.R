library(dplyr)

## DATA LOAD ## Change the filename only here ##
countData <- readRDS("C:/Users/amedeo/Desktop/R_Projects/stool/stool_vita/data/ngs_count/vov_raw_count_harmonized.rds")
rosetta <- read.delim("C:/Users/amedeo/Desktop/R_Projects/stool/stool_vita/script/ID_conversion_table_crc.txt")
converted <- c("C:/Users/amedeo/Desktop/R_Projects/stool/stool_vita/data/ngs_count/vov_raw_count_harmonized_converted.rds")


## miRNA name conversion ##

countData <- countData[as.character(rosetta$Initial_ID),]
countData <- cbind(countData, ID = rosetta$Final_ID)
newCount <- countData %>% group_by(ID) %>% summarise_each(sum)
newCount <- as.data.frame(newCount)
rownames(newCount) <- newCount$ID
newCount$ID <- NULL

vov <- newCount

saveRDS(newCount, file = converted)

all.equal(rownames(vov), rownames(crc))
all.equal(rownames(vov), rownames(cel))

dfmerged <- cbind(cel, vov, crc)
saveRDS(dfmerged, "C:/Users/amedeo/Desktop/R_Projects/stool/stool_vita/data/ngs_count/merged_count_harmonized_converted.rds")



