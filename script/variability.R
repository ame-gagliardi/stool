library(tidyverse)
library(DESeq2)

df <- as.data.frame(readRDS("data/clinical/de_merged_cleaned.rds"))
rownames(df) <- df$id


norm <- read.table("data/normalized_counts.txt", sep = "\t")
i <- intersect(rownames(df), colnames(norm))

norm <- as.data.frame(t(norm[,i]))
df <- df[i,]
all.equal(rownames(norm), rownames(df))

levName <- c(levels(df$age_cat), levels(df$sex), levels(df$alcool), levels(df$wine_consumption), levels(df$smoke),
             levels(df$ncigs), levels(df$phys_act), levels(df$bmi_cat), levels(df$coffee_cat), levels(df$mestr_now))

levName[c(9,15,16)] <- c("w.abst", "cigs.nev", "cigs.form")

dataMean <- as.data.frame(matrix(NA, nrow = 3524, ncol = 30, dimnames = list(c(colnames(norm)),
                                                                        c(levName))))
covars <- c("age_cat", "sex", "alcool", "wine_consumption", "smoke", "ncigs", "phys_act", "bmi_cat", "coffee_cat", "mestr_now")

print(Sys.time())
for(k in 1:length(covars)){
  var <- covars[k]
  cat <- levels(df[,var])

  for(z in 1:length(cat)){
    for(i in 1:length(rownames(dataMean))){
      df[,18] <- norm[,i]
      colnames(df)[18] <- colnames(norm)[i]
      db <- df 
      db <- db %>% 
        dplyr::filter(db[,var] == cat[z])
       dataMean[i,cat[z]] <- round(mean(db[,18], na.rm = TRUE), digits = 3)
    }
  }
}
print(Sys.time())




dataMean[,"w.abst"] <- NULL
dataMean[,"cigs.nev"] <- NULL
dataMean[,"cigs.form"] <- NULL


saveRDS(dataMean, file = "data/miRNA_mean.rds")




dataMedian <- as.data.frame(matrix(NA, nrow = 3524, ncol = 30, dimnames = list(c(colnames(norm)),
                                                                         c(levName))))

print(Sys.time())
for(k in 1:length(covars)){
  var <- covars[k]
  cat <- levels(df[,var])
  
  for(z in 1:length(cat)){
    for(i in 1:length(rownames(dataMedian))){
      df[,18] <- norm[,i]
      colnames(df)[18] <- colnames(norm)[i]
      db <- df 
      db <- db %>% 
        dplyr::filter(db[,var] == cat[z])
      dataMedian[i,cat[z]] <- round(median(db[,18], na.rm = TRUE), digits = 3)
    }
  }
}
print(Sys.time())

saveRDS(dataMedian, file = "data/miRNA_median.rds")
