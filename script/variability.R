library(tidyverse)
library(DESeq2)

df <- read.delim("data/clinical/de_merged_cleaned.txt")
df[,c(3,4,5,7,8,9,11:20)] <- lapply(df[,c(3,4,5,7,8,9,11:20)], as.factor)
str(df)

norm <- read.table("data/normalized_counts.txt", sep = "\t")
i <- intersect(rownames(df), colnames(norm))

norm <- as.data.frame(t(norm[,i]))
df <- df[i,]
all.equal(rownames(norm), rownames(df))

covars <- c("age_cat", "sex", "alcool", "wine_consumption", "smoke", "ncigs", "phys_act", "bmi_cat", "coffee_cat", "mestr_now", "coffee_drinker",
            "alcool_drinker", "age_terz")

levName <- c(levels(df$age_cat), levels(df$sex), levels(df$alcool), levels(df$wine_consumption), levels(df$smoke),
             levels(df$ncigs), levels(df$phys_act), levels(df$bmi_cat), levels(df$coffee_cat), levels(df$mestr_now), levels(df$coffee_drinker),
             levels(df$alcool_drinker), levels(df$age_terz))

levName[c(9,17,18)] <- c("w.abst", "cigs.form", "cigs.nev")

dataMean <- as.data.frame(matrix(NA, nrow = 3524, ncol = length(levName), dimnames = list(c(colnames(norm)),
                                                                        c(levName))))
# Mean

print(Sys.time())
for(k in 1:length(covars)){
  var <- covars[k]
  cat <- levels(df[,var])

  for(z in 1:length(cat)){
    for(i in 1:length(rownames(dataMean))){
      db <- df 
      db[,ncol(df)+1] <- norm[,i]
      colnames(db)[ncol(df)+1] <- colnames(norm)[i]
      db <- db %>% 
        dplyr::filter(db[,var] == cat[z])
       dataMean[i,cat[z]] <- round(mean(db[,ncol(db)], na.rm = TRUE), digits = 3)
    }
  }
}
print(Sys.time())

dataMean[,"w.abst"] <- NULL
dataMean[,"cigs.nev"] <- NULL
dataMean[,"cigs.form"] <- NULL

saveRDS(dataMean, file = "data/miRNA_mean.rds")

# Median


dataMedian <- as.data.frame(matrix(NA, nrow = 3524, ncol = length(levName), dimnames = list(c(colnames(norm)),
                                                                              c(levName))))

print(Sys.time())
for(k in 1:length(covars)){
  var <- covars[k]
  cat <- levels(df[,var])
  
  for(z in 1:length(cat)){
    for(i in 1:length(rownames(dataMedian))){
      db <- df 
      db[,ncol(df)+1] <- norm[,i]
      colnames(db)[ncol(df)+1] <- colnames(norm)[i]
      db <- db %>% 
        dplyr::filter(db[,var] == cat[z])
      dataMedian[i,cat[z]] <- round(median(db[,ncol(db)], na.rm = TRUE), digits = 3)
    }
  }
}
print(Sys.time())

dataMedian[,"w.abst"] <- NULL
dataMedian[,"cigs.nev"] <- NULL
dataMedian[,"cigs.form"] <- NULL

saveRDS(dataMedian, file = "data/miRNA_median.rds")

