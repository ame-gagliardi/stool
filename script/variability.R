
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

data <- as.data.frame(matrix(NA, nrow = 3524, ncol = 30, dimnames = list(c(colnames(norm)),
                                                                        c(levName))))

var <- c("ncigs")
cat <- levels(df$ncigs)

for(z in 1:length(cat)){
  for(i in 1:length(rownames(data))){
    df[,18] <- norm[,i]
    colnames(df)[18] <- colnames(norm)[i]
    db <- df %>% 
      dplyr::filter(ncigs == cat[z])
    data[i,cat[z]] <- round(mean(db[,18], na.rm = TRUE), digits = 3)
  }
}


data[,"w.abst"] <- NULL
data[,"cigs.nev"] <- NULL
data[,"cigs.form"] <- NULL


saveRDS(data, file = "data/miRNA_mean.rds")
