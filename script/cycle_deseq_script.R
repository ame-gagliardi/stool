library(tidyverse)
library(DESeq2)

count.path <- c("data/ngs/raw_counts.rds")       # Raw counts path
covar.path <- c("data/clinical/de_all_merged_cleaned.rds")       # Covars path
results.path <- c("results/by_sex/female/")                         # Results path
cts <- readRDS(count.path)                                     # Counts
df <- readRDS(covar.path)                                      # Covars
rownames(df) <- df$id                                 

#analysis <- c("alcool (28 gr/day treshold)")                     # Type of analysis

variables <- c("age_cat", "smoke", "ncigs", "alcool", "wine_consumption",  # Covariates to cycle
               "phys_act", "coffee_cat", "mestr_now", "bmi_cat", "alcool_28", "alcool_drinker", "coffee_drinker", "age_terz") 

tofactor <- c("study","library","id_pat","age_cat","age_terz", "sex", "smoke", "ncigs", "alcool", "wine_consumption",  # Covariates to cycle
              "phys_act", "coffee_cat", "mestr_now", "bmi_cat", "alcool_28", "alcool_drinker", "coffee_drinker")

df[tofactor] <- lapply(df[tofactor], as.factor)
sapply(df[tofactor], class) 

print(Sys.time())
for(k in 1:length(variables)){
  print("Loading a fresh dataset")
  db <- df
  covars <- c("library","age_cat",variables[k]) # Covariates to include in the model. The last one is the the one to analyze
  if(last(covars) == "bmi_cat"){
    xx <- table(db$bmi_cat)[1]
    levels(db$bmi_cat) <- c("normal", "over", "over", NA)
    yy <- table(db$bmi_cat)[1]
    check <- xx == yy
    if(check == TRUE){
      print("BMI levels changed successfully")
      rm(xx,yy,check)
    }
  }

  print(paste("Deseq on", variables[k]))
  
  check <- all(colnames(cts) == rownames(db))
  if(check == FALSE){
    print(paste0("Tidying count matrix and dataset on ", last(covars)))
    i <- intersect(rownames(db), colnames(cts))
    db <- db[i,]
    cts <- cts[,i]
  }
  
  navalues <- rownames(db)[which(is.na(db[,last(covars)]))]
  m <- length(db[,last(covars)])             
  db <- tidyr::drop_na(db, last(covars))
  n <- length(db[,last(covars)])
  n.na <- m-n     
  print(paste("I've dropped",n.na,"samples for NA: ", paste0(navalues, collapse = " ,")))
  line <- paste0(Sys.time()," - Samples excluded from ", last(covars), " deseq analyses (n = ",n.na,"): ", paste0(navalues, collapse = " ,"))
  write(line,file=paste0(results.path, "de_full_output.txt"),append=TRUE)
  
  if(n.na>0){
    print(paste0("Tidying count matrix and dataset after NA dropping on ", last(covars)))
    i <- intersect(colnames(cts), rownames(db)) 
    cts <- cts[,i]                              
    db <- db[i,]
  }
  
  new.check <- all.equal(colnames(cts), rownames(db))
  if(new.check == TRUE){
    print("I'll proceed to the deseq analysis")
    model <- as.formula(paste("~", paste(covars,collapse = "+")))
    dds <- DESeqDataSetFromMatrix(countData = cts,
                                  colData = db,
                                  design = model)
    
    dds <- DESeq(dds)
    print(paste("Saving deseq results of", last(covars)))
    saveRDS(dds, paste0(results.path, last(covars),".rds"))
  } else {
    print(paste0("Houston we have a problem on", last(covars)))
  }
}
print(Sys.time())
  
  