library(tidyverse)

df <- readRDS("data/clinical/de_all_merged_cleaned.rds")
covar <- c("age_cat", "bmi_cat")
norm <- as.data.frame(t(read.delim("data/normalized_counts.txt")))
## Function to detect all the levels in the specified variables

covarLevels <- function(x,y){
  levName <- vector()
  for(i in 1:length(x)){
    levName <- append(levName, levels(y[,x[i]]))
  }
  return(levName)
}


## Function to calculate the mean of miRNA for every single levels in the dataframe

miRNA_average <- function(x, df){
  df$y <- x
}



sex_linMod <- function(x, db=df_sex) {
  db$y <- x
  fit <- glm(y ~ sex + age ,data=db)
  n <- length(resid(fit))
  coeff <- summary(fit)$coeff
  out <- coeff["sex0", c(1,2,3)]
  out2 <- coeff[1, c(1,2,3)]
  out3 <- c(out,out2,n)
  return(out3)
}


