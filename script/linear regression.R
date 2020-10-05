library(tidyverse)

# Loading data
cts <- readRDS("C:/Users/UappaSguappa/Desktop/R_projects/stool/data/ngs/merged_harmonized_converted.rds")
df <- as.data.frame(readRDS("C:/Users/UappaSguappa/Desktop/R_projects/stool/data/clinical/merged_cleaned.rds"))
rownames(df) <- df$id

load("C:/Users/UappaSguappa/Desktop/R_projects/stool/results/full_model/age/age.rda")
age <- res %>% 
  dplyr::filter(baseMean >= 15 & padj < 0.05)
rm(res, res_1, res_2, res_3, coldata)

load("C:/Users/UappaSguappa/Desktop/R_projects/stool/results/full_model/bmi/bmi.rda")
bmi <- res %>% 
  dplyr::filter(baseMean >= 15 & padj < 0.05)
rm(res, coldata)

load("C:/Users/UappaSguappa/Desktop/R_projects/stool/results/full_model/sex/sex.rda")
sex <- res %>% 
  dplyr::filter(baseMean >= 15 & padj < 0.05)
rm(res, coldata)

## Linear Regression functions
age_linMod <- function(x, db=df_age) {
  db$y <- x
  db$age <- (db$age-mean(db$age))/sd(db$age)
  
  fit <- glm(y ~ age + sex ,data=db)
  n <- length(resid(fit))
  coeff <- summary(fit)$coeff
  out <- coeff["age", c(1,2,3)]
  out <- c(out,n)
  return(out)
}

bmi_linMod <- function(x, db=df_bmi) {
  db$y <- x
  db$age <- (db$age-mean(db$age))/sd(db$age)
  
  fit <- glm(y ~ bmi + age + sex ,data=db)
  n <- length(resid(fit))
  coeff <- summary(fit)$coeff
  out <- coeff["bmi", c(1,2,3)]
  out <- c(out,n)
  return(out)
}

sex_linMod <- function(x, db=df_sex) {
  db$y <- x
  db$age <- (db$age-mean(db$age))/sd(db$age)
  fit <- glm(y ~ sex + age ,data=db)
  n <- length(resid(fit))
  coeff <- summary(fit)$coeff
  out <- coeff["sex", c(1,2,3)]
  out <- c(out,n)
  return(out)
}

## Age linear regression

cts_age <- cts[which(rownames(cts) %in% age$miRNA),]

i <- intersect(rownames(df), colnames(cts_age))
cts_age <- cts_age[,i]
df_age <- df[i,]

ageRes <- apply(cts_age,1,age_linMod)
ageRes <- as.data.frame(t(ageRes))
ageRes$miRNA <- rownames(ageRes)
ageRes <- as_tibble(ageRes) %>% 
  dplyr::rename(t = `t value`, se = `Std. Error`, n = V4) %>% 
  dplyr::mutate(p.value = pnorm(-abs(t))*2) %>% 
  dplyr::relocate(miRNA, .before = "Estimate") %>%
  dplyr::relocate(p.value, .before = "n") %>% 
  dplyr::arrange(p.value)


ageRes <- ageRes[order(ageRes$p),]

## BMI linear regression

cts_bmi <- cts[which(rownames(cts) %in% bmi$miRNA),]

i <- intersect(rownames(df), colnames(cts_bmi))
cts_bmi <- cts_bmi[,i]
df_bmi <- df[i,]

bmiRes <- apply(cts_bmi,1,bmi_linMod)
bmiRes <- t(bmiRes)
bmiRes <- as.data.frame(bmiRes)
bmiRes$p <- pnorm(-abs(bmiRes$t))*2
bmiRes <- bmiRes[order(bmiRes$p),]


# ## Sex linear regression
# 
# cts_sex <- cts[which(rownames(cts) %in% sex$miRNA),]
# 
# i <- intersect(rownames(df), colnames(cts_sex))
# cts_sex <- cts_sex[,i]
# df_sex <- df[i,]
# 
# sexRes <- apply(cts_sex,1,sex_linMod)
# sexRes <- t(sexRes)
# sexRes <- as.data.frame(sexRes)
# sexRes$p <- pnorm(-abs(sexRes$t))*2
# sexRes <- sexRes[order(sexRes$p),]



