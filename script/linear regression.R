library(tidyverse)
library(ggpmisc)

# Loading data
cts <- readRDS("C:/Users/amedeo/Desktop/R_projects/stool/data/ngs/merged_harmonized_converted.rds")
df <- as.data.frame(readRDS("C:/Users/amedeo/Desktop/R_projects/stool/data/clinical/merged_cleaned.rds"))
rownames(df) <- df$id
levels(df$sex) <- c("1","0")

load("C:/Users/amedeo/Desktop/R_projects/stool/results/full_model/age/age.rda")
age <- res %>% 
  dplyr::filter(baseMean >= 15 & padj < 0.05)
rm(res, res_1, res_2, res_3, coldata)

load("C:/Users/amedeo/Desktop/R_projects/stool/results/full_model/bmi/bmi.rda")
bmi <- res %>% 
  dplyr::filter(baseMean >= 15 & padj < 0.05)
rm(res, coldata)

load("C:/Users/amedeo/Desktop/R_projects/stool/results/full_model/sex/sex.rda")
sex <- res %>% 
  dplyr::filter(baseMean >= 15 & padj < 0.05)
rm(res, coldata)

## Linear Regression functions

age_linMod <- function(x, db=df_age) {
  db$y <- x
  fit <- glm(y ~ age + sex ,data=db)
  n <- length(resid(fit))
  coeff <- summary(fit)$coeff
  out <- coeff["age", c(1,2,3)]
  out2 <- coeff[1, c(1,2,3)]
  out3 <- c(out,out2,n)
  return(out3)
}

bmi_linMod <- function(x, db=df_bmi) {
  db$y <- x
  fit <- glm(y ~ bmi + sex + age ,data=db)
  n <- length(resid(fit))
  coeff <- summary(fit)$coeff
  out <- coeff["bmi", c(1,2,3)]
  out2 <- coeff[1, c(1,2,3)]
  out3 <- c(out,out2,n)
  return(out3)
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


## Age linear regression

cts_age <- cts[which(rownames(cts) %in% age$miRNA),]

i <- intersect(rownames(df), colnames(cts_age))
cts_age <- cts_age[,i]
df_age <- df[i,]

ageRes <- apply(cts_age,1,age_linMod)
ageRes <- as.data.frame(t(ageRes))
ageRes$miRNA <- rownames(ageRes)
colnames(ageRes) <- c("Est.Age", "SE.Age", "T.Age", "Intercept", "SE.Int", "T.Int", "N", "miRNA")
ageRes <- as_tibble(ageRes) %>% 
  dplyr::mutate(p.value = pnorm(-abs(T.Age))*2) %>% 
  dplyr::relocate(miRNA, .before = "Est.Age") %>%
  dplyr::relocate(p.value, .before = "N") %>% 
  dplyr::arrange(p.value) %>% 
  dplyr::filter(p.value < 0.05)

saveRDS(ageRes, file = c("C:/Users/amedeo/Desktop/R_Projects/stool/results/linear_regression/age.rds"))

## BMI linear regression

cts_bmi <- cts[which(rownames(cts) %in% bmi$miRNA),]

i <- intersect(rownames(df), colnames(cts_bmi))
cts_bmi <- cts_bmi[,i]
df_bmi <- df[i,]

bmiRes <- apply(cts_bmi,1,bmi_linMod)
bmiRes <- as.data.frame(t(bmiRes))
bmiRes$miRNA <- rownames(bmiRes)
colnames(bmiRes) <- c("Est.Bmi", "SE.Bmi", "T.Bmi", "Intercept", "SE.Int", "T.Int", "N", "miRNA")
bmiRes <- as_tibble(bmiRes) %>% 
  dplyr::mutate(p.value = pnorm(-abs(T.Bmi))*2) %>% 
  dplyr::relocate(miRNA, .before = "Est.Bmi") %>%
  dplyr::relocate(p.value, .before = "N") %>% 
  dplyr::arrange(p.value) %>% 
  dplyr::filter(p.value < 0.05)

saveRDS(bmiRes, file = c("C:/Users/amedeo/Desktop/R_Projects/stool/results/linear_regression/bmi.rds"))

## Sex linear regression

cts_sex <- cts[which(rownames(cts) %in% sex$miRNA),]

i <- intersect(rownames(df), colnames(cts_sex))
cts_sex <- cts_sex[,i]
df_sex <- df[i,]

sexRes <- apply(cts_sex,1,sex_linMod)
sexRes <- as.data.frame(t(sexRes))
sexRes$miRNA <- rownames(sexRes)
colnames(sexRes) <- c("Est.Sex", "SE.Sex", "T.Sex", "Intercept", "SE.Int", "T.Int", "N", "miRNA")
sexRes <- as_tibble(sexRes) %>% 
  dplyr::mutate(p.value = pnorm(-abs(T.Sex))*2) %>% 
  dplyr::relocate(miRNA, .before = "Est.Sex") %>%
  dplyr::relocate(p.value, .before = "N") %>% 
  dplyr::arrange(p.value) %>% 
  dplyr::filter(p.value < 0.05)

saveRDS(sexRes, file = c("C:/Users/amedeo/Desktop/R_Projects/stool/results/linear_regression/sex.rds"))


rm(list=ls())
