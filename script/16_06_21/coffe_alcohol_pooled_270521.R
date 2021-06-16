## 27.05.21 ##
## Adding new alcohol and coffee measures from acque ##
## Improve df readability ##

library(tidyverse)


df <- readRDS("D:/R_Projects/stool/data/clinical/sv_stool_stool_both_samples_pooled.rds")

## Caffè ##

acque_original <- read.delim("D:/R_Projects/stool/data/original_data/discovery_epicdieta_gday_coffetot.csv", sep = ";") %>% 
  dplyr::select(idpaziente, TOT.Coffe) %>% 
  dplyr::mutate(TOT.caffe = round(TOT.Coffe, 2))

acque_original <- acque_original[-162,]

acque_original$idpaziente <- stringr::str_pad(acque_original$idpaziente, 3, side="left", pad="0")
acque_original[, "ID_IIGM"] <- df[match(acque_original$idpaziente, df$id_old, nomatch = NA), "id"]
acque_original$idpaziente <- NULL
acque_original <- acque_original[,c(3,2)]


acque_pooled <- read.delim("D:/R_Projects/stool/data/original_data/pooled_both_acque.csv", sep = ";", header = T) %>% 
  dplyr::select(ID_IIGM, TOT.caffe) %>% 
  dplyr::mutate(TOT.caffe = round(TOT.caffe/7,2))


new_coffee <- rbind(acque_original, acque_pooled)

df[,"new_coffee"] <- new_coffee[match(df$id, new_coffee$ID_IIGM, nomatch = NA), "TOT.caffe"]

## Alcohol


acque_original <- read.delim("D:/R_Projects/stool/data/original_data/discovery_epicdieta_gday_coffetot.csv", sep = ";") %>% 
  dplyr::select(idpaziente, TOT.alcool_g.day) %>% 
  dplyr::mutate(TOT.alcool = round(TOT.alcool_g.day, 2))

acque_original <- acque_original[-162,]

acque_original$idpaziente <- stringr::str_pad(acque_original$idpaziente, 3, side="left", pad="0")
acque_original[, "ID_IIGM"] <- df[match(acque_original$idpaziente, df$id_old, nomatch = NA), "id"]
acque_original$idpaziente <- NULL
acque_original <- acque_original[,c(3,2)]


acque_pooled <- read.delim("D:/R_Projects/stool/data/original_data/pooled_both_acque.csv", sep = ";", header = T) %>% 
  dplyr::select(ID_IIGM, TOT.alcool) %>% 
  dplyr::mutate(TOT.alcool = round(TOT.alcool/7,2))


new_alcohol <- rbind(acque_original, acque_pooled)

df[,"new_alcool"] <- new_alcohol[match(df$id, new_alcohol$ID_IIGM, nomatch = NA), "TOT.alcool"]

## Improve df ##

df <- df %>% 
  dplyr::select(id, id_old, study, library, class, age, age_cat, sex, bmi, bmi_cat, smoke, cigs, menstruation, MET, phys_act, 
                new_coffee, new_alcool) %>% 
  dplyr::rename(coffee_gr = new_coffee, alcohol_gr = new_alcool)

df[,"coffee"] <- ifelse(df$coffee_gr == 0, "no",
                            ifelse(df$coffee_gr > 0 & df$coffee_gr < 16, "low", "high"))

df[, "alcohol"] <- with(df, cases("no" = alcohol_gr == 0,
                                  "low" = alcohol_gr > 0 & alcohol_gr < 12 & sex == "F",
                                  "high" = alcohol_gr > 0 & alcohol_gr > 12 & sex == "F",
                                  "low" = alcohol_gr > 0 & alcohol_gr < 24 & sex == "M",
                                  "high" = alcohol_gr > 0 & alcohol_gr > 24 & sex == "M"))



