## 14-10-2020 ##
library(tidyverse)
library(memisc)
# Faccio correzioni ai livelli
df <- as.data.frame(readRDS("C:/Users/amedeo/Desktop/R_Projects/stool/data/clinical/merged_cleaned.rds"))
#Sex
levels(df$sex)
table(df$sex)
levels(df$sex) <- c("female", "male")
table(df$sex)
#Age
levels(df$age_cat)
table(df$age_cat)
#Fumo
levels(df$smoke)
table(df$smoke)
levels(df$smoke) <- c("never", "former", "current")
table(df$smoke)
#Ncigs
curr_day <- paste0(df$smoke, df$curr_day)
curr_day <- stringr::str_remove_all(curr_day, "current")
curr_day <- stringr::str_remove_all(curr_day, "NA")
curr_day[which(curr_day == "")] <- NA
curr_day <- as.factor(curr_day)
levels(curr_day) <- c("_16", "16_", "16_", "16_", "16_", "16_", "_16", "_16", "former", "never")
df$ncigs <- curr_day
levels(df$ncigs)
df$ncigs <- relevel(df$ncigs, "never")
df$ncigs <- relevel(df$ncigs, "former")
df$ncigs <- relevel(df$ncigs, "never")
table(df$smoke, df$ncigs)
#Alcool
levels(df$alcool)
table(df$alcool)
levels(df$alcool) <- c("abstemious", "light", "heavy")
table(df$alcool)
#Wine
colnames(df)[which(colnames(df)=="alco_class")] <- c("wine_consumption")
levels(df$wine_consumption)
table(df$wine_consumption)
levels(df$wine_consumption) <- c("abstemious", "all", "nowine")
table(df$wine_consumption)
table(df$alcool, df$wine_consumption)
#Phys
levels(df$phys_act)
table(df$phys_act)
levels(df$phys_act) <- c("modAct", "inactive", "modInact")
table(df$phys_act)
#Coffee
levels(df$coffee_cat)
#Menstruation
df$mestr_now <- as.factor(df$mestr_now)
levels(df$mestr_now)
table(df$mestr_now)
levels(df$mestr_now) <- c("no", "yes")
table(df$mestr_now)
##
df <- as_tibble(df[,c(2,1,4,5,6,7,8,9,12,13,14,15,17,22,16,19,139)])

saveRDS(df, "data/clinical/de_merged_cleaned.rds")


## 26-10-2020 ##◙

df <- read.delim("data/clinical/de_merged_cleaned.txt")
df[,"coffee_drinker"] <- as.factor(df[,"coffee_cat"])
levels(df$coffee_drinker) <- c("c.drinker", "c.drinker", "c.no_drinker")
df[,"alcool_drinker"] <- as.factor(df[,"alcool"])
levels(df$alcool_drinker) <- c("a.no_drinker", "a.drinker", "a.drinker")
## Terzili età ##
q <- quantile(df$age, probs=c(0,0.3333,0.6667,1), type=6)
segment <- cut(df$age, breaks=q, include.lowest=TRUE)
df[,"age_terz"] <- segment
levels(df$age_terz) <- c("18_37", "37_53", "53_81")
df <- df[,c(1,2,3,4,5,6,7,20,8,9,10,11,12,13,19,14,16,18,15,17)]

write.table(df, file = "data/clinical/de_merged_cleaned.txt", sep = "\t", quote = FALSE, row.names = T)

## 27-10-2020

acque <- read_delim("data/acque/epicdieta_gday.csv", ";", escape_double = FALSE, na = "NA", trim_ws = TRUE)
alcool <- as_tibble(data.frame(acque$idpaziente, acque$TOT.alcool_g.day)) %>% 
  dplyr::rename(id_old = 1, alc_gr_day = 2) %>% 
  dplyr::distinct(id_old, .keep_all = TRUE) %>% 
  dplyr::filter(id_old != "VOV114") %>% 
  dplyr::mutate(alc_gr_day = str_replace_all(alc_gr_day, ",", ".")) %>% 
  dplyr::mutate(alc_gr_day = round(as.numeric(alc_gr_day), digits =2))

alcool$alcool_28 <- with(alcool, memisc::cases("abstemious" = alc_gr_day == 0,
                                               "light" = alc_gr_day >0 & alc_gr_day<28,
                                               "heavy" = alc_gr_day >=28))
detach_package(memisc)
df <- read.delim("data/clinical/de_merged_cleaned.txt")


## CORREGGO GLI ID DI ACQUE CHE NON HANNO GLI 0 DAVANTI ##
xx <- setdiff(df$id_old, alcool$id_old)
xx <- which(df$id_old %in% xx)
db <- df[xx,]

yy <- setdiff(alcool$id_old, df$id_old)
yy <- which(alcool$id_old %in% yy)
alcool$id_old[yy] <- stringr::str_pad(alcool$id_old[yy], 3, side="left", pad="0")
alcool <- as.data.frame(alcool)
rownames(alcool) <- alcool$id_old
##
df <- merge(df, alcool, by = "id_old")
df <- df %>% 
  dplyr::relocate(alcool_28, .after = alcool) %>% 
  dplyr::relocate(id_old, .after = id)

write.table(df, file = "data/clinical/de_merged_cleaned.txt", sep = "\t", quote = FALSE, row.names = T)

## Cambio i livelli di alcool_28
df <- read.delim("data/clinical/de_merged_cleaned.txt")
df$alcool_28 <- as.factor(df$alcool_28)
levels(df$alcool_28) <- c("abstemious.28", "heavy.28", "light.28")
write.table(df, file = "data/clinical/de_merged_cleaned.txt", sep = "\t", quote = FALSE, row.names = T)
