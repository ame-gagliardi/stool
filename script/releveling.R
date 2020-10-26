## 14-10-2020 ##
library(tidyverse)
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

