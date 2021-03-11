library(tidyverse)
library(table1)

df <- readRDS("data/clinical/sv_stool_both_samples_validation.rds")

# Age
table1::label(df$age) <- c("Age")
levels(df$age_cat) <- c("18-37", "37-53", "53-81")
table1::label(df$age_cat) <- c("Age (Tertiles)")

#Sex
table1::label(df$sex) <- c("Sex")
levels(df$sex) <- c("Female", "Male")

#BMI
table1::label(df$bmi) <- c("BMI")
# levels(df$bmi_cat) <- c("Normal", "Obese", "Overweight", "Underweight")
table1::label(df$bmi_cat) <- c("Bmi (Class)")

#Smoke
levels(df$cigs) <- c("<16 cigs/day", ">16 cigs/day", "Former", "Never")
table1::label(df$cigs) <- c("Smoking status")

#Alcool
levels(df$alcool_28) <- c("Abstemious", "Habitual", "Occasional")
df$alcool_28 <- relevel(df$alcool_28, "Occasional")
df$alcool_28 <- relevel(df$alcool_28, "Abstemious")
table1::label(df$alcool_28) <- c("Alcohol consumption")
#Wine
levels(df$wine_consumption) <- c("Abstemious", "All type of alcohol", "No wine")
df$wine_consumption <- relevel(df$wine_consumption, "No wine")
df$wine_consumption <- relevel(df$wine_consumption, "Abstemious")
table1::label(df$wine_consumption) <- c("Wine consumption")

#Coffee
levels(df$coffee_cat) <- c("Habitual", "Occasional", "Non drinker")
df$coffee_cat <- relevel(df$coffee_cat, "Non drinker")
df$coffee_cat <- relevel(df$coffee_cat, "Occasional")
df$coffee_cat <- relevel(df$coffee_cat, "Non drinker")
table1::label(df$coffee_cat) <- c("Coffee consumption")

#Physical activity
df$phys_act_2 <- as.factor(df$phys_act_2)
levels(df$phys_act_2) <- c("Moderately active", "Inactive", "Moderately active", "Moderately inactive")
df$phys_act_2 <- relevel(df$phys_act_2, "Moderately inactive")
df$phys_act_2 <- relevel(df$phys_act_2, "Moderately active")
table1::label(df$phys_act_2) <- c("Physical activity")

#Menstruation
levels(df$mestr_now) <- c("No", "Yes")
table1::label(df$mestr_now) <- c("Menstruation")

table1::units(df$age) <- "Years"
table1::units(df$ncigs) <- "Cigs/day"

table1::table1(~ age + age_terz + bmi + bmi_cat + ncigs + alcool_28 + wine_consumption + 
                 coffee_cat + phys_act_2 + mestr_now | sex, data = df, topclass = "Rtable1-zebra")

