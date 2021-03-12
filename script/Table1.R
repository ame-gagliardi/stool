library(tidyverse)
library(table1)

df <- readRDS("data/clinical/sv_stool_stool_both_samples_validation.rds")

# Age
table1::label(df$age) <- c("Age")
levels(df$age_cat) <- c("<37", "37-53", ">53")
table1::label(df$age_cat) <- c("Age (Tertiles)")

#Sex
df$sex <- as.factor(df$sex)
table1::label(df$sex) <- c("Sex")
levels(df$sex) <- c("Female", "Male")

#BMI
table1::label(df$bmi) <- c("BMI")
# levels(df$bmi_cat) <- c("Normal", "Obese", "Overweight", "Underweight")
levels(df$bmi_cat) <- c("Underweight", "Normal", "Overweight", "Obese")
table1::label(df$bmi_cat) <- c("Bmi (Class)")

#Smoke
df$cigs <- relevel(df$cigs, ref = "never")
df$cigs <- relevel(df$cigs, ref = "former")
df$cigs <- relevel(df$cigs, ref = "never")
levels(df$cigs) <- c("Never", "Former","<16 cigs/day", ">16 cigs/day")
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
df$phys_act <- as.factor(df$phys_act)
levels(df$phys_act) <- c("Active", "Inactive", "Moderately active", "Moderately inactive")
df$phys_act <- relevel(df$phys_act, "Moderately inactive")
df$phys_act <- relevel(df$phys_act, "Moderately active")
df$phys_act <- relevel(df$phys_act, "Inactive")
table1::label(df$phys_act) <- c("Physical activity")

#Menstruation
df$menstruation <- as.factor(df$menstruation)
levels(df$menstruation) <- c("No", "Yes")
table1::label(df$menstruation) <- c("Menstruation")

table1::units(df$age) <- "yrs"
table1::units(df$cigs) <- "cigs/day"

table1::table1(~ age + age_cat + bmi_cat + cigs + phys_act + menstruation | sex, data = df, topclass = "Rtable1-zebra")

