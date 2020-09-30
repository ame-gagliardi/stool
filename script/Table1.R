library(tidyverse)
library(table1)

df <- as_tibble(readRDS("C:/Users/amedeo/Desktop/R_Projects/stool/data/clinical/merged_cleaned.rds")) %>% 
  dplyr::select(id, study, library, id_pat, age, age_cat, sex, bmi_cat, smoke, ncigs,phys_act, alcool, coffee_cat, no_vino, mestr_now) %>% 
  dplyr::mutate(ncigs = as.factor(ncigs), no_vino = as.factor(no_vino), mestr_now = as.factor(mestr_now)) %>% 
  dplyr::filter(id != "VOV114") %>% 
  dplyr::mutate(ncigs = droplevels(ncigs))

levels(df$smoke) <- c("Never", "Former", "Current")
levels(df$ncigs) <- c("<16", " >16")
levels(df$sex) <- c("Female", "Male")
levels(df$bmi_cat) <- c(NA, "Normal", "Overweight", "Overweight")
levels(df$id_pat) <- c("Control", "Hemorrhoids", "Onnivore", "Vegetarian", "Vegan", "Celiac under diet")
levels(df$phys_act) <- c("Moderately Active", "Inactive", "Moderately Inactive")
df$phys_act <- relevel(df$phys_act, "Moderately Inactive")
df$phys_act <- relevel(df$phys_act, "Moderately Active")
levels(df$no_vino) <- c("All types of alcohol", "No wine consumption")
levels(df$mestr_now) <- c("No", "Yes")
levels(df$alcool) <- c("Abstemious", "Light drinker", "Heavy drinker")
levels(df$coffee_cat) <- c("No", "Light drinker", "Heavy drinker")
levels(df$age_cat) <- c("<40", "40-60", ">60")

table1::label(df$ncigs) <- c("Number of cigarettes")
table1::label(df$id_pat) <- c("Condition")
table1::label(df$age) <- c("Age")
table1::label(df$age_cat) <- c("Age class")
table1::label(df$sex) <- c("Sex")
table1::label(df$bmi_cat) <- c("BMI")
table1::label(df$smoke) <- c("Smoking status")
table1::label(df$phys_act) <- c("Physical activity")
table1::label(df$alcool) <- c("Alcohol consumption")
table1::label(df$coffee_cat) <- c("Coffee consumption")
table1::label(df$no_vino) <- c("Wine consumption")
table1::label(df$mestr_now) <- c("Menstruation")

table1::units(df$age) <- "Years"
table1::units(df$ncigs) <- "Cigs/day"


table1::table1(~ age + age_cat + bmi_cat + smoke+ ncigs + alcool + no_vino + coffee_cat + phys_act + mestr_now| sex, data = df, topclass = "Rtable1-zebra",
               footnote = "BMI: Underweight < 18, Normal 18-25, Overweight 25-30, Obese > 30 -- Alcohol: Abstemious 0 gr/day, Light 0.1-18.5 gr/day, Heavy > 18.5 gr/day -- Physical activity: Women - Inactive (<46.89); Mod. inactive (46.89-82.14); Mod. active (82.14-120.14); Active (> 120.14); Men - Inactive (< 34.00); Mod. inactive (34.00-56.76); Mod. active (56.76-87.06); Active (> 87.06)")
