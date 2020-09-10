library(table1)

df <- readRDS("C:/Users/UappaSguappa//Desktop/R_Projects/stool/stool_vita/data/clinical/merged_study_cleaned_samples.rds")

df$smoke <- factor(df$smoke,
                    levels = c("Non_fumatore", "Ex_fumatore", "Fumatore"),
                    labels = c("Non smoker","Ex smoker","Smoker"))


df$bmi_cat <- factor(df$bmi_cat,
                     levels = c(0,1,2,3),
                     labels = c("Normal","Underweight","Overweight","Obese"))

df$sex <- factor(df$sex,
                 levels = c("Donna","Uomo"),
                 labels = c("Female","Male"))
df$age <- as.numeric(as.character(df$age))

colnames(df)[c(3,6,12,13,127)] <- c("Study","Age","BMI","Smoke", "Alcohol")

table1(~ Age + BMI + Smoke + Study + Alcohol| sex, data = df, topclass = "Rtable1-zebra")


