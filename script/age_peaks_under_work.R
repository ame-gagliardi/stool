library(ggpubr)
library(tidyverse)

# Still in work
# Script for density plot with peak identifier

df <- readRDS("data/clinical/de_merged_cleaned.rds")

var1 <- "male"   # Insert the name of the variable to densify
var2 <- "female"

male <- df[which(df$sex == var1),]
female <- df[which(df$sex == var2),]

# Max uomo
which.max(density(male$age)$y)
density(male$age)$x[240]
density(male$age)$y[density(male$age)$x < 40]
MaxY<- max(density(male$age)$y[density(male$age)$x < 40])
which(density(male$age)$y == MaxY)
density(male$age)$x[195]
# Max donna
which.max(density(female$age)$y)
density(female$age)$x[165]
density(female$age)$y[density(female$age)$x > 40]
MaxY<- max(density(female$age)$y[density(female$age)$x > 40])
which(density(female$age)$y == MaxY)
density(female$age)$x[283]



ggplot(df, aes(x=age)) +
  geom_density(aes(x = age, color = sex)) +
  geom_vline(aes(xintercept = density(male$age)$x[240]), color = "blue", linetype = "dashed", size = 0.2) +
  geom_vline(aes(xintercept = density(male$age)$x[195]), color = "blue", linetype = "dashed", size = 0.2) +
  geom_vline(aes(xintercept = density(female$age)$x[165]), color = "red", linetype = "dashed", size = 0.2) +
  geom_vline(aes(xintercept = density(female$age)$x[283]), color = "red", linetype = "dashed", size = 0.2) +
  geom_rug(aes(x = age, y = 0, color= sex), position = position_jitter(height = 0)) +
  geom_text(aes(x = density(male$age)$x[240] +1 , y = 0.002, label = round(density(male$age)$x[240], digits =2)), size =3) +
  geom_text(aes(x = density(male$age)$x[195] +1 , y = 0.002, label = round(density(male$age)$x[195], digits =2)), size =3) +
  geom_text(aes(x = density(female$age)$x[165] +1 , y = 0.002, label = round(density(female$age)$x[165], digits =2)), size =3) +
  geom_text(aes(x = density(female$age)$x[283] +1 , y = 0.002, label = round(density(female$age)$x[283], digits =2)), size =3)

