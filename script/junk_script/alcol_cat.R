library(memisc)

df <- readRDS("C:/Users/UappaSguappa/Desktop/R_projects/stool/stool_vita/data/clinical/merged_study_cleaned_samples.rds")

df$alc_cat <- with(df, cases("abstemious" = alc_gr_day == 0,
                             "light" = alc_gr_day >0 & alc_gr_day <=6,
                             "habitual" = alc_gr_day >6 & alc_gr_day <=12,
                             "heavy" = alc_gr_day >12 &alc_gr_day <=24,
                             "over" = alc_gr_day >24))
