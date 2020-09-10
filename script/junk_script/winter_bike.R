library(dplyr)


xy <- uomo_var_coded %>% 
  dplyr::select(idpaziente, pa_winter_bike_hours)

xx <- donna_var_coded %>% 
  dplyr::select(idpaziente, pa_winter_bike_hours)


both <- bind_rows(xx, xy)


i <- intersect(both$idpaziente, merged_study_cleaned_samples$old_ID)

rownames(both) <- both$idpaziente
rownames(merged_study_cleaned_samples) <- merged_study_cleaned_samples$old_ID


both <- both[i,]
merged_study_cleaned_samples <- merged_study_cleaned_samples[i,]

merged_study_cleaned_samples[,"pa_winter_bike_hours"] <- both$pa_winter_bike_hours
merged_study_cleaned_samples <- moveMe(merged_study_cleaned_samples, c("pa_winter_bike_hours"), "after", "pa_summer_bike_hours")
rownames(merged_study_cleaned_samples) <- merged_study_cleaned_samples$idpaziente


