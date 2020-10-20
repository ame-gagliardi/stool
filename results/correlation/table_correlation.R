##
age <- readRDS("C:/Users/amedeo/Desktop/R_Projects/stool/results/full_model/df_nofilter/age_cat_tot_full.rds")
alcool <- readRDS("C:/Users/amedeo/Desktop/R_Projects/stool/results/full_model/df_nofilter/alcool_tot_full.rds")
bmi <- readRDS("C:/Users/amedeo/Desktop/R_Projects/stool/results/full_model/df_nofilter/bmi_cat_tot_full.rds")
coffee <- readRDS("C:/Users/amedeo/Desktop/R_Projects/stool/results/full_model/df_nofilter/coffee_cat_tot_full.rds")
mestr <- readRDS("C:/Users/amedeo/Desktop/R_Projects/stool/results/full_model/df_nofilter/mestr_now_tot_full.rds")
ncigs <- readRDS("C:/Users/amedeo/Desktop/R_Projects/stool/results/full_model/df_nofilter/ncigs_tot_full.rds")
phys <- readRDS("C:/Users/amedeo/Desktop/R_Projects/stool/results/full_model/df_nofilter/phys_act_tot_full.rds")
sex <- readRDS("C:/Users/amedeo/Desktop/R_Projects/stool/results/full_model/df_nofilter/sex_tot_full.rds")
smoke <- readRDS("C:/Users/amedeo/Desktop/R_Projects/stool/results/full_model/df_nofilter/smoke_tot_full.rds")
wine <- readRDS("C:/Users/amedeo/Desktop/R_Projects/stool/results/full_model/df_nofilter/wine_consumption_tot_full.rds")
##
age <- age %>% 
  dplyr::mutate(from = "age") %>% 
  dplyr::filter(baseMean >= 15 & padj < 0.05)

alcool <- alcool %>% 
  dplyr::mutate(from = "alcool") %>% 
  dplyr::filter(baseMean >= 15 & padj < 0.05)

bmi <- bmi %>% 
  dplyr::mutate(from = "bmi") %>% 
  dplyr::filter(baseMean >= 15 & padj < 0.05)

coffee <- coffee %>% 
  dplyr::mutate(from = "coffee") %>% 
  dplyr::filter(baseMean >= 15 & padj < 0.05)

mestr <- mestr %>% 
  dplyr::mutate(from = "mestr") %>% 
  dplyr::filter(baseMean >= 15 & padj < 0.05)

ncigs <- ncigs %>% 
  dplyr::mutate(from = "ncigs") %>% 
  dplyr::filter(baseMean >= 15 & padj < 0.05)

phys <- phys %>% 
  dplyr::mutate(from = "phys") %>% 
  dplyr::filter(baseMean >= 15 & padj < 0.05)

sex <- sex %>% 
  dplyr::mutate(from = "sex") %>% 
  dplyr::filter(baseMean >= 15 & padj < 0.05)

smoke <- smoke %>% 
  dplyr::mutate(from = "smoke") %>% 
  dplyr::filter(baseMean >= 15 & padj < 0.05)

wine <- wine %>% 
  dplyr::mutate(from = "wine") %>% 
  dplyr::filter(baseMean >= 15 & padj < 0.05)
##
sig_tot <- bind_rows(age, alcool, bmi, coffee, mestr, ncigs, phys, sex, smoke, wine) %>% 
  dplyr::select(miRNA, from) %>% 
  dplyr::distinct(miRNA, .keep_all = TRUE)


dplyr::group_by(miRNA) %>% 
  dplyr::filter(n()>1)
##

saveRDS(sig_tot, "results/sig_unique.rds")


