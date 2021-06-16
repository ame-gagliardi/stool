libraries <- c("stringi", "stringr", "tidyverse", "memisc")
lapply(libraries, require, character.only = TRUE)

## Cambio il nome delle colonne nel questionario degli uomini ##

# Questionario #
uomo <- read.csv("C:/Users/amedeo/Desktop/R_Projects/sdv_redo/data/acque/uomo_acque_original.csv", sep = ";", header = TRUE)
colnames(uomo) <- tolower(colnames(uomo))

## Domande generali sul fumo ##

colnames(uomo)[11] <- c("smk_sigari_att")
colnames(uomo)[12] <- c("smk_sigari_pass")
colnames(uomo)[13] <- c("smk_pipa_att")
colnames(uomo)[14] <- c("smk_pipa_pass")
colnames(uomo)[15] <- c("smk_smoke_status")

## Domande per i fumatori attuali ##

colnames(uomo)[16] <- c("curr_day")
colnames(uomo)[17] <- c("curr_filtro")
colnames(uomo)[18] <- c("curr_aspirazione")
colnames(uomo)[19] <- c("curr_anni_start")
colnames(uomo)[20] <- c("curr_day_20_filtro")
colnames(uomo)[21] <- c("curr_day_20_no_filtro")
colnames(uomo)[22] <- c("curr_day_20_both_filtro")
colnames(uomo)[23] <- c("curr_day_30_filtro")
colnames(uomo)[24] <- c("curr_day_30_no_filtro")
colnames(uomo)[25] <- c("curr_day_30_both_filtro")
colnames(uomo)[26] <- c("curr_day_40_filtro")
colnames(uomo)[27] <- c("curr_day_40_no_filtro")
colnames(uomo)[28] <- c("curr_day_40_both_filtro")
colnames(uomo)[29] <- c("curr_day_50_filtro")
colnames(uomo)[30] <- c("curr_day_50_no_filtro")
colnames(uomo)[31] <- c("curr_day_50_both_filtro")
colnames(uomo)[32] <- c("curr_quit_month")
colnames(uomo)[33] <- c("curr_quit_long")

## Domande per gli ex fumatori ##

colnames(uomo)[34] <- c("ex_anni_start")
colnames(uomo)[35] <- c("ex_anni_stop")
colnames(uomo)[36] <- c("ex_day_20_filtro")
colnames(uomo)[37] <- c("ex_day_20_no_filtro")
colnames(uomo)[38] <- c("ex_day_20_both_filtro")
colnames(uomo)[39] <- c("ex_day_30_filtro")
colnames(uomo)[40] <- c("ex_day_30_no_filtro")
colnames(uomo)[41] <- c("ex_day_30_both_filtro")
colnames(uomo)[42] <- c("ex_day_40_filtro")
colnames(uomo)[43] <- c("ex_day_40_no_filtro")
colnames(uomo)[44] <- c("ex_day_40_both_filtro")
colnames(uomo)[45] <- c("ex_day_50_filtro")
colnames(uomo)[46] <- c("ex_day_50_no_filtro")
colnames(uomo)[47] <- c("ex_day_50_both_filtro")
colnames(uomo)[48] <- c("ex_ever_stop_before")
colnames(uomo)[49] <- c("ex_long")

## Domande per i non fumatori #à

colnames(uomo)[50] <- c("nev_ever_tried")
colnames(uomo)[51] <- c("nev_ever_tried_occ")
colnames(uomo)[52] <- c("nev_long_occ")
colnames(uomo)[53] <- c("nev_father_smoke")
colnames(uomo)[54] <- c("nev_mother_smoke")
colnames(uomo)[55] <- c("nev_childhood_smoke_exp")
colnames(uomo)[56] <- c("nev_partner_smoke")
colnames(uomo)[57] <- c("nev_partner_pack_daily")
colnames(uomo)[58] <- c("nev_partner_presence")
colnames(uomo)[59] <- c("nev_partner_hours")
colnames(uomo)[60] <- c("nev_other_smoke")
colnames(uomo)[61] <- c("nev_other_smoke_hours")

## Alcool in generale ##

colnames(uomo)[62] <- c("alc_ever_alcohol")
colnames(uomo)[63] <- c("alc_wine")
colnames(uomo)[64] <- c("alc_beer")
colnames(uomo)[65] <- c("alc_spirits")

## Consumo alcool per fasce di età ##

colnames(uomo)[66] <- c("alc_20_wine")
colnames(uomo)[67] <- c("alc_20_beer")
colnames(uomo)[68] <- c("alc_20_spirits")
colnames(uomo)[69] <- c("alc_30_wine")
colnames(uomo)[70] <- c("alc_30_beer")
colnames(uomo)[71] <- c("alc_30_spirits")
colnames(uomo)[72] <- c("alc_40_wine")
colnames(uomo)[73] <- c("alc_40_beers")
colnames(uomo)[74] <- c("alc_40_spirits")
colnames(uomo)[75] <- c("alc_50_wine")
colnames(uomo)[76] <- c("alc_50_beer")
colnames(uomo)[77] <- c("alc_50_spirits")

## Malattie ##

colnames(uomo)[78] <- c("dis_stroke")
colnames(uomo)[79] <- c("dis_stroke_age")
colnames(uomo)[80] <- c("dis_angina")
colnames(uomo)[81] <- c("dis_angina_age")
colnames(uomo)[82] <- c("dis_ictus")
colnames(uomo)[83] <- c("dis_ictus_age")
colnames(uomo)[84] <- c("dis_high_pressure")
colnames(uomo)[85] <- c("dis_high_pressure_age")
colnames(uomo)[86] <- c("dis_high_pressure_meds")
colnames(uomo)[87] <- c("dis_high_col")
colnames(uomo)[88] <- c("dis_high_col_age")
colnames(uomo)[89] <- c("dis_high_col_meds")
colnames(uomo)[90] <- c("dis_diabetes")
colnames(uomo)[91] <- c("dis_diabetes_age")
colnames(uomo)[92] <- c("dis_diabetes_meds")
colnames(uomo)[93] <- c("dis_liver_calc")
colnames(uomo)[94] <- c("dis_liver_calc_age")
colnames(uomo)[95] <- c("dis_gallbladder_removed")
colnames(uomo)[96] <- c("dis_gallbladder_age")
colnames(uomo)[97] <- c("dis_kidney_calc")
colnames(uomo)[98] <- c("dis_kidney_calc_age")
colnames(uomo)[99] <- c("dis_kidney_calc_meds")
colnames(uomo)[100] <- c("dis_intestin_polyps")
colnames(uomo)[101] <- c("dis_intestin_polyps_age")
colnames(uomo)[102] <- c("dis_tumors")
colnames(uomo)[103] <- c("dis_tumors_age")
colnames(uomo)[104] <- c("dis_tumors_site")
colnames(uomo)[105] <- c("dis_ulcer")
colnames(uomo)[106] <- c("dis_ulcer_age")
colnames(uomo)[107] <- c("dis_ulcer_meds")
colnames(uomo)[108] <- c("dis_ulcer_surg")
colnames(uomo)[109] <- c("dis_ulcer_surg_age")

## Storia riproduttiva ##

colnames(uomo)[110] <- c("rep_partner_pregnant")
colnames(uomo)[111] <- c("rep_alive_childs")
colnames(uomo)[112] <- c("rep_male_child")
colnames(uomo)[113] <- c("rep_female_child")

## Varie ##

colnames(uomo)[114] <- c("var_school_age")
colnames(uomo)[115] <- c("var_school_qualification")
colnames(uomo)[116] <- c("var_house_rooms")
colnames(uomo)[117] <- c("var_house_people")
colnames(uomo)[118] <- c("var_fridge_year")
colnames(uomo)[119] <- c("var_car_year")

## Attività fisica ##

colnames(uomo)[120] <- c("pa_summer_walk_hours")
colnames(uomo)[121] <- c("pa_winter_walk_hours")
colnames(uomo)[122] <- c("pa_summer_bike_hours")
colnames(uomo)[123] <- c("pa_winter_walk_hours")
colnames(uomo)[124] <- c("pa_summer_garden_hours")
colnames(uomo)[125] <- c("pa_winter_garden_hours")
colnames(uomo)[126] <- c("pa_brico_hours")
colnames(uomo)[127] <- c("pa_summer_pa_hours")
colnames(uomo)[128] <- c("pa_winter_pa_hours")
colnames(uomo)[129] <- c("pa_sweat")
colnames(uomo)[130] <- c("pa_sweat_hours")
colnames(uomo)[131] <- c("pa_floors_daily")
colnames(uomo)[132] <- c("pa_job")
colnames(uomo)[133] <- c("pa_job_full_time")
colnames(uomo)[134] <- c("pa_job_part_time")

## Storia lavorativa ##

colnames(uomo)[135] <- c("wrk_type_job")
colnames(uomo)[136] <- c("wrk_pesticide_exp")
colnames(uomo)[137] <- c("wrk_pesticide_exp_before_1960")
colnames(uomo)[138] <- c("wrk_pesticide_exp_after_1960")
colnames(uomo)[139] <- c("wrk_steel")
colnames(uomo)[140] <- c("wrk_special_alloy")
colnames(uomo)[141] <- c("wrk_refinery")
colnames(uomo)[142] <- c("wrk_dyes")
colnames(uomo)[143] <- c("wrk_chem_lab")
colnames(uomo)[144] <- c("wrk_hair_dye")
colnames(uomo)[145] <- c("wrk_textile")
colnames(uomo)[146] <- c("wrk_furniture_factory")
colnames(uomo)[147] <- c("wrk_boring_turning")
colnames(uomo)[148] <- c("wrk_welding")
colnames(uomo)[149] <- c("wrk_painting")
colnames(uomo)[150] <- c("wrk_roof_sealing")
colnames(uomo)[151] <- c("wrk_paving")
colnames(uomo)[152] <- c("wrk_demolition")
colnames(uomo)[153] <- c("wrk_truck_driver")
colnames(uomo)[154] <- c("wrk_driver")
colnames(uomo)[155] <- c("wrk_taxi_driver")

## Cambio dei livelli per le variabili che segnano la data al posto dei numeri ##

uomo[,16] <- as.factor(uomo[,16])
levels(uomo[,16]) <- c("","1-3","4-8","14-18","19-23","24-28","29-33","34+","9-13")
uomo[,20] <- as.factor(uomo[,20])
levels(uomo[,20]) <- c("","1-3","4-8","14-18","19-23","34+","9-13")
uomo[,21] <- as.factor(uomo[,21])
levels(uomo[,21]) <- c("","0","4-8","9-13")
uomo[,22] <- as.factor(uomo[,22])
levels(uomo[,22]) <- c("","0","1-3","4-8","14-18","19-23","34+","9-13")
uomo[,23] <- as.factor(uomo[,23])
levels(uomo[,23]) <- c("","0","4-8","14-18","19-23","24-28","34+","9-13")
uomo[,25] <- as.factor(uomo[,25])
levels(uomo[,25]) <- c("","0","4-8","14-18","19-23","24-28","34+","9-13")
uomo[,26] <- as.factor(uomo[,26])
levels(uomo[,26]) <- c("","0","1-3","4-8","14-18","19-23","24-28","29-33","34+","9-13")
uomo[,28] <- as.factor(uomo[,28])
levels(uomo[,28]) <- c("","0","4-8","14-18","19-23","24-28","29-33","34+","9-13")
uomo[,29] <- as.factor(uomo[,29])
levels(uomo[,29]) <- c("","0","1-3","4-8","14-18","19-23","29-33","34+","9-13")
uomo[,31] <- as.factor(uomo[,31])
levels(uomo[,31]) <- c("","0","1-3","4-8","14-18","19-23","29-33","34+","9-13")
uomo[,36] <- as.factor(uomo[,36])
levels(uomo[,36]) <- c("","0","1-3","4-8","14-18","19-23","24-28","34+","9-13")
uomo[,37] <- as.factor(uomo[,37])
levels(uomo[,37]) <- c("0","1-3","4-8","14-18","19-23","9-13")
uomo[,38] <- as.factor(uomo[,38])
levels(uomo[,38]) <- c("","0","1-3","4-8","14-18","19-23","24-28","34+","9-13")
uomo[,39] <- as.factor(uomo[,39])
levels(uomo[,39]) <- c("","0","1-3","4-8","14-18","19-23","24-28","29-33","34+","9-13")
uomo[,40] <- as.factor(uomo[,40])
levels(uomo[,40]) <- c("0","4-8","14-18","19-23","24-28")
uomo[,41] <- as.factor(uomo[,41])
levels(uomo[,41]) <- c("","0","1-3","4-8","14-18","19-23","24-28","29-33","34+","9-13")
uomo[,42] <- as.factor(uomo[,42])
levels(uomo[,42]) <- c("","0","1-3","4-8","14-18","19-23","24-28","29-33","34+","9-13")
uomo[,44] <- as.factor(uomo[,44])
levels(uomo[,44]) <- c("","0","1-3","4-8","14-18","19-23","24-28","29-33","34+","9-13")
uomo[,45] <- as.factor(uomo[,45])
levels(uomo[,45]) <- c("0","1-3","4-8","14-18","19-23","24-28","29-33","34+","9-13")
uomo[,47] <- as.factor(uomo[,47])
levels(uomo[,47]) <- c("0","4-8","14-18","19-23","29-33","34+","9-13")
uomo[,131] <- as.factor(uomo[,131])
levels(uomo[,131]) <- c("1-2","3-4","5-6","7-9","10-12","13-15","16-20","0")

## Cambio le classi di age e bmi ##

uomo$age <- as.character(uomo$age)
uomo$age <- str_replace(uomo$age, ",", ".")
uomo$age <- as.numeric(uomo$age)
uomo$age <- round(uomo$age)

uomo$bmi <- as.character(uomo$bmi)
uomo$bmi <- str_replace(uomo$bmi, ",", ".")
uomo$bmi <- as.numeric(uomo$bmi)

## Trasformo in minuscole il nome delle variabili ##

colnames(uomo) <- tolower(colnames(uomo))



## CAMBIO IL CODING DELLE VARIABILI ##

# Fumo #

uomo$smk_sigari_att <- as.factor(uomo$smk_sigari_att)
levels(uomo$smk_sigari_att) <- c("0","1")
uomo$smk_sigari_pass <- as.factor(uomo$smk_sigari_pass)
levels(uomo$smk_sigari_pass) <- c("0","1")
uomo$smk_pipa_att <- as.factor(uomo$smk_pipa_att)
levels(uomo$smk_pipa_att) <- c("0","1")
uomo$smk_pipa_pass <- as.factor(uomo$smk_pipa_pass)
levels(uomo$smk_pipa_pass) <- c("0","1")
uomo$smk_smoke_status <- as.factor(uomo$smk_smoke_status)
levels(uomo$smk_smoke_status) <- c(NA, "1", "0", "2")
uomo$curr_filtro <- as.factor(uomo$curr_filtro)
levels(uomo$curr_filtro) <- c(NA,"2","0")
uomo$curr_aspirazione <- as.factor(uomo$curr_aspirazione)
levels(uomo$curr_aspirazione) <- c(NA,"0","1","2")
uomo$curr_quit_month <- as.factor(uomo$curr_quit_month)
levels(uomo$curr_quit_month) <-c (NA,"1","0")
uomo$ex_ever_stop_before <- as.factor(uomo$ex_ever_stop_before)
levels(uomo$ex_ever_stop_before) <- c(NA, "1", "0")
uomo$nev_ever_tried <- as.factor(uomo$nev_ever_tried)
levels(uomo$nev_ever_tried) <- c("0", "1")
uomo$nev_ever_tried_occ <- as.factor(uomo$nev_ever_tried_occ)
levels(uomo$nev_ever_tried_occ) <- c("0","1")
uomo$nev_father_smoke <- as.factor(uomo$nev_father_smoke)
levels(uomo$nev_father_smoke) <- c("0","1")
uomo$nev_mother_smoke <- as.factor(uomo$nev_mother_smoke)
levels(uomo$nev_mother_smoke) <- c("0","1")
uomo$nev_childhood_smoke_exp <- as.factor(uomo$nev_childhood_smoke_exp)
levels(uomo$nev_childhood_smoke_exp) <- c(NA,"2","3","1",NA,"0")
uomo$nev_partner_smoke <- as.factor(uomo$nev_partner_smoke)
levels(uomo$nev_partner_smoke) <- c(NA, "0", "1", "2", "0")
uomo$nev_partner_presence <- as.factor(uomo$nev_partner_presence)
levels(uomo$nev_partner_presence) <- c(NA,"1")
uomo$nev_other_smoke <- as.factor(uomo$nev_other_smoke)
levels(uomo$nev_other_smoke) <- c("0","1")

# Alcool #

uomo$alc_ever_alcohol <- as.factor(uomo$alc_ever_alcohol)
levels(uomo$alc_ever_alcohol) <- c(NA,"0","1")
uomo$alc_wine <- as.factor(uomo$alc_wine)
levels(uomo$alc_wine) <- c("1","0","2")
uomo$alc_beer <- as.factor(uomo$alc_beer)
levels(uomo$alc_beer) <- c("1","0","2")
uomo$alc_spirits <- as.factor(uomo$alc_spirits)
levels(uomo$alc_spirits) <- c("1","0","2")

# Malattie #

uomo$dis_stroke <- as.factor(uomo$dis_stroke)
levels(uomo$dis_stroke) <- c("0","1")
uomo$dis_angina <- as.factor(uomo$dis_angina)
levels(uomo$dis_angina) <- c("0","1")
uomo$dis_ictus <- as.factor(uomo$dis_ictus)
levels(uomo$dis_ictus) <- c("0","1")
uomo$dis_high_pressure <- as.factor(uomo$dis_high_pressure)
levels(uomo$dis_high_pressure) <- c("0","1")
uomo$dis_high_col <- as.factor(uomo$dis_high_col)
levels(uomo$dis_high_col) <- c("0","1")
uomo$dis_high_col_meds <- as.factor(uomo$dis_high_col_meds)
levels(uomo$dis_high_col_meds) <- c("0","1")
uomo$dis_diabetes <- as.factor(uomo$dis_diabetes)
levels(uomo$dis_diabetes) <- c("0","1")
uomo$dis_diabetes_meds <- as.factor(uomo$dis_diabetes_meds)
levels(uomo$dis_diabetes_meds) <- c("0","1")
uomo$dis_liver_calc <- as.factor(uomo$dis_liver_calc)
levels(uomo$dis_liver_calc) <- c("0","1")
uomo$dis_gallbladder_removed <- as.factor(uomo$dis_gallbladder_removed)
levels(uomo$dis_gallbladder_removed) <- c("0","1")
uomo$dis_kidney_calc <- as.factor(uomo$dis_kidney_calc)
levels(uomo$dis_kidney_calc) <- c("0","1")
uomo$dis_kidney_calc_meds <- as.factor(uomo$dis_kidney_calc_meds)
levels(uomo$dis_kidney_calc_meds) <- c("0","1")
uomo$dis_intestin_polyps <- as.factor(uomo$dis_intestin_polyps)
levels(uomo$dis_intestin_polyps) <- c("0","1")
uomo$dis_tumors <- as.factor(uomo$dis_tumors)
levels(uomo$dis_tumors) <- c("0","1")
uomo$dis_ulcer <- as.factor(uomo$dis_ulcer)
levels(uomo$dis_ulcer) <- c("0","1")
uomo$dis_ulcer_meds <- as.factor(uomo$dis_ulcer_meds)
levels(uomo$dis_ulcer_meds) <- c("0","1")
uomo$dis_ulcer_surg <- as.factor(uomo$dis_ulcer_surg)
levels(uomo$dis_ulcer_surg) <- c("0","1")

# Storia riproduttiva #

uomo$rep_partner_pregnant <- as.factor(uomo$rep_partner_pregnant)
levels(uomo$rep_partner_pregnant) <- c("0","1")

uomo$rep_alive_childs <- as.factor(uomo$rep_alive_childs)
levels(uomo$rep_alive_childs) <- c(NA,"0","1")

# Bmi ed età #

uomo$age_cat <- with(uomo, cases("_40"= age<40,
                                 "40_60"= age>=40 & age<60,
                                 "60_"= age>=60))

uomo <- dplyr::relocate(uomo, "age_cat", .after = "age")


uomo$bmi_cat <- with(uomo, cases("1" = bmi<18.5,
                                 "0" = bmi>=18.5 & bmi<25,
                                 "2" = bmi>=25 & bmi<30,
                                 "3" = bmi>=30))

uomo <- dplyr::relocate(uomo, "bmi_cat", .after = "bmi")


## Salvo il dataset ##

# saveRDS(uomo, file = "C:/Users/amedeo/Desktop/R_Projects/sdv_redo/data_redo/uomo.rds")
saveRDS(uomo, file = "C:/Users/amedeo/Desktop/check/uomo.rds")
