libraries <- c("stringi", "stringr", "tidyverse", "memisc")
lapply(libraries, require, character.only = TRUE)

## Cambio il nome delle colonne nel questionario degli uomini ##

# Questionario #
donna <- read.csv("C:/Users/amedeo/Desktop/R_Projects/sdv_redo/data/acque/donna_acque_original.csv", sep = ";", header = TRUE)
colnames(donna) <- tolower(colnames(donna))
## Mestruazioni ##

colnames(donna)[11] <- c("mestr_anni_first")
colnames(donna)[12] <- c("mestr_anni_regular")
colnames(donna)[13] <- c("mestr_meds_regular")
colnames(donna)[14] <- c("mestr_3040_freq")
colnames(donna)[15] <- c("mestr_now")
colnames(donna)[16] <- c("mestr_last_12")
colnames(donna)[17] <- c("mestr_pill")
colnames(donna)[18] <- c("mestr_horm")
colnames(donna)[19] <- c("mestr_age_last")
colnames(donna)[20] <- c("mestr_no_last_12")
colnames(donna)[21] <- c("mestr_no_last_12_freq")

## Gravidanze ##

colnames(donna)[22] <- c("preg_ever")
colnames(donna)[23] <- c("preg_first_status")
colnames(donna)[24] <- c("preg_no_tried")
colnames(donna)[25] <- c("preg_misc")
colnames(donna)[26] <- c("preg_misc_many")
colnames(donna)[27] <- c("preg_misc_hosp")
colnames(donna)[28] <- c("preg_misc_first_age")
colnames(donna)[29] <- c("preg_misc_last_age")
colnames(donna)[30] <- c("preg_abort")
colnames(donna)[31] <- c("preg_abort_many")
colnames(donna)[32] <- c("preg_abort_first_age")
colnames(donna)[33] <- c("preg_abort_last_age")
colnames(donna)[34] <- c("preg_stillbirth")
colnames(donna)[35] <- c("preg_stillbirth_many")
colnames(donna)[36] <- c("preg_stillbirth_first_age")
colnames(donna)[37] <- c("preg_stillbirth_last_age")
colnames(donna)[38] <- c("preg_live")
colnames(donna)[39] <- c("preg_live_many")
colnames(donna)[40] <- c("preg_live_first_age")
colnames(donna)[41] <- c("preg_live_male")
colnames(donna)[42] <- c("preg_live_female")
colnames(donna)[43] <- c("preg_breast")
colnames(donna)[44] <- c("preg_breast_wks")
colnames(donna)[45] <- c("preg_breast_month")
colnames(donna)[46] <- c("preg_live_second_age")
colnames(donna)[47] <- c("preg_live_male")
colnames(donna)[48] <- c("preg_live_female")
colnames(donna)[49] <- c("preg_breast_2")
colnames(donna)[50] <- c("preg_breast_2_wks")
colnames(donna)[51] <- c("preg_breast_2_month")
colnames(donna)[52] <- c("preg_live_third_age")
colnames(donna)[53] <- c("preg_live_male")
colnames(donna)[54] <- c("preg_live_female")
colnames(donna)[55] <- c("preg_breast_3")
colnames(donna)[56] <- c("preg_breast_3_wks")
colnames(donna)[57] <- c("preg_breast_3_month")
colnames(donna)[58] <- c("preg_live_last_age")
colnames(donna)[59] <- c("preg_live_male")
colnames(donna)[60] <- c("preg_live_female")
colnames(donna)[61] <- c("preg_breast_last")
colnames(donna)[62] <- c("preg_breast_last_wks")
colnames(donna)[63] <- c("preg_breast_last_month")

## Contraccezione ##

colnames(donna)[64] <- c("pill_ever")
colnames(donna)[65] <- c("pill_yrs")
colnames(donna)[66] <- c("pill_start_yrs")
colnames(donna)[67] <- c("pill_now")
colnames(donna)[68] <- c("pill_stop_yrs")

## Ormoni menopausa ##

colnames(donna)[69] <- c("horm_ever")
colnames(donna)[70] <- c("horm_yrs")
colnames(donna)[71] <- c("horm_start_yrs")
colnames(donna)[72] <- c("horm_now")
colnames(donna)[73] <- c("horm_type")
colnames(donna)[74] <- c("horm_coil")
colnames(donna)[75] <- c("horm_coil_now")

## Sterilità ##

colnames(donna)[76] <- c("ster_medic")
colnames(donna)[77] <- c("ster_confirmed")
colnames(donna)[78] <- c("ster_meds")
colnames(donna)[79] <- c("ster_surg")

## Attività fisica ##

colnames(donna)[80] <- c("pa_house_hours")
colnames(donna)[81] <- c("pa_job")
colnames(donna)[82] <- c("pa_job_full_time")
colnames(donna)[83] <- c("pa_job_part_time")
colnames(donna)[84] <- c("pa_floors_day")
colnames(donna)[85] <- c("pa_summer_walk_hours")
colnames(donna)[86] <- c("pa_winter_walk_hours")
colnames(donna)[87] <- c("pa_summer_bike_hours")
colnames(donna)[88] <- c("pa_winter_bike_hours")
colnames(donna)[89] <- c("pa_summer_garden_hours")
colnames(donna)[90] <- c("pa_winter_garden_hours")
colnames(donna)[91] <- c("pa_brico_hours")
colnames(donna)[92] <- c("pa_summer_pa")
colnames(donna)[93] <- c("pa_winter_pa")
colnames(donna)[94] <- c("pa_sweat")
colnames(donna)[95] <- c("pa_sweat_hours")

## Fumo in generale ##

colnames(donna)[96] <- c("smk_smoke_status")


## Domande per i fumatori  ##

colnames(donna)[97] <- c("curr_day")
colnames(donna)[98] <- c("curr_filtro")
colnames(donna)[99] <- c("curr_aspirazione")
colnames(donna)[100] <- c("curr_anni_start")
colnames(donna)[101] <- c("curr_day_20_filtro")
colnames(donna)[102] <- c("curr_day_20_no_filtro")
colnames(donna)[103] <- c("curr_day_20_both_filtro")
colnames(donna)[104] <- c("curr_day_30_filtro")
colnames(donna)[105] <- c("curr_day_30_no_filtro")
colnames(donna)[106] <- c("curr_day_30_both_filtro")
colnames(donna)[107] <- c("curr_day_40_filtro")
colnames(donna)[108] <- c("curr_day_40_no_filtro")
colnames(donna)[109] <- c("curr_day_40_both_filtro")
colnames(donna)[110] <- c("curr_day_50_filtro")
colnames(donna)[111] <- c("curr_day_50_no_filtro")
colnames(donna)[112] <- c("curr_day_50_both_filtro")
colnames(donna)[113] <- c("curr_quit_month")
colnames(donna)[114] <- c("curr_quit_long")

## Domande per gli ex fumatori ##


colnames(donna)[115] <- c("ex_anni_start")
colnames(donna)[116] <- c("ex_anni_stop")
colnames(donna)[117] <- c("ex_day_20_filtro")
colnames(donna)[118] <- c("ex_day_20_no_filtro")
colnames(donna)[119] <- c("ex_day_20_both_filtro")
colnames(donna)[120] <- c("ex_day_30_filtro")
colnames(donna)[121] <- c("ex_day_30_no_filtro")
colnames(donna)[122] <- c("ex_day_30_both_filtro")
colnames(donna)[123] <- c("ex_day_40_filtro")
colnames(donna)[124] <- c("ex_day_40_no_filtro")
colnames(donna)[125] <- c("ex_day_40_both_filtro")
colnames(donna)[126] <- c("ex_day_50_filtro")
colnames(donna)[127] <- c("ex_day_50_no_filtro")
colnames(donna)[128] <- c("ex_day_50_both_filtro")
colnames(donna)[129] <- c("ex_ever_stop_before")
colnames(donna)[130] <- c("ex_long")

## Domande per i non fumatori ##

colnames(donna)[131] <- c("nev_ever_tried")
colnames(donna)[132] <- c("nev_evet_tried_occ")
colnames(donna)[133] <- c("nev_long_occ")
colnames(donna)[134] <- c("nev_father_smoke")
colnames(donna)[135] <- c("nev_mother_smoke")
colnames(donna)[136] <- c("nev_childhood_smoke_exp")
colnames(donna)[137] <- c("nev_partner_smoke")
colnames(donna)[138] <- c("nev_partner_pack_daily")
colnames(donna)[139] <- c("nev_partner_presence")
colnames(donna)[140] <- c("nev_partner_hours")
colnames(donna)[141] <- c("nev_other_smoke")
colnames(donna)[142] <- c("nev_other_smoke_hours")

## Alcool in generale ##

colnames(donna)[143] <- c("alc_ever_alcohol")
colnames(donna)[144] <- c("alc_wine")
colnames(donna)[145] <- c("alc_beer")
colnames(donna)[146] <- c("alc_spirits")

## Consumo di alcolici per fasce di età ##

colnames(donna)[147] <- c("alc_20_wine")
colnames(donna)[148] <- c("alc_20_beer")
colnames(donna)[149] <- c("alc_20_spirits")
colnames(donna)[150] <- c("alc_30_wine")
colnames(donna)[151] <- c("alc_30_beer")
colnames(donna)[152] <- c("alc_30_spirits")
colnames(donna)[153] <- c("alc_40_wine")
colnames(donna)[154] <- c("alc_40_beers")
colnames(donna)[155] <- c("alc_40_spirits")
colnames(donna)[156] <- c("alc_50_wine")
colnames(donna)[157] <- c("alc_50_beer")
colnames(donna)[158] <- c("alc_50_spirits")

## Malattie ##

colnames(donna)[159] <- c("dis_stroke")
colnames(donna)[160] <- c("dis_stroke_age")
colnames(donna)[161] <- c("dis_angina")
colnames(donna)[162] <- c("dis_angina_age")
colnames(donna)[163] <- c("dis_ictus")
colnames(donna)[164] <- c("dis_ictus_age")
colnames(donna)[165] <- c("dis_high_pressure")
colnames(donna)[166] <- c("dis_high_pressure_age")
colnames(donna)[167] <- c("dis_high_pressure_meds")
colnames(donna)[168] <- c("dis_high_col")
colnames(donna)[169] <- c("dis_high_col_age")
colnames(donna)[170] <- c("dis_high_col_meds")
colnames(donna)[171] <- c("dis_diabetes")
colnames(donna)[172] <- c("dis_diabetes_age")
colnames(donna)[173] <- c("dis_diabetes_meds")
colnames(donna)[174] <- c("dis_liver_calc")
colnames(donna)[175] <- c("dis_liver_calc_age")
colnames(donna)[176] <- c("dis_gallbladder_removed")
colnames(donna)[177] <- c("dis_gallbladder_age")
colnames(donna)[178] <- c("dis_kidney_calc")
colnames(donna)[179] <- c("dis_kidney_calc_age")
colnames(donna)[180] <- c("dis_kidney_calc_meds")
colnames(donna)[181] <- c("dis_intestin_polyps")
colnames(donna)[182] <- c("dis_intestin_polyps_age")
colnames(donna)[183] <- c("dis_tumors")
colnames(donna)[184] <- c("dis_tumors_age")
colnames(donna)[185] <- c("dis_tumors_site")
colnames(donna)[186] <- c("dis_ulcer")
colnames(donna)[187] <- c("dis_ulcer_age")
colnames(donna)[188] <- c("dis_ulcer_meds")
colnames(donna)[189] <- c("dis_ulcer_surg")
colnames(donna)[190] <- c("dis_ulcer_surg_age")
colnames(donna)[191] <- c("dis_breast_surg")
colnames(donna)[192] <- c("dis_breast_surg_age")
colnames(donna)[193] <- c("dis_uter_rem")
colnames(donna)[194] <- c("dis_uter_rem_age")
colnames(donna)[195] <- c("dis_ovaries_surg")
colnames(donna)[196] <- c("dis_ovaries_surg_age")
colnames(donna)[197] <- c("dis_ovaries_rem")

## Varie ##

colnames(donna)[198] <- c("var_marital_status")
colnames(donna)[199] <- c("var_school_age")
colnames(donna)[200] <- c("var_school_qualification")
colnames(donna)[201] <- c("var_house_rooms")
colnames(donna)[202] <- c("var_house_people")
colnames(donna)[203] <- c("var_fridge_year")
colnames(donna)[204] <- c("var_car_year")


## Cambio dei livelli per le variabili che segnano la data al posto dei numeri ##

donna[,16] <- as.factor(donna[,16])
levels(donna[,16]) <- c("","1-3","6-9","10+")

donna[,21] <- as.factor(donna[,21])
levels(donna[,21]) <- c("","1-3","4-5","6-9")

donna[,44] <- as.factor(donna[,44])
levels(donna[,44]) <- c("","2-3","4-5","6-7","1")

donna[,45] <- as.factor(donna[,45])
levels(donna[,45]) <- c("","4-5","6-7","8-9","10-11","12+","2","3")

donna[,45] <- as.factor(donna[,45])
levels(donna[,50]) <- c("4-5","6-7","1")

donna[,50] <- as.factor(donna[,50])
levels(donna[,51]) <- c("","4-5","6-7","8-9","10-11","12+","2",3)

donna[,56] <- as.factor(donna[,56])
levels(donna[,56]) <- c("6-7")

donna[,57] <- as.factor(donna[,57])
levels(donna[,57]) <- c("","4-5","6-7","10-11","12+","2")

donna[,84] <- as.factor(donna[,84])
levels(donna[,84]) <- c("1-2","3-4","5-6","7-9","10-12","13-15","16-20","21+","0")

donna[,97] <- as.factor(donna[,97])
levels(donna[,97]) <- c("","1-3","4-8","14-18","19-23","24-27","29-33","9-13")

donna[,101] <- as.factor(donna[,101])
levels(donna[,101]) <- c("","0","1-3","4-8","14-18","19-23","24-28","29-33","9-13")

donna[,102] <- as.factor(donna[,102])
levels(donna[,102]) <- c("","0","14-18","9-13")

donna[,103] <- as.factor(donna[,103])
levels(donna[,103]) <- c("","0","1-3","4-8","14-18","19-23","29-33","34+","9-13")

donna[,104] <- as.factor(donna[,104])
levels(donna[,104]) <- c("","1-3","4-8","14-18","19-23","24-28","29-33","34+","9-13")

donna[,105] <- as.factor(donna[,105])
levels(donna[,105]) <- c("","0","1-3")

donna[,106] <- as.factor(donna[,106])
levels(donna[,106]) <- c("","0","1-3","4-8","19-23","24-28","29-33","9-13")

donna[,107] <- as.factor(donna[,107])
levels(donna[,107]) <- c("","0","1-3","4-8","14-18","19-23","24-28","29-33","9-13")

donna[,109] <- as.factor(donna[,109])
levels(donna[,109]) <- c("","0","1-3","4-8","14-18","19-23","24-28","29-33","9-13")

donna[,110] <- as.factor(donna[,110])
levels(donna[,110]) <- c("","0","1-3","4-8","14-18","19-23","29-33","9-13")

donna[,112] <- as.factor(donna[,112])
levels(donna[,112]) <- c("","0","1-3","4-8","14-18","19-23","29-33","9-13")

donna[,117] <- as.factor(donna[,117])
levels(donna[,117]) <- c("","0","1-3","4-8","19-23","9-13")

donna[,118] <- as.factor(donna[,118])
levels(donna[,118]) <- c("","0","4-8")

donna[,119] <- as.factor(donna[,119])
levels(donna[,119]) <- c("","0","1-3","4-8","19-23","9-13")

donna[,120] <- as.factor(donna[,120])
levels(donna[,120]) <- c("","0","1-3","4-8","14-18","19-23","9-13")

donna[,121] <- as.factor(donna[,121])
levels(donna[,121]) <- c("","0","4-8")

donna[,122] <- as.factor(donna[,122])
levels(donna[,122]) <- c("","0","1-3","4-8","14-18","19-23","9-13")

donna[,123] <- as.factor(donna[,123])
levels(donna[,123]) <- c("","0","1-3","4-8","19-23","9-13")

donna[,125] <- as.factor(donna[,125])
levels(donna[,125]) <- c("","0","1-3","4-8","19-23","9-13")

donna[,126] <- as.factor(donna[,126])
levels(donna[,126]) <- c("0","4-8","14-18","19-23","9-13")

donna[,128] <- as.factor(donna[,128])
levels(donna[,128]) <- c("0","4-8","14-18","19-23","9-13")

donna[,133] <- as.factor(donna[,133])
levels(donna[,133]) <- c("","4-6","7-10","1 anno o meno", "2-3 anni")

donna[,142] <- as.factor(donna[,142])
levels(donna[,142]) <- c("3-4","5-6","1-2 ore","7 ore o più","Meno di 1 ora")


## Cambio le classi di age ed età ##

donna$age <- as.character(donna$age)
donna$age <- str_replace(donna$age, ",", ".")
donna$age <- as.numeric(donna$age)
donna$age <- round(donna$age)

donna$bmi <- as.character(donna$bmi)
donna$bmi <- str_replace(donna$bmi, ",", ".")
donna$bmi <- as.numeric(donna$bmi)

## Trasformo in minuscolo tutte i nomi delle variabili ##

colnames(donna) <- tolower(colnames(donna))


## CAMBIO IL CODING DELLE VARIABILI ##

# Mestruazioni #

table(donna$mestr_meds_regular)
levels(donna$mestr_meds_regular) <- c("0", "1")
table(donna$mestr_meds_regular)

table(donna$mestr_now)
levels(donna$mestr_now) <- c("0", "1")
table(donna$mestr_now)

table(donna$mestr_pill)
levels(donna$mestr_pill) <- c(NA,"0", "1")
table(donna$mestr_pill)

table(donna$mestr_horm)
levels(donna$mestr_horm) <- c(NA,"0", "1")
table(donna$mestr_horm)

table(donna$mestr_no_last_12)
levels(donna$mestr_no_last_12) <- c(NA,"0", "1")
table(donna$mestr_no_last_12)

# Gravidanza #

table(donna$preg_ever)
levels(donna$preg_ever) <- c("0", "1")
table(donna$preg_ever)

table(donna$preg_no_tried)
levels(donna$preg_no_tried) <- c(NA, "0", "1")
table(donna$preg_no_tried)

table(donna$preg_misc)
levels(donna$preg_misc) <- c(NA, "0", "1")
table(donna$preg_misc)

table(donna$preg_abort)
levels(donna$preg_abort) <- c(NA, "0", "1")
table(donna$preg_abort)

table(donna$preg_stillbirth)
levels(donna$preg_stillbirth) <- c(NA, "1")
table(donna$preg_stillbirth)

table(donna$preg_live)
levels(donna$preg_live) <- c(NA, "1", "0")
table(donna$preg_live)

table(donna$preg_breast)
levels(donna$preg_breast) <- c(NA,"1", "0")
table(donna$preg_breast)

table(donna$preg_breast_2)
levels(donna$preg_breast_2) <- c(NA, "1", "0")
table(donna$preg_breast_2)

table(donna$preg_breast_3)
levels(donna$preg_breast_3) <- c(NA, "1", "0")
table(donna$preg_breast_3)

table(donna$preg_breast_last)
levels(donna$preg_breast_last) <- c(NA,"0")
table(donna$preg_breast_last)

# Contraccezione, menopausa e sterilità #

table(donna$pill_ever)
levels(donna$pill_ever) <- c("0", "1")
table(donna$pill_ever)

table(donna$pill_now)
levels(donna$pill_now) <- c("0", "1")
table(donna$pill_now)

table(donna$horm_ever)
levels(donna$horm_ever) <- c("0", "1")
table(donna$horm_ever)

table(donna$horm_now)
levels(donna$horm_now) <- c(NA,"0", "1")
table(donna$horm_now)

table(donna$horm_coil)
levels(donna$horm_coil) <- c("0", "1")
table(donna$horm_coil)

table(donna$horm_coil_now)
levels(donna$horm_coil_now) <- c(NA, "0", "1")
table(donna$horm_coil_now)

table(donna$ster_medic)
levels(donna$ster_medic) <- c("0", "1")
table(donna$ster_medic)

table(donna$ster_confirmed)
levels(donna$ster_confirmed) <- c(NA, "0", "1")
table(donna$ster_confirmed)

table(donna$ster_meds)
levels(donna$ster_meds) <- c(NA, "0", "1")
table(donna$ster_meds)

table(donna$ster_surg)
levels(donna$ster_surg) <- c(NA, "0", "1")
table(donna$ster_surg)

# Fumo #

donna$smk_smoke_status <- as.factor(donna$smk_smoke_status)
table(donna$smk_smoke_status)
levels(donna$smk_smoke_status) <- c("1", "0", "2")
table(donna$smk_smoke_status)

donna$curr_filtro <- as.factor(donna$curr_filtro)
table(donna$curr_filtro)
levels(donna$curr_filtro) <- c(NA,"0")
table(donna$curr_filtro)

donna$curr_aspirazione <- as.factor(donna$curr_aspirazione)
table(donna$curr_aspirazione)
levels(donna$curr_aspirazione) <- c(NA,"0","1","2")
table(donna$curr_aspirazione)

donna$curr_quit_month <- as.factor(donna$curr_quit_month)
table(donna$curr_quit_month)
levels(donna$curr_quit_month) <-c (NA,"1","0")
table(donna$curr_quit_month)

donna$ex_ever_stop_before <- as.factor(donna$ex_ever_stop_before)
table(donna$ex_ever_stop_before)
levels(donna$ex_ever_stop_before) <- c(NA, "1", "0")
table(donna$ex_ever_stop_before)

donna$nev_ever_tried <- as.factor(donna$nev_ever_tried)
table(donna$nev_ever_tried)
levels(donna$nev_ever_tried) <- c("0", "1")
table(donna$nev_ever_tried)

donna$nev_evet_tried_occ <- as.factor(donna$nev_evet_tried_occ)
table(donna$nev_evet_tried_occ)
levels(donna$nev_evet_tried_occ) <- c("0","1")
table(donna$nev_evet_tried_occ)

donna$nev_father_smoke <- as.factor(donna$nev_father_smoke)
table(donna$nev_father_smoke)
levels(donna$nev_father_smoke) <- c("0","1")
table(donna$nev_father_smoke)

donna$nev_mother_smoke <- as.factor(donna$nev_mother_smoke)
table(donna$nev_mother_smoke)
levels(donna$nev_mother_smoke) <- c("0","1")
table(donna$nev_mother_smoke)

donna$nev_childhood_smoke_exp <- as.factor(donna$nev_childhood_smoke_exp)
table(donna$nev_childhood_smoke_exp)
levels(donna$nev_childhood_smoke_exp) <- c("2","3","0","0","1")
table(donna$nev_childhood_smoke_exp)

donna$nev_partner_smoke <- as.factor(donna$nev_partner_smoke)
table(donna$nev_partner_smoke)
levels(donna$nev_partner_smoke) <- c("0", "1", "2", "0",NA)
table(donna$nev_partner_smoke)

donna$nev_partner_presence <- as.factor(donna$nev_partner_presence)
table(donna$nev_partner_presence)
levels(donna$nev_partner_presence) <- c(NA,"0","1")
table(donna$nev_partner_presence)

donna$nev_other_smoke <- as.factor(donna$nev_other_smoke)
table(donna$nev_other_smoke)
levels(donna$nev_other_smoke) <- c("0","1")
table(donna$nev_other_smoke)

# Alcool #

donna$alc_ever_alcohol <- as.factor(donna$alc_ever_alcohol)
levels(donna$alc_ever_alcohol) <- c(NA,"0","1")
donna$alc_wine <- as.factor(donna$alc_wine)
levels(donna$alc_wine) <- c("1","0","2")
donna$alc_beer <- as.factor(donna$alc_beer)
levels(donna$alc_beer) <- c("1","0","2")
donna$alc_spirits <- as.factor(donna$alc_spirits)
levels(donna$alc_spirits) <- c("1","0","2")

# Malattie #

donna$dis_stroke <- as.factor(donna$dis_stroke)
levels(donna$dis_stroke) <- c("0","1")
donna$dis_angina <- as.factor(donna$dis_angina)
levels(donna$dis_angina) <- c("0","1")
donna$dis_ictus <- as.factor(donna$dis_ictus)
levels(donna$dis_ictus) <- c("0","1")
donna$dis_high_pressure <- as.factor(donna$dis_high_pressure)
levels(donna$dis_high_pressure) <- c("0","1")
donna$dis_high_col <- as.factor(donna$dis_high_col)
levels(donna$dis_high_col) <- c("0","1")
donna$dis_high_col_meds <- as.factor(donna$dis_high_col_meds)
levels(donna$dis_high_col_meds) <- c("0","1")
donna$dis_diabetes <- as.factor(donna$dis_diabetes)
levels(donna$dis_diabetes) <- c("0","1")
donna$dis_diabetes_meds <- as.factor(donna$dis_diabetes_meds)
levels(donna$dis_diabetes_meds) <- c("0","1")
donna$dis_liver_calc <- as.factor(donna$dis_liver_calc)
levels(donna$dis_liver_calc) <- c("0","1")
donna$dis_gallbladder_removed <- as.factor(donna$dis_gallbladder_removed)
levels(donna$dis_gallbladder_removed) <- c("0","1")
donna$dis_kidney_calc <- as.factor(donna$dis_kidney_calc)
levels(donna$dis_kidney_calc) <- c("0","1")
donna$dis_kidney_calc_meds <- as.factor(donna$dis_kidney_calc_meds)
levels(donna$dis_kidney_calc_meds) <- c("0","1")
donna$dis_intestin_polyps <- as.factor(donna$dis_intestin_polyps)
levels(donna$dis_intestin_polyps) <- c("0","1")
donna$dis_tumors <- as.factor(donna$dis_tumors)
levels(donna$dis_tumors) <- c("0","1")
donna$dis_ulcer <- as.factor(donna$dis_ulcer)
levels(donna$dis_ulcer) <- c("0","1")
donna$dis_ulcer_meds <- as.factor(donna$dis_ulcer_meds)
levels(donna$dis_ulcer_meds) <- c("0","1")
donna$dis_ulcer_surg <- as.factor(donna$dis_ulcer_surg)
levels(donna$dis_ulcer_surg) <- c("0","1")

# Bmi ed età #

donna$age_cat <- with(donna, cases("_40"= age<40,
                                 "40_60"= age>=40 & age<60,
                                 "60_"= age>=60))

donna <- dplyr::relocate(donna, "age_cat", .after = "age")


donna$bmi_cat <- with(donna, cases("1" = bmi<18.5,
                                 "0" = bmi>=18.5 & bmi<25,
                                 "2" = bmi>=25 & bmi<30,
                                 "3" = bmi>=30))

donna <- dplyr::relocate(donna, "bmi_cat", .after = "bmi")


## Salvo il dataset ##

# saveRDS(donna, file = "C:/Users/amedeo/Desktop/R_Projects/sdv_redo/data_redo/donna.rds")
saveRDS(donna, file = "C:/Users/amedeo/Desktop/check/donna.rds")

