source("C:/Users/amedeo/Desktop/R_Projects/general_script/libraries_functions.R")
source("C:/Users/amedeo/Desktop/R_Projects/general_script/libraries_graph.R")

# DATA LOADING

lib <- read.delim("C:/Users/amedeo/Desktop/R_Projects/stool/data/original_data/validation_library.csv", sep = ";", header = TRUE)


###### UOMO #####

uomo <- read.delim("C:/Users/amedeo/Desktop/R_Projects/stool/data/original_data/validation_vita_uomini.csv", sep = ";", header = TRUE)

uomo[,148:155] <- NA
uomo <- uomo[,c(1,2,148:155,3:147)]

uomo <- uomo[uomo$idpaziente %in% lib$id,]

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
uomo[,20] <- as.factor(uomo[,20])
uomo[,21] <- as.factor(uomo[,21])
uomo[,22] <- as.factor(uomo[,22])
uomo[,23] <- as.factor(uomo[,23])
uomo[,25] <- as.factor(uomo[,25])
uomo[,26] <- as.factor(uomo[,26])
uomo[,28] <- as.factor(uomo[,28])
uomo[,29] <- as.factor(uomo[,29])
uomo[,31] <- as.factor(uomo[,31])
uomo[,36] <- as.factor(uomo[,36])
uomo[,37] <- as.factor(uomo[,37])
uomo[,38] <- as.factor(uomo[,38])
uomo[,39] <- as.factor(uomo[,39])
uomo[,40] <- as.factor(uomo[,40])
uomo[,41] <- as.factor(uomo[,41])
uomo[,42] <- as.factor(uomo[,42])
uomo[,44] <- as.factor(uomo[,44])
uomo[,45] <- as.factor(uomo[,45])
uomo[,47] <- as.factor(uomo[,47])
uomo[,131] <- as.factor(uomo[,131])

levels(uomo[,16]) <- c("1-3","4-8","9-13")
levels(uomo[,20]) <- c("1-3","4-8","14-18")
levels(uomo[,21]) <- c("0")
levels(uomo[,22]) <- c("1-3","14-18")
levels(uomo[,23]) <- c("0","9-13")
levels(uomo[,25]) <- c("0","9-13")
levels(uomo[,26]) <- c("0")
levels(uomo[,28]) <- c("0")
levels(uomo[,29]) <- c("1-3")
levels(uomo[,31]) <- c("0")
levels(uomo[,36]) <- c("1-3","4-8","14-18","9-13")
levels(uomo[,37]) <- c("0","4-8")
levels(uomo[,38]) <- c("1-3","14-18")
levels(uomo[,39]) <- c("0","1-3","14-18","19-23","34+","9-13")
levels(uomo[,40]) <- c("0", "19-23")
levels(uomo[,41]) <- c("14-18","19-23","34+")
levels(uomo[,42]) <- c("0","1-3","4-8","34+","9-13")
levels(uomo[,44]) <- c("0","1-3")
levels(uomo[,45]) <- c("0","34+")
levels(uomo[,47]) <- c("0")
levels(uomo[,131]) <- c("1-2","3-4","5-6","7-9","10-12","13-15","16-20", "21+" ,"0")

uomo$smk_smoke_status <- as.factor(uomo$smk_smoke_status)
levels(uomo$smk_smoke_status) <- c("former", "former", "never", "current")
uomo <- uomo[-(which(uomo$idpaziente == "VOV089")),]

final_uomo <- as.data.frame(matrix(NA, nrow = length(rownames(uomo)), ncol = 21))
colnames(final_uomo) <- c("id", "id_old", "study", "library", "class", "age", "age_cat", "sex", "bmi", "bmi_cat", "smoke", "cigs",
                          "alcool_gr", "alcool_ua", "ua", "alcool_drinker", "coffee_drinker", "coffee", "menstruation", "MET", "phys_act")

final_uomo[,"id"] <- uomo$idpaziente
rownames(uomo) <- uomo$idpaziente
rownames(final_uomo) <- final_uomo$id
all.equal(rownames(uomo), rownames(final_uomo))

final_uomo[,"study"][1:21] <- c("Celiac") 
final_uomo[,"study"][22:26] <- c("VOV")

tmp <- lib[lib$id %in% final_uomo$id,]
rownames(tmp) <- tmp$id
i <- intersect(rownames(final_uomo), rownames(tmp))
final_uomo <- final_uomo[i,]
tmp <- tmp[i,]
all.equal(rownames(final_uomo), rownames(tmp))
final_uomo[,"library"] <- tmp$library

final_uomo[,"age"] <- tmp$age
final_uomo[,"sex"] <- tmp$sex
final_uomo[,"smoke"] <- uomo$smk_smoke_status
final_uomo[,"cigs"] <- uomo$curr_day
final_uomo[,"cigs"] <- paste0(final_uomo$smoke, final_uomo$cigs)
final_uomo[,"cigs"] <- str_remove_all(final_uomo$cigs, "NA")
final_uomo[,"cigs"] <- str_remove_all(final_uomo$cigs, "current")
final_uomo[,"cigs"] <- as.factor(final_uomo$cigs)
levels(final_uomo$cigs) <- c("2", "6", "11", "former", "never")

final_uomo["Cii_042", "age"] <- 65
final_uomo["Cii_042", "sex"] <- 1
final_uomo["Cii_046", "age"] <- 43
final_uomo["Cii_046", "sex"] <- 1
final_uomo["351_DLEM", "age"] <- 48
final_uomo["351_DLEM", "sex"] <- 1
final_uomo["352_LGE", "age"] <- 62
final_uomo["352_LGE", "sex"] <- 1
final_uomo["351_DLEM", "age"] <- 48
final_uomo["351_DLEM", "sex"] <- 1
final_uomo["Cii_039", "age"] <- 24
final_uomo["Cii_039", "sex"] <- 1
final_uomo["387_DCF", "age"] <- 19
final_uomo["387_DCF", "sex"] <- 1
final_uomo["Cii_052", "age"] <- 65.7
final_uomo["Cii_052", "sex"] <- 1
final_uomo["Cii_053", "age"] <- 63.6
final_uomo["Cii_053", "sex"] <- 1
final_uomo["VOV005", "age"] <- 59.6
final_uomo["VOV005", "sex"] <- 1
final_uomo["VOV099", "age"] <- 59.7
final_uomo["VOV099", "sex"] <- 1
final_uomo["VOV121", "age"] <- 50.1
final_uomo["VOV121", "sex"] <- 1
final_uomo["VOV076", "age"] <- 54.3
final_uomo["VOV076", "sex"] <- 1





## VOV UOMO ##

vov <- read.delim("C:/Users/amedeo/Desktop/R_Projects/stool/data/DATI VOV.csv", sep = ";", header = TRUE)


## Cambio le classi di age e bmi ##

uomo$age <- as.character(uomo$age)
uomo$age <- str_replace(uomo$age, ",", ".")
uomo$age <- as.numeric(uomo$age)
uomo$age <- round(uomo$age)

uomo$bmi <- as.character(uomo$bmi)
uomo$bmi <- str_replace(uomo$bmi, ",", ".")
uomo$bmi <- as.numeric(uomo$bmi)


############## DONNA ##############

donna <- read.delim("C:/Users/amedeo/Desktop/R_Projects/stool/data/original_data/validation_vita_donne.csv", sep = ";", header = TRUE)
donna <- donna[donna$idpaziente %in% lib$id,]
donna[,196:204] <- NA
donna <- donna[,c(1,2,197:204,3:196)]

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

donna$smk_smoke_status <- as.factor(donna$smk_smoke_status)
levels(donna$smk_smoke_status) <- c("former", "never", "current")

torm <- which(duplicated(donna$idpaziente))
donna <- donna[-torm,]
rownames(donna) <- donna$idpaziente

final_donna <- as.data.frame(matrix(NA, nrow = length(rownames(donna)), ncol = 21))
colnames(final_donna) <- c("id", "id_old", "study", "library", "class", "age", "age_cat", "sex", "bmi", "bmi_cat", "smoke", "cigs",
                          "alcool_gr", "alcool_ua", "ua", "alcool_drinker", "coffee_drinker", "coffee", "menstruation", "MET", "phys_act")

final_donna[,"id"] <- donna$idpaziente
rownames(final_donna) <- final_donna$id
all.equal(rownames(donna), rownames(final_donna))

final_donna[,"study"][1:73] <- c("Celiac") 
final_donna[,"study"][74:81] <- c("VOV")

tmp <- lib[lib$id %in% final_donna$id,]
rownames(tmp) <- tmp$id
i <- intersect(rownames(final_donna), rownames(tmp))
final_donna <- final_donna[i,]
tmp <- tmp[i,]
all.equal(rownames(final_donna), rownames(tmp))
final_donna[,"library"] <- tmp$library

final_donna[,"age"] <- tmp$age
final_donna[,"sex"] <- tmp$sex
final_donna[,"smoke"] <- donna$smk_smoke_status
final_donna[,"cigs"] <- donna$curr_day
final_donna[,"cigs"] <- paste0(final_donna$smoke, final_donna$cigs)
final_donna[,"cigs"] <- str_remove_all(final_donna$cigs, "NA")
final_donna[,"cigs"] <- str_remove_all(final_donna$cigs, "current")
final_donna[,"cigs"] <- as.factor(final_donna$cigs)
levels(final_donna$cigs) <- c(NA, "2", "6", "16", "21", "former", "never", "11")
final_donna$cigs <- factor(final_donna$cigs, levels = c("never", "former", 2, 6, 11, 16, 21))
levels(final_donna$cigs) <- c("never", "former", "_16", "_16", "_16", "16_", "16_")
final_donna[,"menstruation"] <- donna$mestr_now
final_donna[,"sex"] <- 0

## MERGE DONNA UOMO ##

df <- rbind(final_uomo, final_donna)
vovData <- read.delim("C:/Users/amedeo/Desktop/R_Projects/stool/data/DATI VOV.csv", sep = ";", header = TRUE)
vovData$sex <- ifelse(vovData$sex == "M",1,0)
celData <- read.delim("C:/Users/amedeo/Desktop/R_Projects/stool/data/original_data/validation_bmiageAme.csv", sep = ";", header = T)
celData <- celData[celData$id %in% df$id,]

df[,"bmi"] <- celData[match(rownames(df), celData$id, nomatch = NA), "bmi"]
df[,"age"] <- celData[match(rownames(df), celData$id, nomatch = NA), "age"]
tmp <- grep("VOV", rownames(df))
df[tmp,"study"] <- c("VOV")
df[tmp,"bmi"] <- vovData[match(rownames(df)[tmp], vovData$id, nomatch = NA), "bmi"]
df[tmp,"age"] <- vovData[match(rownames(df)[tmp], vovData$id, nomatch = NA), "age"]
df[tmp,"sex"] <- vovData[match(rownames(df)[tmp], vovData$id, nomatch = NA), "sex"]
df["CMa_012", "age"] <- 26.1
df["CMa_012", "bmi"] <- 19.96
df["CMa_012", "sex"] <- 0
df["CMa_006", "age"] <- 20
df["CMa_006", "bmi"] <- NA
df["CMa_006", "sex"] <- 0
df[,"studente"] <- celData[match(rownames(df), celData$id, nomatch = NA), "studente"]


### attività fisica per bea ##

padonna <- df %>% 
  dplyr::filter(sex == 0)
all.equal(rownames(padonna), rownames(donna))
pa <- grep("pa_", colnames(donna))
pa <- donna[,pa]
all.equal(rownames(padonna), rownames(pa))
padonna <- cbind(padonna, pa)


pauomo <- df %>% 
  dplyr::filter(sex == 1)
all.equal(rownames(pauomo), rownames(uomo))
pa <- grep("pa_", colnames(uomo))
pa <- uomo[,pa]
all.equal(rownames(pauomo), rownames(pa))
pauomo <- cbind(pauomo, pa)

saveRDS(pauomo, file = "C:/Users/amedeo/Desktop/uomo_pa_bea.rds")
saveRDS(padonna, file = "C:/Users/amedeo/Desktop/donna_pa_bea.rds")
