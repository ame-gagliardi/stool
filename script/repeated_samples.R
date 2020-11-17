source("C:/Users/amedeo/Desktop/R_projects/general_script/useful_functions.R")

## Analysis on repeated samples

df <- readRDS("C:/Users/amedeo/Desktop/R_projects/stool/data/clinical/repeated_samples.rds")
coffee <- read_delim("data/acque/epicdieta_gday_coffetot.csv", ";", escape_double = FALSE, trim_ws = TRUE)

df <- df %>% 
  dplyr::select(idpaziente, study, libraries, id_pat, age, age_cat, sex, bmi, bmi_cat, smk_smoke_status, PA_total)

tmp <- subset(df, study == "VOV")
tmp <- tmp[c(1,4,5,2,3,6),]
tmp[,"cel_id"] <- c("Cii_008", "Cii_011", "Cii_014", "Cii_021", "Cii_026", "Cii_035")
df <- tmp %>% 
  dplyr::relocate(cel_id, .after = idpaziente)

coffee <- coffee[-162,]
coffee <- as.data.frame(coffee)
rownames(coffee) <- coffee$idpaziente

i <- intersect(rownames(df), (rownames(coffee)))
coffee <- coffee[i,]
df <- df[i,]

df[,"coffee"] <- coffee$TOT.Coffe
df[,"alcool"] <- coffee$TOT.alcool_g.day
df[,"vov_date"] <- c("23-11-2017", "11-01-2018", "06-02-2018", "07-02-2018", "05-12-2017", "19-06-2018")
df[,"cel_date"] <- c("14-02-2019", "21-02-2019", "15-04-2019", "11-06-2019", "18-06-2019", "02-07-2019")
colnames(df)[1] <- c("vov_id")
colnames(df)[11] <- "smoke"
colnames(df)[12] <- "phys_act"

df[,"vov_date"] <- as.Date(df$vov_date, format = c("%d-%m-%Y"))
df[,"cel_date"] <- as.Date(df$cel_date, format = c("%d-%m-%Y"))
df[,"days"] <- df$cel_date - df$vov_date

df <- df[,c(1,2,15,16,17,4,6,7,8,9,10,11,14,13,12)]

saveRDS(df, file = "data/clinical/repeated_samples.rds")


## Counts

df <- readRDS("data/clinical/repeated_samples.rds")
cts <- readRDS("data/ngs/normalized_counts.rds")
cel_cts <- readRDS("C:/Users/amedeo/Desktop/R_Projects/cel_rep.rds")
colnames(cel_cts) <- str_replace_all(colnames(cel_cts), "Cii", "Cii_")

i <- intersect(colnames(cts), rownames(df))
vov_rep_cts <- cts[,i]

rownames(df) <- df$cel_id
i <- intersect(colnames(cel_cts), rownames(df))
cel_rep_cts <- cel_cts[,i]


saveRDS(cel_rep_cts, file = "C:/Users/amedeo/Desktop/R_Projects/stool/data/celiac_cts_repeated.rds")
saveRDS(vov_rep_cts, file = "C:/Users/amedeo/Desktop/R_Projects/stool/data/vov_cts_repeated.rds")

##

df <- readRDS("C:/Users/amedeo/Desktop/R_Projects/stool/data/clinical/repeated_samples.rds")
cel_cts <- readRDS("C:/Users/amedeo/Desktop/R_Projects/stool/data/celiac_cts_repeated.rds")
vov_cts <- readRDS("C:/Users/amedeo/Desktop/R_Projects/stool/data/vov_cts_repeated.rds")


