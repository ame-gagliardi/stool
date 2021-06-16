library(tidyverse)
library(memisc)

coffee <- as_tibble(read.table("data/acque/epicdieta_gday_coffetot.csv", sep =";", header = TRUE)) %>% 
  dplyr::select(idpaziente, TOT.Coffe)

df <- as_tibble(readRDS("data/clinical/merged_cleaned.rds"))

diff <- which(!(coffee$idpaziente %in% df$id_old))


coffee[diff,"idpaziente"] <- c("010", "040", "006", "005", "001", "048", "031", "063", "023", "014", "012", "018", "019", "082",
                               "074", "076", "075", "013", "081", "078", "028", "039", "094", "092", "091", "099")

i <- intersect(df$id_old, coffee$idpaziente)

df <- as_tibble(merge(df, coffee, by.x = "id_old", by.y = "idpaziente"))
df <- df %>% 
  dplyr::relocate(TOT.Coffe, .after = alcool) %>% 
  dplyr::rename(coffee = TOT.Coffe)

df$coffee_cat <- with(df, cases("no_coffee" = coffee ==0,
                                "light_drinker" = coffee >0 & coffee <8.59,
                                "heavy_drinker" = coffee >= 8.59))

df <- dplyr::relocate(df, coffee_cat, .after = coffee)

saveRDS(df, "data/clinical/merged_cleaned.rds")

