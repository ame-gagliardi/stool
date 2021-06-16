## All pooled script ##

df <- readRDS("C:/Users/amedeo/Desktop/R_Projects/stool/data/clinical/sv_stool_stool_both_samples_discovery.rds")
df2 <- readRDS("C:/Users/amedeo/Desktop/R_Projects/stool/data/clinical/sv_stool_stool_both_samples_validation.rds")

i <- intersect(colnames(df), colnames(df2))
df <- df[,i]
df2 <- df2[,i]

df3 <- rbind(df, df2)

uomo <- read.delim("C:/Users/amedeo/Desktop/PA_INDEX_man_all_discovery_e_validation.csv", sep = ";") %>% 
  dplyr::select(id, study, MET, PA_total)
donna <- read.delim("C:/Users/amedeo/Desktop/PA_INDEX_women_all_discovery_e_validation.csv", sep = ";") %>% 
  dplyr::select(id, study, MET, PA_total)

df_met <- rbind(uomo, donna)

# celiac <- df_met %>% 
#   dplyr::filter(study == "Celiac") %>% 
#   dplyr::select(id)
# 
# crc <- df_met %>% 
#   dplyr::filter(study == "CRC") %>% 
#   dplyr::select(id)
# 
# vov <- df_met %>% 
#   dplyr::filter(study == "VOV") %>% 
#   dplyr::select(id)

## CRC ##
df_met$id[which(df_met$study == "CRC")] <- str_remove_all(df_met$id[which(df_met$study == "CRC")], "VF")
which(df_met$id == "G04")
which(df_met$id == "G19")
df_met$id[53] <- c("GF004")
df_met$id[95] <- c("GF019")
df_met$id[which(df_met$study == "CRC")] <- stringr::str_c("VF",df_met$id[which(df_met$study == "CRC")])
df_met$id[which(df_met$study == "CRC")] <- str_replace_all(df_met$id[which(df_met$study == "CRC")], "VFGF", "GF")

## VOV
which(df_met$id == "VOV121")
which(df3$id == "VOV121")
df3 <- df3[-255,]
df_met <- df_met[-c(34,78),]
rownames(df_met) <- df_met$id

i <- intersect(rownames(df3), df_met$id)
df_met <- df_met[i,]
df3 <- df3[i,]
all.equal(rownames(df3), rownames(df_met))

df3[,"MET"] <- df_met$MET
df3[,"phys_act"] <- df_met$PA_total

levels(df3$age_cat) <- c("<37", "37-53", ">53", "<37", ">53")
levels(df3$sex) <- c("F", "M", "F", "M")
levels(df3$cigs) <- c("never", "former", "<16", ">16")
levels(df3$menstruation) <- c("NO","YES", "NO", "YES")
df3$phys_act <- as.factor(df3$phys_act)
df3$phys_act <- relevel(df3$phys_act, "Moderately active")
df3$phys_act <- relevel(df3$phys_act, "Moderately inactive")
df3$phys_act <- relevel(df3$phys_act, "Inactive")

df3 %>% gather() %>% head()

df4 <- df3 %>% 
  dplyr::select(age_cat, sex, bmi_cat, smoke, cigs, menstruation, phys_act)

ggplot(gather(df4), aes(value)) + 
  geom_bar() +
  facet_wrap(~key, scales = 'free_x')



saveRDS(df3, file = "C:/Users/amedeo/Desktop/df_total.rds")

cts1 <- readRDS("data/ngs/sv_stool_stool_both_raw_counts_discovery.rds")
cts2 <- readRDS("data/ngs/sv_stool_stool_both_raw_counts_validation.rds")

i <- intersect(rownames(cts1), rownames(cts2))

cts1 <- cts1[i,]
cts2 <- cts2[i,]
all.equal(rownames(cts1), rownames(cts2))

cts <- cbind(cts1,cts2)
i <- intersect(colnames(cts), rownames(df3))
cts <- cts[,i]
df3 <- df3[i,]
all.equal(colnames(cts), rownames(df3))

saveRDS(cts, file = "C:/Users/amedeo/Desktop/cts_total.rds")
