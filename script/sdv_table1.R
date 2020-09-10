uomo <- readRDS("C:/Users/amedeo/Desktop/R_Projects/stool/stool_vita/data/svuomo_dataset_coded.rds")
colnames(uomo)[80] <- c("dis_angina")
donna <- readRDS("C:/Users/amedeo/Desktop/R_Projects/stool/stool_vita/data/svdonna_dataset_coded.rds")


i <- intersect(colnames(donna), colnames(uomo))
xx <- donna[,i]
xy <- uomo[,i]
all.equal(colnames(xy), colnames(xx))

xx <- xx[,c(1,2,3,4,5,6,22)]
xy <- xy[,c(1,2,3,4,5,6,22)]

dfmerged <- rbind(xx,xy)

dfmerged$Sex <- factor((dfmerged$Sex),
                       levels = c(0,1),
                       labels = c("Donna","Uomo"))

dfmerged$smk_smoke_status <- factor((dfmerged$smk_smoke_status),
                                     levels = c(0,1,2),
                                     labels = c("Non fumatore","Ex fumatore", "Fumatore"))

colnames(dfmerged)[7] <- c("Smoke")


crc <- dfmerged[which(dfmerged$Study == "CRC"),]
vov <- dfmerged[which(dfmerged$Study == "VOV"),]
cel <- dfmerged[which(dfmerged$Study == "Celiac"),]

crc$id_pat <- factor(crc$id_pat,
                     levels = c(0,1,2,3,4),
                     labels = c("Controllo","Polipi","Infiammazione","CRC","Emorroidi"))

vov$id_pat <- factor(vov$id_pat,
                     levels = c(0,1,2),
                     labels = c("Onnivori","Vegetariani","Vegani"))

cel$id_pat <- factor(cel$id_pat,
                     levels = c(0,1,2),
                     labels = c("Celiaco dieta","Nuova diagnosi","Controllo"))    

rm(donna, uomo,xx,xy,i)

## ELIMINO POLIPI/INFIAMMAZIONI/TUMORI CRC_DATASET ##

crc <- crc[which(crc$id_pat == "Controllo" | crc$id_pat == "Emorroidi"),]
crc$id_pat <- droplevels(crc$id_pat)

## ELIMINO RIPETUTI DA VOV ##

vov$idpaziente <- as.character(vov$idpaziente)

vov <- vov[which(vov$idpaziente != "VOV085"),]
vov <- vov[which(vov$idpaziente != "VOV107"),]
vov <- vov[which(vov$idpaziente != "VOV116"),]
vov <- vov[which(vov$idpaziente != "VOV119"),]
vov <- vov[which(vov$idpaziente != "VOV094"),]
vov <- vov[which(vov$idpaziente != "VOV139"),]

## ELIMINO RIPETUTI E NUOVE DIAGNOSI DALLA CELIACHIA ##

cel$idpaziente <- as.character(cel$idpaziente)

cel <- cel[which(cel$idpaziente != "Cii_008"),]
cel <- cel[which(cel$idpaziente != "Cii_011"),]
cel <- cel[which(cel$idpaziente != "Cii_014"),]
cel <- cel[which(cel$idpaziente != "Cii_021"),]
cel <- cel[which(cel$idpaziente != "Cii_026"),]
cel <- cel[which(cel$idpaziente != "Cii_035"),]

cel <- cel[which(cel$id_pat == "Celiaco dieta" | cel$id_pat == "Controllo"),]

#ROWNAMES#

rownames(crc) <- crc$idpaziente
rownames(vov) <- vov$idpaziente
rownames(cel) <- cel$idpaziente

## TABLE 1 ##

crc$Age <- as.numeric(as.character(crc$Age))

vov$Age <- as.character(vov$Age)
vov$Age <- substr(vov$Age,1,nchar(vov$Age)-2)
vov$Age <- as.numeric(vov$Age)

cel$Age <- as.character(cel$Age)
cel$Age <- substr(cel$Age,1,nchar(cel$Age)-2)
cel$Age <- as.numeric(cel$Age)


library(table1)

table1(~Sex + Age + Smoke | id_pat, data=crc)
table1(~Sex + Age + Smoke | id_pat, data=vov)
table1(~Sex + Age + Smoke | id_pat, data=cel)


## SALAVATAGGIO DATASET ##

saveRDS(vov, "C:/Users/amedeo/Desktop/R_Projects/stool/stool_vita/data/sdv_vov_controls.rds")
saveRDS(crc, "C:/Users/amedeo/Desktop/R_Projects/stool/stool_vita/data/sdv_crc_controls.rds")
saveRDS(cel, "C:/Users/amedeo/Desktop/R_Projects/stool/stool_vita/data/sdv_cel_controls.rds")

dfmerged <- rbind(crc, vov, cel)
saveRDS(dfmerged, "C:/Users/amedeo/Desktop/R_Projects/stool/stool_vita/data/sdv_control_all.rds")

