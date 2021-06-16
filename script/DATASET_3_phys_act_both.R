
# Aggiungo l'attività fisica calcolata da Bea #

# uomo <- readRDS("C:/Users/amedeo/Desktop/R_Projects/sdv_redo/data_redo/uomo.rds")
# rownames(uomo) <- uomo$idpaziente
# donna <- readRDS("C:/Users/amedeo/Desktop/R_Projects/sdv_redo/data_redo/donna.rds")
# rownames(donna) <- donna$idpaziente

uomo <- readRDS("C:/Users/amedeo/Desktop/check/uomo.rds")
rownames(uomo) <- uomo$idpaziente
donna <- readRDS("C:/Users/amedeo/Desktop/check/donna.rds")
rownames(donna) <- donna$idpaziente



pa_uomo <- read.table("C:/Users/amedeo/Desktop/R_Projects/sdv_redo/data/acque/PA_INDEX_men.csv", header = TRUE, sep = ";")
rownames(pa_uomo) <- pa_uomo$idpaziente
pa_donna <- read.table("C:/Users/amedeo/Desktop/R_Projects/sdv_redo/data/acque/PA_INDEX_women.csv", header = TRUE, sep = ",")
rownames(pa_donna) <- pa_donna$idpaziente

xy <- uomo$idpaziente %in% pa_uomo$idpaziente
xx <- donna$idpaziente %in% pa_donna$idpaziente

i <- intersect(rownames(uomo), rownames(pa_uomo))
uomo <- uomo[i,]
pa_uomo <- pa_uomo[i,]
all.equal(rownames(uomo), rownames(pa_uomo))


i <- intersect(rownames(donna), rownames(pa_donna))
donna <- donna[i,]
pa_donna <- pa_donna[i,]
all.equal(rownames(donna), rownames(pa_donna))

#

donna[,c(207,208,209,210)] <- pa_donna[,c(8,9,10,11)]
uomo[,c(158,159,160,161)] <- pa_uomo[,c(8,9,10,11)]

# Salvo

# saveRDS(uomo, file = "C:/Users/amedeo/Desktop/R_Projects/sdv_redo/data_redo/uomo.rds")
# saveRDS(donna, file = "C:/Users/amedeo/Desktop/R_Projects/sdv_redo/data_redo/donna.rds")

saveRDS(uomo, file = "C:/Users/amedeo/Desktop/check/uomo.rds")
saveRDS(donna, file = "C:/Users/amedeo/Desktop/check/donna.rds")
