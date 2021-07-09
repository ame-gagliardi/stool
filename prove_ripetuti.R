source("D:/R_Projects/general/FUNCTION_custom.R")
source("D:/R_Projects/general/GRAPH_libraries.R")

df <- readRDS("D:/R_Projects/stool/results/repeated_samples/repeated_df.rds") 
cts <- readRDS("D:/R_Projects/stool/results/repeated_samples/repeated_cts_norm.rds")
mirna <- read.delim("D:/R_Projects/stool/data/downstream/filter_mirna.txt", sep = "\t")
mirna$miRNA <- trimws(mirna$miRNA)
stable <- c("hsa-miR-3125", "hsa-miR-6075", "hsa-miR-320e-5p:Novel", "hsa-miR-151a-3p", "hsa-miR-4487",
            "hsa-miR-6124-3p:Novel", "hsa-miR-501-3p", "hsa-miR-4313-3p:Novel", "hsa-miR-9901", "hsa-miR-647-3p:Novel")

occurence <- as.data.frame(table(mirna$miRNA))
colnames(occurence) <- c("mirna", "freq")

once <- occurence %>% 
  dplyr::filter(freq == 1)

rep <- occurence %>% 
  dplyr::filter(freq > 1)


filtered_cts <- cts %>% 
  dplyr::filter(rownames(cts) %in% once$mirna)

colnames(filtered_cts) <- c("LA_T0", "AR_T0", "SG_T0", "BP_T0", "AN_T0", "ST_T0", 
                            "LA_T1", "AR_T1", "SG_T1", "BP_T1", "AN_T1", "ST_T1")


la <- filtered_cts %>% 
  dplyr::select(LA_T0, LA_T1)

###

i <- intersect(rownames(cts), stable)

stable_cts <- cts[i,]

colnames(stable_cts) <- c("LA_T0", "AR_T0", "SG_T0", "BP_T0", "AN_T0", "ST_T0", 
                            "LA_T1", "AR_T1", "SG_T1", "BP_T1", "AN_T1", "ST_T1")

AR <- stable_cts %>% 
  dplyr::select(AR_T0, AR_T1)

