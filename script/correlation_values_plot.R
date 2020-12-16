source("C:/Users/amedeo/Desktop/R_Projects/general_script/functions.R")
source("C:/Users/amedeo/Desktop/R_Projects/general_script/graphics_libraries.R")

refVar <- c("alcool_gr") # Variabile d'interesse
refRes <- c("filtered") # All o filtered


all <- readRDS(paste0("results/correlation/", refVar, "_both_", refRes, "_correlation.rds"))
male <- readRDS(paste0("results/correlation/", refVar, "_male_", refRes, "_correlation.rds"))
female <- readRDS(paste0("results/correlation/", refVar, "_female_", refRes, "_correlation.rds"))

all.equal(rownames(all), rownames(male))
all.equal(rownames(all), rownames(female))

all <- all %>% 
  dplyr::select(miRNA,SC, SC.p, SC.fdr) %>% 
  dplyr::rename(sca = SC, sca.p = SC.p, sca.fd = SC.fdr)

male <- male %>% 
  dplyr::select(SC, SC.p, SC.fdr) %>% 
  dplyr::rename(scm = SC, scm.p = SC.p, scm.fd = SC.fdr)

female <- female %>% 
  dplyr::select(SC, SC.p, SC.fdr) %>% 
  dplyr::rename(scf = SC, scf.p = SC.p, scf.fd = SC.fdr)

df <- bind_cols(all, male, female)
df[,"index"] <- seq(1:length(rownames(df)))

db <- df %>% 
  tidyr::pivot_longer(cols = c(2,5,8), names_to = "name", values_to = "SC")

db$name <- as.factor(db$name)
levels(db$name) <- c("All samples", "Females", "Males")
db$dataset <- db$name

p <- ggplot(db, aes(x = index, y = SC)) +
  geom_point(aes(col = dataset)) +
  geom_hline(yintercept = -0.2) + 
  geom_hline(yintercept = 0.2) +
  ylim(-1,1) +
  labs(title = paste0(firstup(refVar), " Spearman correlation in different datasets"), y = "Spearman correlation", x = "miRNA index") +
  geom_label_repel(aes(label = ifelse(SC < -0.2 & sca.fd < 0.05 & dataset == "All samples" | 
                                      SC > 0.2 & sca.fd < 0.05 & dataset == "All samples" , as.character(miRNA), ''), 
                       fill = dataset, fontface = "bold")) +
  geom_label_repel(aes(label = ifelse(SC < -0.2 & scm.fd < 0.05 & dataset == "Males" | 
                                      SC > 0.2 & scm.fd < 0.05 & dataset == "Males" , as.character(miRNA), ''),
                       fill = dataset, fontface = "bold"))  +
  geom_label_repel(aes(label = ifelse(SC < -0.2 & scf.fd < 0.05 & dataset == "Females" | 
                                      SC > 0.2 & scf.fd < 0.05 & dataset == "Females" , as.character(miRNA), ''), 
                       fill = dataset, fontface = "bold"))

ggsave(p, filename = paste0("results/figures/", refVar, "_both_", refRes, "_correlation.jpg"), width = 12, height = 14, units = "in")

rm(list=ls())


  
