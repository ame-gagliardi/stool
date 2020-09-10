## Script per la creazione di grafici vari

## LIBRARY LOADING ##
library(tidyverse)
library(ggplot2)

## DATA LOADING ##

# Carico i risultati dell'analisi di DESeq

result.path <- c("C:/Users/amedeo/Desktop/R_Projects/stool/stool_vita/results/smoke_results.rda")
count.path <- c("C:/Users/amedeo/Desktop/R_Projects/stool/stool_vita/data/ngs_count/merged_count_harmonized_converted.rds")
covar.path <- c("C:/Users/amedeo/Desktop/R_Projects/stool/stool_vita/data/clinical/merged_study_cleaned_samples.rds") # Inserisci qua il path della tabella delle covariate

rawCount <- readRDS(count.path)
coldata <- readRDS(covar.path)
load(result.path)


## BOXPLOT

df <- rawCount[res$gene,]
rownames(df) <- str_replace(rownames(df), ":", "_")

df_m <- reshape2::melt(as.matrix(df))
colnames(df_m) <- c("feature","sample","value")
df_m[,"smoke"] <- coldata[match(df_m[,"sample"], rownames(coldata), nomatch = 0),"smoke"] ## Metti il nome della variabile d'interesse

name <- rownames(df)

df_m <- df_m[complete.cases(df_m$smoke),]


ggplot2::ggplot(data = df_m[which(df_m$feature %in% name),], aes(x=smoke, y=log(value))) +
  geom_boxplot(aes(fill = smoke)) + 
  facet_wrap(facets = ~ feature) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

## SINGLE BOXPLOT ##

for(i in 1:length(name)){
  p <- ggplot2::ggplot(data = df_m[which(df_m$feature == name[i]),], aes(x=smoke, y=log(value))) +
       geom_boxplot(aes(fill = smoke)) + 
       theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank()) +
       ggtitle(paste(name[i])) +
       theme(plot.title = element_text(hjust = 0.5))
       ggsave(plot = p, filename = paste0("C:/Users/amedeo/Desktop/xx/",name[i],".png"))
}

## SINGLE VIOLINPLOT ##

for(i in 1:length(name)){
  p <- ggplot2::ggplot(data = df_m[which(df_m$feature == name[i]),], aes(x=bmi_cat, y=log(value), fill = bmi_cat)) +
       geom_violin(trim = FALSE) +
       geom_boxplot(width = 0.1, fill = "white") + 
       theme_minimal() +
       ggtitle(paste(name[i])) +
       theme(plot.title = element_text(hjust = 0.5))
       ggsave(plot = p, filename = paste0("C:/Users/amedeo/Desktop/R_Projects/stool/stool_vita/images/bmi/violinplot/",name[i],".png"))
}
