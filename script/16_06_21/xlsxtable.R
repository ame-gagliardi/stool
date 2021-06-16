library(tidyverse)
library(openxlsx)

# Parameters
refDB <- "female"
result.path <- paste0("results/by_sex/", refDB)


#Script start
covars <- c("age_cat", "age_terz", "alcool", "alcool_28", "alcool_drinker", 
            "bmi_cat", "coffee_cat", "coffee_drinker", "phys_act", "smoke", "wine_consumption", "ncigs")

for(i in 1:length(covars)){
  refCov <- covars[i]
  files <- list.files(paste0(result.path, "/tables/", refCov))
  files
  
  for(i in 1:length(files)){
    assign(paste0("df",i), read.delim(paste0(result.path,"/tables/",refCov,"/",files[i])))
    n <- length(grep("df", ls()))
    }
  if(n >=3){
    assign(paste0(refCov,"_db"), merge(df1, df2, by = "ID") %>% 
             merge(df3, by = "ID") %>% 
             dplyr::relocate(contains("Average"), .after = ID) %>% 
             dplyr::relocate(contains("Median"), .after = ID))
    }else{
      assign(paste0(refCov, "_db"), df1)
    }
  rm(list=ls(pattern = "df"))
}


## Create excel workbook

wb <- createWorkbook()

for(i in 1:length(covars)){
  addWorksheet(wb, covars[i])
}

for(i in 1:length(covars)){
  writeData(wb, covars[i], get(paste0(covars[i],"_db")), startRow = 2, startCol = 1)
}


saveWorkbook(wb, file = "results/female_results.xlsx", overwrite = TRUE)
