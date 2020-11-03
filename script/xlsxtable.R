library(tidyverse)
library(openxlsx)

# Parameters
refDB <- "male"
result.path <- paste0("results/by_sex/", refDB)


#Script start
covars <- c("age_cat", "age_terz", "alcool", "alcool_28", "alcool_drinker", 
            "bmi_cat", "coffee_cat", "coffee_drinker", "phys_act", "smoke", "wine_consumption")

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

writeData(wb, "age_cat", age_cat_db, startRow = 1, startCol = 1)
writeData(wb, "age_terz", age_terz_db, startRow = 1, startCol = 1)
writeData(wb, "alcool", alcool_db, startRow = 1, startCol = 1)
writeData(wb, "alcool_28", alcool_28_db, startRow = 1, startCol = 1)
writeData(wb, "alcool_drinker", alcool_drinker_db, startRow = 1, startCol = 1)
writeData(wb, "bmi_cat", bmi_cat_db, startRow = 1, startCol = 1)
writeData(wb, "coffee_cat", coffee_cat_db, startRow = 1, startCol = 1)
writeData(wb, "coffee_drinker", coffee_drinker_db, startRow = 1, startCol = 1)
writeData(wb, "phys_act", phys_act_db, startRow = 1, startCol = 1)
writeData(wb, "smoke", smoke_db, startRow = 1, startCol = 1)
writeData(wb, "wine_consumption", wine_consumption_db, startRow = 1, startCol = 1)
writeData(wb, "ncigs", ncigs_db, startRow = 1, startCol = 1)

saveWorkbook(wb, file = "results/male_results.xlsx", overwrite = TRUE)
