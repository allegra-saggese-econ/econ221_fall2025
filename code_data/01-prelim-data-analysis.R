# author: Allegra Saggese
# last updated: 02 nov 2025
# purpose: prelim analysis for macro measurement 
library(tidyr)
library(dplyr)




## LOAD IN DATASETS 
owd    <- getwd()

# find files
paths <- list.files(pattern = "\\.(csv|xlsx)$", ignore.case = TRUE, full.names = TRUE)

# read helper
read_any <- function(p){
  ext <- tolower(tools::file_ext(p))
  if (ext == "csv") {
    read.csv(p, stringsAsFactors = FALSE, check.names = FALSE)
  } else if (ext == "xlsx") {
    as.data.frame(readxl::read_excel(p, sheet = 1), check.names = FALSE)
  } else {
    NULL
  }
}

datasets <- setNames(lapply(paths, read_any),
                     tools::file_path_sans_ext(basename(paths)))

# back to original WD
setwd(owd)

# what we get 
names(datasets)




