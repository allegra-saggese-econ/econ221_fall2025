# -----------------------------------------------------------------------
###### ###### ###### ######  LOAD IN PACKAGES  ###### ###### ###### ###### 
# -----------------------------------------------------------------------

# general data cleaning 
library(tidyr)
library(dplyr)
library(purrr)
library(readxl)
library(stringr)

# graphics
library(ggplot2)
library(scales)

# API relevant packages 
library(tidycensus) # census data 
library(httr)
library(jsonlite) # CA tax data 


# set working directory to load in data 
owd    <- getwd()
target <- file.path(getwd(), "code_data")
if (!dir.exists(target)) stop("Missing 'code_data' inside: ", getwd())
setwd(target)
getwd()


# -----------------------------------------------------------------------
###### ###### ###### ######  LOAD IN DATASETS  ###### ###### ###### ###### 
# -----------------------------------------------------------------------



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

# now datasets is a list of all the dfs that I have for analysis 
datasets <- setNames(lapply(paths, read_any),
                     tools::file_path_sans_ext(basename(paths)))

# back to original WD
setwd(owd)

##### WILL NEED TO ADJUST TO READ IN THE DATASETS THAT ARE EXCEL + SHEET --- THIS IS CRASHING THE READ IN HELPER 

