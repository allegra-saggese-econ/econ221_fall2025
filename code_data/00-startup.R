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


# -----------------------------------------------------------------------
###### ###### ###### ###### DIRECTORY NAMES  ###### ###### ###### ###### 
# -----------------------------------------------------------------------

# set working directory to load in data 
if (basename(owd) != "code_data") {
  target <- file.path(owd, "code_data")
  if (!dir.exists(target)) stop("Missing 'code_data' inside: ", owd)
  setwd(target)
}

# set base
base_dir <- getwd()

# add in input / output directory for visualizations 
raw_dir         <- file.path(base_dir, "inputs")
output_figs     <- file.path(base_dir, "output-figs")
output_tables   <- file.path(base_dir, "output-tables")

# -----------------------------------------------------------------------
###### ###### ###### ######  LOAD IN DATASETS  ###### ###### ###### ###### 
# -----------------------------------------------------------------------


# find files
paths <- list.files(
  path = raw_dir,
  pattern = "\\.(csv|xlsx)$",
  ignore.case = TRUE,
  full.names = TRUE
)


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

