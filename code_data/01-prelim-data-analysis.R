# author: Allegra Saggese
# last updated: 06 nov 2025
# purpose: prelim analysis for macro measurement 
library(tidyr)
library(dplyr)
library(ggplot2)

# -----------------------------------------------------------------------
###### ###### ###### ######  LOAD IN DATASETS  ###### ###### ###### ###### 
# -----------------------------------------------------------------------

owd    <- getwd()
target <- file.path(getwd(), "code_data")
if (!dir.exists(target)) stop("Missing 'code_data' inside: ", getwd())
setwd(target)
getwd()

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

# data review
stopifnot(is.list(datasets), length(names(datasets)) == length(datasets))

is_intlike <- function(x) is.numeric(x) && all(is.na(x) | x == floor(x))
likely_year_colnames <- function(nm) grepl("(^|_)(year|yr)(_|$)", nm, ignore.case = TRUE)

scan_year_cols <- function(df) {
  by_name <- names(df)[likely_year_colnames(names(df))]
  by_vals <- names(df)[sapply(df, function(x)
    is_intlike(x) && mean(x >= 1900 & x <= 2100, na.rm = TRUE) > 0.8)]
  unique(c(by_name, by_vals))
}

key_candidates <- function(df) {
  keys <- c("year","Year","YR","FIPS","STATEFP","STATE","state",
            "county","County","county_name","NAICS","naics",
            "zip","ZIP","ZIPCODE","zip_code","geoid","GEOID")
  intersect(keys, names(df))
}

peek_columns <- function(df, n_show = 40) {
  cls <- sapply(df, function(x) paste(class(x), collapse = "|"))
  nunq <- sapply(df, function(x) length(unique(x[!is.na(x)])))
  nna  <- sapply(df, function(x) sum(is.na(x)))
  ex   <- sapply(df, function(x) {
    v <- unique(x[!is.na(x)])
    paste(utils::head(v, 3), collapse = " | ")
  })
  out <- data.frame(column = names(df),
                    class = unname(cls),
                    n_unique = unname(nunq),
                    n_na = unname(nna),
                    example = unname(ex),
                    stringsAsFactors = FALSE)
  out[seq_len(min(nrow(out), n_show)), , drop = FALSE]
}

year_ranges <- function(df, ycols) {
  if (!length(ycols)) return(NULL)
  do.call(rbind, lapply(ycols, function(yc) {
    v <- as.integer(df[[yc]])
    v <- v[!is.na(v)]
    if (!length(v)) return(NULL)
    data.frame(column = yc, min_year = min(v), max_year = max(v))
  }))
}

small_cats <- function(df, max_levels = 30) {
  idx <- which(sapply(df, function(x) is.character(x) || is.factor(x)))
  idx <- idx[sapply(df[idx], function(x) length(unique(x[!is.na(x)])) <= max_levels)]
  if (!length(idx)) return(NULL)
  out <- lapply(names(idx), function(nm) {
    vals <- sort(unique(df[[nm]][!is.na(df[[nm]])]))
    data.frame(column = nm, level = vals, stringsAsFactors = FALSE)
  })
  do.call(rbind, out)
}

explore_dataset <- function(nm, df, show_rows = 3) {
  cat("\n", strrep("=", 78), "\n", sep = "")
  cat("DATASET: ", nm, "\n", sep = "")
  cat("dim: ", nrow(df), " x ", ncol(df), "\n", sep = "")
  cat("colnames[1:10]: ", paste(utils::head(colnames(df), 10), collapse = ", "), "\n", sep = "")
  
  keys <- key_candidates(df)
  if (length(keys)) cat("key candidates: ", paste(keys, collapse = ", "), "\n", sep = "")
  
  ycols <- scan_year_cols(df)
  if (length(ycols)) {
    yr <- year_ranges(df, ycols)
    if (!is.null(yr)) {
      cat("year ranges:\n")
      print(yr, row.names = FALSE)
    }
  }
  
  cat("\ncolumn summary (first 40):\n")
  print(peek_columns(df, n_show = 40), row.names = FALSE)
  
  cat("\nhead():\n")
  suppressWarnings(print(utils::head(df, show_rows)))
  
  cats <- small_cats(df, max_levels = 15)
  if (!is.null(cats)) {
    cat("\nsmall categorical previews (<=15 levels):\n")
    print(cats, row.names = FALSE)
  }
}

# run on all datasets
invisible(lapply(names(datasets), function(nm) explore_dataset(nm, datasets[[nm]])))

# consolidate the dictionaries 
make_dictionary <- function(datasets) {
  do.call(rbind, lapply(names(datasets), function(nm) {
    df <- datasets[[nm]]
    data.frame(
      dataset = nm,
      column = names(df),
      class = sapply(df, function(x) paste(class(x), collapse="|")),
      n = nrow(df),
      n_na = sapply(df, function(x) sum(is.na(x))),
      n_unique = sapply(df, function(x) length(unique(x[!is.na(x)]))),
      stringsAsFactors = FALSE
    )
  }))
}

dict <- make_dictionary(datasets)
dict$na_pct <- round(100 * dict$n_na / pmax(dict$n, 1), 2)
utils::write.csv(dict, "00_column_dictionary.csv", row.names = FALSE)

# Optional: save year coverage
year_cov <- do.call(rbind, lapply(names(datasets), function(nm) {
  df <- datasets[[nm]]
  ycols <- scan_year_cols(df)
  yr <- year_ranges(df, ycols)
  if (is.null(yr)) return(NULL)
  cbind(data.frame(dataset = nm, stringsAsFactors = FALSE), yr)
}))
if (!is.null(year_cov)) utils::write.csv(year_cov, "00_year_ranges.csv", row.names = FALSE)


# -----------------------------------------------------------------------
###### ###### ###### ######  DATA CLEANING  ###### ###### ###### ###### 
# -----------------------------------------------------------------------

# DF 1 ------ TAX COLLECTION DATA 
nm <- "2023-FRED-CA-tax-collected"  
tax_q_df <- datasets[[nm]]

# convert to date format so we parse out month, yr, and fiscal quarter
tax_q_df$observation_date <- as.Date(tax_q_df$observation_date, format = "%m/%d/%y")
d <- tax_q_df$observation_date

# calendar month and year
m <- ifelse(is.na(d), NA_integer_, as.integer(format(d, "%m")))
y <- ifelse(is.na(d), NA_integer_, as.integer(format(d, "%Y")))
q <- ifelse(is.na(d), NA_character_, paste0("Q", ((m - 1L - 6L) %% 12L) %/% 3L + 1L))
fy <- ifelse(is.na(d), NA_integer_, ifelse(m >= 7L, y + 1L, y))

# attach columns back
tax_q_df$month          <- m
tax_q_df$year           <- y
tax_q_df$fiscal_quarter <- q
tax_q_df$fiscal_year    <- fy

# save back into the list
datasets[[nm]] <- tax_q_df

# quick check
str(datasets[[nm]][, c("observation_date","month","year","fiscal_quarter","fiscal_year")])
head(datasets[[nm]][, c("observation_date","month","year","fiscal_quarter","fiscal_year")])


# early plot of tax collected in california over time 
ggplot(tax_q_df, aes(x = observation_date, y = total_tax_collected_CA)) +
  geom_line() +
  geom_point() +
  labs(x = "Year", y = "Tax collected (US millions $)") +
  theme_minimal()




# DF 2 ------ AGGREGATE GDP DATA 
gdp <- "2024-CA-GDP-TOTAL"
gdp_df <- datasets[[gdp]]

gdp_df <- gdp_df[-c(1:4), ]
names(gdp_df) <- c("year", "current_us_dollars", "1997_dollars", "deflator_1997_100",
                   "current_us", "annual_pct_change", "deflator")

# make numeric 
gdp_df$year <- as.numeric(gdp_df$year)

# drop NA cols in year
gdp_df_v2 <- gdp_df[!is.na(gdp_df$year), ]

# make the other vals numeric 
gdp_df_v2$current_us_dollars <- as.numeric(gdp_df_v2$current_us_dollars)
gdp_df_v2$`1997_dollars` <- as.numeric(gdp_df_v2$`1997_dollars`)
gdp_df_v2$annual_pct_change <- as.numeric(gdp_df_v2$annual_pct_change)


# plot GDP over time (pct change, and total - aggregate)
ggplot(gdp_df_v2, aes(x = year, y = annual_pct_change)) +
  geom_line() +
  geom_point() +
  labs(x = "Year", y = "Percentage change in GDP per year") +
  theme_minimal()

ggplot(gdp_df_v2, aes(x = year, y = current_us_dollars)) +
  geom_line() +
  geom_point() +
  labs(x = "Year", y = "GDP (US dollars)") +
  theme_minimal()



# DF 3 ------ Personal Consumption Expenditure (PCE) for CA
pce <- "CA-PCE-annual-FRED-FED"
pce_df <- datasets[[pce]]

pce_df$observation_date <- as.Date(pce_df$observation_date, format = "%Y-%d-%m")
e <- pce_df$observation_date

# calendar month and year
m <- ifelse(is.na(e), NA_integer_, as.integer(format(e, "%m"))) # STILL NOT WORKING 
y <- ifelse(is.na(e), NA_integer_, as.integer(format(e, "%Y")))
q <- ifelse(is.na(e), NA_character_, paste0("Q", ((m - 1L - 6L) %% 12L) %/% 3L + 1L))
fy <- ifelse(is.na(e), NA_integer_, ifelse(m >= 7L, y + 1L, y))

# attach columns back
pce_df$month          <- m # WRONG WRONG 
pce_df$year           <- y
pce_df$fiscal_quarter <- q
pce_df$fiscal_year    <- fy

# save back into the list
datasets[[pce]] <- pce_df
class(pce_df$CAPCE)

# plot against time 
ggplot(pce_df, aes(x = year, y = CAPCE)) +
  geom_line() +
  geom_point() +
  labs(x = "Year", y = "CA PCE (US dollars)") +
  theme_minimal()


