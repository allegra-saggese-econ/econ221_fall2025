# author: Allegra Saggese
# last updated: 04 Dec 2025
# purpose: final analysis for macro measurement 


owd <- getwd()
# if current folder is not 'code_data', move into it
if (basename(owd) != "code_data") {
  target <- file.path(owd, "code_data")
  if (!dir.exists(target)) stop("Missing 'code_data' inside: ", owd)
  setwd(target)
}

# execute start file with data, etc. 
source("00-startup.R")
# inspect data sets 
names(datasets)

# -----------------------------------------------------------------------
###### ###### ###### ######  PRE-ANALYSIS: SUFFICIENT DATA UPLOAD   ###### ###### ###### ###### 
# -----------------------------------------------------------------------

# data review --- check to ensure sheets all loaded - note manual edits made to the data 
stopifnot(is.list(datasets), length(names(datasets)) == length(datasets))

is_intlike <- function(x) is.numeric(x) && all(is.na(x) | x == floor(x))
likely_year_colnames <- function(nm) grepl("(^|_)(year|yr)(_|$)", nm, ignore.case = TRUE)

# scan for relevant col names
scan_year_cols <- function(df) {
  by_name <- names(df)[likely_year_colnames(names(df))]
  by_vals <- names(df)[sapply(df, function(x)
    is_intlike(x) && mean(x >= 1900 & x <= 2100, na.rm = TRUE) > 0.8)]
  unique(c(by_name, by_vals))
}

# try to ID relevant cols across cols - although we're going to use a manual approach
key_candidates <- function(df) {
  keys <- c("year","Year","YR","FIPS","STATEFP","STATE","state",
            "county","County","county_name","NAICS","naics",
            "zip","ZIP","ZIPCODE","zip_code","geoid","GEOID")
  intersect(keys, names(df))
}

# function to review cols quickly and their names 
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

# check years on data 
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

# big review of datasets 
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

# run on all datasets -- load in the datasets to review ranges of years 
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

# browse universe of variables --- although its not so standard - this doesn't work 
dict <- make_dictionary(datasets)
dict$na_pct <- round(100 * dict$n_na / pmax(dict$n, 1), 2)
utils::write.csv(dict, "00_column_dictionary.csv", row.names = FALSE)

#  save year coverage
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

# DF 1a ------ TAX COLLECTION DATA (TOTAL) FROM FRED FED (including income and corporate tax)
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

tax_q_df_collapse <- tax_q_df %>%
  group_by(year) %>%
  summarise(total_tax_year = sum(total_tax_collected_CA, na.rm = TRUE))

# early plot of tax collected in california over time 
ggplot(tax_q_df, aes(x = observation_date, y = total_tax_collected_CA)) +
  geom_line() +
  geom_point() +
  labs(x = "Year", y = "Tax collected (US millions $)") +
  theme_minimal()


# DF 1b ----- sales and use tax in CA only 
collected_tax <- "CA-FED-taxable_sales_collected"
tax_coll_df <- datasets[[collected_tax]]
names(tax_coll_df) <- tolower(names(tax_coll_df))
tax_coll_df$amount_mills_usd <- tax_coll_df$amount_thousands_usd/1000 # create mil col for comparison


# DF 1c ----- income tax in CA only 
collected_inc_tax <- "CAINCTAX"
inc_tax_coll_df <- datasets[[collected_inc_tax]]
names(inc_tax_coll_df) <- tolower(names(inc_tax_coll_df)) 


# convert to date format so we parse out month, yr, and fiscal quarter
inc_tax_coll_df$observation_date <- as.Date(inc_tax_coll_df$observation_date, format = "%m/%d/%y")
d <- inc_tax_coll_df$observation_date

# calendar month and year
m <- ifelse(is.na(d), NA_integer_, as.integer(format(d, "%m")))
y <- ifelse(is.na(d), NA_integer_, as.integer(format(d, "%Y")))
q <- ifelse(is.na(d), NA_character_, paste0("Q", ((m - 1L - 6L) %% 12L) %/% 3L + 1L))
fy <- ifelse(is.na(d), NA_integer_, ifelse(m >= 7L, y + 1L, y))

# attach columns back
inc_tax_coll_df$month          <- m
inc_tax_coll_df$year           <- y
inc_tax_coll_df$fiscal_quarter <- q
inc_tax_coll_df$fiscal_year    <- fy

# save back into the list
datasets[[collected_inc_tax]] <- inc_tax_coll_df

# quick check
str(datasets[[collected_inc_tax]][, c("observation_date","month","year","fiscal_quarter","fiscal_year")])
head(datasets[[collected_inc_tax]][, c("observation_date","month","year","fiscal_quarter","fiscal_year")])

inc_tax_coll_df <- inc_tax_coll_df[-(1:58), ] # start in 2000 (earlier years not useful)
inc_tax_coll_df$amount_mills_usd <- inc_tax_coll_df$ca_income_tax_thousands_of_dollars/1000 # create mil col for comparison

## PLOT ALL 3 LINES 
ggplot() +
  geom_line(data = inc_tax_coll_df,
            aes(x = year, y = amount_mills_usd, color = "Income Tax"),
            size = 1) +
  geom_line(data = tax_coll_df,
            aes(x = year, y = amount_mills_usd, color = "Sales Tax"),
            size = 1) +
  geom_line(data = tax_q_df_collapse,
            aes(x = year, y = total_tax_year, color = "Total Tax"),
            size = 1) +
  labs(
    x = "Year",
    y = "Tax collected (millions USD)",
    color = "FRED FED series"
  ) +
  theme_minimal()

# DF 1d ----- UR unemployment rate in CA for biz cycle graph
UR <- "CAUR"
ur_df <- datasets[[UR]]
names(ur_df) <- tolower(names(ur_df))

ur_df$date <- as.Date(ur_df$observation_date, format = "%m/%d/%y")
ur_df$year <- format(ur_df$date, "%Y")

# create annual average for exploratory stats 
ur_yearly <- ur_df %>%
  group_by(year) %>%
  summarise(urate_avg = mean(caur, na.rm = TRUE))

ur_yearly$year <- as.numeric(ur_yearly$year) # make numeric for plot 


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
ggplot(gdp_df_v2, aes(x = year, y = gdp_nominal_dollars)) +
  geom_line() +
  geom_point() +
  labs(x = "Year", y = "GDP (millions USD)") +
  theme_minimal()


# DF 3 ------ Personal Consumption Expenditure (PCE) for CA

pce <- "CA-PCE-annual-FRED-FED" # total for CA (not per capita)
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
  labs(x = "Year", y = "CA PCE (millions US dollars)") +
  theme_minimal()


### PLOT GDP AND STUFF 
ggplot() +
  
  # ---- PCE ----
geom_line(data = pce_df,
          aes(x = year, y = CAPCE, color = "PCE"),
          size = 1) +
  geom_point(data = pce_df,
             aes(x = year, y = CAPCE, color = "PCE"),
             size = 2) +
  
  # ---- GDP ----
geom_line(data = gdp_df_v2,
          aes(x = year, y = gdp_nominal_dollars, color = "GDP"),
          size = 1) +
  geom_point(data = gdp_df_v2,
             aes(x = year, y = gdp_nominal_dollars, color = "GDP"),
             size = 2) +
  
  # ---- Axes ----
scale_y_continuous(
  name = "PCE & GDP (millions USD)")
  # ---- Colors ----
scale_color_manual(values = c(
  "PCE" = "blue",
  "GDP" = "red",
  "Unemployment Rate" = "darkgreen"
)) +
  
  # ---- Labels ----
labs(
  x = "Year",
  color = "Series"
) +
  
  theme_minimal()

# create PCE - sales tax 
df_merge <- merge(
  inc_tax_coll_df |> dplyr::rename(pce = amount_mills_usd),
  tax_coll_df     |> dplyr::rename(salestax = amount_mills_usd),
  by = "year"
)

df_merge$diff_pce_sales <- df_merge$pce - df_merge$salestax


### PLOT - unemployment vs change in GDP / PCE 
ggplot(df_merge, aes(x = year, y = diff_pce_sales)) +
  geom_line(color = "purple", size = 1.2) +
  geom_point(color = "purple", size = 2) +
  labs(
    x = "Year",
    y = "PCE – Sales Tax (millions USD)",
    title = "Difference Between PCE and Sales Tax"
  ) +
  theme_minimal()


## AGAINST UNEMPLOYMENT
# scale unemployment so it fits on left axis
scale_factor <- max(df_merge$diff_pce_sales, na.rm = TRUE) / 
  max(ur_yearly$urate_avg, na.rm = TRUE)

# restrict sample
ur_filtered <- ur_yearly %>% 
  dplyr::filter(year %in% df_merge$year)

ggplot() +
  # ----- Difference Line (PCE – Sales Tax) -----
geom_line(data = df_merge,
          aes(x = year, y = diff_pce_sales, color = "PCE – Sales Tax"),
          size = 1.2) +
  geom_point(data = df_merge,
             aes(x = year, y = diff_pce_sales, color = "PCE – Sales Tax"),
             size = 2) +
  
  # ----- Unemployment Rate (scaled) -----
geom_line(data = ur_filtered,
          aes(x = year, y = urate_avg * scale_factor, color = "Avg Unemployment"),
          size = 1, linetype = "dashed") +
  
  # ----- Axes -----
scale_y_continuous(
  name = "PCE – Sales Tax (millions USD)",
  sec.axis = sec_axis(~ . / scale_factor,
                      name = "Unemployment Rate (%)")
) +
  
  # ----- Legend Colors -----
scale_color_manual(values = c(
  "PCE – Sales Tax" = "purple",
  "Avg Unemployment" = "darkgreen"
)) +
  
  labs(
    x = "Year",
    color = "Series",
    title = "PCE - collected sales vs. unemployment rate"
  ) +
  theme_minimal()


# DF 4  ------ LOCAL TAX RATES IN CA 

sales_tax_rates <- read_excel("code_data/SalesTaxRates.xlsx") # DONT RE-READ IN - TAKE FROM DATAFRAME
colnames(sales_tax_rates) <- as.character(unlist(sales_tax_rates[1, ]))
sales_tax_rates <- sales_tax_rates[-1, ]
names(sales_tax_rates) <- tolower(names(sales_tax_rates))

sales_tax_rates <- sales_tax_rates %>%
  mutate(
    rate = gsub("%", "", rate),        # remove percent signs
    rate = gsub("[^0-9\\.]", "", rate),# drop any stray symbols/spaces
    rate = as.numeric(rate) / ifelse(max(rate, na.rm = TRUE) > 1, 100, 1)  # divide by 100 only if values look like percents
  )

sales_tax_avg <- sales_tax_rates %>%
  group_by(county) %>%
  summarise(avg_rate = mean(rate, na.rm = TRUE), .groups = "drop") %>%
  mutate(year = 2025)

# take CA average for the previous years 
sales_tax_hist <- data.frame(
  year = 2005:2024,
  eff_rate = NA_real_
)

# taken from https://cdtfa.ca.gov/taxes-and-fees/sales-use-tax-rates-history.htm#note
#### MOVE THIS TO A NEW SCRIPT - MANUAL DATA CREATIONS 
sales_tax_hist$eff_rate <- c(
  .0725, .0725, .0725, .0725, .0725, .0725, .0725, .0725,
  .0750, .0750, .0750, .0750, .0725, .0725, .0825, .0825, 
  .0725, .0725, .0725, .0725
)

sales_tax_avg_full <- sales_tax_avg %>%
  bind_rows(
    sales_tax_avg %>%
      select(county) %>%
      crossing(year = 2005:2024) %>%
      mutate(avg_rate = NA_real_)
  ) %>%
  arrange(county, year)

# now merge in eff_rate for 2005–2024
sales_tax_avg_full <- left_join(sales_tax_avg_full, sales_tax_hist, by = "year")
head(sales_tax_avg_full)

# combine cols 
sales_tax_avg_full <- sales_tax_avg_full %>%
  mutate(rate = ifelse(!is.na(eff_rate), eff_rate, avg_rate)) %>%
  select(county, year, rate)


# from BLS data - estimate quintile tax incidence over bundles
incidence_avg <- c(
  q1 = 0.293,
  q2 = 0.180,
  q3 = 0.200,
  q4 = 0.259,
  q5 = 0.342
)


# DF 5 ------- MEDIAN INCOME OVER TIME 
years <- setdiff(2005:2023, 2020)   # skip 2020

# NOTE THE API-CALLED DATA IS NOT IN DATAFRAMES -- MOVE THIS TO A SEPARATE SCRIPT TO CALL ACS DATA SEP AND STORE 
acs_income <- map_dfr(years, ~
                        get_acs(
                          geography = "county",
                          state = "CA",
                          variables = "B19013_001",
                          year = .x,
                          survey = "acs1"
                        ) %>%
                        mutate(source = "ACS", year = .x)
)

add_quintiles <- function(df, value_col = "estimate", year_col = "year") {
  df %>%
    group_by(.data[[year_col]]) %>%
    mutate(quintile = ntile(.data[[value_col]], 5)) %>%
    ungroup()
}

acs_income_q <- add_quintiles(acs_income)

# remove California and any other padding for match
acs_income_q <- acs_income_q %>%
  mutate(
    county = trimws(sub(", California$", "", NAME))
  )


# DF 6 ------- BLS CE by QUINTILE for CA 
path <- "code_data/BLS_CES_tables_per_yr.xlsx" # DONT RE-READ IN - TAKE FROM DATAFRAME
sheets <- excel_sheets(path)

bls_df <- map_dfr(sheets, ~
                    read_excel(path, sheet = .x) %>%
                    mutate(year = .x)  # keep sheet name as year
)

# not great format - pulled data I needed manually in Excel first 
bls_df <- read_excel("code_data/BLS_CES_tables_per_yr.xlsx", sheet = 7)
names(bls_df) <- tolower(names(bls_df))
names(bls_df)[2:6] <- c("q1", "q2", "q3", "q4", "q5")

# take growth rate from CA FED of PCE to interpolate missing years (2005-2017)
pce_growth <- pce_df %>%
  arrange(year) %>%
  mutate(pce_growth = CAPCE / dplyr::lag(CAPCE))

# backfill
pce_g <- pce_growth %>% filter(year < 2017) %>% arrange(desc(year))
pce_g <- pce_g[order(pce_g$year), ]

# for each quintile, backcast CE
years_to_fill <- 2005:2016
base_vals <- as.numeric(bls_df[bls_df$year == 2017, c("q1","q2","q3","q4","q5")])
g <- pce_g$pce_growth[pce_g$year %in% years_to_fill]
cum_factor <- rev(cumprod(rev(g)))   # inverse growth

backcast <- data.frame(
  year = years_to_fill,
  q1 = base_vals[1] / cum_factor,
  q2 = base_vals[2] / cum_factor,
  q3 = base_vals[3] / cum_factor,
  q4 = base_vals[4] / cum_factor,
  q5 = base_vals[5] / cum_factor
)

bls_df_full <- bind_rows(backcast, bls_df)

# merge in with the ACS data on quintiles
acs_income_q <- acs_income_q %>%
  mutate(quintile = paste0("q", quintile)) %>%      
  left_join(
    bls_df_full %>%
      select(year, q1, q2, q3, q4, q5) %>%
      tidyr::pivot_longer(
        cols = starts_with("q"),
        names_to = "quintile",
        values_to = "pce_estimate"
      ),
    by = c("year", "quintile")
  )


# DF 6 --------- POPULATION ESTIMATES 

# agg data 
capop <- read.csv("code_data/CAPOP.csv")
capop <- capop  %>%
  rename(pop = CAPOP) %>%          
  mutate(pop = tolower(pop),       
         pop = as.numeric(pop) * 1000,
         year = as.integer(format(as.Date(observation_date), "%Y")))  # convert values to numeric and multiply by 1000

# county level data
cty_pop <- read_excel("code_data/CA-county-pop-estimates-final.xlsx", sheet=1)
names(cty_pop)[2:ncol(cty_pop)] <- paste0("yr_", 2000:2025)
names(cty_pop) <- tolower(names(cty_pop))

cty_pop_long <- cty_pop %>% # pivot longer 
  pivot_longer(
    cols = starts_with("yr_"),
    names_to = "year",
    values_to = "pop_est"
  ) %>%
  mutate(year = as.integer(sub("yr_", "", year)))


# -----------------------------------------------------------------------
###### ###### ###### ######  DATA MERGING  ###### ###### ###### ###### 
# -----------------------------------------------------------------------

# combine ACS quintile data w/ effective tax rates + estimate for tax incidence on PCE 
acs_income_q <- acs_income_q %>%
  mutate(county = str_remove(county, " County$"),
         county = str_trim(county))

# sales tax (eff rate at county level) in with the quintile<>county data
merged_df <- acs_income_q %>%
  left_join(sales_tax_avg_full, by = c("county", "year"))

# check merge col 
merged_df$quintile <- as.character(merged_df$quintile)
unique(merged_df$quintile)

merged_df$incidence_avg <- incidence_avg[merged_df$quintile]
merged_df$est_taxable_spend <- merged_df$incidence_avg * merged_df$pce_estimate # create new col for taxable spend
merged_df$est_tax_rev <- merged_df$est_taxable_spend * merged_df$rate # create new col for taxable spend

merged_df_v2 <- merged_df %>%
  left_join(cty_pop_long, by = c("county", "year"))

# estimated collectable revenue at the county level 
merged_df_v2$est_tax_tocollect <- merged_df_v2$est_tax_rev * merged_df_v2$pop_est # create new col for taxable spend

# EXPORT THE MERGED dataset so data cleaning process isn't necessary next time 


# -----------------------------------------------------------------------
###### ###### ###### ######  Estimate tax collection  ###### ###### ###### ###### 
# -----------------------------------------------------------------------

# CREATE YR LEVEL DF 
annual_est <- merged_df_v2 %>%
  group_by(year) %>%
  summarise(
    est_tax_rev_percapita = mean(est_tax_rev, na.rm = TRUE),
    est_rev_to_collect= sum(est_tax_tocollect, na.rm = TRUE),
    pop = sum(pop_est, na.rm = TRUE),
    pce_est = mean(pce_estimate, na.rm = TRUE),
    est_taxable_tot = mean(est_taxable_spend, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(year != 2023)

# merge in collected tax data
annual_est <- left_join(annual_est, collected_tax %>% 
                                select(year, amount_thousands_usd), by = "year")

annual_est <- annual_est %>%
  mutate(reported_tax = amount_thousands_usd * 1000)

# test plot per capita
ggplot(annual_est, aes(x = year, y = est_tax_rev_percapita)) +
  geom_line() +
  geom_point() +
  labs(
    x = "Year",
    y = "Tax Revenue (per capita)",
    title = "CA Sales Tax (per capita): Estimate"
  ) +
  theme_minimal()

# plot total (estimate)
ggplot(annual_est, aes(x = year, y = est_rev_to_collect)) +
  geom_line() +
  geom_point() +
  labs(
    x = "Year",
    y = "Estimated Tax Revenue (total)",
    title = "CA Sales tax estimate"
  ) +
  theme_minimal()

# plot total against collected 
ggplot(annual_est, aes(x = year)) +
  geom_line(aes(y = est_rev_to_collect, color = "Estimated"), linewidth = 1.1) +
  geom_line(aes(y = reported_tax, color = "Collected"), linewidth = 1.1) +
  geom_point(aes(y = est_rev_to_collect, color = "Estimated"), size = 2) +
  geom_point(aes(y = reported_tax, color = "Collected"), size = 2) +
  labs(
    x = "Year",
    y = "Revenue (nominal USD)",
    color = "",
    title = "CA Sales tax estimate v. actual (using county-level disaggregation)"
  ) +
  theme_minimal()


# -----------------------------------------------------------------------
###### ###### ###### ######  Compare county w/ agg vals  ###### ###### ###### ###### 
# -----------------------------------------------------------------------

# test for measurement error 
annual_vals_for_state <- merged_df_v2 %>% group_by(year) %>%
  summarise(
    rate = mean(rate, na.rm = TRUE),
    incidence_avg = mean(incidence_avg, na.rm = TRUE),
    .groups = "drop"
  )

state_level_est <- left_join(annual_vals_for_state, pce_df %>% 
                          select(year, CAPCE), by = "year")

state_level_est <- left_join(state_level_est, annual_est %>% 
                               select(year, pop), by = "year")

# write estimate var
state_level_est$total_est_rev <- (state_level_est$incidence_avg*state_level_est$CAPCE)*state_level_est$rate

# plot total (state level est)
ggplot(state_level_est, aes(x = year, y = total_est_rev)) +
  geom_line() +
  geom_point() +
  labs(
    x = "Year",
    y = "Estimated Tax Revenue (thousands USD)",
    title = "CA Sales tax estimate"
  ) +
  theme_minimal()


# plot state, county and actual 
ggplot(annual_est, aes(x = year)) +
  geom_line(aes(y = est_rev_to_collect, color = "estimated"), linewidth = 1.1) +
  geom_line(aes(y = reported_tax, color = "collected"), linewidth = 1.1) +
  geom_point(aes(y = est_rev_to_collect, color = "estimated"), size = 2) +
  geom_point(aes(y = reported_tax, color = "collected"), size = 2) +
  
  # overlay data from another dataframe p
  geom_line(
    data = state_level_est,
    aes(x = year, y = (total_est_rev*1000000), color = "state-level"),
    linewidth = 1.1,
    linetype = "dashed"
  ) +
  geom_point(
    data = state_level_est,
    aes(x = year, y = (total_est_rev*1000000), color = "state-level"),
    size = 2
  ) +
  
  labs(
    x = "Year",
    y = "Revenue (nominal USD)",
    color = "",
    title = "CA Sales Tax: Modelled vs. actual collections"
  ) +
  theme_minimal()

# calculate transactions that are taxable vs. estimate of taxable amount
taxable_trans <- read_excel("code_data/CA-salestax-year-industry.xlsx")
names(taxable_trans) <- as.character(unlist(taxable_trans[1, ]))
taxable_trans <- taxable_trans[-1, ]

names(taxable_trans) <- names(taxable_trans) |>
  tolower() |>
  gsub(" ", "_", x = _)

# calc annual amount
taxable_trans_annual <- taxable_trans %>%
  filter(quarter == "A", !is.na(naics)) %>%     # keep only annual rows with valid NAICS codes
  mutate(
    taxable_transactions_amount = as.numeric(gsub(",", "", taxable_transactions_amount))
  ) %>%
  group_by(calendar_year) %>%
  summarise(
    real_tax_amt_total = sum(taxable_transactions_amount, na.rm = TRUE),
    .groups = "drop"
  )

annual_est$tot_taxable = annual_est$est_taxable_tot*annual_est$pop
annual_est <- annual_est %>%
  mutate(year = as.numeric(year))

taxable_trans_annual <- taxable_trans_annual %>%
  mutate(year = as.numeric(calendar_year))

# compare estimates 
combined_df <- left_join(
  annual_est %>% select(year, tot_taxable),
  taxable_trans_annual %>% select(year, real_tax_amt_total),
  by = "year"
)

# then plot both lines on the same graph - note that model EXCLUDES public spend 
ggplot(combined_df, aes(x = year)) +
  geom_line(aes(y = tot_taxable, color = "Model Estimate"), linewidth = 1.1) +
  geom_line(aes(y = real_tax_amt_total, color = "Reported Taxable Sales"), linewidth = 1.1) +
  geom_point(aes(y = tot_taxable, color = "Model Estimate"), size = 2) +
  geom_point(aes(y = real_tax_amt_total, color = "Reported Taxable Sales"), size = 2) +
  labs(
    x = "Year",
    y = "Taxable Sales (nominal USD)",
    color = "",
    title = "California: Estimated vs. Reported Taxable Transactions"
  ) +
  theme_minimal()


# add in public admin spending to estimate - see if it adjusts 
taxable_trans_public <- taxable_trans %>%
  filter(
    business_type == "Public Administration",
    quarter == "A"
  ) %>%
  mutate(
    taxable_transactions_amount = as.numeric(gsub(",", "", taxable_transactions_amount))
  ) %>%
  group_by(calendar_year) %>%
  summarise(
    real_tax_amt_total = sum(taxable_transactions_amount, na.rm = TRUE),
    .groups = "drop"
  )

# join in w the combined data to compare once we add public admin 
combined_df <- combined_df %>%
  mutate(year = as.numeric(year))

taxable_trans_public <- taxable_trans_public %>%
  mutate(calendar_year = as.numeric(calendar_year))

combined_df <- left_join(
  combined_df,
  taxable_trans_public %>% select(calendar_year, real_tax_amt_total) %>%
    rename(year = calendar_year, public_admin_tax = real_tax_amt_total),
  by = "year"
)

# create new columns
combined_df <- combined_df %>%
  mutate(
    est_tax_amt_w_public = tot_taxable + public_admin_tax,
    diff_est_vs_model = est_tax_amt_w_public - real_tax_amt_total
  )


# -----------------------------------------------------------------------
###### ###### ###### ###### GRAPHICS  ###### ###### ###### ###### 
# -----------------------------------------------------------------------


ggplot(combined_df, aes(x = year)) +
  geom_line(aes(y = est_tax_amt_w_public/1000000, color = "PCE est. w/ public admin"), linewidth = 1.1) +
  geom_line(aes(y = real_tax_amt_total/1000000, color = "CA reported trans."), linewidth = 1.1) +
  geom_point(aes(y = est_tax_amt_w_public/1000000, color = "PCE est. w/ public admin"), size = 2) +
  geom_point(aes(y = real_tax_amt_total/1000000, color = "CA reported trans."), size = 2) +
  scale_y_continuous(labels = label_comma()) +
  scale_color_manual(
    values = c(
      "PCE est. w/ public admin" = "#f8766d",   # blue (example)
      "CA reported trans." = "#619cff"              # red (example)
    )
  ) +
  labs(
    x = "Year",
    y = "Taxable transactions (millions USD)",
    color = "",
    title = "Total amount of taxable sales estimate v. reported"
  ) +
  theme_minimal()

#### ADDITIONAL APPROACH: add annual rate of tax in to estimate denominator again 
v2_tax_est <- combined_df %>% left_join(annual_vals_for_state, by = c("year"))

# create new col for estimated rev
v2_tax_est$trans_est_revenue <- (v2_tax_est$real_tax_amt_total*v2_tax_est$incidence_avg)*v2_tax_est$incidence_avg

# compare transaction-based revenue with household level revenue projection
annual_est <- annual_est %>% mutate(year = as.numeric(year))
v2_tax_est <- v2_tax_est %>% mutate(year = as.numeric(year))

# merge on year
combined_plot_df <- left_join(
  annual_est %>% select(year, est_rev_to_collect),
  v2_tax_est %>% select(year, trans_est_revenue),
  by = "year"
)

# plot both series (i.e. two estimates )
ggplot(combined_plot_df, aes(x = year)) +
  geom_line(aes(y = est_rev_to_collect, color = "PCE-based estimate"), linewidth = 1.1) +
  geom_line(aes(y = trans_est_revenue, color = "Transactions estimate"), linewidth = 1.1, linetype = "dashed") +
  geom_point(aes(y = est_rev_to_collect, color = "PCE-based estimate"), size = 2) +
  geom_point(aes(y = trans_est_revenue, color = "Transactions estimate"), size = 2) +
  labs(
    x = "Year",
    y = "Revenue (nominal USD)",
    color = "",
    title = "Comparison of Revenue Estimates"
  ) +
  theme_minimal()

combined_plot_df <- combined_plot_df %>%
  left_join(
    collected_tax %>%
      mutate(collected_tax = as.numeric(amount_thousands_usd) * 1000) %>%  # scale values
      select(year, collected_tax),
    by = "year"
  )

ggplot(combined_plot_df, aes(x = year)) +
  geom_line(aes(y = est_rev_to_collect/1000000, color = "PCE rev. est."), linewidth = 1.1, , linetype = "dashed") +
  geom_line(aes(y = trans_est_revenue/1000000, color = "Trans. rev. est."), linewidth = 1.1, linetype = "dashed") +
  geom_line(aes(y = collected_tax/1000000, color = "Reported rev."), linewidth = 1.1) +
  geom_point(aes(y = est_rev_to_collect/1000000, color = "PCE rev. est."), size = 2) +
  geom_point(aes(y = trans_est_revenue/1000000, color = "Trans. rev. est."), size = 2) +
  geom_point(aes(y = collected_tax/1000000, color = "Reported rev."), size = 2) +
  labs(
    x = "Year",
    y = "Revenue (million USD)",
    color = "",
    title = "Tax revenue estimates and actual comparisons"
  ) +
  scale_y_continuous(labels = scales::label_comma()) +
  theme_minimal()

# -----------------------------------------------------------------------
###### ###### ###### ######  CALC FORMULA VALS  ###### ###### ###### ###### 
# -----------------------------------------------------------------------

# ratio of collection based on [actual rev collection] / [projected revenue]
gamma_t = combined_plot_df$collected_tax / combined_plot_df$est_rev_to_collect
gamma_tot_trans =  combined_plot_df$collected_tax / combined_plot_df$trans_est_revenue 

delta_t = (combined_plot_df$collected_tax - combined_plot_df$est_rev_to_collect) / combined_plot_df$est_rev_to_collect
delta_tot_trans = (combined_plot_df$collected_tax - combined_plot_df$trans_est_revenue) / combined_plot_df$trans_est_revenue 

# upload GVA (GSP) data - note the nuance is necessary here 
sagdp2_df <- read.csv("code_data/2025-BEA-GDP-CA-by-NAICS.csv", skip = 3)
names(sagdp2_df) <- names(sagdp2_df) |> tolower() |> gsub(" ", "_", x = _)

gva_ca <- sagdp2_df %>%
  filter(description == "All industry total") %>%
  select(where(is.numeric) | starts_with("x")) %>%  # keep numeric year columns
  pivot_longer(
    cols = everything(),
    names_to = "year",
    values_to = "gva_t"
  ) %>%
  mutate(
    year = as.numeric(gsub("[^0-9]", "", year)),
    gva_t = as.numeric(gva_t)
  ) %>%
  arrange(year)

head(gva_ca)

# now read in GDP data -- note its in millions USD
ca_gdp <- read_excel("code_data/2024-CA-GDP-TOTAL.xlsx", sheet = 2)
ca_exports <- read_excel("code_data/CA_Exports_Imports-onetable.xlsx", sheet = 2)


# calculate NT_prod --- note subsidies are high in 2002 and then again later 
EVADE_tbl <- left_join(ca_gdp, gva_ca, by = c("year"))
EVADE_tbl$nt_prod = EVADE_tbl$gdp_nominal_dollars - EVADE_tbl$gva_t
# check negs 
EVADE_tbl <- EVADE_tbl %>%
  mutate(nt_share = (nt_prod / gdp_nominal_dollars) * 100)

# filter table for relevant years only
EVADE_tbl <- EVADE_tbl %>%
  filter(year >= 2005)

# get params -- start first with PCE 
years <- 2005:2021
delta_tbl <- tibble(year = years, delta = delta_tot_trans)
gamma_tbl <- tibble(year = years, gamma = gamma_tot_trans)

EVADE_full <- EVADE_tbl %>%
  left_join(ca_exports %>% select(year, exports_mill_dollars), by = "year") %>%
  left_join(delta_tbl, by = "year") %>%
  left_join(gamma_tbl, by = "year")

# compute x_t --- export share 
EVADE_full <- EVADE_full %>%
  mutate(x_t = exports_mill_dollars / gva_t)

# calculate EVADE measures first - GVA_full
EVADE_full <- EVADE_full %>%
  mutate(
    gva_full = gva_t + (1 - delta) * (1 - x_t) * gva_t
  )

# merge in collected revenues 
combined_plot_df$year <- as.numeric(combined_plot_df$year)
EVADE_full <- EVADE_full %>%
  left_join(
    combined_plot_df %>% select(year, collected_tax),
    by = "year"
  )

# calculate tax gao
EVADE_full <- EVADE_full %>%
  mutate(
    phi_t = gamma,
    tax_gap_ratio = (1 / phi_t - 1) * ((collected_tax/1000000) / gdp_nominal_dollars)
  )

# calculate GDP full to review 
EVADE_full <- EVADE_full %>%
  mutate(
    gpd_full = gva_t + nt_prod + (1 - delta_t) * (1 - x_t) * gva_t + (1 / phi_t - 1) * collected_tax/1000000
  )

# get final measure 
EVADE_full <- EVADE_full %>%
  mutate(
    evade_ratio = (gpd_full - gdp_nominal_dollars) / gdp_nominal_dollars
  )


#### FIRST VISUALS TO MAP #### 
EVADE_full <- EVADE_full %>% mutate(year = as.numeric(year))

plot_df <- EVADE_full %>%
  select(year, gdp_nominal_dollars, gpd_full) %>%
  pivot_longer(
    cols = -year,
    names_to = "series",
    values_to = "value"
  )

# plot
ggplot(plot_df, aes(x = year, y = value, color = series)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2) +
  labs(
    x = "Year",
    y = "Millions of USD",
    color = "",
    title = "California: GDP Components and Tax Collections Over Time"
  ) +
  scale_y_continuous(labels = scales::label_comma()) +
  theme_minimal()



####### RUN w/ different delta / gamma estimators 
# get params -- start first with PCE 
years <- 2005:2021
delta_tbl <- tibble(year = years, delta = delta_t)
gamma_tbl <- tibble(year = years, gamma = gamma_t)

EVADE_full_v2 <- EVADE_tbl %>%
  left_join(ca_exports %>% select(year, exports_mill_dollars), by = "year") %>%
  left_join(delta_tbl, by = "year") %>%
  left_join(gamma_tbl, by = "year")

# compute x_t --- export share 
EVADE_full_v2 <- EVADE_full_v2 %>%
  mutate(x_t = exports_mill_dollars / gva_t)

# calculate EVADE measures first - GVA_full
EVADE_full_v2 <- EVADE_full_v2 %>%
  mutate(
    gva_full = gva_t + (1 - delta) * (1 - x_t) * gva_t
  )

# merge in collected revenues 
combined_plot_df$year <- as.numeric(combined_plot_df$year)
EVADE_full_v2 <- EVADE_full_v2 %>%
  left_join(
    combined_plot_df %>% select(year, collected_tax),
    by = "year"
  )

# calculate tax gao
EVADE_full_v2 <- EVADE_full_v2 %>%
  mutate(
    phi_t = gamma,
    tax_gap_ratio = (1 / phi_t - 1) * ((collected_tax/1000000) / gdp_nominal_dollars)
  )

# calculate GDP full to review 
EVADE_full_v2 <- EVADE_full_v2 %>%
  mutate(
    gpd_full = gva_t + nt_prod + (1 - delta_t) * (1 - x_t) * gva_t + (1 / phi_t - 1) * collected_tax/1000000
  )

# get final measure 
EVADE_full_v2 <- EVADE_full_v2 %>%
  mutate(
    evade_ratio = (gpd_full - gdp_nominal_dollars) / gdp_nominal_dollars
  )


#### FIRST VISUALS TO MAP #### 
EVADE_full_v2 <- EVADE_full_v2 %>% mutate(year = as.numeric(year))

plot_df <- EVADE_full_v2 %>%
  select(year, gdp_nominal_dollars, gpd_full) %>%
  pivot_longer(
    cols = -year,
    names_to = "series",
    values_to = "value"
  )

# plot
ggplot(plot_df, aes(x = year, y = value, color = series)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2) +
  labs(
    x = "Year",
    y = "Millions of USD",
    color = "",
    title = "California: GDP Components and Tax Collections Over Time"
  ) +
  scale_y_continuous(labels = scales::label_comma()) +
  theme_minimal()


# plot the EVADE ratios with the two different estimates 
evade_1_labeled <- EVADE_full %>%
  select(year, evade_ratio) %>%
  mutate(version = "EVADE 1")

evade_2_labeled <- EVADE_full_v2 %>%
  select(year, evade_ratio) %>%
  mutate(version = "EVADE 2")

evade_compare <- bind_rows(evade_1_labeled, evade_2_labeled)

# with ratio at 1 line 
ggplot(evade_compare, aes(x = year, y = evade_ratio, color = version)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2) +
  geom_hline(yintercept = 1, linetype = "dotted", color = "purple") +
  labs(
    x = "Year",
    y = "Evade Ratio",
    color = "Model Version",
    title = "Comparison of California EVADE Ratios Across Model Versions"
  ) +
  theme_minimal()


# EVADE YoY change vs. GDP YoY change 
evade_yoy <- EVADE_full %>%
  select(year, evade_ratio, gdp_nominal_dollars) %>%
  rename(evade_ratio_v1 = evade_ratio, gdp_v1 = gdp_nominal_dollars) %>%
  left_join(
    EVADE_full_v2 %>%
      select(year, evade_ratio, gdp_nominal_dollars) %>%
      rename(evade_ratio_v2 = evade_ratio, gdp_v2 = gdp_nominal_dollars),
    by = "year"
  ) %>%
  arrange(year) %>%
  mutate(
    yoy_evade_v1 = 100 * (evade_ratio_v1 / lag(evade_ratio_v1) - 1),
    yoy_evade_v2 = 100 * (evade_ratio_v2 / lag(evade_ratio_v2) - 1),
    yoy_gdp_v1   = 100 * (gdp_v1 / lag(gdp_v1) - 1)
  )

# show only the relevant columns
evade_yoy %>%
  select(year, yoy_evade_v1, yoy_evade_v2, yoy_gdp_v1)

# plot CHANGE 
plot_yoy <- evade_yoy %>%
  select(year, yoy_evade_v1, yoy_evade_v2, yoy_gdp_v1) %>%
  pivot_longer(
    cols = starts_with("yoy_"),
    names_to = "series",
    values_to = "yoy_change"
  )


