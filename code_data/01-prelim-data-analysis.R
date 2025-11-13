# author: Allegra Saggese
# last updated: 06 nov 2025
# purpose: prelim analysis for macro measurement 

# execute for packages and files 
source("startup.R")
# inspect data sets 
names(datasets)

# data review --- NEED TO FIX HELPER - CRASH W/ EXCEL TAKING ONE SHEET 
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

# DF 1a ------ TAX COLLECTION DATA (TOTAL)
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

# DF 1b ----- sales and use tax in CA only 
collected_tax <- read_xlsx("code_data/CA-taxable_sales_collected.xlsx")
names(collected_tax) <- tolower(names(collected_tax))



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
  labs(x = "Year", y = "CA PCE (US dollars)") +
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
merged_df_v2$est_tax_tocollect <- merged_df_v2$est_tax_rev * merged_df_v2$pop_est# create new col for taxable spend


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


