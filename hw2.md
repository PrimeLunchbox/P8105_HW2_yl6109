p8105_hw2_yl6109
================
Yurou Liu
2025-09-29

## Problem 1

### pols-month

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(readr)
library(tidyr)


pols_df = read_csv("./data/pols-month.csv") %>%
  janitor::clean_names()
```

    ## Rows: 822 Columns: 9

    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl  (8): prez_gop, gov_gop, sen_gop, rep_gop, prez_dem, gov_dem, sen_dem, r...
    ## date (1): mon
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
pols_df_sep = pols_df %>% 
  separate(mon, c("year", "month", "day")) 
pos_df_month = pols_df_sep %>% 
  mutate(month = recode(month, `01` = "January", `02` = "February", `03` = "March", `04` = "April", `05` = "May", `06` = "June", `07` = "July", `08` = "August", `09` = "September", `10` = "October", `11` = "November", `12` = "December"))


month_pols = pos_df_month %>% 
  mutate(president = ifelse(prez_gop > prez_dem, "gop", "dem")) 


month2_pols = select(month_pols, -prez_dem, -prez_gop, -day)
```

### snp

``` r
snp_df = read_csv("./data/snp.csv") %>% 
  janitor::clean_names()
```

    ## Rows: 787 Columns: 2
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (1): date
    ## dbl (1): close
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
sep_snp = snp_df %>%
  separate(date, c("date", "month", "year"))


sep2_snp = sep_snp %>% 
  arrange(month)


month_snp = sep2_snp %>% 
  mutate(month = recode(month, `1` = "January", `2` = "February", `3` = "March", `4` = "April")) 


month_snp$year = as.numeric(sep_snp$year)


month2_snp = month_snp %>% 
  mutate(year = ifelse(year <= 25, 2000 + year, 1900 + year)) %>% 
  arrange(year) %>% 
  select(year, month, everything()) %>% 
  select(-date)
```

### unemployment

``` r
unemployment_df = read_csv("./data/unemployment.csv") %>% 
  janitor::clean_names() %>% 
  pivot_longer(
    jan:dec, 
    names_to = "month",
    values_to = "unemployment_rate(%)"
  )
```

    ## Rows: 68 Columns: 13
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl (13): Year, Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
month_un = unemployment_df %>% 
  mutate(month = recode(month, "jan" = "January", "feb" = "February", "mar" = "March", "apr" = "April", "may" = "May", "jun" = "June", "jul" = "July", "aug" = "August", "sep" = "September", "oct" = "October", "nov" = "November", "dec" = "December"))
```

### merging the three datasets

``` r
month2_pols$year = as.numeric(month2_pols$year)

fianl_df = month2_pols %>% 
  left_join(month2_snp, by = c("year", "month")) %>% 
  left_join(month_un, by = c("year", "month"))
```

### description

## Problem 2

``` r
library(readxl)

mr_tw_df = read_excel("./data/wheel.xlsx", sheet = "Mr. Trash Wheel") %>% 
  janitor::clean_names() %>% 
  select(-x15, -x16) %>%
  filter(row_number() != 708) %>% 
  mutate(trash_wheel_type = "Mr")
```

    ## New names:
    ## • `` -> `...15`
    ## • `` -> `...16`

``` r
prof_tw_df = read_excel("./data/wheel.xlsx", sheet = "Professor Trash Wheel") %>% 
  janitor::clean_names() %>% 
  filter(row_number() != 133) %>% 
  mutate(trash_wheel_type = "Professor")


gf_tw_df = read_excel("./data/wheel.xlsx", sheet = "Gwynns Falls Trash Wheel") %>% 
  janitor::clean_names() %>% 
  filter(row_number() != 350) %>% 
  mutate(trash_wheel_type = "Gwynns_Falls")


mr_tw_df$year = as.numeric(mr_tw_df$year)
final_df = bind_rows(mr_tw_df, prof_tw_df, gf_tw_df) %>% 
  select(year, month, date, trash_wheel_type, dumpster, everything()) %>%
  arrange(year, month, date)
```

### Description

## Problem 3

construct the merged dataframe.

``` r
zc_df = read_csv("./data/Zip Codes.csv") %>% 
  janitor::clean_names() %>% 
  select(-file_date, -county_code, -state_fips)
```

    ## Rows: 322 Columns: 7
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (4): County, County Code, File Date, Neighborhood
    ## dbl (3): State FIPS, County FIPS, ZipCode
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
znyc_df = read_csv("./data/Zip_NYC.csv") %>%
  janitor::clean_names() %>% 
  pivot_longer(
    x2015_01_31:x2024_08_31,
    names_to = "date",
    names_prefix = "x",
    values_to = "price") %>% 
  separate(date, into = c("year", "month", "day")) %>% 
  mutate(county_name = recode(county_name, "Bronx County" = "Bronx", "Kings County" = "Kings", "New York County" = "New York", "Queens County" = "Queens", "Richmond County" = "Richmond")) %>%
  mutate(county = county_name, zip_code = region_name) %>% 
  select(-county_name, -region_name, -state_name) 
```

    ## Rows: 149 Columns: 125
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr   (6): RegionType, StateName, State, City, Metro, CountyName
    ## dbl (119): RegionID, SizeRank, RegionName, 2015-01-31, 2015-02-28, 2015-03-3...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
merged_df = zc_df %>%
  left_join(znyc_df, by = "zip_code", "county") %>% 
  mutate(county = county.x) %>%
  select(-county.x, -county.y, -region_type) %>% 
  arrange(year, month, day) %>% 
  mutate(month = recode(month, `01` = "January", `02` = "February", `03` = "March", `04` = "April", `05` = "May", `06` = "June", `07` = "July", `08` = "August", `09` = "September", `10` = "October", `11` = "November", `12` = "December")) %>% select(year, month, day, state, city, county, county_fips, neighborhood, region_id, zip_code, size_rank, price, everything()) 
```

    ## Warning in left_join(., znyc_df, by = "zip_code", "county"): Detected an unexpected many-to-many relationship between `x` and `y`.
    ## ℹ Row 1 of `x` matches multiple rows in `y`.
    ## ℹ Row 4757 of `y` matches multiple rows in `x`.
    ## ℹ If a many-to-many relationship is expected, set `relationship =
    ##   "many-to-many"` to silence this warning.

``` r
final_merged_df = merged_df %>% 
  filter(row_number() < 17517)
```

count number of unique observations

``` r
library(dplyr)

# for different zip codes observed,
unique_zc_count = 
  final_merged_df %>% 
  summarize(n_distinct(zip_code))

# for different neighborhoods observed,
unique_nbhd_count = 
  final_merged_df %>% 
  summarize(n_distinct(neighborhood))

# for different exact zip codes that are not included in zillow datasets,
zc_not_included = merged_df %>% 
  filter(row_number() >= 17517 & row_number() <= 17687) %>% 
  distinct(zip_code) %>%
  arrange(zip_code)

unique_zc_count %>% print()
```

    ## # A tibble: 1 × 1
    ##   `n_distinct(zip_code)`
    ##                    <int>
    ## 1                    149

``` r
unique_nbhd_count %>% print()
```

    ## # A tibble: 1 × 1
    ##   `n_distinct(neighborhood)`
    ##                        <int>
    ## 1                         43

``` r
zc_not_included %>% print()
```

    ## # A tibble: 171 × 1
    ##    zip_code
    ##       <dbl>
    ##  1    10008
    ##  2    10020
    ##  3    10041
    ##  4    10043
    ##  5    10045
    ##  6    10047
    ##  7    10048
    ##  8    10055
    ##  9    10072
    ## 10    10080
    ## # ℹ 161 more rows

in total, 17516 observations exists. 149 unique zip codes are included.
43 unique neighborhoods are included.

according to merged_df with na year, zip codes that are not in the
Zillow dataset can be found using the r code above.

For price difference between Jan 2020 and Jan 2021,

``` r
library(tidyr)

y20_data = final_merged_df %>% 
  filter(month == "January" & year == "2020") %>% 
  select(zip_code, price, neighborhood) %>% 
  rename(y20_price = price)

y21_data = final_merged_df %>% 
  filter(month == "January" & year == "2021") %>% 
  select(zip_code, price, neighborhood) %>%
  rename(y21_price = price)

comparison = y20_data %>% 
  inner_join(y21_data, by = "zip_code") %>% 
  filter(!is.na(y20_price) & !is.na(y21_price)) %>% 
  mutate(price_dif = y20_price - y21_price) %>% 
  mutate(neighborhood = neighborhood.x) %>% 
  select(-neighborhood.x, -neighborhood.y)
```

    ## Warning in inner_join(., y21_data, by = "zip_code"): Detected an unexpected many-to-many relationship between `x` and `y`.
    ## ℹ Row 13 of `x` matches multiple rows in `y`.
    ## ℹ Row 13 of `y` matches multiple rows in `x`.
    ## ℹ If a many-to-many relationship is expected, set `relationship =
    ##   "many-to-many"` to silence this warning.

``` r
largest_drop = comparison %>% 
  arrange(price_dif) %>% 
  head(10) %>% 
  select(zip_code, neighborhood, y20_price, y21_price, price_dif)

# the zip codes and corresponding neighborhoods of top 10 largest drops in rent price from 2020 to 2021 is shown below.
largest_drop %>% print()
```

    ## # A tibble: 10 × 5
    ##    zip_code neighborhood              y20_price y21_price price_dif
    ##       <dbl> <chr>                         <dbl>     <dbl>     <dbl>
    ##  1    11432 Jamaica                       1895.     1996.   -101.  
    ##  2    10467 Bronx Park and Fordham        1650.     1745.    -95.0 
    ##  3    10458 Bronx Park and Fordham        1376.     1420.    -44.4 
    ##  4    10463 Kingsbridge and Riverdale     2169.     2206.    -36.5 
    ##  5    10463 Kingsbridge and Riverdale     2169.     2206.    -36.5 
    ##  6    10463 Kingsbridge and Riverdale     2169.     2206.    -36.5 
    ##  7    10463 Kingsbridge and Riverdale     2169.     2206.    -36.5 
    ##  8    10462 Southeast Bronx               1682.     1707.    -25.1 
    ##  9    11229 Southern Brooklyn             1783.     1797.    -14.2 
    ## 10    11220 Sunset Park                   2135.     2140.     -5.37
