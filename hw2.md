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

mr_tw_df = read_excel("./data/wheel.xlsx", 
                       sheet = "Mr. Trash Wheel") %>% 
  janitor::clean_names() %>% 
  select(-x15, -x16) %>%
  filter(row_number() != 708) %>% 
  mutate(trash_wheel_type = "Mr")
```

    ## New names:
    ## • `` -> `...15`
    ## • `` -> `...16`

``` r
prof_tw_df = read_excel("./data/wheel.xlsx", 
                       sheet = "Professor Trash Wheel") %>% 
  janitor::clean_names() %>% 
  filter(row_number() != 133) %>% 
  mutate(trash_wheel_type = "Professor")


gf_tw_df = read_excel("./data/wheel.xlsx", 
                       sheet = "Gwynns Falls Trash Wheel") %>% 
  janitor::clean_names() %>% 
  filter(row_number() != 350) %>% 
  mutate(trash_wheel_type = "Gwynns_Falls")


mr_tw_df$year = as.numeric(mr_tw_df$year)
final_df = bind_rows(mr_tw_df, prof_tw_df, gf_tw_df) %>% 
  select(year, month, date, trash_wheel_type, dumpster, everything()) %>%
  arrange(year, month, date)
```
