Interracial Meet 2017
================
2020-02-24

  - [Read in data](#read-in-data)
      - [Subsection](#subsection)
  - [Section 2](#section-2)

``` r
# Libraries
library(tidyverse)

library(haven)

# Parameters

file_raw <-
  "/Users/angel/GitHub/dcl-2020-01/angela/c01-own/data/hcmst_2017.rds"

#Race Recode
race_recode <-
  c(
    "Black or African American" = "black", 
    "American Indian or Alaska Native" = "Amer Indian", 
    "Native Hawaiian or other Pacific Islander" = "Asian Pac Islander", 
    "Asian" = "Asian Pac Islander", 
    "White" = "white", 
    "Other (please specify)" = "Other", 
    "Asian or Pacific Islander" = "Asian Pac Islander"
  )

#===============================================================================

# Code
```

## Read in data

``` r
data_2017 <-
  file_raw %>% 
  read_dta() %>% 
  mutate_if(is.labelled, as_factor) 

# glimpse(data_2017)
```

``` r
clean_data <- 
  data_2017 %>% 
  rename(
    White = Race_1, 
    'Black or African American' = Race_2, 
    'American Indian or Alaska Native' = Race_3, 
    Asian = Race_4,
    'Native Hawaiian or other Pacific Islander' = Race_5, 
    Other = Race_6, 
    partner_race = w6_q6b, 
    partner_hispanic = w6_q6a
  ) %>% 
  pivot_longer(
    cols = c(White:Other), 
    names_to = "respondent_race", 
    values_to = "true"
  ) %>% 
  filter(true == "Yes") %>% 
  drop_na(respondent_race, partner_race) %>% 
  mutate(
    ppethm = str_replace(ppethm, ".+,\\s", ""),
    partner_race =
      recode(partner_race, !!! race_recode),
    partner_race = 
      case_when(
        partner_hispanic == "No (Not Latino or Hispanic)" ~ 
          str_c("NH ", partner_race), 
        partner_hispanic %in% c(
          "Yes, Mexican, Mexican American, Chicano", 
          "Yes, Other Latino/Hispanic", 
          "Yes, Puerto Rican"
        ) ~ 
          str_replace(partner_race, ".*", "Hispanic")
      ), 
    respondent_race = 
      recode(respondent_race, !!! race_recode, .default = respondent_race), 
    respondent_race = 
      case_when(
        ppethm == "Hispanic" ~  (respondent_race = "Hispanic"),
        TRUE ~ str_c("NH ", respondent_race)
      )
  ) %>%
  filter(partnership_status %in% c("married", "partnered, not married"))
```

### Subsection

``` r
data_2017 %>% 
  select(Q32, hcm2017q24_school:hcm2017q24_met_online)
```

    ## # A tibble: 3,510 x 22
    ##    Q32   hcm2017q24_scho~ hcm2017q24_coll~ hcm2017q24_mil hcm2017q24_chur~
    ##    <fct> <fct>            <fct>            <fct>          <fct>           
    ##  1 <NA>  no               no               no             no              
    ##  2 No, ~ no               no               no             no              
    ##  3 Yes,~ no               no               no             no              
    ##  4 <NA>  no               no               no             no              
    ##  5 No, ~ no               no               no             no              
    ##  6 No, ~ no               no               no             no              
    ##  7 No, ~ no               no               no             no              
    ##  8 No, ~ yes              no               no             no              
    ##  9 <NA>  <NA>             <NA>             <NA>           <NA>            
    ## 10 <NA>  no               no               no             yes             
    ## # ... with 3,500 more rows, and 17 more variables: hcm2017q24_vol_org <fct>,
    ## #   hcm2017q24_customer <fct>, hcm2017q24_bar_restaurant <fct>,
    ## #   hcm2017q24_party <fct>, hcm2017q24_internet_other <fct>,
    ## #   hcm2017q24_internet_dating <fct>, hcm2017q24_internet_soc_network <fct>,
    ## #   hcm2017q24_internet_game <fct>, hcm2017q24_internet_chat <fct>,
    ## #   hcm2017q24_internet_org <fct>, hcm2017q24_public <fct>,
    ## #   hcm2017q24_blind_date <fct>, hcm2017q24_vacation <fct>,
    ## #   hcm2017q24_single_serve_nonint <fct>, hcm2017q24_business_trip <fct>,
    ## #   hcm2017q24_work_neighbors <fct>, hcm2017q24_met_online <fct>

``` r
clean_data %>% 
  filter(Q32  == "Yes, an app on my phone (like Tinder or Grindr)") %>% 
  select(
    "phone_app" = Q32, 
    "school" = hcm2017q24_school, 
    "college" = hcm2017q24_college, 
    "military" = hcm2017q24_mil, 
    "church" = hcm2017q24_church, 
    "vol_org" = hcm2017q24_vol_org, 
    "customer" = hcm2017q24_customer, 
    "bar_restaurant" = hcm2017q24_bar_restaurant, 
    "party" = hcm2017q24_party, 
    "coworkers" = hcm2017q24_work_neighbors, 
    "internet" = hcm2017q24_met_online,
    respondent_race, 
    partner_race
  ) %>% 
  pivot_longer(
    cols = phone_app:internet, 
    values_to = "how_they_met", 
    names_to = "where_they_met"
  ) %>% 
  filter(how_they_met %in% 
           c("yes", "Yes, an app on my phone (like Tinder or Grindr)")
  ) %>% 
  count(where_they_met)
```

    ## # A tibble: 4 x 2
    ##   where_they_met     n
    ##   <chr>          <int>
    ## 1 bar_restaurant    18
    ## 2 internet          47
    ## 3 military           1
    ## 4 phone_app         55

## Section 2
