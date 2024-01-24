### if underlying data sources are updated
### then have to check how transitions coding changes

# install.packages("devtools")
# devtools::install_github("vdeminstitute/ERT")
# devtools::install_github("vdeminstitute/vdemdata")
library(ERT)
library(vdemdata)
### last used versions: 13.0
installed.packages()[rownames(installed.packages()) == "ERT", "Version"]
installed.packages()[rownames(installed.packages()) == "vdemdata", "Version"]
library(tidyverse)
library(readxl)
library(writexl)
library(googlesheets4)
library(googledrive)

### loading Geoff's original transitions coding
v6 <- read_excel(here::here("transitions/original_data", 
                            "transitions2020_v6.xlsx")) %>%
  select(country, year, ccode, vdem_id, tjet_dtrid, isq_dtrid, tjrc_dtrid, 
         tjrc_ttype, tjrc_rupture, tjrc_negotiated, tjrc_newstate, polity2, 
         demo_duration, bmr_demo, bmr_trans, bmr_duration, coup, v2x_polyarchy, 
         v2x_libdem) %>%
  rename(v2x_polyarchy_vdem_gd = v2x_polyarchy, 
         v2x_libdem_vdem_gd = v2x_libdem, 
         polity2_gd = polity2,
         demo_duration_gd = demo_duration, 
         bmr_demo_gd = bmr_demo,
         bmr_trans_gd = bmr_trans,
         bmr_dur_gd = bmr_duration,
         coup_gd = coup)
countries_v6 <- v6 %>%
  select(country, ccode) %>%
  distinct()

### BMR data available at 
### https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/FENWWR
countries_bmr <- read_csv(here::here("transitions/original_data", 
                                     "democracy-v4.0.csv"), 
                          show_col_types = FALSE) %>%
  select(country, ccode) %>%
  mutate(country = str_to_title(country)) %>%
  distinct()

### Polity data at 
### https://www.systemicpeace.org/inscrdata.html
countries_p5 <- read_excel(here::here("transitions/original_data", 
                                      "p5v2018.xls")) %>%
  select(country, ccode) %>%
  distinct()

### merging Polity 5 and BMR

full_join(countries_p5, countries_bmr, by = "ccode") %>%
  filter(country.x != country.y) %>%
  arrange(country.x) %>%
  print(n = Inf)
### Sudan codes don't match

other <- read_excel(here::here("transitions/original_data", "p5v2018.xls")) %>%
  filter(year > 1948) %>% 
  mutate(ccode = ifelse(country == "Sudan-North" & ccode == 626, 625, ccode), 
         ccode = ifelse(country == "South Sudan" & ccode == 525, 626, ccode)) %>%
  select(country, ccode, year, polity, polity2, change, regtrans) %>%
  rename(polity_p5 = polity,
         polity2_p5 = polity2,
         change_p5 = change,
         regtrans_p5 = regtrans) %>% 
  full_join(read_csv(here::here("transitions/original_data", 
                                "democracy-v4.0.csv"), 
                     show_col_types = FALSE) %>% 
              select(country, ccode, year, democracy, 
                     democracy_trans, democracy_omitteddata) %>%
              mutate(ccode = ifelse(country == "SUDAN, NORTH" & 
                                      ccode == 624, 625, ccode),
                     country = str_to_title(country)) %>% 
              rename(country_bmr = country, 
                     dem_bmr = democracy, 
                     dem_trans_bmr = democracy_trans, 
                     dem_omit_bmr = democracy_omitteddata) %>% 
              filter(year > 1948) %>% 
              filter(!(country_bmr %in% c("Estonia", "Latvia", "Lithuania") & 
                         year < 1991)), 
            by = c("ccode", "year")) %>% 
  # filter(str_detect(country, "Sudan")) %>% arrange(ccode, year) %>% print(n = Inf)
  filter(!(country == "Sudan" & country_bmr == "Sudan, North" & 
             ccode == 625 & year == 2011) & # Sudan
           !(ccode == 260 & year == 1990) & # Germany
           !(ccode == 347 & year == 1991) & # Serbia
           !(ccode == 347 & year == 2006) & # Serbia
           !(ccode == 530 & year == 1993) & # Ethiopia
           !(ccode == 678 & year == 1990) & # Yemen
           !(ccode == 816 & year == 1976) ) %>% # Vietnam
  # deleted duplicates in years of transformation
  mutate(country = if_else(is.na(country), country_bmr, country)) %>% 
  rename(ccode_other = ccode) %>% 
  select(-country_bmr) %>% 
  mutate(
    country = if_else(country == "Antigua", "Antigua & Barbuda", country), 
    country = if_else(country == "Bosnia", "Bosnia and Herzegovina", country), 
    country = if_else(country == "Central African Rep.", "Central African Republic", country),
    country = if_else(country == "Congo, Dem. Rep.", "Democratic Republic of the Congo", country),
    country = if_else(country == "Congo, Rep.", "Republic of the Congo", country),
    country = if_else(country == "Congo Brazzaville", "Republic of the Congo", country),
    country = if_else(country == "Congo Kinshasa", "Democratic Republic of the Congo", country),
    country = if_else(country == "Congo-Brazzaville", "Republic of the Congo", country),
    country = if_else(country == "Cote D'Ivoire", "Ivory Coast", country),
    country = if_else(country == "Cote D'ivoire", "Ivory Coast", country),
    country = if_else(country == "Czechoslovakia", "Czech Republic", country),
    country = if_else(country == "East Timor", "Timor-Leste", country),
    country = if_else(country == "Gambia", "The Gambia", country),
    country = if_else(country == "Germany East", "German Democratic Republic", country),
    country = if_else(country == "Germany West", "Germany", country),
    country = if_else(country == "Korea, North", "North Korea", country),
    country = if_else(country == "Korea, South", "South Korea", country),
    country = if_else(country == "Korea North", "North Korea", country), 
    country = if_else(country == "Korea South", "South Korea", country),
    country = if_else(country == "Liechstenstein", "Liechtenstein", country), 
    country = if_else(country == "Macedonia", "North Macedonia", country),
    country = if_else(country == "Micronesia, Fed.", "Micronesia", country),
    country = if_else(country == "Myanmar (Burma)", "Burma/Myanmar", country),
    country = if_else(country == "Myanmar", "Burma/Myanmar", country),
    country = if_else(country == "Samoa (Western)", "Samoa", country), 
    country = if_else(country == "Sao Tome & Principe", "Sao Tome and Principe", country),
    country = if_else(country == "Serbia and Montenegro", "Serbia", country),
    country = if_else(country == "Slovak Republic", "Slovakia", country),
    country = if_else(country == "South Vietnam", "Republic of Vietnam", country),
    country = if_else(country == "St. Vincent & Gren.", "St. Vincent", country), 
    country = if_else(country == "Sudan, North", "Sudan", country),
    country = if_else(country == "Sudan-North", "Sudan", country),
    country = if_else(country == "Sudan, South", "South Sudan", country),
    country = if_else(country == "Swaziland", "Eswatini", country),
    country = if_else(country == "Timor Leste", "Timor-Leste", country),
    country = if_else(country == "Trinidad & Tobago", "Trinidad and Tobago", country),
    country = if_else(country == "UAE", "United Arab Emirates", country),
    country = if_else(country == "United States", "United States of America", country),
    country = if_else(country == "USSR", "Russia", country),
    country = if_else(country == "Vietnam North", "Vietnam", country),
    country = if_else(country == "Yemen North", "Yemen", country),
    country = if_else(country == "Yemen South", "South Yemen", country),
    country = if_else(country == "Yugoslavia", "Serbia", country)
  )

other %>%
  group_by(country, year) %>%
  filter(n() > 1) %>%
  arrange(country, year) %>%
  print(n = Inf)
countries_other <- other %>%
  select(country) %>%
  distinct() %>% 
  arrange(country) %>%
  unlist(use.names = FALSE)

### merging VDem and GWF

countries_vdem <- vdem %>%
  select(country_name, COWcode) %>%
  distinct() %>%
  filter(!is.na(COWcode))

countries_gwf <- read_delim(here::here("transitions", "original_data",
                                       "GWF_AllPoliticalRegimes.txt"), 
                            show_col_types = FALSE) %>%
  mutate(cowcode = case_when(
    cowcode == 255 & year %in% 1950:1990 ~ 260,
    cowcode == 315 & year == 1993 ~ 316,
    cowcode == 678 & year >= 1990 ~ 679,
    TRUE ~ cowcode)) %>%
  select(cowcode, gwf_country) %>%
  distinct()

countries_vdem %>%
  full_join(countries_gwf, by = c("COWcode" = "cowcode")) %>%
  filter(country_name != gwf_country)
### need to be careful with Czechia

df <- vdem %>%
  tibble() %>% 
  filter(year > 1948) %>% 
  mutate(country_name = str_replace(country_name, "Czechia", 
                                    "Czech Republic")) %>% 
  select(country_id, COWcode, country_name, year, 
         v2x_regime, v2x_regime_amb, e_boix_regime, e_democracy_trans, 
         e_p_polity, e_polity2, e_pt_coup, e_coups) %>% 
  mutate(v2x_regime = recode_factor(v2x_regime, 
           '0' = "Closed autocracy", 
           '1' = "Electoral Autocracy", 
           '2' = "Electoral Democracy", 
           '3' = "Liberal Democracy"),
         v2x_regime_amb = recode_factor(v2x_regime_amb, 
           '0' = "Closed autocracy", 
           '1' = "Closed autocracy upper bound", 
           '2' = "Electoral autocracy lower bound", 
           '3' = "Electoral Autocracy", 
           '4' = "Electoral autocracy upper bound", 
           '5' = "Electoral democracy lower bound", 
           '6' = "Electoral Democracy", 
           '7' = "Electoral democracy upper bound", 
           '8' = "Liberal democracy lower bound", 
           '9' = "Liberal Democracy"),
         e_pt_coup = recode_factor(e_pt_coup, 
           '0' = "No coup attempt occurred", 
           '1' = "Unssuccessful coup attempt occurred", 
           '2' = "Successful coup attempt occurred") ) %>%
  rename(ccode_vdem = COWcode,
         v2x_regime_vdem = v2x_regime,
         bmr_dem_vdem = e_boix_regime,
         bmr_trans_vdem = e_democracy_trans,
         polity_vdem = e_p_polity, 
         polity2_vdem = e_polity2, 
         coups_pipe_vdem = e_coups, 
         coup_pt_vdem = e_pt_coup) %>% 
  full_join(read_delim(here::here("transitions/original_data", 
                                  "GWF_AllPoliticalRegimes.txt"), 
                       show_col_types = FALSE) %>%
              mutate(gwf_regimetype = if_else(is.na(gwf_regimetype), 
                                              gwf_nonautocracy, 
                                              gwf_regimetype), 
                     gwf_fail = ifelse(gwf_country == "Czechoslovakia" & 
                                         cowcode == 315 & year == 1992, 1, gwf_fail) ) %>% 
              filter(year > 1948 & !(gwf_country == "Czechoslovakia" & 
                                       cowcode == 315 & year == 1993)) %>% 
              select(cowcode, year, gwf_country, gwf_regimetype, 
                     gwf_next, gwf_prior, gwf_fail) %>% 
              mutate(cowcode = case_when(
                cowcode == 255 & year %in% 1950:1990 ~ 260,
                cowcode == 315 & year == 1993 ~ 316,
                cowcode == 678 & year >= 1990 ~ 679,
                TRUE ~ cowcode) ) %>% 
              rename(ccode_gwf = cowcode,
                     regime_gwf = gwf_regimetype,
                     reg_next_gwf = gwf_next, 
                     reg_prior_gwf = gwf_prior, 
                     reg_fail_gwf = gwf_fail), 
              by = c("ccode_vdem" = "ccode_gwf", "year" = "year"))

# df %>%
#   group_by(country_id, ccode_vdem, country_name, year) %>%
#   filter(n() > 1) %>%
#   arrange(country_name, year)

### merging VDem, BMR, Polity, etc

countries_df <- df %>% 
  select(country_name, ccode_vdem) %>% 
  distinct()
countries_df$country_name[!countries_df$country_name %in% countries_other]
countries_other[!countries_other %in% countries_df$country_name]

df <- df %>%
  full_join(other, by = c("country_name" = "country", "year" = "year")) %>% I

df <- df %>% 
  left_join(df %>%
              select(country_name, year, polity_p5, polity2_p5) %>%
              mutate(year = year + 1) %>% 
              rename(polity_lag = polity_p5,
                     polity2_lag = polity2_p5),
            by = c("country_name", "year") ) %>%
  left_join(df %>%
              select(country_name, year, polity_p5, polity2_p5) %>%
              mutate(year = year - 1) %>% 
              rename(polity_nxt = polity_p5,
                     polity2_nxt = polity2_p5),
            by = c("country_name", "year") ) %>%
  mutate(trans_p5_yr = case_when( (polity_p5 == -88 & polity2_p5 > 0 & 
                                     polity2_p5 > polity2_lag ) |  
                                  (polity_p5 == -88 & polity2_nxt > 0 & 
                                     polity2_nxt > polity2_lag ) |
                                  (polity_p5 - polity_lag >= 3 & 
                                     polity_vdem > 0) ~ year), 
         trans_bmr_yr = case_when(dem_trans_bmr == 1 ~ year),
         trans_gwf_yr = case_when(reg_fail_gwf == 1 & 
                                    reg_next_gwf == "democracy" ~ year)) %>%
  select(-polity_lag, -polity2_lag, -polity_nxt, -polity2_nxt, 
         -reg_next_gwf, -reg_prior_gwf) 

df <- df %>% 
  left_join(df %>%
              select(country_name, year, trans_p5_yr) %>%
              mutate(year = year + 1) %>% 
              rename(trans_p5_yr_lag = trans_p5_yr),
            by = c("country_name", "year") ) %>%
  mutate(trans_p5_yr = replace(trans_p5_yr, 
                               trans_p5_yr_lag == trans_p5_yr - 1, 
                               NA_integer_) ) %>% 
  select(-trans_p5_yr_lag) %>%  
  group_by(country_name) %>% 
  fill(trans_gwf_yr, trans_bmr_yr, trans_p5_yr) %>% 
  ungroup() %>% 
  mutate(trans_p5 = case_when(year <= 2018 & !is.na(trans_p5_yr) ~ 
                                paste(ccode_other, trans_p5_yr, 
                                      year - trans_p5_yr, sep = "-")),
         trans_bmr = case_when(year <= 2020 & !is.na(trans_bmr_yr) ~ 
                                 paste(ccode_other, trans_bmr_yr, 
                                       year - trans_bmr_yr, sep = "-")),
         trans_gwf = case_when(year <= 2010 & !is.na(trans_gwf_yr) ~ 
                                 paste(ccode_vdem, trans_gwf_yr, 
                                       year - trans_gwf_yr, sep = "-")))

ert <- episodes %>% 
  filter(year > 1948) %>% 
  mutate(country_name = str_replace(country_name, "Czechia", 
                                    "Czech Republic")) %>% 
  select(country_id, country_name, year, v2x_regime, reg_type, 
         reg_start_year, reg_end_year, v2x_polyarchy, reg_trans, 
         dem_founding_elec, aut_founding_elec, row_regch_event, 
         dem_ep, dem_ep_start_year, dem_ep_end_year, dem_ep_subdep, 
         dem_ep_outcome, dem_ep_outcome_agg, 
         aut_ep, aut_ep_start_year, aut_ep_end_year, 
         aut_ep_subreg, aut_ep_outcome, aut_ep_outcome_agg) %>% 
  mutate(v2x_regime_orig = v2x_regime,
         v2x_regime = recode_factor(v2x_regime, 
           '0' = "Closed autocracy", 
           '1' = "Electoral Autocracy", 
           '2' = "Electoral Democracy", 
           '3' = "Liberal Democracy"),
         reg_type = recode_factor(reg_type, 
           '0' = "Autocracy", 
           '1' = "Democracy"),
         reg_trans = recode_factor(reg_trans,
           '-1' = "Democratic breakdown",
           '0' = "No regime transition",
           '1' = "Democratic transition"),
         row_regch_event = recode_factor(row_regch_event, 
           '-1' = "Change from democracy to autocracy on RoW", 
           '0' = "No regime change on RoW", 
           '1' = "Change from autocracy to democracy on RoW"),
         dem_ep_outcome = recode_factor(dem_ep_outcome, 
           '0' = "No democratization episode during this year",
           '1' = "Democratic transition",
           '2' = "Preempted democratic transition",
           '3' = "Stabilized electoral autocracy",
           '4' = "Reverted liberalization",
           '5' = "Deepened democracy",
           '6' = "Outcome censored"),
         dem_ep_outcome_agg = recode_factor(dem_ep_outcome_agg, 
           '0' = "No democratization episode during this year",
           '1' = "Democratic transition",
           '2' = "No democratic transition",
           '3' = "Deepened democracy",
           '4' = "Outcome censored"),
         aut_ep_outcome = recode_factor(aut_ep_outcome, 
           '0' = "No autocratization episode during this year",
           '1' = "Democratic breakdown",
           '2' = "Preempted democratic breakdown",
           '3' = "Diminished democracy",
           '4' = "Averted regression",
           '5' = "Regressed autocracy",
           '6' = "Outcome censored"),
         aut_ep_outcome_agg = recode_factor(aut_ep_outcome_agg, 
           '0' = "No autocratization episode during this year",
           '1' = "Democratic breakdown",
           '2' = "No democratic breakdown",
           '3' = "Regressed autocracy",
           '4' = "Outcome censored")) 

trans <- ert %>%
  left_join(ert %>%
              select(country_id, year, v2x_regime_orig) %>%
              mutate(year = year + 1) %>% 
              rename(v2x_regime_orig_lag = v2x_regime_orig),
            by = c("country_id", "year")) %>% 
  mutate(trans_row_yr = case_when(year >= dem_ep_start_year & 
                                    year <= dem_ep_end_year &           
                                    v2x_regime_orig > v2x_regime_orig_lag ~ 
                                    year)) %>% 
  group_by(country_id, dem_ep_start_year) %>% 
  mutate(new = case_when(!is.na(dem_ep_start_year) & 
                           year == min(year) ~ year),
         reg_start = case_when(year == new ~ v2x_regime_orig),
         reg_max = case_when(!is.na(dem_ep_start_year) ~ max(v2x_regime_orig)),
         trans_row_yr = min(trans_row_yr, na.rm = TRUE), 
         trans_row_yr = case_when(year == trans_row_yr ~ trans_row_yr)) %>% 
  fill(reg_start) %>% 
  mutate(trans_ert_yr = case_when(year >= dem_ep_start_year &
                             year <= dem_ep_end_year &
                             reg_max > reg_start ~ year),
         trans_beg = case_when(year >= trans_ert_yr ~ 
                                 min(trans_ert_yr, na.rm = TRUE)),
         trans_ert_yr = case_when(year == trans_beg ~ year) ) %>%
  ungroup() %>% 
  group_by(country_id) %>%
  fill(trans_beg, trans_row_yr) %>%
  ungroup() %>%
  mutate(trans_ert = case_when(!is.na(trans_beg) ~ 
                                 paste(country_id, trans_beg, 
                                       year - trans_beg, 
                                       sep = "-")),
         trans_row = case_when(!is.na(trans_row_yr) ~ 
                                 paste(country_id, trans_row_yr, 
                                       year - trans_row_yr, sep = "-"))) %>% 
  select(country_id, country_name, year, v2x_polyarchy, v2x_regime,
         # reg_trans, reg_start, reg_max, trans_ert_yr, trans_beg, 
         trans_ert, trans_ert_yr, trans_row, trans_row_yr, dem_ep_start_year, 
         dem_ep_end_year, dem_founding_elec, dem_ep_outcome
         # aut_ep_start_year, aut_ep_end_year, aut_founding_elec, aut_ep_outcome
         ) %>% 
  rename(v2x_regime_ert = v2x_regime)

countries_trans <- trans %>% 
  select(country_name, country_id) %>% 
  arrange(country_name) %>% 
  distinct()

df <- df %>% 
  full_join(trans, by = c("country_id", "country_name", "year")) %>% 
  mutate(country_name = if_else(country_name == "Ivory Coast", 
                                "Cote d'Ivoire", country_name),
         country_name = if_else(country_name == "Yemen" & year %in% 1962:1989, 
                                "North Yemen", country_name),
         country_name = if_else(country_name == "Czech Republic" & year <= 1992, 
                                "Czechoslovakia", country_name))

### merge by country names and check carefully

### initial check 
# v6 %>%
#   select(country, ccode, vdem_id) %>%
#   full_join(df %>% select(country_name, country_id),
#             by = c("country" = "country_name")) %>%
#   unique() %>%
#   filter(is.na(vdem_id) | is.na(ccode) | vdem_id != country_id) %>%
#   print(n = Inf)

ifit <- read_csv(here::here("transitions/original_data/Freeman.csv"), 
                 show_col_types = FALSE) %>%
  mutate(tr_year = as.integer(str_split_i(description, "(?<=\\d{4}):", i = 1)),
         tr_descr = str_split_i(description, "(?<=\\d{4}):", i = 2),
         tr_descr = str_squish(tr_descr),
         country = str_replace(country, fixed(" and "), " & "),
         country = str_replace(country, fixed("Korea, North"), "North Korea"),
         country = str_replace(country, fixed("Korea, South"), "South Korea"),
         country = str_replace(country, fixed("Lao PDR"), "Laos"),
         country = str_replace(country, fixed("Macedonia"), "North Macedonia"),
         country = str_replace(country, fixed("Marshall islands"), "Marshall Islands"),
         country = str_replace(country, fixed("Saint "), "St. "),
         country = str_replace(country, fixed("Solomon islands"), "Solomon Islands"),
         country = str_replace(country, fixed("St. Vincent & the Grenadines"), "St. Vincent"), 
         country = str_replace(country, fixed("Swaziland"), "Eswatini"),
         country = str_replace(country, fixed("United States"), "United States of America"),
         country = if_else(country == "Congo", "Republic of the Congo", country)) %>% 
  select(-description, -un_entry) %>% 
  filter(!is.na(tr_year)) %>%
  # filter(tr_year > 1969 & tr_year < 2021 & !is.na(tr_year)) %>% 
  rename(tr_num_ifit = num_trans,
         tr_dem_ifit = trans_dem,
         tr_confl_ifit = trans_confl,
         tr_new_ifit = trans_new,
         tr_descr_ifit = tr_descr)

### loading previous version for comparison to new

# prev <- drive_get("TJET_transitions") %>%
#   read_sheet(sheet = "transitions") %>%
#   select(ccode, ccode_vdem, vdem_id, country, year, trans_year, trans_note,
#          p5_year, trans_p5, bmr_year, trans_bmr, old_trans_bmr,
#          ert_year, trans_ert, old_trans_ert, tjet_dtrid, row_yr, trans_row,
#          dem_bmr, old_dem_bmr, dem_trans_bmr, old_dem_trans_bmr,
#          polity_p5, polity2_p5, change_p5, regtrans_p5, v2x_regime_amb,
#          v2x_polyarchy, dem_ep_start_year, dem_ep_end_year,
#          dem_founding_elec, dem_ep_outcome) %>%
#   write_csv(here::here("transitions", "transitions_with_notes.csv"), na = "")

prev <- read_csv(here::here("transitions/transitions_with_notes.csv"), 
                 show_col_types = FALSE) %>% 
  select(country, ccode, year, trans_year, trans_note, p5_year, bmr_year, ert_year)

### writing revised  file

df <- df %>%
  select(country_id, ccode_vdem, country_name, year, trans_p5_yr, 
         trans_p5, trans_bmr_yr, trans_bmr, trans_ert_yr, trans_ert, 
         trans_row_yr, trans_row, trans_gwf_yr, trans_gwf, polity_p5, 
         polity2_p5, change_p5, regtrans_p5, dem_bmr, dem_trans_bmr, 
         dem_omit_bmr, v2x_regime_amb, v2x_regime_ert, dem_ep_start_year, 
         dem_ep_end_year, dem_ep_outcome, dem_founding_elec, v2x_polyarchy, 
         regime_gwf, reg_fail_gwf)

# unique(ifit$country)[!unique(ifit$country) %in% unique(df$country_name)]

transitions <- df %>% 
  left_join(v6 %>% 
              select(ccode, vdem_id, country, year, tjet_dtrid, isq_dtrid, 
                     tjrc_dtrid, tjrc_rupture, tjrc_negotiated, tjrc_newstate),
            by = c("country_name" = "country", "year" = "year")) %>% 
  rename(country = country_name) %>% 
  arrange(country, year) %>% 
  # mutate(check = case_when(tjet_dtrid %in% c(3, 11, 60) ~ "check")) %>% 
  rename(ccode_old = ccode) %>% 
  mutate(# ccode = if_else(is.na(ccode) & !is.na(ccode_vdem), ccode_vdem, ccode),
         p5_yr = case_when(year == trans_p5_yr ~ year), 
         bmr_yr = case_when(year == trans_bmr_yr ~ year), 
         ert_yr = case_when(year == trans_ert_yr ~ year), 
         row_yr = case_when(year == trans_row_yr ~ year), 
         gwf_yr = case_when(year == trans_gwf_yr ~ year)) %>% 
  ### these are just for comparison, 
  ### otherwise use those ending in _year from google sheet 
  mutate(country = str_replace(country, fixed(" and "), " & "),
         country = str_replace(country, fixed("Burma/Myanmar"), "Myanmar"),
         country = str_replace(country, fixed("The Gambia"), "Gambia")) %>% 
  # full_join(ifit, by = c("country" = "country", "year" = "tr_year") ) %>%
  # mutate(ifit_yr = case_when(!is.na(tr_descr_ifit) ~ year)) %>%
  mutate(country = str_replace(country, "Democratic Republic of the Congo", 
                               "DR Congo"),
         country = str_replace(country, "Republic of the Congo",
                               "Congo (Brazzaville)"),
         country = str_replace(country, "German Democratic Republic", 
                               "German Democratic Republic (East)") ) %>% 
  filter(!country %in% c("Hong Kong", "Palestine/Gaza", "Palestine/West Bank", 
                         "Somaliland", "Zanzibar") ) %>% 
  left_join(prev, by = c("country" = "country", "year" = "year")) %>% 
  select(ccode, ccode_vdem, country_id, country, year, trans_year, trans_note, 
         p5_year, trans_p5, bmr_year, trans_bmr, ert_year, trans_ert, row_yr, 
         trans_row, gwf_yr, trans_gwf, tjet_dtrid, isq_dtrid, tjrc_dtrid, 
         dem_bmr, dem_trans_bmr, polity_p5, polity2_p5, change_p5, regtrans_p5, 
         v2x_regime_amb, v2x_polyarchy, dem_ep_start_year, dem_ep_end_year, 
         dem_founding_elec, dem_ep_outcome, regime_gwf, reg_fail_gwf) %>% 
  rename(country_id_vdem = country_id) %>%
  write_csv(here::here("transitions", "transitions_new_revised.csv"), na = "") %>% I
  # sheet_write(ss = drive_get("TJET_transitions"), sheet = "new")

### at this point, the trans_year field was coded based on our rules
### OR after a data update, new was checked against old, and trans_year and 
### trans_note adjusted (this script does not automatically code these fields)

### only used the code below for adding to Airtable once
### subsequent updates were entered manually
# transitions %>%
#   filter(year < 2021) %>%
#   filter(!is.na(trans_year) | !is.na(trans_note)) %>%
#   mutate(trans = case_when(year == trans_year ~ 1, 
#                            year != trans_year ~ 0,
#                            is.na(trans_year) ~ 0) ) %>%
#   select(country, trans, trans_year, p5_year, bmr_year, ert_year, 
#          trans_note, tjet_dtrid, year) %>% 
#   write_csv("~/Desktop/for_Airtable.csv", na = "")

transitions %>% 
  select(ccode, country_id_vdem, country, year, trans_year, 
         p5_year, bmr_year, ert_year, dem_bmr, dem_trans_bmr,
         polity_p5, polity2_p5, v2x_regime_amb, dem_ep_outcome) %>% 
  filter(!is.na(trans_year) ) 
