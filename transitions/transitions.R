# install.packages("devtools")
# devtools::install_github("vdeminstitute/ERT")
# devtools::install_github("vdeminstitute/vdemdata")
library(ERT)
library(vdemdata)
library(tidyverse)
library(readxl)
library(writexl)
library(here)
library(googlesheets4)
library(googledrive)


# read_excel("../transitions2020_v5.xlsx") %>% 
#   mutate(vdem_id = if_else(country == "Cote d'Ivoire", 64, vdem_id)) %>% 
#   ### missing South Vietnam, in VDem Republic of Vietnam (ID 35)   
#   mutate(vdem_id = if_else(country == "Germany" & is.na(vdem_id), 77, vdem_id)) %>% 
#   filter(!(country == "Germany" & year == 1990 & ccode == 260))
v6 <- read_excel(here("transitions/data/transitions2020_v6.xlsx")) %>%
  select(country, year, ccode, vdem_id, tjet_dtrid, isq_dtrid, tjrc_dtrid, 
         tjrc_ttype, tjrc_rupture, tjrc_negotiated, tjrc_newstate,
         polity2, demo_duration, bmr_demo, bmr_trans, bmr_duration, coup,
         v2x_polyarchy, v2x_libdem) %>%
  rename(v2x_polyarchy_vdem = v2x_polyarchy, 
         v2x_libdem_vdem = v2x_libdem)

other <- read_excel(here("transitions/data/p5v2018.xls")) %>% 
  filter(year > 1948) %>% 
  select(country, ccode, year, polity, polity2, change, regtrans) %>%
  rename(polity_p5 = polity,
         polity2_p5 = polity2,
         change_p5 = change,
         regtrans_p5 = regtrans) %>% 
  full_join(read_csv(here("transitions/data/democracy-v3.0.csv")) %>% 
              filter(year > 1948) %>% 
              select(country, ccode, year, democracy, democracy_trans, democracy_omitteddata) %>%
              mutate(country = str_to_title(country)) %>% 
              rename(country_bmr = country, 
                     dem_bmr = democracy, 
                     dem_trans_bmr = democracy_trans, 
                     dem_omit_bmr = democracy_omitteddata), 
            by = c("ccode", "year")) %>%
  mutate(country = if_else(is.na(country), country_bmr, country)) %>% 
  select(-country_bmr) %>% 
  mutate(country = if_else(country == "Bosnia", "Bosnia and Herzegovina", country), 
         country = if_else(country == "Antigua", "Antigua & Barbuda", country), 
         country = if_else(country == "Liechstenstein", "Liechtenstein", country), 
         country = if_else(country == "Micronesia, Fed.", "Micronesia", country),
         country = if_else(country == "Samoa (Western)", "Samoa", country), 
         country = if_else(country == "St. Vincent & Gren.", "St. Vincent", country), 
         country = if_else(country == "Myanmar (Burma)", "Burma/Myanmar", country),
         country = if_else(country == "Congo Brazzaville", "Republic of the Congo", country),
         country = if_else(country == "Congo Kinshasa", "Democratic Republic of the Congo", country),
         country = if_else(country == "Congo-Brazzaville", "Republic of the Congo", country),
         country = if_else(country == "Germany East", "German Democratic Republic", country),
         country = if_else(country == "Korea North", "North Korea", country),
         country = if_else(country == "Korea South", "South Korea", country),
         country = if_else(country == "Macedonia", "North Macedonia", country),
         country = if_else(country == "Sao Tome & Principe", "Sao Tome and Principe", country),
         country = if_else(country == "Slovak Republic", "Slovakia", country),
         country = if_else(country == "Timor Leste", "Timor-Leste", country),
         country = if_else(country == "UAE", "United Arab Emirates", country),
         country = if_else(country == "United States", "United States of America", country),
         country = if_else(country == "Cote D'Ivoire", "Ivory Coast", country),
         country = if_else(country == "Czechoslovakia", "Czech Republic", country),
         country = if_else(country == "Germany West", "Germany", country),
         country = if_else(country == "South Vietnam", "Republic of Vietnam", country),
         country = if_else(country == "Yemen South", "South Yemen", country),
         country = if_else(country == "Swaziland", "Eswatini", country),
         country = if_else(country == "Yugoslavia", "Serbia", country),
         country = if_else(country == "Yemen North", "Yemen", country),
         country = if_else(country == "Sudan-North", "Sudan", country),
         country = if_else(country == "Sudan, North", "Sudan", country),
         country = if_else(country == "Vietnam North", "Vietnam", country),
         country = if_else(country == "USSR", "Russia", country),
         country = if_else(country == "Gambia", "The Gambia", country)) %>% 
  filter( !(ccode == 678 & year == 1990) & !(ccode == 816 & year == 1976) & 
          !(ccode == 530 & year == 1993) & !(ccode == 345 & year == 1991) & !(ccode == 260 & year == 1990) & 
          !(ccode == 624 & year %in% 2011:2015) & !(ccode == 625 & year == 2011) ) %>% 
  rename(ccode_p5bmr = ccode)

# deleted duplicates in years of transformation: 
# Yemen     678  1990
# Vietnam   816  1976
# Ethiopia   530  1993
# Serbia    345  1991
# Germany   260  1990
# Sudan     624  2011:2015
# Sudan     625  2011

# other %>% group_by(country, year) %>% filter(n() > 1) %>% arrange(country, year) %>% print(n = Inf)

df <- vdem %>% 
  tibble() %>% 
  filter(year > 1948) %>% 
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
  # select(country_id, COWcode, country_name) %>%
  full_join(read_delim(here("transitions/data/GWF_AllPoliticalRegimes.txt")) %>%
              filter(year > 1948) %>% 
              mutate(gwf_regimetype = if_else(is.na(gwf_regimetype), gwf_nonautocracy, gwf_regimetype)) %>% 
              select(cowcode, year, gwf_country, gwf_regimetype, gwf_next, gwf_prior, gwf_fail) %>% 
              mutate(cowcode = case_when(cowcode == 255 & year %in% 1950:1990 ~ 260,
                                         cowcode == 315 & year == 1993 ~ 316,
                                         cowcode == 678 & year >= 1990 ~ 679,
                                         TRUE ~ cowcode) ) %>% 
              rename(ccode_gwf = cowcode,
                     regime_gwf = gwf_regimetype,
                     reg_next_gwf = gwf_next, 
                     reg_prior_gwf = gwf_prior, 
                     reg_fail_gwf = gwf_fail), 
              # %>% select(cowcode, gwf_country),
              # by = c("country_name" = "gwf_country")) %>% unique() %>% filter(COWcode != cowcode)
              by = c("ccode_vdem" = "ccode_gwf", "year" = "year")) %>%
  full_join(other, by = c("country_name" = "country", "year" = "year"))

### checking data in VDem against original data: should use original because there are few discrepancies
# df %>%
#   select(country_name, year, polity_vdem, polity2_vdem, polity_p5, polity2_p5) %>%
#   filter( (polity_vdem != polity_p5) |
#           (polity2_vdem != polity2_p5) |
#           (is.na(polity_vdem) & !is.na(polity_p5)) |
#           (!is.na(polity_vdem) & is.na(polity_p5)) ) %>%
#   print(n = Inf)
# df %>%
#   select(country_name, year, bmr_dem_vdem, bmr_trans_vdem, dem_bmr, dem_trans_bmr) %>%
#   filter( (bmr_dem_vdem != dem_bmr) |
#             (bmr_trans_vdem != dem_trans_bmr) |
#             (!is.na(bmr_dem_vdem) & is.na(dem_bmr)) ) %>%
#   print(n = Inf)
### original BMR data also includes a lot of micro states not in VDem 

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
  mutate(trans_p5_yr = case_when( (polity_p5 == -88 & polity2_p5 > 0 & polity2_p5 > polity2_lag ) |  
                                  (polity_p5 == -88 & polity2_nxt > 0 & polity2_nxt > polity2_lag ) |
                                  (polity_p5 - polity_lag >= 3 & polity_vdem > 0) ~ year), 
         trans_bmr_yr = case_when(dem_trans_bmr == 1 ~ year),
         trans_gwf_yr = case_when(reg_fail_gwf == 1 & reg_next_gwf == "democracy" ~ year)) 

df <- df %>% 
  left_join(df %>%
              select(country_name, year, trans_p5_yr) %>%
              mutate(year = year + 1) %>% 
              rename(trans_p5_yr_lag = trans_p5_yr),
            by = c("country_name", "year") ) %>%
  mutate(trans_p5_yr = replace(trans_p5_yr, trans_p5_yr_lag == trans_p5_yr - 1, NA_integer_) ) %>% 
  group_by(ccode_vdem) %>% 
  fill(trans_gwf_yr) %>% 
  ungroup() %>% 
  group_by(ccode_p5bmr) %>% 
  fill(trans_bmr_yr, trans_p5_yr) %>% 
  ungroup() %>% 
  mutate(trans_p5 = case_when(year <= 2018 & !is.na(trans_p5_yr) ~ paste(ccode_p5bmr, trans_p5_yr, year - trans_p5_yr, sep = "-")),
         trans_bmr = case_when(year <= 2015 & !is.na(trans_bmr_yr) ~ paste(ccode_p5bmr, trans_bmr_yr, year - trans_bmr_yr, sep = "-")),
         trans_gwf = case_when(year <= 2010 & !is.na(trans_gwf_yr) ~ paste(ccode_vdem, trans_gwf_yr, year - trans_gwf_yr, sep = "-")))

ert <- episodes %>% 
  filter(year > 1948) %>% 
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
  mutate(trans_row_yr = case_when(year >= dem_ep_start_year & year <= dem_ep_end_year &           
                                    v2x_regime_orig > v2x_regime_orig_lag ~ year)) %>% 
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
         trans_beg = case_when(year >= trans_ert_yr ~ min(trans_ert_yr, na.rm = TRUE)),
         trans_ert_yr = case_when(year == trans_beg ~ year) ) %>%
  ungroup() %>% 
  group_by(country_id) %>%
  fill(trans_beg, trans_row_yr) %>%
  ungroup() %>%
  mutate(trans_ert = case_when(!is.na(trans_beg) ~ paste(country_id, trans_beg, year - trans_beg, sep = "-")),
         trans_row = case_when(!is.na(trans_row_yr) ~ paste(country_id, trans_row_yr, year - trans_row_yr, sep = "-"))) %>% 
  select(country_id, country_name, year, v2x_polyarchy, v2x_regime,
         # reg_trans, reg_start, reg_max, trans_ert_yr, trans_beg, 
         trans_ert, trans_ert_yr, trans_row, trans_row_yr,
         dem_ep_start_year, dem_ep_end_year, dem_founding_elec, dem_ep_outcome, 
         aut_ep_start_year, aut_ep_end_year, aut_founding_elec, aut_ep_outcome) %>% 
  rename(v2x_regime_ert = v2x_regime)

# to_merge <- trans %>%
#   group_by(trans_ert) %>%
#   mutate(year_beg = min(year)) %>%
#   ungroup() %>%
#   select(trans_ert, country_name, country_id, year_beg) %>%
#   unique() %>%
#   filter(!is.na(trans_ert))

# v6 %>%
#   group_by(tjet_dtrid) %>%
#   mutate(year_beg = min(year)) %>%
#   ungroup() %>%
#   select(tjet_dtrid, country, vdem_id, year_beg) %>%
#   unique() %>%
#   filter(!is.na(tjet_dtrid)) %>%
#   full_join(to_merge,
#             by = c("country" = "country_name",
#                    "vdem_id" = "country_id",
#                    "year_beg" = "year_beg")) %>%
#   arrange(country, year_beg) %>%
#   select(tjet_dtrid, trans_ert, country, vdem_id, year_beg) %>%
#   filter(year_beg > 1969 & year_beg < 2021) %>%
#   write_csv("~/Desktop/transition_start_years.csv", na = "") %>%
#   write_xlsx("../../Transitions/transition_start_years.xlsx")

df <- df %>% 
  full_join(trans, by = c("country_id", "country_name", "year")) %>%
  mutate (country_name = if_else(country_name == "Ivory Coast", "Cote d'Ivoire", country_name),
          country_name = if_else(country_name == "Yemen" & year %in% 1962:1989, "North Yemen", country_name),
          country_name = if_else(country_name == "Czech Republic" & year <= 1992, "Czechoslovakia", country_name)) 

### merge by country names and check carefully
# v6 %>%
#   select(country, ccode, vdem_id) %>%
#   full_join(df %>% select(country_name, country_id),
#             by = c("country" = "country_name")) %>%
#   unique() %>%
#   filter(is.na(vdem_id) | is.na(ccode) | vdem_id != country_id) %>%
#   print(n = Inf)

ifit <- read_csv(here("transitions/data/Freeman.csv")) %>%
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
  filter(tr_year > 1969 & tr_year < 2021 & !is.na(tr_year)) %>% 
  rename(tr_num_ifit = num_trans,
         tr_dem_ifit = trans_dem,
         tr_confl_ifit = trans_confl,
         tr_new_ifit = trans_new,
         tr_descr_ifit = tr_descr)

prev <- read_csv(here("transitions/transitions_with_notes.csv"),
                 col_types = list(note_GD = col_character())) %>% 
  select(country, year, NEW, note_Timo, note_GD, tjet_dtrid) %>% 
  rename(trans_year = NEW,
         tjet_dtrid_prev = tjet_dtrid)

v6 %>% 
  rename(polity2_gd = polity2,
         demo_duration_gd = demo_duration, 
         bmr_demo_gd = bmr_demo,
         bmr_trans_gd = bmr_trans,
         bmr_dur_gd = bmr_duration,
         coup_gd = coup,
         v2x_polyarchy_vdem_gd = v2x_polyarchy_vdem, 
         v2x_libdem_vdem_gd = v2x_libdem_vdem) %>% 
  full_join(df, by = c("country" = "country_name", "year" = "year")) %>% 
  # mutate(check = case_when(tjet_dtrid %in% c(3, 11, 60) ~ "check")) %>% 
  # filter(year > 1969 & year < 2021) %>%
  mutate(ccode = if_else(is.na(ccode) & !is.na(ccode_vdem), ccode_vdem, ccode),
         p5_yr = case_when(year == trans_p5_yr ~ year), 
         bmr_yr = case_when(year == trans_bmr_yr ~ year), 
         ert_yr = case_when(year == trans_ert_yr ~ year), 
         row_yr = case_when(year == trans_row_yr ~ year), 
         gwf_yr = case_when(year == trans_gwf_yr ~ year)) %>%
  mutate(country = str_replace(country, fixed(" and "), " & "),
         country = str_replace(country, fixed("Burma/Myanmar"), "Myanmar"),
         country = str_replace(country, fixed("The Gambia"), "Gambia")) %>% 
  full_join(ifit, by = c("country" = "country", "year" = "tr_year") ) %>% 
  mutate(ifit_yr = case_when(!is.na(tr_descr_ifit) ~ year)) %>% 
  arrange(country, year) %>% 
  full_join(prev, by = c("country" = "country", "year" = "year")) %>% 
  select(country, year, trans_year, note_Timo, note_GD, 
         tjet_dtrid,  
         p5_yr, trans_p5, 
         bmr_yr, trans_bmr, 
         ert_yr, trans_ert, 
         row_yr, trans_row, 
         gwf_yr, trans_gwf, ifit_yr,
         polity_p5, polity_vdem, polity2_p5, polity2_vdem, polity2_gd, 
         change_p5, regtrans_p5, demo_duration_gd, 
         dem_bmr, dem_omit_bmr, bmr_dem_vdem, bmr_demo_gd, dem_trans_bmr, bmr_trans_vdem, bmr_trans_gd, bmr_dur_gd,
         v2x_regime_amb, v2x_regime_vdem, v2x_polyarchy, 
         dem_ep_start_year, dem_ep_end_year, dem_founding_elec, dem_ep_outcome, 
         aut_ep_start_year, aut_ep_end_year, aut_founding_elec, aut_ep_outcome, 
         # v2x_polyarchy_vdem_gd, v2x_libdem_vdem_gd, 
         coup_gd, coup_pt_vdem, coups_pipe_vdem, 
         regime_gwf, reg_next_gwf, reg_prior_gwf, reg_fail_gwf,
         tr_num_ifit, tr_dem_ifit, tr_confl_ifit, tr_new_ifit, tr_descr_ifit, 
         gwf_country, ccode, country_id, ccode_p5bmr, ccode_vdem) %>% 
  rename(vdem_id = country_id) %>% 
  unique() %>%
  # filter(v2x_regime_vdem !=	v2x_regime_ert) 
  write_csv(here("transitions/transitions_new.csv"), na = "") # %>% write_xlsx("../../Transitions/transitions_comparison_timo.xlsx")

### need to clean up below
# Czech Republic
# Czechoslovakia
# Palestine/WB
# Palestine/Gaza
# Somaliland
# Zanzibar
# indicators for source data
# make sure to only use existing country years



# read_csv("~/Desktop/new.csv") %>% 
#   select(country, year, NEW_Timo, note_Timo, tjet_dtrid) %>% 
#   group_by(country, tjet_dtrid) %>%
#   mutate(tjet_start = case_when(!is.na(tjet_dtrid) ~  min(year)),
#          actual_new = str_split_i(note_Timo, "\\(|\\)", i = 2)) %>%
#   ungroup() %>%
#   filter(!is.na(NEW_Timo) | !is.na(note_Timo) | year == tjet_start) %>%
#   rename("new_start" = "NEW_Timo",
#          "note" = "note_Timo") %>%
#   select(country, new_start, tjet_start, actual_new, tjet_dtrid, note) %>%
#   unique() %>%
#   write_csv("~/Desktop/comparison.csv", na = "")


transitions <- drive_get("transitions") %>% 
  read_sheet(sheet = "transitions_new") 
transitions %>%
  filter(!is.na(trans_year) | !is.na(flag) | !is.na(note)) %>%
  mutate(trans = case_when(year == trans_year ~ 1, 
                           year != trans_year ~ 0,
                           is.na(trans_year) ~ 0), 
         flag = ifelse(is.na(flag), 0, flag)) %>% 
  select(country, trans, flag, trans_year, p5_year, bmr_year, ert_year, note, tjet_dtrid, year) %>% 
  # filter(trans == 1 & trans_year > 1969) %>% print(n = Inf)
  write_csv("~/Desktop/for_Airtable.csv", na = "")
