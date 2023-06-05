require(tidyverse)
require(here)

load(here("data", "tjetdb.RData"), verbose = TRUE)

countrylist <- db$Countries %>% 
  mutate(beg = as.integer(str_sub(begin_date, 1, 4)), 
         end = as.integer(str_sub(end_date, 1, 4)), 
         beg = ifelse(beg <= 1965, 1965, beg),
         beg = ifelse(country == "Slovenia", 1991, beg),
         beg = ifelse(country == "Andorra", 1993, beg),
         beg = ifelse(country == "Kiribati", 1997, beg),
         beg = ifelse(country == "Liechtenstein", 1990, beg),
         beg = ifelse(country == "Marshall Islands", 1991, beg),
         beg = ifelse(country == "Micronesia", 1992, beg),
         beg = ifelse(country == "Monaco", 1993, beg),
         end = ifelse(country == "Serbia & Montenegro", 2005, end),
         end = ifelse(country == "German Federal Republic (West)", 1989, end),
         end = ifelse(country == "Yemen Arab Republic (North)", 1989, end),
         region_sub_un = ifelse(is.na(intregion), subregion, intregion) ) %>% 
  select(country, ccode, ccode_case, ccode_ksg, beg, end, region, region_sub_un, region_wb, focus) %>% 
  rename("tjet_focus" = "focus") %>% 
  arrange(country)

translist <- read_csv("transitions/transitions_new_revised.csv") %>%
  # filter(!is.na(trans_year) & trans_year >= 1960) %>% 
  filter(country != "Serbia & Montenegro") %>% 
  mutate(
         # country = ifelse(country == "Democratic Republic of the Congo", "DR Congo", country),
         # country = ifelse(country == "Eswatini", "Swaziland (Eswatini)", country),
         # country = ifelse(country == "German Democratic Republic", "German Democratic Republic (East)", country),
         # country = ifelse(country == "North Yemen", "Yemen Arab Republic (North)", country),
         # country = ifelse(country == "Republic of the Congo", "Congo (Brazzaville)", country),
         # country = ifelse(country == "Republic of Vietnam", "Vietnam (Democratic Republic of)", country),
         # country = ifelse(country == "St. Vincent", "St. Vincent & the Grenadines", country),
         # country = ifelse(country == "South Yemen", "Yemen People's Republic (South)", country)
         # ccode = ifelse(country == "Serbia & Montenegro", 345, ccode)
         ) %>% 
  rename("country_trans" = "country") %>% 
  full_join(countrylist %>% select(country, ccode, beg, end), by = "ccode") %>%
  # full_join(countrylist %>% select(country, ccode, beg) , by = "country") %>%
  arrange(country, year) %>%
  filter(year >= 1965 & year >= beg & year <= end) %>% 
  # select(country, ccode, country_trans, beg, end, year, v2x_polyarchy) %>% filter(country != country_trans)  %>% print(n = Inf)
  mutate(transition = ifelse(is.na(trans_year), 0, 1), 
         dem_polity = ifelse(polity_p5 >= 6, 1, 0),
         dem_vdem = ifelse(str_detect(str_to_lower(v2x_regime_amb), "democracy"), 1, 0)) %>% 
  select(country, ccode, year, transition, 
         # p5_year, bmr_year, ert_year, trans_note, v2x_polyarchy,
         # dem_ep_start_year, dem_ep_end_year, v2x_regime_amb, polity_p5,
         dem_bmr, dem_polity, dem_vdem) %>% 
  group_by(ccode) %>% 
  mutate(dem_polity_min = min(dem_polity, na.rm = TRUE),
         dem_polity_max = max(dem_polity, na.rm = TRUE),
         dem_polity_max = ifelse(is.infinite(dem_polity_max), NA, dem_polity_max),
         dem_bmr_min = min(dem_bmr, na.rm = TRUE),
         dem_bmr_max = max(dem_bmr, na.rm = TRUE),
         dem_bmr_max = ifelse(is.infinite(dem_bmr_max), NA, dem_bmr_max),
         dem_vdem_min = min(dem_vdem, na.rm = TRUE),
         dem_vdem_max = max(dem_vdem, na.rm = TRUE),
         dem_vdem_max = ifelse(is.infinite(dem_vdem_max), NA, dem_vdem_max),
         sources = 3 - (is.na(dem_polity_max) + is.na(dem_bmr_max) + is.na(dem_vdem_max)),
         regime = max(transition, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(context_bmr = case_when(dem_bmr_min == 0 & dem_bmr_max == 0 ~ 0,
                                 dem_bmr_min == 1 & dem_bmr_max == 1 ~ 1),
         context_polity = case_when(dem_polity_min == 0 & dem_polity_max == 0 ~ 0,
                                   dem_polity_min == 1 & dem_polity_max == 1 ~ 1),
         context_vdem = case_when(dem_vdem_min == 0 & dem_vdem_max == 0 ~ 0,
                                 dem_vdem_min == 1 & dem_vdem_max == 1 ~ 1),
         context_dem = rowSums(across(all_of(c("context_bmr", "context_polity", "context_vdem"))), na.rm = TRUE),
         regime = case_when(regime == 1 ~ "transitional", 
                            regime == 0 & context_dem == 0 & sources > 0 ~ "autocratic",
                            regime == 0 & (context_dem == sources | context_dem > 1) ~ "democratic",
                            country == "India" ~ "democratic"),
         reg_democ = ifelse(regime == "democratic", 1, 0), 
         reg_autoc = ifelse(regime == "autocratic", 1, 0), 
         reg_trans = ifelse(regime == "transitional", 1, 0)
    ) %>% 
  select(country, ccode, year, transition, dem_bmr, dem_polity, dem_vdem, regime, reg_democ, reg_autoc, reg_trans)

confllist <- read_csv("conflicts/confl_dyads.csv") %>% 
  select(location, gwno_loc, ep_start_year, ep_end_year) %>% 
  rowwise() %>% 
  mutate(year = list(ep_start_year:ep_end_year)) %>% 
  ungroup() %>% 
  unnest_longer(year) %>% 
  select(-ep_start_year, -ep_end_year) %>% 
  distinct() %>% 
  mutate(conflict = 1) %>% 
  filter(year >= 1965 & year <= 2020)

amnesties <- db$Amnesties %>% 
  left_join(countrylist %>% select(ccode, ccode_case) %>% distinct(), 
            by = "ccode") %>% 
  arrange(ccode_case, amnestyYear) %>%  
  group_by(ccode_case, amnestyYear) %>%
  mutate(amnesties = n(),
         SGBV = max(SGBV)) %>% 
  ungroup() %>% 
  select(ccode_case, amnestyYear, amnesties, SGBV) %>% 
  distinct() %>% 
  rename("year" = "amnestyYear", 
         "amnesties_SGBV" = "SGBV") %>% 
  filter(year >= 1970 & year <= 2020)

reparations <- db$Reparations %>%
  left_join(countrylist %>% select(ccode, ccode_case) %>% distinct(), 
            by = "ccode") %>% 
  arrange(ccode_case, yearCreated) %>%  
  mutate(SGBV = ifelse(harmsSexualViolence == 1 | genderCrimes == "yes" | lgbtqCrimes == "yes", 1, 0) ) %>%    
  group_by(ccode_case, yearCreated) %>%
  mutate(reparations = n(),
         SGBV = max(SGBV)) %>% 
  ungroup() %>% 
  select(ccode_case, yearCreated, reparations, SGBV) %>% 
  distinct() %>% 
  rename("year" = "yearCreated",
         "reparations_SGBV" = "SGBV") %>% 
  filter(year >= 1970 & year <= 2020)

tcs <- db$TruthCommissions %>%
  left_join(countrylist %>% select(ccode, ccode_case) %>% distinct(), 
            by = "ccode") %>% 
  arrange(ccode_case, yearPassed) %>%  
  group_by(ccode_case, yearPassed) %>%
  mutate(tcs = n(),
         SGBV = max(SGBV)) %>% 
  ungroup() %>% 
  select(ccode_case, yearPassed, tcs, SGBV) %>% 
  distinct() %>% 
  rename("year" = "yearPassed",
         "tcs_SGBV" = "SGBV") %>% 
  filter(year >= 1970 & year <= 2020)

trials <- db$Trials %>% 
  rename(ccode = "ccode_Accused") %>% 
  left_join(countrylist %>% select(ccode, ccode_case) %>% distinct(), 
            by = "ccode") %>% 
  arrange(ccode_case, yearStart) %>%  
  mutate(domestic = ifelse(str_detect(trialType, "domestic"), 1, 0), 
         SGBV = ifelse(rape_Accused == 1 | sexualViolence_Accused == 1 | otherSGBV_Accused == 1, 1, 0) ) %>% 
  select(ccode_case, yearStart, domestic, SGBV) 

other <- trials %>% 
  filter(domestic == 0) %>% 
  select(ccode_case, yearStart, SGBV) %>%
  group_by(ccode_case, yearStart) %>%
  mutate(trials_other = n(),
         SGBV = max(SGBV)) %>%
  ungroup() %>%
  select(ccode_case, yearStart, trials_other, SGBV) %>% 
  distinct() %>%
  rename("year" = "yearStart",
         "trials_other_SGBV" = "SGBV") %>% 
  filter(year >= 1970 & year <= 2020)

domestic <- trials %>% 
  filter(domestic == 1) %>% 
  select(ccode_case, yearStart, SGBV) %>%
  group_by(ccode_case, yearStart) %>%
  mutate(trials_domestic = n(),
         SGBV = max(SGBV)) %>%
  ungroup() %>%
  select(ccode_case, yearStart, trials_domestic, SGBV) %>% 
  distinct() %>%
  rename("year" = "yearStart",
         "trials_domestic_SGBV" = "SGBV") %>% 
  filter(year >= 1970 & year <= 2020)

db[["CountryYears"]] <- map(countrylist$country , function(ctry) {
  beg <- countrylist %>%
    filter(country == ctry) %>%
    select(beg) %>%
    unlist(use.names = FALSE)
  end <- countrylist %>%
    filter(country == ctry) %>%
    select(end) %>%
    unlist(use.names = FALSE)
  expand_grid(country = ctry, year = beg:end) %>%
    tibble()
}) %>%
  do.call(rbind, .) %>%
  full_join(countrylist, by = "country") %>%
  mutate(cyID = paste(ccode, year, sep = "-")) %>%
  full_join(translist, by = c("country", "ccode", "year") ) %>% 
  full_join(confllist, by = c("ccode_ksg" = "gwno_loc", "year" = "year") ) %>% 
  mutate(conflict = ifelse(is.na(conflict), 0, 1)) %>%
  group_by(ccode_case) %>%
  mutate(conflict = max(conflict) ) %>%
  ungroup() %>% 
  select(cyID, country, year, ccode, ccode_case, ccode_ksg, tjet_focus, region, 
         regime, reg_democ, reg_autoc, reg_trans, conflict) %>% 
  filter(year >= 1970) %>% 
  left_join(amnesties, by = c("ccode_case", "year") ) %>%  
  mutate(amnesties = ifelse(is.na(amnesties), 0, amnesties),
         amnesties_SGBV = ifelse(is.na(amnesties_SGBV), 0, amnesties_SGBV)) %>% 
  left_join(reparations, by = c("ccode_case", "year") ) %>%  
  mutate(reparations = ifelse(is.na(reparations), 0, reparations),
         reparations_SGBV = ifelse(is.na(reparations_SGBV), 0, reparations_SGBV)) %>% 
  left_join(tcs, by = c("ccode_case", "year") ) %>% 
  mutate(tcs = ifelse(is.na(tcs), 0, tcs),
         tcs_SGBV = ifelse(is.na(tcs_SGBV), 0, tcs_SGBV)) %>% 
  left_join(domestic, by = c("ccode_case", "year") ) %>% 
  mutate(trials_domestic = ifelse(is.na(trials_domestic), 0, trials_domestic),
         trials_domestic_SGBV = ifelse(is.na(trials_domestic_SGBV), 0, trials_domestic_SGBV)) %>% 
  left_join(other, by = c("ccode_case", "year") ) %>% 
  mutate(trials_other = ifelse(is.na(trials_other), 0, trials_other),
         trials_other_SGBV = ifelse(is.na(trials_other_SGBV), 0, trials_other_SGBV) ) # %>% write_csv("~/Desktop/temp.csv") 

db$Countries <- countrylist  %>% 
  mutate(beg = ifelse(beg <= 1970, 1970, beg)) 

db$dictionary <- read_csv(here("pipeline", "dictionary.csv"))         

save(db, file = here("data", "tjetdb.RData"))
