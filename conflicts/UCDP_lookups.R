library(tidyverse)
library(lubridate)
library(here)

### tables for lookups in database

# problems(read_csv(here("conflicts/original_data/ucdp-prio-acd-241.csv")))
read_csv(here("conflicts/original_data/ucdp-prio-acd-241.csv")) %>% 
  filter(type_of_conflict > 2) %>% 
  group_by(conflict_id) %>%
  mutate(year_beg = min(year),
         year_end = max(year),
         ep_end_date = if_else(is.na(ep_end_date) & year == 2023 & ep_end == 0, 
                               as_date("2024-01-01"), ep_end_date),
         date_end = max(ep_end_date, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(date_end = if_else(date_end == as_date("2024-01-01"), 
                            NA, date_end)) %>%
  select(conflict_id, location, year_beg, start_date, year_end, date_end, 
         # start_date2, ep_end_date, ep_end, type_of_conflict, intensity_level,
         # side_a, side_a_id, side_a_2nd, side_b, side_b_id, side_b_2nd, 
         incompatibility, territory_name) %>% 
  arrange(conflict_id, year_beg) %>%
  distinct() %>% 
  write_csv(here("conflicts/confl.csv"), na = "")

# problems(read_csv(here("conflicts/original_data/ucdp-dyadic-241.csv")))
## not using these columns
read_csv(here("conflicts/original_data/ucdp-dyadic-241.csv")) %>% 
  filter(type_of_conflict > 2) %>%
  group_by(dyad_id) %>%
  mutate(year_beg = min(year),
         year_end = max(year)) %>%
  ungroup() %>%
  select(dyad_id, conflict_id, location, year_beg, start_date, year_end,
         # start_date2, type_of_conflict, intensity_level, 
         # side_a, side_a_id, side_a_2nd, side_b, side_b_id, side_b_2nd,
         incompatibility, territory_name) %>% 
  arrange(dyad_id, year_beg) %>%
  unique() %>% 
  write_csv(here("conflicts/dyad.csv"), na = "")

### conflict spells (not used after all)

read_csv(here("conflicts/original_data/ucdp-prio-acd-241.csv")) %>% 
  filter(type_of_conflict > 2) %>%
  group_by(conflict_id) %>%
  mutate(year_beg = min(year),
         year_end = max(year),
         start_year = year(start_date2),
         end_year = year(ep_end_date)) %>%
  ungroup() %>%
  group_by(conflict_id, start_year) %>%
  mutate(max_end_year = max(end_year, na.rm = TRUE),
         end_year = if_else(is.na(end_year), max_end_year, end_year),
         end_year = if_else(is.infinite(end_year), year_end, end_year)) %>%
  ungroup() %>%
  arrange(location, conflict_id, start_year) %>%
  select(location, gwno_loc, start_year, end_year, 
         conflict_id, incompatibility, territory_name) %>% 
  unique() 

### country-year (not used after all)

read_csv(here("conflicts/original_data/ucdp-prio-acd-241.csv")) %>% 
  filter(type_of_conflict > 2)  %>%
  select(location, gwno_loc, year, conflict_id, type_of_conflict, 
         incompatibility, intensity_level, territory_name) %>%
  arrange(location, gwno_loc, year) %>%
  group_by(location, gwno_loc, year) %>%
  mutate(conflict_id = paste(conflict_id, collapse = "; "), 
         type_of_conflict = paste(type_of_conflict, collapse = "; "),
         incompatibility = paste(incompatibility, collapse = "; "), 
         territory_name = paste(territory_name, collapse = "; "),
         territory_name = if_else(territory_name == "NA", "", territory_name),
         intensity_level = max(intensity_level)) %>%
  ungroup() %>%
  rename("max_intensity_level" = "intensity_level") %>%
  unique() 
  
### dyad spells for conflict tables on country pages  
# problems(read_csv(here("conflicts/original_data/ucdp-dyadic-221.csv")))
# read_csv(here("conflicts/original_data/ucdp-dyadic-221.csv"))[c(5, 19)]
## not using these columns
read_csv(here("conflicts/original_data/ucdp-dyadic-241.csv")) %>%
  filter(type_of_conflict > 2) %>%
  mutate(ep_start_year = year(start_date2)) %>%
  group_by(dyad_id, ep_start_year) %>%
  mutate(ep_end_year = max(year),
         intensity_level = max(intensity_level),
         type_of_conflict = max(type_of_conflict)) %>%
  ungroup() %>%
  mutate(ep_years = if_else(ep_start_year == ep_end_year, 
                            as.character(ep_start_year), 
                            paste(ep_start_year, ep_end_year, sep = "-")),
         intensity = case_when(intensity_level == 1 ~ "Conflict",
                               intensity_level == 2 ~ "War"),
         intensity = if_else(type_of_conflict == 4, 
                             paste("Internationalized", intensity, sep = " "), 
                             intensity), 
         side_b = if_else(is.na(territory_name), side_b, 
                          paste(side_b, " [territory: ", 
                                territory_name, "]", sep = "") )) %>% 
  arrange(location, dyad_id, ep_start_year) %>% 
  select(dyad_id, conflict_id, location, gwno_loc, side_b, 
         # side_a, side_a_2nd, side_b_2nd, 
         ep_years, intensity, start_date2, ep_start_year, ep_end_year) %>% 
  rename(ep_start_date = start_date2) %>% 
  unique() %>% 
  left_join(read_csv(here("conflicts/confl.csv")) %>% 
              select(conflict_id, start_date, date_end) %>% 
              rename(confl_start = start_date, 
                     confl_end = date_end), 
            by = "conflict_id") %>% 
  write_csv(here("conflicts/confl_dyads.csv"), na = "")
