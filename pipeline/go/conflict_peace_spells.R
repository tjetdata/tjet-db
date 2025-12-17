require(tidyverse)

#####################
### conflict episodes
#####################

acd <- read_csv(here::here(
  "conflicts",
  "original_data",
  "UcdpPrioConflict_v25_1.csv"
)) |>
  filter(type_of_conflict > 2) |>
  select(
    conflict_id,
    location,
    gwno_loc,
    year,
    side_b,
    incompatibility,
    territory_name,
    intensity_level,
    cumulative_intensity,
    start_date,
    start_date2,
    ep_end_date
  ) |>
  mutate(territory_name = str_trim(territory_name)) |>
  arrange(location, conflict_id, year)

###############
### dyad counts
###############

dyad <- read_csv(here::here(
  "conflicts",
  "original_data",
  "Dyadic_v25_1.csv"
)) |>
  filter(type_of_conflict > 2) |>
  select(
    dyad_id,
    conflict_id,
    location,
    gwno_loc,
    year,
    incompatibility,
    territory_name,
    intensity_level,
    start_date,
    start_date2
  ) |>
  reframe(
    .by = c(
      conflict_id,
      location,
      gwno_loc,
      year,
      incompatibility,
      territory_name
    ),
    n_dyads = n()
  ) |>
  mutate(territory_name = str_trim(territory_name))

########################
### termination outcomes
########################

term <- read_csv(here::here(
  "conflicts",
  "original_data",
  "UCDPConflictTerminationDataset_v4_2024_Conflict.csv"
)) |>
  filter(type_of_conflict > 2) |>
  select(
    conflict_id,
    location,
    gwno_loc,
    year,
    incompatibility,
    territory_name,
    intensity_level,
    cumulative_intensity,
    start_date,
    start_date2,
    c_epid,
    c_epno,
    c_ep_startyear,
    c_epterm,
    c_outcome,
    c_ep_endyear,
    c_ependdate,
    c_ep_durcount
  ) |> # c_ep_dur
  arrange(location, conflict_id, year)

#########################
### battle-related deaths
#########################

brd <- read_csv(here::here(
  "conflicts",
  "original_data",
  "BattleDeaths_v25_1_conf.csv"
)) |>
  filter(type_of_conflict > 2) |>
  select(
    conflict_id,
    location_inc,
    gwno_loc,
    year,
    incompatibility,
    territory_name,
    bd_best,
    bd_low,
    bd_high
  ) |>
  mutate(gwno_loc = as.integer(gwno_loc)) |>
  rename(location = location_inc)

lacina <- readxl::read_xls(here::here(
  "conflicts",
  "original_data",
  "PRIO Battle Deaths Dataset 31.xls"
)) |>
  filter(type > 2) |>
  mutate(
    gwnoloc = as.integer(ifelse(gwnoloc == -99, 750, gwnoloc)),
    location = ifelse(location == "Hyderabad", "India", location),
    bdeadbes = ifelse(bdeadbes == -999, NA, bdeadbes),
    startdate = as_date(startdate),
    startdate2 = as_date(startdate2),
    ependdate = as_date(ependdate),
    old_id = as.character(id)
  ) |>
  left_join(
    read_csv(
      here::here("conflicts", "original_data", "translate_conf.csv"),
      show_col_types = FALSE
    ),
    by = "old_id"
  ) |>
  rename(
    conflict_id = new_id,
    gwno_loc = gwnoloc,
    incompatibility = incomp
  ) |>
  select(
    conflict_id,
    gwno_loc,
    year,
    incompatibility,
    bdeadlow,
    bdeadhig,
    bdeadbes
  )

########
### SVAC
########

# readxl::read_excel(here::here("conflicts", "original_data", "SVAC_3.2_complete.xlsx")) |>
#   filter(type > 2) |>
#   rename(
#     conflict_id = conflictid,
#     incompatibility = incomp
#   ) |>
#   select(conflict_id, location, gwnoloc, year, incompatibility,
#     actor, actorid, actor_type, conflictyear, interm, postc,
#     state_prev, ai_prev, hrw_prev
#   ) |>
#   arrange(location, conflict_id, year)

#######################
### pulling it together
#######################

confl_ep_years <- acd |>
  mutate(
    ep_beg = year(start_date2),
    ep_end = year(ep_end_date),
    bd_beg = year(start_date)
  ) |>
  group_by(conflict_id) |>
  mutate(confl_beg = min(ep_beg)) |>
  ungroup() |>
  full_join(
    dyad,
    by = c(
      "conflict_id",
      "location",
      "gwno_loc",
      "incompatibility",
      "territory_name",
      "year"
    )
  ) |>
  full_join(
    term,
    by = c(
      "conflict_id",
      "location",
      "gwno_loc",
      "incompatibility",
      "territory_name",
      "year",
      "intensity_level",
      "cumulative_intensity",
      "start_date",
      "start_date2"
    )
  ) |>
  mutate(
    c_epterm = ifelse(
      conflict_id == 234 & year == 1996 & c_epterm == 0,
      1,
      c_epterm
    ),
    c_epterm = ifelse(
      conflict_id == 234 & year == 1994 & c_epterm == 1,
      0,
      c_epterm
    ),
    c_outcome = ifelse(
      conflict_id == 234 & year == 1996 & is.na(c_outcome),
      5,
      c_outcome
    ),
    c_outcome = ifelse(
      conflict_id == 234 & year == 1994 & c_outcome == 5,
      NA,
      c_outcome
    ),
    c_ep_endyear = ifelse(
      conflict_id == 234 & year == 1996 & is.na(c_ep_endyear),
      1996,
      c_ep_endyear
    ),
    c_ep_endyear = ifelse(
      conflict_id == 234 & year == 1994 & c_ep_endyear == 1994,
      NA,
      c_ep_endyear
    ),
    c_ependdate = ifelse(
      conflict_id == 234 & year == 1996 & is.na(c_ependdate),
      ep_end_date,
      c_ependdate
    ) |>
      as_date(),
    c_ependdate = ifelse(
      conflict_id == 234 & year == 1994 & !is.na(c_ependdate),
      NA,
      c_ependdate
    ) |>
      as_date()
  ) |>
  mutate(
    gwno_loc = as.integer(ifelse(
      location == "Hyderabad" & gwno_loc == 751,
      750,
      gwno_loc
    )),
    location = ifelse(
      location == "Hyderabad" & gwno_loc == 750,
      "India",
      location
    ),
    ep_end_date = as_date(ifelse(
      is.na(ep_end_date) & !is.na(c_ependdate),
      c_ependdate,
      ep_end_date
    )),
    outcome = case_when(
      c_outcome == 1 ~ "peace agreement",
      c_outcome == 2 ~ "ceasefire agreement",
      c_outcome == 3 ~ "victory for government side",
      c_outcome == 4 ~ "victory for non-state side",
      c_outcome == 5 ~ "low activity",
      c_outcome == 6 ~ "actor ceases to exist"
    )
  ) |>
  left_join(
    brd,
    by = c(
      "conflict_id",
      "location",
      "gwno_loc",
      "year",
      "incompatibility",
      "territory_name"
    )
  ) |>
  left_join(
    lacina,
    by = c("conflict_id", "gwno_loc", "year", "incompatibility")
  ) |>
  mutate(
    bd_best = ifelse(is.na(bd_best), bdeadbes, bd_best),
    bd_low = ifelse(is.na(bd_low), bdeadlow, bd_low),
    bd_high = ifelse(is.na(bd_high), bdeadhig, bd_high),
    bd_best = ifelse(is.na(bd_best) & intensity_level == 1, 25, bd_best),
    bd_low = ifelse(is.na(bd_low) & intensity_level == 1, 25, bd_low),
    bd_high = ifelse(is.na(bd_high) & intensity_level == 1, 999, bd_high),
    bd_best = ifelse(is.na(bd_best) & intensity_level == 2, 1000, bd_best),
    bd_low = ifelse(is.na(bd_low) & intensity_level == 2, 1000, bd_low),
    bd_high = ifelse(is.na(bd_high) & intensity_level == 2, 9999, bd_high)
  ) |>
  select(-bdeadlow, -bdeadhig, -bdeadbes) |>
  # mutate(
  #   confl_new = ifelse(year == confl_beg, 1, 0),
  #   confl_new_25 = confl_new * ifelse(bd_cumu_ep_end >= 25, 1, 0),
  #   confl_new_100 = confl_new * ifelse(bd_cumu_ep_end >= 100, 1, 0),
  #   confl_new_1000 = confl_new * ifelse(bd_cumu_ep_end >= 1000, 1, 0),
  #   confl_new_ep = ifelse(year == ep_beg & year != confl_beg, 1, 0),
  #   confl_new_ep_25 = confl_new_ep * ifelse(bd_cumu_ep_end >= 25, 1, 0),
  #   confl_new_ep_100 = confl_new_ep * ifelse(bd_cumu_ep_end >= 100, 1, 0),
  #   confl_new_ep_1000 = confl_new_ep * ifelse(bd_cumu_ep_end >= 1000, 1, 0),
  #   confl_cont = ifelse(year > ep_beg, 1, 0),
  #   confl_cont_25 = confl_cont * ifelse(bd_cumu_ep_end >= 25, 1, 0),
  #   confl_cont_100 = confl_cont * ifelse(bd_cumu_ep_end >= 100, 1, 0),
  #   confl_cont_1000 = confl_cont * ifelse(bd_cumu_ep_end >= 1000, 1, 0)
  # ) |>
  arrange(gwno_loc, conflict_id, ep_beg, year) |>
  group_by(conflict_id) |>
  mutate(
    recur_later = ifelse(c_epno < max(c_epno), 1, 0),
    # confl_last = ifelse(year == max(year), year, NA),
    confl_last = max(year),
    # ep_next_beg = ifelse(c_epterm == 1, lead(ep_beg) - ep_end, NA),
    ep_prev_end = ifelse(c_epno > 1 & year == ep_beg, lag(ep_end), NA),
    ep_next_beg = ifelse(c_epterm == 1, lead(ep_beg), NA),
    pc_yrs = ep_next_beg - ep_end - 1,
    bd_cumu_confl = cumsum(bd_best),
  ) |>
  group_by(conflict_id, ep_beg) |>
  mutate(
    n_dyads_ep = max(n_dyads),
    bd_cumu_ep = cumsum(bd_best),
    bd_cumu_ep_end = max(bd_cumu_ep)
  ) |>
  ungroup() |>
  group_by(conflict_id) |>
  mutate(
    bd_ep_before = ifelse(c_epno > 1 & year == ep_beg, lag(bd_cumu_confl), NA)
  ) |>
  ungroup() |>
  arrange(conflict_id, ep_beg) |>
  group_by(conflict_id, ep_beg) |>
  fill(ep_end, .direction = "up") |>
  fill(
    ep_prev_end,
    bd_ep_before,
    .direction = "down"
  ) |>
  mutate(
    ep_threshold = case_when(
      bd_cumu_ep_end >= 1000 ~ 1000,
      bd_cumu_ep_end >= 500 ~ 500,
      bd_cumu_ep_end >= 100 ~ 100,
      bd_cumu_ep_end >= 25 ~ 25
    ),
    confl_before_threshold = case_when(
      bd_ep_before >= 1000 ~ 1000,
      bd_ep_before >= 500 ~ 500,
      bd_ep_before >= 100 ~ 100,
      bd_ep_before >= 25 ~ 25
    )
  ) |>
  ungroup()

#######################################
### collapse to conflict episode spells
#######################################

# confl_ep_years |>
#   filter(c_epterm == 1) |>
#   select(
#     conflict_id,
#     location,
#     ep_beg,
#     ep_end,
#     bd_beg,
#     confl_beg,
#     confl_last,
#     n_dyads_ep,
#     recur_later,
#     ep_prev_end,
#     ep_next_beg,
#     pc_yrs,
#     c_epno,
#     c_ep_durcount,
#     outcome,
#     bd_ep_before,
#     bd_cumu_confl,
#     bd_cumu_ep_end,
#     confl_before_threshold,
#     ep_threshold
#   )

confl_ep_years <- confl_ep_years |>
  filter(c_epterm == 1) |> ## & confl_last > 1950
  mutate(ep_next_beg = ifelse(is.na(ep_next_beg), 2025, ep_next_beg)) |>
  rowwise() |>
  mutate(year = list((ep_end + 1):(ep_next_beg - 1))) |>
  ungroup() |>
  select(
    conflict_id,
    location,
    gwno_loc,
    year,
    incompatibility,
    territory_name,
    cumulative_intensity,
    n_dyads,
    n_dyads_ep,
    start_date,
    start_date2,
    ep_end_date,
    bd_beg,
    confl_beg,
    ep_beg,
    ep_end,
    confl_last,
    c_epno,
    c_epid,
    outcome,
    recur_later,
    ep_next_beg,
    bd_cumu_confl,
    bd_cumu_ep,
    bd_cumu_ep_end,
    ep_threshold,
    confl_before_threshold
  ) |>
  unnest(year) |>
  mutate(
    intensity_level = 0,
    pc_dur = year - ep_end,
    sample = "post-conflict"
  ) |>
  full_join(
    confl_ep_years,
    by = c(
      "conflict_id",
      "location",
      "gwno_loc",
      "year",
      "incompatibility",
      "territory_name",
      "intensity_level",
      "cumulative_intensity",
      "n_dyads",
      "n_dyads_ep",
      "start_date",
      "start_date2",
      "ep_end_date",
      "bd_beg",
      "confl_beg",
      "ep_beg",
      "ep_end",
      "confl_last",
      "c_epno",
      "c_epid",
      "outcome",
      "recur_later",
      "ep_next_beg",
      "bd_cumu_confl",
      "bd_cumu_ep",
      "bd_cumu_ep_end",
      "ep_threshold",
      "confl_before_threshold"
    )
  ) |>
  arrange(conflict_id, year) |>
  mutate(
    sample = case_when(
      sample == "post-conflict" ~ "post-conflict",
      is.na(sample) & c_epterm == 1 ~ "switch year",
      is.na(sample) & c_epterm == 0 & intensity_level > 0 ~ "conflict"
    ),
    ep_next_beg = ifelse(ep_next_beg == 2025, NA, ep_next_beg),
    pc_dur = ifelse(is.na(pc_dur) & c_epterm == 1, 0, pc_dur)
  ) |>
  filter(!(location == "South Vietnam" & year > 1975)) |>
  filter(!(location == "South Yemen" & year > 1990)) |>
  mutate(
    gwno_loc = ifelse(
      str_detect(location, fixed("Serbia (Yugoslavia)")) & year > 2005,
      340,
      gwno_loc
    )
  )

confl_ep_years |>
  filter(.by = c(conflict_id, year), n() > 1)

###################
### example country
###################

# confl_ep_years |>
#   select(
#     conflict_id,
#     sample,
#     year,
#     location,
#     gwno_loc,
#     ep_beg,
#     ep_end,
#     bd_beg,
#     confl_beg,
#     confl_last,
#     c_epno,
#     c_epterm,
#     c_ep_durcount,
#     outcome,
#     n_dyads,
#     recur_later,
#     ep_next_beg,
#     pc_yrs,
#     bd_best,
#     bd_ep_before,
#     bd_cumu_confl,
#     bd_cumu_ep,
#     bd_cumu_ep_end,
#     confl_before_threshold,
#     ep_threshold
#   ) |>
#   # select(
#   #   -location,
#   #   -side_b,
#   #   -incompatibility,
#   #   -territory_name,
#   #   -intensity_level,
#   #   -cumulative_intensity,
#   #   -n_dyads,
#   #   -n_dyads_ep,
#   #   -start_date,
#   #   -start_date2,
#   #   -ep_end_date,
#   #   -bd_beg,
#   #   -confl_beg,
#   #   -ep_prev_end,
#   #   -ep_beg,
#   #   -ep_end,
#   #   -confl_last,
#   #   -c_epno,
#   #   -c_epid,
#   #   -c_epterm,
#   #   -c_ep_startyear,
#   #   -c_ep_endyear,
#   #   -c_ependdate,
#   #   -c_outcome,
#   #   -c_ep_durcount,
#   #   -outcome,
#   #   -pc_yrs,
#   #   -pc_dur,
#   #   -recur_later,
#   #   -ep_next_beg,
#   #   -bd_best,
#   #   -bd_low,
#   #   -bd_high,
#   #   -bd_cumu_confl,
#   #   -bd_ep_before,
#   #   -bd_cumu_ep,
#   #   -bd_cumu_ep_end,
#   #   -ep_threshold,
#   #   -confl_before_threshold
#   # ) |>
#   filter(gwno_loc == 540) |>
#   print(n = Inf)

#########################################################
### merging in existing TJET measures (not by conflictID)
#########################################################

# df <- read_csv(
#   here::here("tjet_datasets/tjet_cy_analyses.csv")
# )

# df |>
#   select(
#     country_case,
#     year,
#     # starts_with("aco_"),
#     starts_with("dco_"),
#     starts_with("pco_"),
#     starts_with("confl_"),
#     starts_with("outcome_")
#   ) |>
#   select(!ends_with("_cflag")) |>
#   filter(str_detect(country_case, "Angola")) |>
#   print(n = 55)

to_merge <- df |>
  select(
    country_case,
    ccode_ksg,
    year,
    tj_laws,
    regu_trs_dom_sta,
    regu_cce_dom_sta,
    tran_trs_dom_dtj_ctj,
    tran_trs_dom_ctj,
    trs_int_sta,
    trs_int_opp,
    amnesty_dtj_ctj_sta_opp,
    amnesty_pol,
    tcs_all_created,
    rep_created,
  ) |>
  arrange(country_case, year) |>
  group_by(country_case) |>
  mutate(
    across(
      .cols = !c(ccode_ksg, year),
      .fns = ~ cumsum(.x),
      .names = "sum_{.col}"
    )
  ) |>
  ungroup() |>
  select(ccode_ksg, year, starts_with("sum_")) |>
  mutate(
    year = year + 1,
    ccode_ksg = ifelse(ccode_ksg == 345 & year == 2006, 340, ccode_ksg)
  )

confl_ep_years <- confl_ep_years |>
  left_join(
    df |>
      select(
        ccode_ksg,
        year,
        tj_yr_zero
      ),
    by = c(gwno_loc = "ccode_ksg", year = "year")
  ) |>
  left_join(
    to_merge,
    by = c(gwno_loc = "ccode_ksg", year = "year")
  )

confl_ep_years |>
  select(
    conflict_id,
    location,
    gwno_loc,
    year,
    tj_yr_zero,
    sum_tj_laws,
    sum_regu_trs_dom_sta,
    sum_regu_cce_dom_sta,
    sum_tran_trs_dom_dtj_ctj,
    sum_tran_trs_dom_ctj,
    sum_trs_int_sta,
    sum_trs_int_opp,
    sum_amnesty_dtj_ctj_sta_opp,
    sum_amnesty_pol,
    sum_tcs_all_created,
    sum_rep_created,
  ) |>
  # filter(year > 1970 & year < 2021) |>
  summary()

#############################
### conflict-matched measures
#############################

# load("~/Documents/GitHub/tjet-db/data/tjetdb.RData")
# source("adapted-from-tjet-db/AmnestyMeasureConfl.R")
# source("adapted-from-tjet-db/ReparationMeasuresConfl.R")
# source("adapted-from-tjet-db/TCMeasureConfl.R")
# source("adapted-from-tjet-db/TrialsMeasureConfl.R")

confl_ep_years <- AmnestyMeasure(
  confl_df = TRUE,
  cy = confl_ep_years,
  nexus_vars = "dcj",
  who_opts = c("sta", "opp")
)
confl_ep_years <- AmnestyMeasure(
  confl_df = TRUE,
  cy = confl_ep_years,
  nexus_vars = "pcj",
  who_opts = c("sta", "opp")
)
confl_ep_years <- AmnestyMeasure(
  confl_df = TRUE,
  cy = confl_ep_years,
  nexus_vars = "dcj",
  who_opts = "sta"
)
confl_ep_years <- AmnestyMeasure(
  confl_df = TRUE,
  cy = confl_ep_years,
  nexus_vars = "pcj",
  who_opts = "sta"
)
confl_ep_years <- AmnestyMeasure(
  confl_df = TRUE,
  cy = confl_ep_years,
  nexus_vars = "dcj",
  who_opts = "opp"
)
confl_ep_years <- AmnestyMeasure(
  confl_df = TRUE,
  cy = confl_ep_years,
  nexus_vars = "pcj",
  who_opts = "opp"
)
confl_ep_years <- AmnestyMeasure(
  confl_df = TRUE,
  cy = confl_ep_years,
  who_opts = "sta",
  what_opts = "hrv"
)
confl_ep_years <- AmnestyMeasure(
  confl_df = TRUE,
  cy = confl_ep_years,
  who_opts = "opp",
  what_opts = "hrv"
)
confl_ep_years <- AmnestyMeasure(
  confl_df = TRUE,
  cy = confl_ep_years,
  peace_vars = "peaceSettlement"
)

amnesty_vars <- c(
  "amnesty_ucdp_dcj_sta_opp",
  "amnesty_ucdp_pcj_sta_opp",
  "amnesty_ucdp_dcj_sta",
  "amnesty_ucdp_pcj_sta",
  "amnesty_ucdp_dcj_opp",
  "amnesty_ucdp_pcj_opp",
  "amnesty_ucdp_sta_hrv",
  "amnesty_ucdp_opp_hrv",
  "amnesty_ucdp_peaceagree"
)

confl_ep_years <- confl_ep_years |>
  arrange(conflict_id, year) |>
  group_by(
    conflict_id,
    temp = year >= 1970
  ) |>
  mutate(
    across(
      .cols = all_of(amnesty_vars),
      .fns = ~ lag(cumsum(.x)),
      .names = "sum_{.col}"
    )
  ) |>
  ungroup() |>
  select(-temp)

# confl_ep_years |>
#   select(
#     conflict_id,
#     location,
#     year,
#     contains("amnesty_")
#   ) |>
#   summary()

confl_ep_years <- ReparationMeasures(
  confl_df = TRUE,
  cy = confl_ep_years
) |>
  select(
    -rep_ucdp_created,
    -rep_ucdp_symbolic_created,
    -rep_ucdp_compensation_created,
    -rep_ucdp_services_created,
    -rep_ucdp_diffamount,
    -rep_ucdp_outreach,
    -rep_ucdp_alteration,
    -rep_ucdp_foreclose,
    -rep_ucdp_accessibility,
    -rep_ucdp_victim_centered,
    -rep_ucdp_harms,
    -rep_ucdp_binary,
    -rep_ucdp_compensation,
    -rep_ucdp_symbolic,
    -rep_ucdp_services,
    -rep_ucdp_victim_centered_beg,
    -rep_ucdp_harms_beg,
    -rep_ucdp_peaceagree_created,
    -rep_ucdp_individual_created,
    -rep_ucdp_collective_created,
    -rep_ucdp_paidout_created,
    -rep_ucdp_scope_beg
  )

# confl_ep_years |>
#   select(
#     conflict_id,
#     location,
#     year,
#     contains("rep_ucdp_")
#   ) |>
#   # filter(year > 1969) |>
#   summary()

confl_ep_years <- TCmeasure(
  confl_df = TRUE,
  cy = confl_ep_years,
  new_col_name = "tcs_ucdp_ctj",
  start_year_var = "yearBeginOperation",
  filter_nexus_vars = "fitsConflictTJ",
  filter_crimes_vars = "all",
  independence_opts = NULL,
  aims_opts = NULL,
  consult_vars = NULL,
  powers_vars = NULL,
  testimony_vars = NULL,
  reports_vars = NULL,
  recommend_vars = NULL,
  monitor_vars = NULL
) |>
  select(
    -tcs_ucdp_ctj,
    -tcs_ucdp_ctj_n,
    -tcs_ucdp_ctj_beg,
    -tcs_ucdp_ctj_binary
  ) |>
  arrange(conflict_id, year) |>
  group_by(
    conflict_id,
    temp = year >= 1970
  ) |>
  mutate(sum_tcs_ucdp_ctj_created = lag(cumsum(tcs_ucdp_ctj_created))) |>
  ungroup() |>
  select(-temp)

confl_ep_years <- TCmeasure(
  confl_df = TRUE,
  cy = confl_ep_years,
  new_col_name = "tcs_ucdp_ctj_victim_process",
  start_year_var = "yearBeginOperation",
  filter_nexus_vars = "fitsConflictTJ",
  filter_crimes_vars = "all",
  aims_opts = c(
    "truth for victims",
    "memorialization",
    "apology",
    "recognition of victims",
    "reparation"
  ),
  independence_opts = NULL,
  consult_vars = "consultedVictims",
  powers_vars = "allocateReparations",
  testimony_vars = "encourageVictimTestimony",
  reports_vars = NULL,
  recommend_vars = NULL,
  monitor_vars = NULL
) %>%
  select(
    -tcs_ucdp_ctj_victim_process_n,
    -tcs_ucdp_ctj_victim_process_binary,
    -tcs_ucdp_ctj_victim_process_created,
    -tcs_ucdp_ctj_victim_process_beg
  )

confl_ep_years <- TCmeasure(
  confl_df = TRUE,
  cy = confl_ep_years,
  new_col_name = "tcs_ucdp_ctj_victim_outcome",
  start_year_var = "yearCompleteOperation",
  filter_nexus_vars = "fitsConflictTJ",
  filter_crimes_vars = "all",
  aims_opts = NULL,
  independence_opts = NULL,
  consult_vars = NULL,
  powers_vars = NULL,
  testimony_vars = NULL,
  reports_vars = "reportPubliclyAvailable",
  recommend_vars = "recommendReparations",
  monitor_vars = "mandatePeriodicMonitoringImplementation"
) %>%
  select(
    -tcs_ucdp_ctj_victim_outcome_n,
    -tcs_ucdp_ctj_victim_outcome_binary,
    -tcs_ucdp_ctj_victim_outcome_created,
    -tcs_ucdp_ctj_victim_outcome_beg
  )

confl_ep_years <- TCmeasure(
  confl_df = TRUE,
  cy = confl_ep_years,
  new_col_name = "tcs_ucdp_ctj_peace_process",
  start_year_var = "yearBeginOperation",
  filter_nexus_vars = "fitsConflictTJ",
  filter_crimes_vars = "all",
  aims_opts = c(
    "reconciliation",
    "coexistence",
    "dialogue",
    "non-recurrence"
  ),
  independence_opts = NULL,
  consult_vars = NULL,
  powers_vars = "grantAmnesty",
  testimony_vars = "heldPublicHearings",
  reports_vars = NULL,
  recommend_vars = NULL,
  monitor_vars = NULL
) %>%
  select(
    -tcs_ucdp_ctj_peace_process_n,
    -tcs_ucdp_ctj_peace_process_binary,
    -tcs_ucdp_ctj_peace_process_created,
    -tcs_ucdp_ctj_peace_process_beg
  )

confl_ep_years <- TCmeasure(
  confl_df = TRUE,
  cy = confl_ep_years,
  new_col_name = "tcs_ucdp_ctj_peace_outcome",
  start_year_var = "yearCompleteOperation",
  filter_nexus_vars = "fitsConflictTJ",
  filter_crimes_vars = "all",
  aims_opts = NULL,
  independence_opts = NULL,
  consult_vars = NULL,
  powers_vars = NULL,
  testimony_vars = NULL,
  reports_vars = "reportPubliclyAvailable",
  recommend_vars = NULL,
  monitor_vars = NULL
) %>%
  select(
    -tcs_ucdp_ctj_peace_outcome_n,
    -tcs_ucdp_ctj_peace_outcome_binary,
    -tcs_ucdp_ctj_peace_outcome_created,
    -tcs_ucdp_ctj_peace_outcome_beg
  )

# confl_ep_years |>
#   select(conflict_id, location, year, contains("tcs_ucdp_ctj")) |>
#   summary()

confl_ep_years <- TrialsMeasure(
  confl_df = TRUE,
  cy = confl_ep_years,
  prefix = "tran",
  measure = "trs",
  type_opts = "dom",
  nexus_vars = "dcj",
  memb_opts = "sta"
) |>
  arrange(conflict_id, year) |>
  group_by(
    conflict_id,
    temp = year >= 1970
  ) |>
  mutate(
    sum_tran_trs_dom_ucdp_dcj_sta = lag(cumsum(tran_trs_dom_ucdp_dcj_sta))
  ) |>
  ungroup() |>
  select(-temp)

confl_ep_years <- TrialsMeasure(
  confl_df = TRUE,
  cy = confl_ep_years,
  prefix = "tran",
  measure = "trs",
  type_opts = "dom",
  nexus_vars = "pcj",
  memb_opts = "sta"
) |>
  arrange(conflict_id, year) |>
  group_by(
    conflict_id,
    temp = year >= 1970
  ) |>
  mutate(
    sum_tran_trs_dom_ucdp_pcj_sta = lag(cumsum(tran_trs_dom_ucdp_pcj_sta))
  ) |>
  ungroup() |>
  select(-temp)

confl_ep_years <- TrialsMeasure(
  confl_df = TRUE,
  cy = confl_ep_years,
  prefix = "tran",
  measure = "cce",
  type_opts = "dom",
  nexus_vars = "dcj",
  memb_opts = "sta"
) |>
  arrange(conflict_id, year) |>
  group_by(
    conflict_id,
    temp = year >= 1970
  ) |>
  mutate(
    sum_tran_cce_dom_ucdp_dcj_sta = lag(cumsum(tran_cce_dom_ucdp_dcj_sta))
  ) |>
  ungroup() |>
  select(-temp)

confl_ep_years <- TrialsMeasure(
  confl_df = TRUE,
  cy = confl_ep_years,
  prefix = "tran",
  measure = "cce",
  type_opts = "dom",
  nexus_vars = "pcj",
  memb_opts = "sta"
) |>
  arrange(conflict_id, year) |>
  group_by(
    conflict_id,
    temp = year >= 1970
  ) |>
  mutate(
    sum_tran_cce_dom_ucdp_pcj_sta = lag(cumsum(tran_cce_dom_ucdp_pcj_sta))
  ) |>
  ungroup() |>
  select(-temp)

confl_ep_years <- TrialsMeasure(
  confl_df = TRUE,
  cy = confl_ep_years,
  prefix = "tran",
  measure = "trs",
  type_opts = "dom",
  nexus_vars = "dcj",
  memb_opts = "opp"
) |>
  arrange(conflict_id, year) |>
  group_by(
    conflict_id,
    temp = year >= 1970
  ) |>
  mutate(
    sum_tran_trs_dom_ucdp_dcj_opp = lag(cumsum(tran_trs_dom_ucdp_dcj_opp))
  ) |>
  ungroup() |>
  select(-temp)

confl_ep_years <- TrialsMeasure(
  confl_df = TRUE,
  cy = confl_ep_years,
  prefix = "tran",
  measure = "trs",
  type_opts = "dom",
  nexus_vars = "pcj",
  memb_opts = "opp"
) |>
  arrange(conflict_id, year) |>
  group_by(
    conflict_id,
    temp = year >= 1970
  ) |>
  mutate(
    sum_tran_trs_dom_ucdp_pcj_opp = lag(cumsum(tran_trs_dom_ucdp_pcj_opp))
  ) |>
  ungroup() |>
  select(-temp)

confl_ep_years <- TrialsMeasure(
  confl_df = TRUE,
  cy = confl_ep_years,
  prefix = "tran",
  measure = "cce",
  type_opts = "dom",
  nexus_vars = "dcj",
  memb_opts = "opp"
) |>
  arrange(conflict_id, year) |>
  group_by(
    conflict_id,
    temp = year >= 1970
  ) |>
  mutate(
    sum_tran_cce_dom_ucdp_dcj_opp = lag(cumsum(tran_cce_dom_ucdp_dcj_opp))
  ) |>
  ungroup() |>
  select(-temp)

confl_ep_years <- TrialsMeasure(
  confl_df = TRUE,
  cy = confl_ep_years,
  prefix = "tran",
  measure = "cce",
  type_opts = "dom",
  nexus_vars = "pcj",
  memb_opts = "opp"
) |>
  arrange(conflict_id, year) |>
  group_by(
    conflict_id,
    temp = year >= 1970
  ) |>
  mutate(
    sum_tran_cce_dom_ucdp_pcj_opp = lag(cumsum(tran_cce_dom_ucdp_pcj_opp))
  ) |>
  ungroup() |>
  select(-temp)

##########
### saving
##########

confl_ep_years |>
  write_csv(
    here::here("tjet_datasets", "tjet_conflict_peace_spells.csv"),
    na = ""
  ) |>
  write_csv(
    here::here(dropbox_path, "tjet_conflict_peace_spells.csv"),
    na = ""
  )
