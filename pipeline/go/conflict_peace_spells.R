library(tidyverse)

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
    side_a_id,
    side_b_id,
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
    n_dyads_active = n()
  ) |>
  mutate(territory_name = str_trim(territory_name))

dyads_cumu <- read_csv(here::here(
  "conflicts",
  "original_data",
  "Dyadic_v25_1.csv"
)) |>
  filter(type_of_conflict > 2) |>
  select(
    dyad_id,
    conflict_id,
    year,
  ) |>
  arrange(conflict_id, year, dyad_id) |>
  reframe(
    .by = c(conflict_id, year),
    dyads_yr = list(unique(dyad_id))
  ) |>
  mutate(
    .by = conflict_id,
    dyads_cumu = accumulate(
      dyads_yr,
      \(acc, x) unique(c(acc, x)),
      .init = numeric(0) # Start with empty numeric vector
    )[-1] # Remove the .init element
  ) |>
  rowwise() |>
  mutate(dyads_cumu = length(dyads_cumu)) |>
  select(conflict_id, year, dyads_cumu)

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

#######
### OSV
#######

osv <- here::here(
  "conflicts",
  "original_data",
  "OneSided_v25_1.csv"
) |>
  read_csv() |>
  select(
    # location,
    gwno_location,
    # conflict_id,
    # dyad_id,
    actor_id,
    actor_name_fulltext,
    year,
    best_fatality_estimate,
    # is_government_actor
  ) |>
  mutate(gwno_location = str_split(gwno_location, ", ")) |>
  unnest(gwno_location) |>
  mutate(gwno_location = as.integer(gwno_location))

#######
### EPR
#######

epr <- here::here(
  "conflicts",
  "original_data",
  "ACD2EPR-2021.csv"
) |>
  read_csv(show_col_types = FALSE) |>
  left_join(
    here::here(
      "conflicts",
      "original_data",
      "Dyadic_v25_1.csv"
    ) |>
      read_csv() |>
      select(dyad_id, conflict_id) |>
      distinct(),
    by = c("dyadid" = "dyad_id")
  ) |>
  mutate(
    claim = case_when(
      claim == 0 ~ "no ethnic claim", # No claim
      claim == 1 ~ "direct claim", # Direct evidence for claim
      claim == 2 ~ "indirect claim", # Indirect evidence, e.g., group name, for claim
      claim == -1 ~ "unknown"
    ), # No information
    # claim: Describes whether a rebel group has made an exclusive claim to fight on behalf of an ethnic group
    recruitment = case_when(
      recruitment == 0 ~ "no ethnic recruitment", # No recruitment
      recruitment == 1 ~ "ethnic recruitment by rebels", # Recruitment
      recruitment == 2 ~ "ethnic recruitment by both", # Ethnic group members are recruited by the rebels and the government
      recruitment == -1 ~ "unknown"
    ), # No information
    # recruitment: Describes whether a rebel group is recruiting from an ethnic group
    support = case_when(
      support == 0 ~ "no majority support", # No or little support
      support == 1 ~ "majority support", # Large support
      support == 2 ~ "group supports both (non-ethnic conflict)", # One ethnic group supports both the rebel group and the government (only in non-ethnic conflicts)
      support == -1 ~ "unknown"
    ), # No information
    # Support: Describes whether a rebel group is supported by at least 50% of the members of an ethnic group
    reb_claim = ifelse(claim %in% c("direct claim", "indirect claim"), 1, 0),
    reb_recruit = ifelse(
      recruitment %in%
        c("ethnic recruitment by rebels", "ethnic recruitment by both"),
      1,
      0
    ),
    gov_recruit = ifelse(recruitment == "ethnic recruitment by both", 1, 0)
  ) |>
  arrange(gwid, conflict_id, dyadid) |>
  reframe(
    .by = c(gwid, statename, conflict_id, dyadid),
    groups_epr = str_flatten_comma(unique(group), na.rm = FALSE),
    grps_in_dyad_epr = n(),
    reb_claim_epr = sum(reb_claim, na.rm = TRUE),
    reb_recruit_epr = sum(reb_recruit, na.rm = TRUE),
    gov_recruit_epr = sum(gov_recruit, na.rm = TRUE)
  ) |>
  reframe(
    .by = c(gwid, conflict_id),
    groups_epr = str_flatten_comma(unique(groups_epr), na.rm = FALSE),
    dyads_epr = n(),
    grps_in_dyads_epr = sum(grps_in_dyad_epr),
    reb_claim_epr = sum(reb_claim_epr, na.rm = TRUE),
    reb_recruit_epr = sum(reb_recruit_epr, na.rm = TRUE),
    gov_recruit_epr = sum(gov_recruit_epr, na.rm = TRUE)
  )

##########
### issues
##########

issues <- here::here(
  "conflicts",
  "original_data",
  "ucdp_issues_dataset_dyadyear_232.csv"
) |>
  read_delim(show_col_types = FALSE) |>
  mutate(
    ethnic = ifelse(
      !is.na(ethnicity_1) |
        !is.na(ethnicity_2) |
        !is.na(ethnicity_3) |
        !is.na(ethnicity_4),
      1,
      0
    ),
    sideb_regional = case_when(
      geography_1 == 2000 |
        geography_2 == 2000 |
        geography_3 == 2000 |
        geography_4 == 2000 ~ 1,
      is.na(geography_1) |
        is.na(geography_2) |
        is.na(geography_3) |
        is.na(geography_4) ~ 0
    ),
    sideb_national = case_when(
      geography_1 == 3000 |
        geography_2 == 3000 |
        geography_3 == 3000 |
        geography_4 == 3000 ~ 1,
      is.na(geography_1) |
        is.na(geography_2) |
        is.na(geography_3) |
        is.na(geography_4) ~ 0
    ),
    sideb_subnational = case_when(
      geography_1 == 4000 |
        geography_2 == 4000 |
        geography_3 == 4000 |
        geography_4 == 4000 ~ 1,
      is.na(geography_1) |
        is.na(geography_2) |
        is.na(geography_3) |
        is.na(geography_4) ~ 0
    ),
    side_b = str_trim(side_b)
  ) |>
  select(
    dyad_id,
    conflict_id,
    year,
    side_b,
    ethnic,
    # "7201", "ethnic collective targeting"
    sideb_regional,
    sideb_national,
    sideb_subnational,
    "10101",
    "10201",
    "10202",
    "10203",
    "10204"
  ) |>
  rename(
    sideb_trp = "10101", # "Call for truth and reconciliation processes (10101)"
    sideb_accountabiliy = "10201", # "Accountability/prosecution/investigation (10201)"
    sideb_amnesty = "10202", # "Amnesties (10202)"
    sideb_recognition = "10203", # "Recognition of wrongdoing (10203)"
    sideb_restoration = "10204" # "Compensation/restoration (10204)"
  ) |>
  mutate(
    sideb_accountabiliy = ifelse(
      sideb_accountabiliy > 1,
      1,
      sideb_accountabiliy
    ),
    dyad_id = ifelse(dyad_id == 12088, 875, dyad_id),
    conflict_id = ifelse(conflict_id == 13349, 222, conflict_id)
  ) |>
  left_join(
    here::here(
      "conflicts",
      "original_data",
      "Dyadic_v25_1.csv"
    ) |>
      read_csv() |>
      reframe(
        .by = c(conflict_id, dyad_id),
        year_min = min(year)
      ),
    by = c("conflict_id", "dyad_id")
  ) |>
  mutate(
    # ccode_ksg = as.integer(gwno_loc),
    year = as.integer(ifelse(year == 1000, year_min, year))
  ) |>
  select(
    conflict_id,
    dyad_id,
    year,
    ethnic,
    # sideb_regional,
    # sideb_national,
    # sideb_subnational,
    # sideb_trp,
    # sideb_accountabiliy,
    # sideb_amnesty,
    # sideb_recognition,
    # sideb_restoration
  ) |>
  reframe(
    .by = c(conflict_id, year),
    ethnic_avg = mean(ethnic, na.rm = TRUE)
  )

#########################
### UCDP peace agreements
#########################

intgrs <- c(
  "gwno",
  "paid",
  "conflict_id",
  "year",
  "cease",
  "amn",
  "recon",
  "no_dyad"
)

pas <- here::here(
  "conflicts",
  "original_data",
  "ucdp-peace-agreements-221.xlsx"
) |>
  readxl::read_excel() |>
  filter(!str_detect(gwno, ", ")) |>
  mutate(conflict_id = str_split(conflict_id, ", ")) |>
  unnest(conflict_id) |>
  mutate(
    across(all_of(intgrs), ~ as.integer(.x))
  ) |>
  mutate(
    inclusive = case_when(
      .default = inclusive,
      inclusive == -1 ~ "comprehensive",
      inclusive == 1 ~ "comprehensive",
      inclusive == 2 ~ "dyadic",
    ) |>
      as.factor(),
    pa_type = case_when(
      pa_type == 1 ~ "full",
      pa_type == 2 ~ "partial",
      pa_type == 3 ~ "process",
    ) |>
      as.factor(),
    ended = if_else(ended == "True", 1, 0),
    duration = as_date(duration)
  ) |>
  rename(
    gwno_loc = gwno,
    ucpd_pa_id = paid,
    pa_dyad_id = dyad_id,
    pa_ended = ended,
    pa_duration = duration,
    pa_cease = cease,
    pa_amn = amn,
    pa_recon = recon,
    pa_inclusive = inclusive,
    pa_dyads = no_dyad
  ) |>
  select(
    gwno_loc,
    ucpd_pa_id,
    conflict_id,
    pa_dyad_id,
    year,
    # pa_name,
    pa_type,
    # pa_ended,
    # pa_duration,
    # pa_cease,
    # pa_amn,
    # pa_recon,
    pa_inclusive,
    pa_dyads,
  ) |>
  arrange(gwno_loc, conflict_id, year) |>
  reframe(
    .by = c(gwno_loc, conflict_id, year),
    pa_n = n(),
    ucpd_pa_id = str_flatten_comma(unique(ucpd_pa_id)),
    pa_dyad_id = str_flatten_comma(unique(pa_dyad_id)),
    pa_type = str_flatten_comma(pa_type),
    pa_inclusive = str_flatten_comma(pa_inclusive)
  ) |>
  mutate(
    pa_dyad_id = str_split(pa_dyad_id, ", ")
  ) |>
  rowwise() |>
  mutate(
    pa_dyad_id = list(sort(unique(pa_dyad_id))),
    pa_dyads = length(pa_dyad_id),
    pa_dyad_id = str_flatten_comma(pa_dyad_id)
  ) |>
  ungroup() |>
  select(
    gwno_loc,
    conflict_id,
    year,
    pa_n,
    pa_dyads,
    pa_dyad_id,
    # pa_type
  ) |>
  print()

#######
### PAM
#######

# pam <- here::here("conflicts", "original_data", "PAM_ID_2.0.xlsx") |>
#   readxl::read_xlsx() |>
#   mutate(
#     .by = pam_caseid,
#     beg = min(year),
#     end = max(year)
#   ) |>
#   mutate(
#     war_start = as_date(war_start),
#     cease_date = as_date(cease_date)
#   ) |>
#   select(
#     pam_caseid,
#     country,
#     cowcode,
#     year,
#     year_count,
#     accordname,
#     war_start,
#     cease_date,
#     beg,
#     end,
#     amnest_prov,
#     humrts_prov,
#     prisr_prov,
#     repar_prov,
#     truth_prov
#   ) |>
#   distinct() |>
#   arrange(country, cease_date)

#######
### PAX
#######

tj_vars <- c(
  "TjGen",
  "TjAm",
  "TjAmPro",
  "TjSan",
  "TjPower",
  "TjCou",
  "TjJaNc",
  "TjJaIc",
  "TjMech",
  "TjPrire",
  "TjVet",
  "TjVic",
  "TjMis",
  "TjRep",
  "TjRSym",
  "TjRMa",
  "TjNR"
)

pax <- here::here(
  "conflicts",
  "original_data",
  "pax_data_2144_agreements_v9_10.csv"
) |>
  read_csv() |>
  rename(
    entity = Con,
    process_id = PP,
    process_name = PPName,
    date_sign = Dat,
    agreement_id = AgtId,
    agreement_name = Agt,
    confl_type = Agtp,
    gwno_loc = Loc1GWNO,
    conflict_id = UcdpCon,
    ucpd_pa_id = UcdpAgr,
    pam_id = PamAgr
  ) |>
  arrange(entity, process_name, date_sign) |>
  filter(confl_type %in% c("Intra")) |> # type of conflicts
  filter(Stage %in% c("SubComp", "SubPar")) |> # stage of the process
  mutate(
    gwno_loc = as.integer(gwno_loc),
    conflict_id = as.integer(conflict_id),
    ucpd_pa_id = if_else(
      ucpd_pa_id == "N/A" | ucpd_pa_id == "na",
      NA,
      ucpd_pa_id
    ),
    ucpd_pa_id = as.integer(ucpd_pa_id),
    pam_id = if_else(pam_id == "N/A", NA, pam_id),
    pam_id = as.integer(pam_id),
    stage = case_when(
      Stage == "SubComp" ~ "comprehensive",
      Stage == "SubPar" ~ "partial",
      Stage == "Cea" ~ "ceasefire"
    ),
    year = year(date_sign)
  ) |>
  select(
    gwno_loc,
    conflict_id,
    year,
    process_id,
    stage,
    ucpd_pa_id,
    # all_of(tj_vars)
  ) |>
  filter(!is.na(conflict_id)) |>
  reframe(
    .by = c(gwno_loc, conflict_id, year),
    pax_n = n(),
    # stage = str_flatten_comma(stage)
  ) |>
  print()

agreements <- full_join(
  pas,
  pax,
  by = c("gwno_loc", "conflict_id", "year")
) |>
  arrange(gwno_loc, conflict_id, year) |>
  mutate(
    peace_agree = if_else(
      pa_n > 0 | pax_n > 0,
      1,
      0
    )
  ) |>
  print(n = 30)

#########
### NSAEX
#########

ids <- here::here("conflicts", "original_data", "translate_conf.csv") |>
  read_csv() |>
  rename(
    conflict_id = new_id
  ) |>
  filter(!str_detect(old_id, "-")) |>
  filter(!str_detect(old_id, "XXX")) |>
  mutate(ucdpid = as.integer(old_id)) |>
  filter(!is.na(ucdpid)) |>
  select(conflict_id, ucdpid)

nsaex <- here::here(
  "conflicts",
  "original_data",
  "nsa_v3.4_21November2013.asc"
) |>
  read_delim() |>
  arrange(ucdpid, dyadid) |>
  left_join(ids, by = "ucdpid") |>
  mutate(
    beg = as.integer(str_sub(startdate, 1, 4)),
    end = as.integer(str_sub(enddate, 1, 4))
  ) |>
  rowwise() |>
  mutate(year = list(beg:end)) |>
  ungroup() |>
  unnest(year) |>
  select(
    conflict_id,
    dyadid,
    year,
    side_a,
    side_b,
    rebestimate,
    rebstrength
  ) |>
  rename(
    side_a_nsa = side_a,
    side_b_nsa = side_b,
    rebestimate_nsa = rebestimate,
    rebstrength_nsa = rebstrength
  ) |>
  arrange(conflict_id, year) |>
  reframe(
    .by = c(conflict_id, year),
    side_a_nsa = str_flatten_comma(unique(side_a_nsa)),
    side_b_nsa = str_flatten_comma(unique(side_b_nsa)),
    rebestimate_nsa = sum(rebestimate_nsa, na.rm = TRUE),
    rebstrength_nsa = str_flatten_comma(unique(rebstrength_nsa)),
  ) |>
  mutate(rebestimate_nsa = if_else(rebestimate_nsa == 0, NA, rebestimate_nsa))

########
### PNCC
########

# here::here("conflicts", "original_data", "pncc_dy_16042022.csv") |>
#   read_csv()

#######################
### pulling it together
#######################

ctry_confl_yrs <- acd |>
  select(
    gwno_loc,
    year
  ) |>
  mutate(
    gwno_loc = as.integer(gwno_loc),
    gwno_loc = ifelse(gwno_loc == 751, 750, gwno_loc),
    ctry_confl_yrs = 1
  ) |>
  distinct() |>
  arrange(gwno_loc, year) |>
  group_by(gwno_loc) |>
  mutate(
    ctry_confl_yrs = cumsum(ctry_confl_yrs)
  ) |>
  ungroup()

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
    dyads_cumu,
    by = c(
      "conflict_id",
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
    # pc_yrs = ep_next_beg - ep_end - 1,
    pc_yrs = case_when(
      !is.na(ep_next_beg) ~ ep_next_beg - ep_end - 1,
      is.na(ep_next_beg) ~ 2025 - ep_end - 1
    ),
    bd_cumu_confl = cumsum(bd_best),
  ) |>
  group_by(conflict_id, ep_beg) |>
  mutate(
    n_dyads_ep = max(n_dyads_active),
    bd_cumu_ep = cumsum(bd_best),
    bd_cumu_ep_end = max(bd_cumu_ep)
  ) |>
  ungroup() |>
  group_by(conflict_id) |>
  mutate(
    bd_before_ep = ifelse(c_epno > 1 & year == ep_beg, lag(bd_cumu_confl), NA)
  ) |>
  ungroup() |>
  arrange(conflict_id, ep_beg) |>
  group_by(conflict_id, ep_beg) |>
  fill(ep_end, .direction = "up") |>
  fill(
    ep_prev_end,
    bd_before_ep,
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
      bd_before_ep >= 1000 ~ 1000,
      bd_before_ep >= 500 ~ 500,
      bd_before_ep >= 100 ~ 100,
      bd_before_ep >= 25 ~ 25
    ),
    confl_to_date_threshold = case_when(
      bd_cumu_confl >= 1000 ~ 1000,
      bd_cumu_confl >= 500 ~ 500,
      bd_cumu_confl >= 100 ~ 100,
      bd_cumu_confl >= 25 ~ 25
    )
  ) |>
  ungroup() |>
  mutate(territorial = if_else(!is.na(territory_name), 1, 0)) |>
  mutate(
    .by = conflict_id,
    confl_yrs = 1,
    confl_yrs = cumsum(confl_yrs)
  ) |>
  full_join(
    ctry_confl_yrs,
    by = c("gwno_loc", "year"),
  ) |>
  left_join(
    epr,
    by = c("gwno_loc" = "gwid", "conflict_id")
  ) |>
  left_join(
    nsaex |>
      select(
        conflict_id,
        year,
        rebestimate_nsa,
        rebstrength_nsa
      ),
    by = c("conflict_id", "year")
  )

################################################################
### expand to conflict episode spells incl. post-conflict period
################################################################

confl_yrs <- here::here("data", "countries.csv") |>
  read_csv() |>
  select(name_short, ccode_ksg, beg_ksg, end_ksg) |>
  arrange(ccode_ksg, beg_ksg, end_ksg) |>
  filter(!is.na(ccode_ksg)) |>
  reframe(
    .by = ccode_ksg,
    beg_ksg = min(beg_ksg, na.rm = TRUE),
    end_ksg = max(end_ksg, na.rm = TRUE)
  ) |>
  mutate(
    beg = as.integer(str_sub(beg_ksg, 1, 4)),
    beg = if_else(beg < 1946, 1946, beg),
    end = as.integer(str_sub(end_ksg, 1, 4)),
    end = if_else(end == 2020, 2024, end)
  ) |>
  select(ccode_ksg, beg, end) |>
  right_join(
    confl_ep_years |>
      select(gwno_loc, conflict_id, confl_beg) |>
      distinct(),
    by = c("ccode_ksg" = "gwno_loc")
  ) |>
  mutate(
    beg = if_else(beg < confl_beg, confl_beg, beg)
  ) |>
  mutate(
    .by = conflict_id,
    year = list(beg:end)
  ) |>
  unnest(year) |>
  select(conflict_id, year)

fctrs <- c(
  "location",
  "side_b",
  "territory_name",
  "outcome",
  "groups_epr",
  "rebstrength_nsa"
)

confl_ep_years <- confl_yrs |>
  left_join(
    confl_ep_years,
    by = c("conflict_id", "year")
  ) |>
  arrange(conflict_id, year) |>
  mutate(
    intensity_level = if_else(is.na(intensity_level), 0, intensity_level),
    sample = case_when(
      c_epterm == 1 ~ "switch year",
      c_epterm == 0 & intensity_level > 0 ~ "conflict",
      is.na(c_epterm) & intensity_level > 0 ~ "right-censored conflict",
      intensity_level == 0 ~ "post-conflict"
    ) |>
      as.factor()
  ) |>
  mutate(
    across(all_of(fctrs), ~ as.factor(.x)),
  ) |>
  fill(
    .by = conflict_id,
    .direction = "down",
    c_epno,
    c_epid,
    bd_beg,
    confl_beg,
    confl_last,
    confl_yrs,
    ctry_confl_yrs,
    rebestimate_nsa,
    rebstrength_nsa,
  ) |>
  fill(
    .by = c(conflict_id, c_epno),
    .direction = "down",
    gwno_loc,
    location,
    # c_epterm,
    side_b,
    side_a_id,
    side_b_id,
    incompatibility,
    territory_name,
    territorial,
    cumulative_intensity,
    start_date,
    start_date2,
    ep_end_date,
    ep_beg,
    ep_end,
    n_dyads_active,
    dyads_cumu,
    n_dyads_ep,
    ep_prev_end,
    outcome,
    bd_cumu_confl,
    bd_cumu_ep,
    bd_cumu_ep_end,
    bd_before_ep,
    ep_threshold,
    confl_before_threshold,
    confl_to_date_threshold,
    groups_epr,
    dyads_epr,
    grps_in_dyads_epr,
    reb_claim_epr,
    reb_recruit_epr,
    gov_recruit_epr,
  ) |>
  fill(
    .by = c(conflict_id, c_epno),
    .direction = "downup",
    ep_end_date,
    # c_ep_startyear,
    # c_ep_endyear,
    # c_ependdate,
    recur_later,
    ep_next_beg
  ) |>
  mutate(
    rebestimate_nsa = if_else(year > 2011, NA, rebestimate_nsa),
    rebstrength_nsa = if_else(year > 2011, NA, rebstrength_nsa),
    pc_dur = if_else(intensity_level == 0, year - ep_end, NA)
  ) |>
  mutate(
    gwno_loc = ifelse(
      str_detect(location, fixed("Serbia (Yugoslavia)")) & year > 2005,
      340,
      gwno_loc
    )
  )

confl_ep_years |>
  filter(.by = c(conflict_id, year), n() > 1)

confl_ep_years <- confl_ep_years |>
  left_join(issues, by = c("conflict_id", "year")) |> ### full_join shows missing spell years, need to check this above
  arrange(gwno_loc, conflict_id, year) |>
  fill(
    .by = c(gwno_loc, conflict_id),
    ethnic_avg,
    .direction = "down"
  ) |>
  mutate(
    ethnic_avg = if_else(year > 2017, NA, ethnic_avg),
    ethnic_avg = if_else(is.na(ethnic_avg) & year %in% 1989:2017, 0, ethnic_avg)
  ) |>
  left_join(
    agreements,
    by = c("gwno_loc", "conflict_id", "year")
  ) |>
  arrange(gwno_loc, conflict_id, year) |>
  fill(
    .by = c(gwno_loc, conflict_id),
    peace_agree
  ) |>
  mutate(peace_agree = if_else(is.na(peace_agree), 0, peace_agree))

atrocities <- confl_ep_years |>
  select(
    gwno_loc,
    conflict_id,
    year,
    side_a_id,
    side_b_id
  ) |>
  filter(year >= 1989) |>
  mutate(
    side_b_id = str_split(side_b_id, ", ")
  ) |>
  unnest(side_b_id) |>
  mutate(
    side_a_id = as.integer(side_a_id),
    side_b_id = as.integer(side_b_id)
  ) |>
  left_join(
    osv |>
      select(-actor_name_fulltext),
    by = c(
      "gwno_loc" = "gwno_location",
      "side_b_id" = "actor_id",
      "year"
    )
  ) |>
  rename(osv_side_b = best_fatality_estimate) |>
  reframe(
    .by = c(gwno_loc, conflict_id, year, side_a_id),
    osv_side_b = sum(osv_side_b)
  ) |>
  left_join(
    osv |>
      select(-actor_name_fulltext),
    by = c(
      "gwno_loc" = "gwno_location",
      "side_a_id" = "actor_id",
      "year"
    )
  ) |>
  rename(osv_side_a = best_fatality_estimate) |>
  # filter(.by = c("conflict_id", "year"), n() > 1)
  select(-side_a_id) |>
  mutate(
    osv_side_a = if_else(
      is.na(osv_side_a) & year >= 1989,
      0,
      osv_side_a
    ),
    osv_side_b = if_else(
      is.na(osv_side_b) & year >= 1989,
      0,
      osv_side_b
    )
  )

confl_ep_years <- confl_ep_years |>
  full_join(
    atrocities,
    by = c("gwno_loc", "conflict_id", "year")
  )

###################
### example country
###################

# confl_ep_years |>
#   filter(gwno_loc == 540 & year < 2006) |>
#   select(
#     conflict_id,
#     sample,
#     year,
#     location,
#     # gwno_loc,
#     ep_beg,
#     ep_end,
#     # bd_beg,
#     # confl_beg,
#     # confl_last,
#     c_epno,
#     c_epterm,
#     # c_ep_durcount,
#     outcome,
#     # recur_later,
#     # ep_next_beg,
#     # pc_yrs,
#     # bd_best,
#     # bd_before_ep,
#     # bd_cumu_confl,
#     # bd_cumu_ep,
#     # bd_cumu_ep_end,
#     contains("dyad"),
#     # contains("bd_"),
#     contains("_threshold")
#   ) |>
#   print(n = Inf)

#########################################################
### merging in existing TJET measures (not by conflictID)
#########################################################

df <- read_csv(
  here::here("tjet_datasets/tjet_cy_analyses.csv")
)

# df |>
#   select(
#     country_case,
#     year,
#     # starts_with("aco_"),
#     # starts_with("dco_"),
#     # starts_with("pco_"),
#     # starts_with("confl_"),
#     starts_with("amnesty_"),
#   ) |>
#   # select(!ends_with("_cflag")) |>
#   # filter(str_detect(country_case, "Angola")) |>
#   names()

to_merge <- df |>
  rename(bd_ctry = bd_best) |>
  mutate(
    subintregion = case_when(
      !is.na(intregion) ~ intregion,
      is.na(intregion) ~ subregion
    )
  ) |>
  select(
    country_case,
    ccode_ksg,
    year,
    region,
    subregion,
    subintregion,
    ICC_referral,
    ICC_prelim,
    ICC_prelim_region,
    ICC_investigation,
    ICC_investigation_region,
    ICC_proceedings,
    ICC_arrest_warrant,
    ICC_arrest_warrant_region,
    ICC_proceedings_n,
    ICC_proceedings_n_region,
    icc_sp,
    icc_sp_region,
    dtr,
    pop_wdi,
    gdp_const_wdi,
    gdppc_const_wdi,
    latent_pop_wdi_mean,
    latent_pop_wdi_mean_log,
    latent_gdp_wdi_mean,
    latent_gdp_wdi_mean_log,
    latent_gdppc_wdi_mean,
    latent_gdppc_wdi_mean_log,
    income_level_latent_gdppc_wdi,
    income_level_gdppc_wdi,
    v2x_liberal,
    v2x_polyarchy,
    reg_type_vdem,
    reg_trans_vdem,
    v2x_regime_cat,
    transition,
    dem_reversion,
    v2juncind,
    v2juhcind,
    v2jucomp,
    v2juhccomp,
    legacy_mean,
    pko_mission,
    bd_ctry,
    deaths_state_osv,
    deaths_nonstate_osv,
    conflicts_osv,
    deaths_all_osv,
    deaths_civilians_osv,
    milper_nmc,
    cinc_nmc,
    forces_pers_total_wdi,
    forces_pers_perclabor_wdi,
    military_exp_percgdp_wdi,
    military_exp_percexp_wdi,
    hom_rate_wdi,
    # tj_laws,
    # regu_trs_dom_sta,
    # regu_cce_dom_sta,
    # tran_trs_dom_dtj_ctj,
    # tran_trs_dom_ctj,
    # trs_int_sta,
    # trs_int_opp,
    # amnesty_dtj_ctj_sta_opp,
    # amnesty_pol,
    # tcs_all_created,
    # rep_created,
    amnesty_hrv,
    amnesty_sta_hrv,
    amnesty_opp_hrv,
    amnesty_dtj_ctj_sta_opp,
    amnesty_ctj_sta_opp,
    rep_paidout,
    tcs_dtj_ctj_binary,
    tcs_report_public,
    tran_cce_dom_dtj_ctj_sta_hi,
    tcs_reconciliation,
    tcs_ctj_reconciliation
  ) |>
  arrange(country_case, year) |>
  #   group_by(country_case) |>
  #   mutate(
  #     across(
  #       .cols = !c(ccode_ksg, year),
  #       .fns = ~ cumsum(.x),
  #       .names = "sum_{.col}"
  #     )
  #   ) |>
  #   ungroup() |>
  #   select(ccode_ksg, year, starts_with("sum_")) |>
  mutate(
    # year = year + 1, # for lagging
    ccode_ksg = ifelse(ccode_ksg == 345 & year == 2006, 340, ccode_ksg)
  )

confl_ep_years <- confl_ep_years |>
  filter(year >= 1970) |>
  left_join(
    to_merge,
    by = c(gwno_loc = "ccode_ksg", year = "year")
  ) |>
  left_join(
    df |>
      select(
        ccode_ksg,
        year,
        tj_yr_zero,
        yr_cce_sta_hi,
        yr_tcs,
        yr_rep,
        yr_vet
      ),
    by = c(gwno_loc = "ccode_ksg", year = "year")
  )

# confl_ep_years |>
#   select(
#     conflict_id,
#     location,
#     gwno_loc,
#     year,
#     tj_yr_zero,
#     sum_tj_laws,
#     sum_regu_trs_dom_sta,
#     sum_regu_cce_dom_sta,
#     sum_tran_trs_dom_dtj_ctj,
#     sum_tran_trs_dom_ctj,
#     sum_trs_int_sta,
#     sum_trs_int_opp,
#     sum_amnesty_dtj_ctj_sta_opp,
#     sum_amnesty_pol,
#     sum_tcs_all_created,
#     sum_rep_created,
#   ) |>
#   # filter(year > 1970 & year < 2021) |>
#   summary()

#############################
### conflict-matched measures
#############################

source("pipeline/fx/AmnestyMeasure.R")
source("pipeline/fx/ReparationMeasures.R")
source("pipeline/fx/TCMeasure.R")
source("pipeline/fx/TrialsMeasure.R")

confl_ep_years <- AmnestyMeasure(
  confl_df = TRUE,
  cy = confl_ep_years
)
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
  "amnesty_ucdp",
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
