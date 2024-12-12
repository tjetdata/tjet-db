### trials

### tested with confirmation that all_trs_dom_ctj_dtj_dcj_pcj_all == trials_domestic == all_trs_dom
# df <- TrialsMeasure(cy = df, prefix = "all", measure = "trs", type_opts = "dom", nexus_vars = c("hrs", "con", "ctj", "dtj", "dcj", "pcj"), memb_opts = "all") %>%
#   rename(all_trs_dom = all_trs_dom_ctj_dtj_dcj_pcj)

#### transitional prosecutions (prefix: tran)

## Transitional human rights prosecutions: tran_trs_dom_dtj
df <- TrialsMeasure(cy = df, prefix = "tran", measure = "trs", type_opts = "dom", nexus_vars = "dtj", memb_opts = "all")

## Intrastate conflict prosecutions: tran_trs_dom_ctj
df <- TrialsMeasure(cy = df, prefix = "tran", measure = "trs", type_opts = "dom", nexus_vars = "ctj", memb_opts = "all")

## transitional prosecutions
df <- TrialsMeasure(cy = df, prefix = "tran", measure = "trs", type_opts = "dom", nexus_vars = c("dtj", "ctj"), memb_opts = "all")

## dtj trials of state agents: _dtj_sta
df <- TrialsMeasure(cy = df, prefix = "tran", measure = "trs", type_opts = "dom", nexus_vars = "dtj", memb_opts = "sta") %>%
  mutate(
    tran_trs_dom_dtj_sta_binary = ifelse(tran_trs_dom_dtj_sta > 0, 1, 0),
    tran_trs_dom_dtj_sta_scale = case_when(
      tran_trs_dom_dtj_sta == 0 ~ 0,
      tran_trs_dom_dtj_sta %in% 1:2 ~ 1,
      tran_trs_dom_dtj_sta > 2 ~ 2
    )
  )
df <- TrialsMeasure(cy = df, prefix = "tran", measure = "tro", type_opts = "dom", nexus_vars = "dtj", memb_opts = "sta")
df <- TrialsMeasure(cy = df, prefix = "tran", measure = "tfc", type_opts = "dom", nexus_vars = "dtj", memb_opts = "sta") %>%
  arrange(country_case, year) %>%
  group_by(country_case) %>%
  mutate(tran_tfc_dom_dtj_sta_cumu = cumsum(tran_tfc_dom_dtj_sta))
df <- TrialsMeasure(cy = df, prefix = "tran", measure = "cct", type_opts = "dom", nexus_vars = "dtj", memb_opts = "sta")
df <- TrialsMeasure(cy = df, prefix = "tran", measure = "cce", type_opts = "dom", nexus_vars = "dtj", memb_opts = "sta")
df <- TrialsMeasure(cy = df, prefix = "tran", measure = "crt", type_opts = "dom", nexus_vars = "dtj", memb_opts = "sta")
df <- TrialsMeasure(cy = df, prefix = "tran", measure = "sen", type_opts = "dom", nexus_vars = "dtj", memb_opts = "sta")
df <- TrialsMeasure(cy = df, prefix = "tran", measure = "trs", type_opts = "dom", nexus_vars = "dtj", memb_opts = "sta", rank_opts = "hi")
df <- TrialsMeasure(cy = df, prefix = "tran", measure = "tro", type_opts = "dom", nexus_vars = "dtj", memb_opts = "sta", rank_opts = "hi")
df <- TrialsMeasure(cy = df, prefix = "tran", measure = "tfc", type_opts = "dom", nexus_vars = "dtj", memb_opts = "sta", rank_opts = "hi") %>%
  arrange(country_case, year) %>%
  group_by(country_case) %>%
  mutate(tran_tfc_dom_dtj_sta_hi_cumu = cumsum(tran_tfc_dom_dtj_sta_hi))
df <- TrialsMeasure(cy = df, prefix = "tran", measure = "cct", type_opts = "dom", nexus_vars = "dtj", memb_opts = "sta", rank_opts = "hi")
df <- TrialsMeasure(cy = df, prefix = "tran", measure = "cce", type_opts = "dom", nexus_vars = "dtj", memb_opts = "sta", rank_opts = "hi")
df <- TrialsMeasure(cy = df, prefix = "tran", measure = "crt", type_opts = "dom", nexus_vars = "dtj", memb_opts = "sta", rank_opts = "hi")
df <- TrialsMeasure(cy = df, prefix = "tran", measure = "sen", type_opts = "dom", nexus_vars = "dtj", memb_opts = "sta", rank_opts = "hi")

## ctj trials of state agents: _ctj_sta
df <- TrialsMeasure(cy = df, prefix = "tran", measure = "trs", type_opts = "dom", nexus_vars = "ctj", memb_opts = "sta") %>%
  mutate(
    tran_trs_dom_ctj_sta_binary = ifelse(tran_trs_dom_ctj_sta > 0, 1, 0),
    tran_trs_dom_ctj_sta_scale = case_when(
      tran_trs_dom_ctj_sta == 0 ~ 0,
      tran_trs_dom_ctj_sta %in% 1:2 ~ 1,
      tran_trs_dom_ctj_sta > 2 ~ 2
    )
  )
df <- TrialsMeasure(cy = df, prefix = "tran", measure = "tro", type_opts = "dom", nexus_vars = "ctj", memb_opts = "sta")
df <- TrialsMeasure(cy = df, prefix = "tran", measure = "tfc", type_opts = "dom", nexus_vars = "ctj", memb_opts = "sta") %>%
  arrange(country_case, year) %>%
  group_by(country_case) %>%
  mutate(tran_tfc_dom_ctj_sta_cumu = cumsum(tran_tfc_dom_ctj_sta))
df <- TrialsMeasure(cy = df, prefix = "tran", measure = "cct", type_opts = "dom", nexus_vars = "ctj", memb_opts = "sta")
df <- TrialsMeasure(cy = df, prefix = "tran", measure = "cce", type_opts = "dom", nexus_vars = "ctj", memb_opts = "sta")
df <- TrialsMeasure(cy = df, prefix = "tran", measure = "crt", type_opts = "dom", nexus_vars = "ctj", memb_opts = "sta")
df <- TrialsMeasure(cy = df, prefix = "tran", measure = "sen", type_opts = "dom", nexus_vars = "ctj", memb_opts = "sta")
df <- TrialsMeasure(cy = df, prefix = "tran", measure = "trs", type_opts = "dom", nexus_vars = "ctj", memb_opts = "sta", rank_opts = "hi")
df <- TrialsMeasure(cy = df, prefix = "tran", measure = "tro", type_opts = "dom", nexus_vars = "ctj", memb_opts = "sta", rank_opts = "hi")
df <- TrialsMeasure(cy = df, prefix = "tran", measure = "tfc", type_opts = "dom", nexus_vars = "ctj", memb_opts = "sta", rank_opts = "hi") %>%
  arrange(country_case, year) %>%
  group_by(country_case) %>%
  mutate(tran_tfc_dom_ctj_sta_hi_cumu = cumsum(tran_tfc_dom_ctj_sta_hi))
df <- TrialsMeasure(cy = df, prefix = "tran", measure = "cct", type_opts = "dom", nexus_vars = "ctj", memb_opts = "sta", rank_opts = "hi")
df <- TrialsMeasure(cy = df, prefix = "tran", measure = "cce", type_opts = "dom", nexus_vars = "ctj", memb_opts = "sta", rank_opts = "hi")
df <- TrialsMeasure(cy = df, prefix = "tran", measure = "crt", type_opts = "dom", nexus_vars = "ctj", memb_opts = "sta", rank_opts = "hi")
df <- TrialsMeasure(cy = df, prefix = "tran", measure = "sen", type_opts = "dom", nexus_vars = "ctj", memb_opts = "sta", rank_opts = "hi")

## combined dtj and ctj trials of state agents: _dtj_ctj_sta
df <- TrialsMeasure(cy = df, prefix = "tran", measure = "trs", type_opts = "dom", nexus_vars = c("dtj", "ctj"), memb_opts = "sta") %>%
  mutate(
    tran_trs_dom_dtj_ctj_sta_binary = ifelse(tran_trs_dom_dtj_ctj_sta > 0, 1, 0),
    tran_trs_dom_dtj_ctj_sta_scale = case_when(
      tran_trs_dom_dtj_ctj_sta == 0 ~ 0,
      tran_trs_dom_dtj_ctj_sta %in% 1:2 ~ 1,
      tran_trs_dom_dtj_ctj_sta > 2 ~ 2
    )
  )
df <- TrialsMeasure(cy = df, prefix = "tran", measure = "tro", type_opts = "dom", nexus_vars = c("dtj", "ctj"), memb_opts = "sta")
df <- TrialsMeasure(cy = df, prefix = "tran", measure = "tfc", type_opts = "dom", nexus_vars = c("dtj", "ctj"), memb_opts = "sta") %>%
  arrange(country_case, year) %>%
  group_by(country_case) %>%
  mutate(tran_tfc_dom_dtj_ctj_sta_cumu = cumsum(tran_tfc_dom_dtj_ctj_sta))
df <- TrialsMeasure(cy = df, prefix = "tran", measure = "cct", type_opts = "dom", nexus_vars = c("dtj", "ctj"), memb_opts = "sta")
df <- TrialsMeasure(cy = df, prefix = "tran", measure = "cce", type_opts = "dom", nexus_vars = c("dtj", "ctj"), memb_opts = "sta")
df <- TrialsMeasure(cy = df, prefix = "tran", measure = "crt", type_opts = "dom", nexus_vars = c("dtj", "ctj"), memb_opts = "sta")
df <- TrialsMeasure(cy = df, prefix = "tran", measure = "sen", type_opts = "dom", nexus_vars = c("dtj", "ctj"), memb_opts = "sta")
df <- TrialsMeasure(cy = df, prefix = "tran", measure = "trs", type_opts = "dom", nexus_vars = c("dtj", "ctj"), memb_opts = "sta", rank_opts = "hi")
df <- TrialsMeasure(cy = df, prefix = "tran", measure = "tro", type_opts = "dom", nexus_vars = c("dtj", "ctj"), memb_opts = "sta", rank_opts = "hi")
df <- TrialsMeasure(cy = df, prefix = "tran", measure = "tfc", type_opts = "dom", nexus_vars = c("dtj", "ctj"), memb_opts = "sta", rank_opts = "hi") %>%
  arrange(country_case, year) %>%
  group_by(country_case) %>%
  mutate(tran_tfc_dom_dtj_ctj_sta_hi_cumu = cumsum(tran_tfc_dom_dtj_ctj_sta_hi))
df <- TrialsMeasure(cy = df, prefix = "tran", measure = "cct", type_opts = "dom", nexus_vars = c("dtj", "ctj"), memb_opts = "sta", rank_opts = "hi")
df <- TrialsMeasure(cy = df, prefix = "tran", measure = "cce", type_opts = "dom", nexus_vars = c("dtj", "ctj"), memb_opts = "sta", rank_opts = "hi")
df <- TrialsMeasure(cy = df, prefix = "tran", measure = "crt", type_opts = "dom", nexus_vars = c("dtj", "ctj"), memb_opts = "sta", rank_opts = "hi")
df <- TrialsMeasure(cy = df, prefix = "tran", measure = "sen", type_opts = "dom", nexus_vars = c("dtj", "ctj"), memb_opts = "sta", rank_opts = "hi")

## transitional trials of opposition members: _dtj_ctj_opp
df <- TrialsMeasure(cy = df, prefix = "tran", measure = "trs", type_opts = "dom", nexus_vars = c("dtj", "ctj"), memb_opts = "opp")

## ctj trials of opposition members: _ctj_opp
df <- TrialsMeasure(cy = df, prefix = "tran", measure = "trs", type_opts = "dom", nexus_vars = "ctj", memb_opts = "opp") %>%
  mutate(
    tran_trs_dom_ctj_opp_binary = ifelse(tran_trs_dom_ctj_opp > 0, 1, 0),
    tran_trs_dom_ctj_opp_scale = case_when(
      tran_trs_dom_ctj_opp == 0 ~ 0,
      tran_trs_dom_ctj_opp %in% 1:2 ~ 1,
      tran_trs_dom_ctj_opp > 2 ~ 2
    )
  )
df <- TrialsMeasure(cy = df, prefix = "tran", measure = "tro", type_opts = "dom", nexus_vars = "ctj", memb_opts = "opp")
df <- TrialsMeasure(cy = df, prefix = "tran", measure = "tfc", type_opts = "dom", nexus_vars = "ctj", memb_opts = "opp") %>%
  arrange(country_case, year) %>%
  group_by(country_case) %>%
  mutate(tran_tfc_dom_ctj_opp_cumu = cumsum(tran_tfc_dom_ctj_opp))
df <- TrialsMeasure(cy = df, prefix = "tran", measure = "cct", type_opts = "dom", nexus_vars = "ctj", memb_opts = "opp")
df <- TrialsMeasure(cy = df, prefix = "tran", measure = "cce", type_opts = "dom", nexus_vars = "ctj", memb_opts = "opp")
df <- TrialsMeasure(cy = df, prefix = "tran", measure = "crt", type_opts = "dom", nexus_vars = "ctj", memb_opts = "opp")
df <- TrialsMeasure(cy = df, prefix = "tran", measure = "sen", type_opts = "dom", nexus_vars = "ctj", memb_opts = "opp")
df <- TrialsMeasure(cy = df, prefix = "tran", measure = "trs", type_opts = "dom", nexus_vars = "ctj", memb_opts = "opp", rank_opts = "hi")
df <- TrialsMeasure(cy = df, prefix = "tran", measure = "tro", type_opts = "dom", nexus_vars = "ctj", memb_opts = "opp", rank_opts = "hi")
df <- TrialsMeasure(cy = df, prefix = "tran", measure = "tfc", type_opts = "dom", nexus_vars = "ctj", memb_opts = "opp", rank_opts = "hi") %>%
  arrange(country_case, year) %>%
  group_by(country_case) %>%
  mutate(tran_tfc_dom_ctj_opp_hi_cumu = cumsum(tran_tfc_dom_ctj_opp_hi))
df <- TrialsMeasure(cy = df, prefix = "tran", measure = "cct", type_opts = "dom", nexus_vars = "ctj", memb_opts = "opp", rank_opts = "hi")
df <- TrialsMeasure(cy = df, prefix = "tran", measure = "cce", type_opts = "dom", nexus_vars = "ctj", memb_opts = "opp", rank_opts = "hi")
df <- TrialsMeasure(cy = df, prefix = "tran", measure = "crt", type_opts = "dom", nexus_vars = "ctj", memb_opts = "opp", rank_opts = "hi")
df <- TrialsMeasure(cy = df, prefix = "tran", measure = "sen", type_opts = "dom", nexus_vars = "ctj", memb_opts = "opp", rank_opts = "hi")

# TrialsMeasure(cy = df, prefix = "tran", measure = "trs", type_opts = "dom", nexus_vars = "dtj", memb_opts = "opp") %>%
#   select(tran_trs_dom_dtj_opp) %>%
#   filter(tran_trs_dom_dtj_opp > 0)

#### regular HRs prosecutions (prefix: regu)

## trials of state agents that are not dtj or ctj: [_hrs_con]_Xdtj_Xctj_sta
df <- TrialsMeasure(cy = df, prefix = "regu", measure = "trs", type_opts = "dom", nexus_vars = "hrs", excl_nexus_vars = c("dtj", "ctj"), memb_opts = "sta") %>%
  mutate(
    regu_trs_dom_sta_binary = ifelse(regu_trs_dom_sta > 0, 1, 0),
    regu_trs_dom_sta_scale = case_when(
      regu_trs_dom_sta == 0 ~ 0,
      regu_trs_dom_sta %in% 1:2 ~ 1,
      regu_trs_dom_sta > 2 ~ 2
    )
  )
df <- TrialsMeasure(cy = df, prefix = "regu", measure = "tro", type_opts = "dom", nexus_vars = "hrs", excl_nexus_vars = c("dtj", "ctj"), memb_opts = "sta")
df <- TrialsMeasure(cy = df, prefix = "regu", measure = "tfc", type_opts = "dom", nexus_vars = "hrs", excl_nexus_vars = c("dtj", "ctj"), memb_opts = "sta") %>%
  arrange(country_case, year) %>%
  group_by(country_case) %>%
  mutate(regu_tfc_dom_sta_cumu = cumsum(regu_tfc_dom_sta))
df <- TrialsMeasure(cy = df, prefix = "regu", measure = "cct", type_opts = "dom", nexus_vars = "hrs", excl_nexus_vars = c("dtj", "ctj"), memb_opts = "sta")
df <- TrialsMeasure(cy = df, prefix = "regu", measure = "cce", type_opts = "dom", nexus_vars = "hrs", excl_nexus_vars = c("dtj", "ctj"), memb_opts = "sta")
df <- TrialsMeasure(cy = df, prefix = "regu", measure = "crt", type_opts = "dom", nexus_vars = "hrs", excl_nexus_vars = c("dtj", "ctj"), memb_opts = "sta")
df <- TrialsMeasure(cy = df, prefix = "regu", measure = "sen", type_opts = "dom", nexus_vars = "hrs", excl_nexus_vars = c("dtj", "ctj"), memb_opts = "sta")

# TrialsMeasure(cy = df, prefix = "regu", measure = "trs", type_opts = "dom", nexus_vars = c("hrs", "con"), excl_nexus_vars = c("dtj", "ctj"), memb_opts = "opp") %>%
#   select(regu_trs_dom_opp) %>% summary

#### low level conflict-related prosecutions: (prefix: oppo)

## conflict trials which are not ctj (i.e. not matched to UCDP conflict codes) of state agents and opposition: [_con]_Xctj_sta_opp
df <- TrialsMeasure(cy = df, prefix = "oppo", measure = "trs", type_opts = "dom", nexus_vars = "con", excl_nexus_vars = "ctj", memb_opts = c("sta", "opp"))
df <- TrialsMeasure(cy = df, prefix = "oppo", measure = "tro", type_opts = "dom", nexus_vars = "con", excl_nexus_vars = "ctj", memb_opts = c("sta", "opp"))
df <- TrialsMeasure(cy = df, prefix = "oppo", measure = "tfc", type_opts = "dom", nexus_vars = "con", excl_nexus_vars = "ctj", memb_opts = c("sta", "opp")) %>%
  arrange(country_case, year) %>%
  group_by(country_case) %>%
  mutate(oppo_tfc_dom_sta_opp_cumu = cumsum(oppo_tfc_dom_sta_opp))
df <- TrialsMeasure(cy = df, prefix = "oppo", measure = "cct", type_opts = "dom", nexus_vars = "con", excl_nexus_vars = "ctj", memb_opts = c("sta", "opp"))
df <- TrialsMeasure(cy = df, prefix = "oppo", measure = "cce", type_opts = "dom", nexus_vars = "con", excl_nexus_vars = "ctj", memb_opts = c("sta", "opp"))
df <- TrialsMeasure(cy = df, prefix = "oppo", measure = "crt", type_opts = "dom", nexus_vars = "con", excl_nexus_vars = "ctj", memb_opts = c("sta", "opp"))
df <- TrialsMeasure(cy = df, prefix = "oppo", measure = "sen", type_opts = "dom", nexus_vars = "con", excl_nexus_vars = "ctj", memb_opts = c("sta", "opp"))

#### intl & foreign

df <- TrialsMeasure(cy = df, measure = "trs", type_opts = "int", nexus_vars = c("hrs", "con"), memb_opts = "sta")
df <- TrialsMeasure(cy = df, measure = "tro", type_opts = "int", nexus_vars = c("hrs", "con"), memb_opts = "sta")
df <- TrialsMeasure(cy = df, measure = "tfc", type_opts = "int", nexus_vars = c("hrs", "con"), memb_opts = "sta")
df <- TrialsMeasure(cy = df, measure = "cct", type_opts = "int", nexus_vars = c("hrs", "con"), memb_opts = "sta")
df <- TrialsMeasure(cy = df, measure = "cce", type_opts = "int", nexus_vars = c("hrs", "con"), memb_opts = "sta")
df <- TrialsMeasure(cy = df, measure = "crt", type_opts = "int", nexus_vars = c("hrs", "con"), memb_opts = "sta")
df <- TrialsMeasure(cy = df, measure = "sen", type_opts = "int", nexus_vars = c("hrs", "con"), memb_opts = "sta")
df <- TrialsMeasure(cy = df, measure = "trs", type_opts = "int", nexus_vars = c("hrs", "con"), memb_opts = "sta", rank_opts = "hi")
df <- TrialsMeasure(cy = df, measure = "tro", type_opts = "int", nexus_vars = c("hrs", "con"), memb_opts = "sta", rank_opts = "hi")
df <- TrialsMeasure(cy = df, measure = "tfc", type_opts = "int", nexus_vars = c("hrs", "con"), memb_opts = "sta", rank_opts = "hi")
df <- TrialsMeasure(cy = df, measure = "cct", type_opts = "int", nexus_vars = c("hrs", "con"), memb_opts = "sta", rank_opts = "hi")
df <- TrialsMeasure(cy = df, measure = "cce", type_opts = "int", nexus_vars = c("hrs", "con"), memb_opts = "sta", rank_opts = "hi")
df <- TrialsMeasure(cy = df, measure = "crt", type_opts = "int", nexus_vars = c("hrs", "con"), memb_opts = "sta", rank_opts = "hi")
df <- TrialsMeasure(cy = df, measure = "sen", type_opts = "int", nexus_vars = c("hrs", "con"), memb_opts = "sta", rank_opts = "hi")

df <- TrialsMeasure(cy = df, measure = "trs", type_opts = "int", nexus_vars = c("hrs", "con"), memb_opts = "opp")
df <- TrialsMeasure(cy = df, measure = "tro", type_opts = "int", nexus_vars = c("hrs", "con"), memb_opts = "opp")
df <- TrialsMeasure(cy = df, measure = "tfc", type_opts = "int", nexus_vars = c("hrs", "con"), memb_opts = "opp")
df <- TrialsMeasure(cy = df, measure = "cct", type_opts = "int", nexus_vars = c("hrs", "con"), memb_opts = "opp")
df <- TrialsMeasure(cy = df, measure = "cce", type_opts = "int", nexus_vars = c("hrs", "con"), memb_opts = "opp")
df <- TrialsMeasure(cy = df, measure = "crt", type_opts = "int", nexus_vars = c("hrs", "con"), memb_opts = "opp")
df <- TrialsMeasure(cy = df, measure = "sen", type_opts = "int", nexus_vars = c("hrs", "con"), memb_opts = "opp")
df <- TrialsMeasure(cy = df, measure = "trs", type_opts = "int", nexus_vars = c("hrs", "con"), memb_opts = "opp", rank_opts = "hi")
df <- TrialsMeasure(cy = df, measure = "tro", type_opts = "int", nexus_vars = c("hrs", "con"), memb_opts = "opp", rank_opts = "hi")
df <- TrialsMeasure(cy = df, measure = "tfc", type_opts = "int", nexus_vars = c("hrs", "con"), memb_opts = "opp", rank_opts = "hi")
df <- TrialsMeasure(cy = df, measure = "cct", type_opts = "int", nexus_vars = c("hrs", "con"), memb_opts = "opp", rank_opts = "hi")
df <- TrialsMeasure(cy = df, measure = "cce", type_opts = "int", nexus_vars = c("hrs", "con"), memb_opts = "opp", rank_opts = "hi")
df <- TrialsMeasure(cy = df, measure = "crt", type_opts = "int", nexus_vars = c("hrs", "con"), memb_opts = "opp", rank_opts = "hi")
df <- TrialsMeasure(cy = df, measure = "sen", type_opts = "int", nexus_vars = c("hrs", "con"), memb_opts = "opp", rank_opts = "hi")

df <- TrialsMeasure(cy = df, measure = "trs", type_opts = "for", nexus_vars = c("hrs", "con"), memb_opts = "sta")
df <- TrialsMeasure(cy = df, measure = "tro", type_opts = "for", nexus_vars = c("hrs", "con"), memb_opts = "sta")
df <- TrialsMeasure(cy = df, measure = "tfc", type_opts = "for", nexus_vars = c("hrs", "con"), memb_opts = "sta")
df <- TrialsMeasure(cy = df, measure = "cct", type_opts = "for", nexus_vars = c("hrs", "con"), memb_opts = "sta")
df <- TrialsMeasure(cy = df, measure = "cce", type_opts = "for", nexus_vars = c("hrs", "con"), memb_opts = "sta")
df <- TrialsMeasure(cy = df, measure = "crt", type_opts = "for", nexus_vars = c("hrs", "con"), memb_opts = "sta")
df <- TrialsMeasure(cy = df, measure = "sen", type_opts = "for", nexus_vars = c("hrs", "con"), memb_opts = "sta")
df <- TrialsMeasure(cy = df, measure = "trs", type_opts = "for", nexus_vars = c("hrs", "con"), memb_opts = "sta", rank_opts = "hi")
df <- TrialsMeasure(cy = df, measure = "tro", type_opts = "for", nexus_vars = c("hrs", "con"), memb_opts = "sta", rank_opts = "hi")
df <- TrialsMeasure(cy = df, measure = "tfc", type_opts = "for", nexus_vars = c("hrs", "con"), memb_opts = "sta", rank_opts = "hi")
df <- TrialsMeasure(cy = df, measure = "cct", type_opts = "for", nexus_vars = c("hrs", "con"), memb_opts = "sta", rank_opts = "hi")
df <- TrialsMeasure(cy = df, measure = "cce", type_opts = "for", nexus_vars = c("hrs", "con"), memb_opts = "sta", rank_opts = "hi")
df <- TrialsMeasure(cy = df, measure = "crt", type_opts = "for", nexus_vars = c("hrs", "con"), memb_opts = "sta", rank_opts = "hi")
df <- TrialsMeasure(cy = df, measure = "sen", type_opts = "for", nexus_vars = c("hrs", "con"), memb_opts = "sta", rank_opts = "hi")

df <- TrialsMeasure(cy = df, measure = "trs", type_opts = "for", nexus_vars = c("hrs", "con"), memb_opts = "opp")
df <- TrialsMeasure(cy = df, measure = "tro", type_opts = "for", nexus_vars = c("hrs", "con"), memb_opts = "opp")
df <- TrialsMeasure(cy = df, measure = "tfc", type_opts = "for", nexus_vars = c("hrs", "con"), memb_opts = "opp")
df <- TrialsMeasure(cy = df, measure = "cct", type_opts = "for", nexus_vars = c("hrs", "con"), memb_opts = "opp")
df <- TrialsMeasure(cy = df, measure = "cce", type_opts = "for", nexus_vars = c("hrs", "con"), memb_opts = "opp")
df <- TrialsMeasure(cy = df, measure = "crt", type_opts = "for", nexus_vars = c("hrs", "con"), memb_opts = "opp")
df <- TrialsMeasure(cy = df, measure = "sen", type_opts = "for", nexus_vars = c("hrs", "con"), memb_opts = "opp")
df <- TrialsMeasure(cy = df, measure = "trs", type_opts = "for", nexus_vars = c("hrs", "con"), memb_opts = "opp", rank_opts = "hi")
df <- TrialsMeasure(cy = df, measure = "tro", type_opts = "for", nexus_vars = c("hrs", "con"), memb_opts = "opp", rank_opts = "hi")
df <- TrialsMeasure(cy = df, measure = "tfc", type_opts = "for", nexus_vars = c("hrs", "con"), memb_opts = "opp", rank_opts = "hi")
df <- TrialsMeasure(cy = df, measure = "cct", type_opts = "for", nexus_vars = c("hrs", "con"), memb_opts = "opp", rank_opts = "hi")
df <- TrialsMeasure(cy = df, measure = "cce", type_opts = "for", nexus_vars = c("hrs", "con"), memb_opts = "opp", rank_opts = "hi")
df <- TrialsMeasure(cy = df, measure = "crt", type_opts = "for", nexus_vars = c("hrs", "con"), memb_opts = "opp", rank_opts = "hi")
df <- TrialsMeasure(cy = df, measure = "sen", type_opts = "for", nexus_vars = c("hrs", "con"), memb_opts = "opp", rank_opts = "hi")
