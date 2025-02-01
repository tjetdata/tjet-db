library(tidyverse)
library(gt)
# library(coefplot)
# library(multipanelfigure)
# library(cowplot)
library(patchwork)

### get data

# base <- "~/Documents/GitHub/tjet-db/tjet_datasets"
base <- "tjet_datasets"
db <- list(
  Amnesties = read_csv(here::here(base, "tjet_amnesties.csv")),
  Reparations = read_csv(here::here(base, "tjet_reparations.csv")),
  TruthCommissions = read_csv(here::here(base, "tjet_tcs.csv")),
  Trials = read_csv(here::here(base, "tjet_trials.csv")),
  Accused = read_csv(here::here(base, "tjet_accused.csv")),
  Vettings = read_csv(here::here(base, "tjet_vettings.csv")),
  Investigations = read_csv(here::here(base, "tjet_un_investigations.csv"))
)

n_records <- nrow(db[["Amnesties"]]) + nrow(db[["Reparations"]]) + nrow(db[["TruthCommissions"]]) +
  nrow(db[["Trials"]]) + nrow(db[["Accused"]]) + nrow(db[["Vettings"]]) + nrow(db[["Investigations"]])

# db[["Trials"]] |>
#   select(trialID, trialType, yearStart, yearEnd) |>
#   mutate(length = yearEnd - yearStart + 1) |>
#   group_by(trialType) |>
#   reframe(length = mean(length))

# db[["Trials"]] |>
#   select(trialID, trialType, contains("rank") ) |>
#   select(trialType, anyHighRank) |>
#   table()

df <- read_csv(here::here(base, "tjet_cy_analyses.csv")) %>%
  mutate(
    gdp_per_capita = rgdpe_pwt / pop_pwt,
    gdp_per_capita_log = log(gdp_per_capita),
    pop_log = log(pop_pwt)
  ) %>%
  mutate(trials = trials_domestic + trials_intl + trials_foreign) %>%
  filter(year >= 1970 & year <= 2020)

read_csv(here::here(base, "tjet_codebook.csv")) |> 
 reframe(.by = section, 
         n_var = n()) |> 
  print(n = Inf) 

### rescaling

# to_put_on_unit_scale <- c("theta_mean_fariss")
# df %>%
#   select(any_of(to_put_on_unit_scale)) %>%
#   summary()
UnitScale <- function(col) {
  col <- col - min(col, na.rm = TRUE)
  col <- col / max(col, na.rm = TRUE)
  return(col)
}
# df <- df %>%
#   mutate(across(any_of(to_put_on_unit_scale), UnitScale)) %>%
#   mutate(trials = trials_domestic + trials_intl + trials_foreign) %>%
#   filter(year >= 1970 & year <= 2020)
# df %>%
#   select(any_of(to_put_on_unit_scale)) %>%
#   summary()

### Table 1: basic counts

table1 <- list()

table1[["Amnesties"]] <- db[["Amnesties"]] %>%
  filter(amnestyYear %in% 1970:2020) %>%
  arrange(country) %>%
  group_by(country) %>%
  reframe(n = n()) %>%
  ungroup() %>%
  reframe(
    mech = "Amnesties",
    n = sum(n),
    ctry_n = n()
  )

table1[["Reparations"]] <- db[["Reparations"]] %>%
  filter(yearCreated %in% 1970:2020) %>%
  arrange(country) %>%
  group_by(country) %>%
  reframe(n = n()) %>%
  ungroup() %>%
  reframe(
    mech = "Reparations",
    n = sum(n),
    ctry_n = n()
  )

table1[["Truth Commissions"]] <- db[["TruthCommissions"]] %>%
  filter(yearPassed %in% 1970:2020) %>%
  # filter(authorizedByState == 1 & temporaryBodyReport == 1 & focusedPast == 1 & investigatePatternAbuse == 1) %>%
  arrange(country) %>%
  group_by(country) %>%
  reframe(n = n()) %>%
  ungroup() %>%
  reframe(
    mech = "Truth Commissions",
    n = sum(n),
    ctry_n = n()
  )

table1[["Criminal prosecutions"]] <- db[["Trials"]] %>%
  filter(yearStart %in% 1970:2020) %>%
  arrange(country_Accused) %>%
  group_by(country_Accused) %>%
  reframe(n = n()) %>%
  reframe(
    mech = "Criminal prosecutions",
    n = sum(n),
    ctry_n = n()
  )

table1[["Trial types"]] <- db[["Trials"]] %>%
  filter(yearStart %in% 1970:2020) %>%
  mutate(trialType = ifelse(trialType == "international (hybrid)", "international", trialType)) %>%
  arrange(country_Accused, trialType) %>%
  group_by(country_Accused, trialType) %>%
  reframe(n = n()) %>%
  ungroup() %>%
  group_by(trialType) %>%
  reframe(
    n = sum(n),
    ctry_n = n()
  ) %>%
  rename(mech = trialType)

table1[["Vetting policies"]] <- db[["Vettings"]] %>%
  filter(yearStart %in% 1970:2020) %>%
  filter(is.na(alterationOf)) %>%
  arrange(country) %>%
  group_by(country) %>%
  reframe(n = n()) %>%
  ungroup() %>%
  reframe(
    mech = "Vetting policies",
    n = sum(n),
    ctry_n = n()
  )

table1[["UN Investigations"]] <- db[["Investigations"]] %>%
  filter(beg %in% 1970:2020) %>%
  arrange(country) %>%
  group_by(country) %>%
  reframe(n = n()) %>%
  ungroup() %>%
  reframe(
    mech = "UN Investigations",
    n = sum(n),
    ctry_n = n()
  )

table1 <- table1 |>
  bind_rows() |>
  write_csv("descriptives/table1.csv")

### totals from cy-dataset

totals <- df %>%
  reframe(
    amnesties = sum(amnesties, na.rm = TRUE),
    trials = sum(trials, na.rm = TRUE),
    tcs = sum(tcs, na.rm = TRUE),
    reparations = sum(reparations, na.rm = TRUE),
    vettings = sum(vettings, na.rm = TRUE)
  ) %>%
  pivot_longer(everything()) %>%
  mutate(
    name = str_replace(name, "amnesties", "Amnesties"),
    name = str_replace(name, "trials", "Trials"),
    name = str_replace(name, "tcs", "Truth Commissions"),
    name = str_replace(name, "reparations", "Reparations"),
    name = str_replace(name, "vettings", "Vettings")
  ) %>%
  arrange(name)

totals %>%
  gt() %>%
  tab_options(column_labels.hidden = TRUE) |>
  cols_label(name = "", value = "Total")

### data release trends Figure 1 from cy-dataset

df_fig1 <- df %>%
  group_by(year) %>%
  reframe(
    amnesties = sum(amnesties, na.rm = TRUE),
    trials = sum(trials, na.rm = TRUE),
    trials_domestic = sum(trials_domestic, na.rm = TRUE),
    trials_intl = sum(trials_intl, na.rm = TRUE),
    trials_foreign = sum(trials_foreign, na.rm = TRUE),
    tcs = sum(tcs, na.rm = TRUE),
    reparations = sum(reparations, na.rm = TRUE),
    vettings = sum(vettings, na.rm = TRUE),
    uninv_beg = sum(uninv_beg, na.rm = TRUE)
  )

totals <- df_fig1 %>%
  reframe(
    amnesties = sum(amnesties, na.rm = TRUE),
    trials = sum(trials, na.rm = TRUE),
    trials_domestic = sum(trials_domestic, na.rm = TRUE),
    trials_intl = sum(trials_intl, na.rm = TRUE),
    trials_foreign = sum(trials_foreign, na.rm = TRUE),
    tcs = sum(tcs, na.rm = TRUE),
    reparations = sum(reparations, na.rm = TRUE),
    vettings = sum(vettings, na.rm = TRUE),
    uninv_beg = sum(uninv_beg, na.rm = TRUE)
  )

df_fig1 %>%
  pivot_longer(all_of(c("amnesties", "trials", "tcs", "reparations", "vettings", "uninv_beg")),
    names_to = "key", values_to = "value"
  ) %>%
  mutate(
    key = str_replace(key, "amnesties", paste("Amnesties\n(n = ", totals$amnesties, ")", sep = "")),
    key = str_replace(key, "trials", paste("Prosecutions\n(n = ", totals$trials, ")", sep = "")),
    key = str_replace(key, "tcs", paste("Truth Commissions\n(n = ", totals$tcs, ")", sep = "")),
    key = str_replace(key, "reparations", paste("Reparations\n(n = ", totals$reparations, ")", sep = "")),
    key = str_replace(key, "vettings", paste("Vettings\n(n = ", totals$vettings, ")", sep = "")),
    key = str_replace(key, "uninv_beg", paste("UN Investigations\n(n = ", totals$uninv_beg, ")", sep = ""))
  ) %>%
  ggplot() +
  # geom_line(aes(x = as.integer(as.character(year)), y = value, col = key, group = key), linewidth = 1) +
  geom_line(aes(x = as.integer(as.character(year)), y = value, group = key), linewidth = 0.5) +
  facet_grid(key ~ ., scales = "free_y") +
  scale_y_continuous(
    breaks = function(x) floor(max(x)), minor_breaks = NULL # ,
    # expand = expansion(mult = c(0, .1))
  ) +
  ylab("") +
  xlab("") +
  theme_bw() +
  theme(
    legend.position = "none",
    legend.title = element_blank(),
    # strip.text.y = element_blank()
    strip.text.y = element_text(angle = 0),
    # strip.background = element_rect(fill = "white"),
    plot.margin = margin(t = 8, r = 2, b = 0, l = 0, unit = "pt")
  )
ggsave("descriptives/tj_trends.png", plot = last_plot(), width = 6, height = 4)

## old figure

# df %>%
#   group_by(year) %>%
#   summarize(
#     amnesties = sum(amnesties, na.rm = TRUE),
#     trials = sum(trials, na.rm = TRUE),
#     tcs = sum(tcs, na.rm = TRUE),
#     reparations = sum(reparations, na.rm = TRUE)
#   ) %>%
#   pivot_longer(all_of(c("amnesties", "trials", "tcs", "reparations")),
#     names_to = "key", values_to = "value"
#   ) %>%
#   mutate(
#     key = str_replace(key, "amnesties", "Amnesties"),
#     key = str_replace(key, "trials", "Trials"),
#     key = str_replace(key, "tcs", "Truth Commissions"),
#     key = str_replace(key, "reparations", "Reparations")
#   ) %>%
#   ggplot() +
#   geom_line(aes(x = as.integer(as.character(year)), y = value, col = key, group = key), linewidth = 1) +
#   facet_grid(key ~ ., scales = "free_y") +
#   scale_y_continuous(
#     breaks = function(x) floor(max(x)), minor_breaks = NULL # ,
#     # expand = expansion(mult = c(0, .1))
#   ) +
#   scale_colour_manual(
#     values = c("orange", "black", "red", "blue"),
#     limits = c(
#       "Reparations",
#       "Amnesties",
#       "Trials",
#       "Truth Commissions"
#     ),
#     aesthetics = c("colour", "fill")
#   ) +
#   ylab("") +
#   xlab("") +
#   theme_bw() +
#   theme(
#     legend.position = "none",
#     legend.title = element_blank(),
#     legend.text = element_text(size = 20),
#     axis.text = element_text(size = 20),
#     axis.title = element_text(size = 20),
#     strip.text.y = element_blank(),
#     plot.margin = margin(t = 8, r = 6, b = 0, l = 0, unit = "pt")
#   )
# # guides(col = guide_legend(ncol = 3))

### regions

df_regions <- df %>%
  group_by(region) %>%
  summarize(
    amnesties = sum(amnesties, na.rm = TRUE),
    trials = sum(trials, na.rm = TRUE),
    tcs = sum(tcs, na.rm = TRUE),
    reparations = sum(reparations, na.rm = TRUE)
  ) %>%
  ungroup()

df_regions %>%
  mutate(
    amnesties = round(amnesties / sum(amnesties) * 100, 1),
    trials = round(trials / sum(trials) * 100, 1),
    tcs = round(tcs / sum(tcs) * 100, 1),
    reparations = round(reparations / sum(reparations) * 100, 1)
  ) %>%
  pivot_longer(cols = !region) %>%
  full_join(
    df_regions %>%
      pivot_longer(cols = !region, values_to = "raw"),
    by = c("region", "name")
  ) %>%
  mutate(name = case_when(
    name == "amnesties" ~ "Amnesties",
    name == "trials" ~ "Trials",
    name == "tcs" ~ "Truth Commissions",
    name == "reparations" ~ "Reparations"
  )) %>%
  ggplot(aes(fill = region, y = value, x = name)) +
  geom_bar(position = position_dodge(), stat = "identity", color = "black") +
  geom_text(aes(label = raw), vjust = -0.5, position = position_dodge(0.9), size = 2) +
  # scale_fill_grey(start = 0.8, end = 0.2) +
  # scale_colour_manual(values = c("orange", "black", "red", "blue", "darkgreen"),
  #                     aesthetics = c("colour", "fill")) +
  theme_bw() +
  theme(
    panel.border = element_blank(),
    legend.position = "bottom",
    legend.title = element_blank(),
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.grid.major.x = element_blank()
  ) +
  ylab("Percentage")
ggsave("descriptives/tj_regions.png", plot = last_plot(), width = 6, height = 4)


### Table 3: trial sub-types

table3 <- list()

table3[["domestic"]] <- df %>%
  filter(year >= 1970 & year <= 2020) |>
  reframe(
    regu_trs_dom_sta = sum(regu_trs_dom_sta, na.rm = TRUE),
    tran_trs_dom_dtj = sum(tran_trs_dom_dtj, na.rm = TRUE),
    tran_trs_dom_ctj = sum(tran_trs_dom_ctj, na.rm = TRUE),
    oppo_trs_dom_sta_opp = sum(oppo_trs_dom_sta_opp, na.rm = TRUE)
  ) |>
  pivot_longer(everything(), names_to = "subtype", values_to = "n") |>
  mutate(
    type = "domestic",
    subtype = case_when(
      subtype == "regu_trs_dom_sta" ~ "Regular human rights prosecutions",
      subtype == "tran_trs_dom_dtj" ~ "Transitional human rights prosecutions",
      subtype == "tran_trs_dom_ctj" ~ "Intrastate conflict prosecutions",
      subtype == "oppo_trs_dom_sta_opp" ~ "Opposition prosecutions"
    )
  )

table3[["foreign"]] <- db[["Trials"]] |>
  filter(trialType == "foreign" & yearStart >= 1970 & yearStart <= 2020) |>
  rename(subtype = jurisdiction) |>
  mutate(
    subtype = paste(str_to_sentence(subtype), " jurisdiction", sep = ""),
    subtype = ifelse(is.na(subtype), "NOT CODED", subtype)
  ) |>
  group_by(subtype) |>
  reframe(
    type = "foreign",
    n = n()
  )

table3[["international"]] <- db[["Trials"]] |>
  filter(trialType %in% c("international (hybrid)", "international") & yearStart >= 1970 & yearStart <= 2020) |>
  mutate(
    subtype = case_when(
      legalSystem %in% c("ICTR", "ICTY") ~ "Ad Hoc Tribunals",
      legalSystem == "ICC" ~ "International Criminal Court",
      legalSystem %in% c("ECCC", "Iraqi High Tribunal", "SCSL", "STL", "UNMIK/EULEX", "SPSC, Timor Leste") ~ "Hybrid Tribunals"
    ),
  ) |>
  group_by(subtype) |>
  reframe(
    type = "international",
    n = n()
  )

table3 <- table3 |>
  bind_rows() |>
  select(type, subtype, n) |>
  write_csv("descriptives/table3.csv")

### Trials over time

to_plot <- db[["Trials"]] %>%
  mutate(
    trialType = ifelse(trialType == "international (hybrid)", "other", trialType),
    trialType = ifelse(trialType == "foreign", "other", trialType),
    trialType = ifelse(trialType == "international", "other", trialType)
  ) %>%
  select(yearStart, trialType)

to_plot %>%
  filter(trialType %in% c("domestic", "don't know")) %>%
  group_by(yearStart) %>%
  mutate(count = n()) %>%
  ggplot() +
  geom_line(aes(x = yearStart, y = count), linewidth = 1) +
  xlab("") +
  ylab("") +
  theme_bw() +
  theme(legend.position = "none") +
  ggtitle("Domestic Trials")
ggsave("descriptives/trials_domestic.png", plot = last_plot(), width = 6, height = 4)

to_plot %>%
  filter(trialType == "other") %>%
  group_by(yearStart) %>%
  mutate(count = n()) %>%
  ggplot() +
  geom_line(aes(x = yearStart, y = count), linewidth = 1) +
  xlab("") +
  ylab("") +
  theme_bw() +
  theme(legend.position = "none") +
  ggtitle("International (incl Hybrid) & Foreign Trials")
ggsave("descriptives/trials_intl.png", plot = last_plot(), width = 6, height = 4)

### trial contexts and types

db[["Trials"]] %>%
  mutate(
    HRs = ifelse(humanRights > 0 | HRs_charges > 0, 1, 0),
    trialType = ifelse(trialType == "foreign", "other", trialType),
    trialType = ifelse(trialType == "international (hybrid)", "other", trialType),
    trialType = ifelse(trialType == "international", "other", trialType)
  ) %>%
  select(yearStart, HRs, fitsPostAutocraticTJ, fitsConflictTJ, trialType) %>%
  group_by(yearStart, trialType) %>%
  reframe(
    trials_HRs = sum(HRs, na.rm = TRUE),
    trials_trans = sum(fitsPostAutocraticTJ, na.rm = TRUE),
    trials_confl = sum(fitsConflictTJ, na.rm = TRUE)
  ) %>%
  pivot_longer(all_of(c("trials_HRs", "trials_trans", "trials_confl")),
    names_to = "key", values_to = "value"
  ) %>%
  mutate(
    key = str_replace(key, "trials_HRs", "all human rights trials"),
    key = str_replace(key, "trials_trans", "transitional trials"),
    key = str_replace(key, "trials_confl", "conflict trials")
  ) %>%
  filter(trialType == "domestic") %>%
  ggplot() +
  geom_line(aes(x = yearStart, y = value, col = key), linewidth = 1) +
  scale_colour_manual(
    values = c("#089A82", "#565AB6", "black"),
    limits = c(
      "all human rights trials",
      "transitional trials",
      "conflict trials"
    ),
    aesthetics = c("colour", "fill")
  ) +
  xlab("") +
  ylab("") +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank()
  ) +
  ggtitle("Domestic Trials")
ggsave("descriptives/trials_domestic_nexus.png", plot = last_plot(), width = 6, height = 4)

db[["Trials"]] %>%
  filter(trialType != "domestic") %>%
  mutate(trialType = ifelse(trialType == "international (hybrid)", "international", trialType)) %>%
  select(yearStart, trialType) %>%
  group_by(yearStart, trialType) %>%
  reframe(count = n()) %>%
  ggplot() +
  geom_line(aes(x = yearStart, y = count, col = trialType), linewidth = 1) +
  scale_colour_manual(
    values = c("#089A82", "#565AB6"),
    limits = c(
      "international",
      "foreign"
    ),
    aesthetics = c("colour", "fill")
  ) +
  xlab("") +
  ylab("") +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank()
  ) +
  ggtitle("International (incl Hybrid) & Foreign Trials")
ggsave("descriptives/trials_intl_foreign.png", plot = last_plot(), width = 6, height = 4)

db[["Trials"]] %>% 
  select(trialID, yearStart, trialType) |> 
  mutate(trialType = ifelse(trialType == "international (hybrid)", "international", trialType), 
         trialType = ifelse(trialType == "international", "international\n& hybrid" , trialType)) %>%
  reframe(.by = c(yearStart, trialType), 
          count = n()) |> 
  arrange(trialType, yearStart) |> 
  ggplot() +
  geom_line(aes(x = yearStart, y = count, group = trialType, col = trialType), 
            linewidth = 1) +
  facet_grid(trialType ~ ., scales = "free_y") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1)) + 
  scale_colour_manual(
    values = c("#565AB6","black", "#089A82"),
    limits = c("international\n& hybrid","foreign", "domestic"),
    aesthetics = c("colour", "fill")
  ) + 
  xlab("") +
  ylab("") +
  theme_bw() +
  theme(legend.position = "none", 
        plot.margin = margin(t = 1, r = 1, b = -12, l = -12)
        )
ggsave("descriptives/trials_types.png", plot = last_plot(), width = 1536, height = 960, units = "px")

### Trials addressing SGBV over time

to_plot <- db[["Trials"]] %>%
  mutate(
    # SGBV = ifelse(rape_Accused > 0 | sexualViolence_Accused > 0 | otherSGBV_Accused > 0, 1, 0),
    trialType = ifelse(trialType == "international (hybrid)", "other", trialType),
    trialType = ifelse(trialType == "foreign", "other", trialType),
    trialType = ifelse(trialType == "international", "other", trialType)
  ) %>%
  select(yearStart, trialType, SGBV)

to_plot %>%
  filter(trialType %in% c("domestic", "don't know")) %>%
  group_by(yearStart) %>%
  summarise(
    count = n(),
    SGBV = sum(SGBV)
  ) %>%
  ggplot() +
  geom_line(aes(x = yearStart, y = count), linewidth = 1) +
  geom_area(aes(x = yearStart, y = SGBV), fill = "#565AB6") +
  xlab("") +
  ylab("") +
  theme_bw() +
  # scale_colour_manual(values = c("SGBV" = "red")) +
  theme(legend.position = "bottom")
ggsave("descriptives/trials_domestic_SGBV.png", plot = last_plot(), width = 6, height = 4)

to_plot %>%
  filter(trialType == "other") %>%
  group_by(yearStart) %>%
  summarise(
    count = n(),
    SGBV = sum(SGBV)
  ) %>%
  ggplot() +
  geom_line(aes(x = yearStart, y = count), linewidth = 1) +
  geom_area(aes(x = yearStart, y = SGBV, fill = SGBV), fill = "#565AB6") +
  xlab("") +
  ylab("") +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank()
  ) +
  ggtitle("International (incl Hybrid) & Foreign Trials addressing SGBV")
ggsave("descriptives/trials_intl_foreign_SGBV.png", plot = last_plot(), width = 6, height = 4)

### International & Hybrid Trials

db[["Trials"]] %>%
  filter(trialType %in% c("international (hybrid)", "international")) %>%
  mutate(trialType = ifelse(trialType == "international (hybrid)", "hybrid", trialType)) %>%
  select(yearStart, trialType) %>%
  group_by(yearStart, trialType) %>%
  reframe(count = n()) %>%
  ggplot() +
  geom_line(aes(x = yearStart, y = count, col = trialType), linewidth = 1) +
  scale_colour_manual(
    values = c("#089A82", "#565AB6"),
    limits = c("international", "hybrid"),
    aesthetics = c("colour", "fill")
  ) +
  xlab("") +
  ylab("") +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank()
  ) +
  ggtitle("International & Hybrid Trials")
ggsave("descriptives/trials_intl_hybrid.png", plot = last_plot(), width = 6, height = 4)

### defendants

db[["Trials"]] %>%
  filter(fitsConflictTJ > 0 | humanRights > 0 | HRs_charges > 0) %>%
  mutate(
    rank = case_when(
      anyHighRank == 1 ~ "high",
      anyHighRank == 0 ~ "low"
    ),
    affiliation = case_when(
      anyStateAgent == 1 ~ "state agents",
      anyOpposedToGov == 1 ~ "opposition",
      anyStateAgent == 0 & anyOpposedToGov == 0 ~ "unknown"
    )
  ) %>%
  filter(affiliation != "unknown") %>%
  group_by(rank, affiliation) %>%
  tally() %>%
  spread(affiliation, n) %>%
  ungroup() %>%
  gt()

### TCs

## if used, needs to be revisited
df %>%
  mutate(across(any_of(c("tcs_victim_process_beg", "tcs_powers_beg")), UnitScale)) %>%
  select(year, tcs_metcriteria_created, tcs_public_process_beg, tcs_victim_process_beg, tcs_powers_beg) %>%
  group_by(year) %>%
  reframe(across(everything(), ~ mean(.x, na.rm = TRUE))) %>%
  mutate(across(
    all_of(c("tcs_public_process_beg", "tcs_victim_process_beg", "tcs_powers_beg")),
    ~ ifelse(tcs_metcriteria_created > 0, .x / tcs_metcriteria_created, 0)
  )) %>%
  select(year, tcs_public_process_beg, tcs_victim_process_beg, tcs_powers_beg) %>%
  filter(year > 1971) %>%
  pivot_longer(cols = -year) %>%
  mutate(name = case_when(
    name == "tcs_public_process_beg" ~ "Public process index",
    name == "tcs_victim_process_beg" ~ "Victim-centered process index",
    name == "tcs_powers_beg" ~ "Powers index"
  )) %>%
  ggplot() +
  geom_line(aes(x = year, y = value, color = name)) +
  scale_colour_manual(
    values = c("#089A82", "#565AB6","black"),
    limits = c("Victim-centered process index", "Public process index", "Powers index"),
    aesthetics = c("colour", "fill")
  ) +
  theme_bw() +
  ylab("Average index value") +
  theme(
    axis.title.x = element_blank(),
    legend.title = element_blank(),
    legend.position = "inside", 
    legend.position.inside = c(0.99, 0.01),
    legend.justification = c(0.99, 0.01),
    legend.background = element_rect(fill = "white"),
    legend.key.height = unit(10, "pt")
  )
ggsave("descriptives/tcs_process.png", plot = last_plot(), width = 6, height = 4)


### TCs reports
# could later use 3 point scale for policy recommendation

reports <- db[["TruthCommissions"]] |>
  # filter(authorizedByState == 1 & temporaryBodyReport == 1 & ## met criteria
  #          focusedPast == 1 & investigatePatternAbuse == 1) |> 
  mutate(
    issued = case_when(finalReportIssued == "Yes" ~ 1, TRUE ~ 0),
    public = case_when(reportPubliclyAvailable == "Yes" ~ 1, TRUE ~ 0),
    responsibility = case_when(reportEstablishResponsibility == "Yes" ~ 1, TRUE ~ 0), 
    publishnames = case_when(perpetratorNamesPublished == "Yes" ~ 1, TRUE ~ 0)
  ) |> 
  rename(
    year = yearCompleteOperation, 
    rec_prosecute = recommendProsecutions,
    rec_repair = recommendReparations,
    rec_reform = reportRecommendInstitutionalReform,
    ref_legal = legalReform,
    ref_judicial = judicialReforms,
    ref_hrs = humanRightsReforms,
    ref_vetting = vetting,
    ref_ssr = SecuritySectorReforms,
    ref_gender = genderReform,
    ref_corruption = corruptionReforms
  ) |>
  filter(year < 2021 & issued == 1) 

reports_yr <- reports |> 
  reframe(.by = year,
          across(c(issued, public, responsibility, publishnames, rec_prosecute, 
                   rec_repair, rec_reform, ref_legal, ref_judicial, ref_hrs, 
                   ref_vetting, ref_ssr, ref_gender, ref_corruption),
                 ~ sum(.x, na.rm = TRUE))
  ) |> 
  arrange(year) |> 
  full_join(expand_grid(year = 1970:2020), by = "year") |>
  arrange(year) |>
  mutate(across(!year, ~ ifelse(is.na(.x), 0, .x))) |>
  full_join(
    df %>%
      reframe(.by = year, 
              recommendations = sum(tcs_recommendations_beg)),
    by = "year"
  ) %>% 
  mutate(recommendations = ifelse(issued > 0, recommendations / issued, NA),
         # recommendations = ifelse(issued > 0, recommendations / issued, NA) * 20,
         recommendations = ifelse(is.na(recommendations), 0, recommendations))

reports_decade <- reports |> 
  mutate(decade = paste(str_sub(as.character(year), start = 1, end = 3), "0s", sep = ""), 
         decade = ifelse(decade == "2020s", "2010s", decade) ) |>
  reframe(.by = decade,
          across(c(issued, public, responsibility, publishnames, rec_prosecute, 
                   rec_repair, rec_reform, ref_legal, ref_judicial, ref_hrs, 
                   ref_vetting, ref_ssr, ref_gender, ref_corruption),
                 ~ sum(.x, na.rm = TRUE))
          ) |> 
  arrange(decade) |> 
  full_join(
    df %>%
      mutate(decade = paste(str_sub(as.character(year), start = 1, end = 3), "0s", sep = ""), 
             decade = ifelse(decade == "2020s", "2010s", decade)) |>
      reframe(.by = decade, 
              recommendations = sum(tcs_recommendations_beg)),
    by = "decade"
  ) %>% 
  mutate(recommendations = ifelse(issued > 0, recommendations / issued, NA),
         recommendations = ifelse(is.na(recommendations), 0, recommendations))

reports_yr |> 
  pivot_longer(c(issued, responsibility, publishnames) ) |> 
  mutate(
    name = factor(name, 
                  levels = c("issued", "responsibility", "publishnames"), 
                  labels = c("reports issued", "established\nrepsonsibility", 
                             "published\nperpetrator names"))
  ) |> 
  ggplot() + 
  geom_line(aes(x = year, y = value, group = name, col = name), col = "black") + 
  facet_grid(name ~ ., scales = "fixed") +
  ylab("Number of final reports") +
  theme_bw() + 
  theme(axis.title.x = element_blank(), 
        legend.position = "none") 
ggsave("descriptives/tcs_responsibility.png", plot = last_plot(), width = 6, height = 4)

reports_decade |> 
  mutate(responsibility = responsibility / issued * 100, 
         publishnames = publishnames / issued * 100, 
         ) |> 
  select(decade, issued, responsibility, publishnames) |>
  pivot_longer(c(issued, responsibility, publishnames) ) |> 
  mutate(
    name = factor(name, 
                  levels = c("issued", "responsibility", "publishnames"), 
                  labels = c("number of\nreports issued", "percentage of reports\nthat established\nrepsonsibility", 
                             "percentage of reports\nthat published\nperpetrator names"))
    ) |> 
  ggplot() + 
  geom_line(aes(x = decade, y = value, group = name, col = name), col = "black") + 
  facet_grid(name ~ ., scales = "free_y") +
  ylab("Percentage of final reports") +
  theme_bw() + 
  theme(axis.title = element_blank(), 
        legend.position = "none") 
ggsave("descriptives/tcs_responsibility_perc.png", plot = last_plot(), width = 6, height = 4)

reports_yr |> 
  pivot_longer(c(issued, rec_prosecute, rec_repair, rec_reform)) |> 
  mutate(name = factor(name, 
                       levels = c("issued", "rec_prosecute", "rec_repair", "rec_reform"), 
                       labels = c("reports issued", "recommending proscutions", 
                                  "recommending reparations", "recommending reforms"))
  ) |> 
  ggplot() + 
  geom_line(aes(x = year, y = value, col = name), col = "black") + 
  facet_wrap(name ~ ., scales = "fixed", ncol = 1) +
  ylab("Number of final reports") +
  theme_bw() + 
  theme(axis.title.x = element_blank(), 
        legend.title = element_blank(), 
        legend.position = "none")
ggsave("descriptives/tcs_recs_types.png", plot = last_plot(), width = 6, height = 4)
  
reports_decade |> 
  mutate(
    rec_prosecute = rec_prosecute / issued * 100,
    rec_repair = rec_repair / issued * 100,
    rec_reform = rec_reform / issued * 100) |>
  select(decade, rec_prosecute, rec_repair, rec_reform) |> 
  pivot_longer(c(rec_prosecute, rec_repair, rec_reform)) |> 
  mutate(
    name = factor(name, 
                  levels = c("rec_prosecute", "rec_repair", "rec_reform"), 
                  labels = c("percentage of reports that recommended prosecutions", 
                             "percentage of reports that recommended reparations", 
                             "percentage of reports that recommended reforms"))
  ) |> 
  ggplot() + 
  geom_line(aes(x = decade, y = value, group = name, col = name), col = "black") + 
  facet_wrap(name ~ ., scales = "free_y", ncol = 1) +
  ylab("Number of final reports") +
  theme_bw() + 
  theme(axis.title = element_blank(), 
        legend.title = element_blank(), 
        legend.position = "none")
  ggsave("descriptives/tcs_recs_types_perc.png", plot = last_plot(), width = 6, height = 4)
  
reports_yr |> 
  pivot_longer(c(issued, ref_legal, ref_judicial, ref_hrs, ref_vetting, 
                 ref_ssr, ref_gender, ref_corruption)) |> 
  mutate(name = factor(name, 
                       levels = c("issued", "ref_legal", "ref_judicial", 
                                  "ref_hrs", "ref_vetting", "ref_ssr", 
                                  "ref_gender", "ref_corruption"), 
                       labels = c("reports issued", "legal reform", 
                                  "judicial reform", "human rights reform", 
                                  "vetting", "security sector reform", 
                                  "gender reform", "corruption reform"))
  ) |> 
  ggplot() + 
  geom_line(aes(x = year, y = value, col = name), col = "black") + 
  facet_wrap(name ~ ., scales = "fixed", ncol = 2, nrow = 4) +
  ylab("Number of final reports") +
  theme_bw() + 
  theme(axis.title.x = element_blank(), 
        legend.title = element_blank(), 
        legend.position = "none") 
ggsave("descriptives/tcs_ref_types.png", plot = last_plot(), width = 6, height = 4)

recs <- reports_decade |> 
  mutate(across(c(rec_prosecute, rec_repair, ref_legal, ref_judicial, 
                  ref_ssr, ref_gender), 
                ~ .x / issued * 100)
         ) |>
  pivot_longer(c(rec_prosecute, rec_repair, ref_legal, ref_judicial, 
                 ref_ssr, ref_gender)) |> 
  mutate(name = factor(name, 
                       levels = c("rec_prosecute", "rec_repair", "ref_legal", 
                                  "ref_judicial", "ref_ssr", "ref_gender"), 
                       labels = c("prosecutions", "reparations", "legal reform", 
                                  "judicial reform", "security sector reform", 
                                  "gender reform"))
  ) |> 
  ggplot() + 
  geom_line(aes(x = decade, y = value, group = name), col = "black") + 
  geom_text(aes(x = "2000s", y = 10, label = name), size = 3) +
  facet_grid(name ~ ., scales = "fixed") +
  ylab("percentage of reports that recommend:") +
  theme_bw() + 
  theme(axis.title.x = element_blank(), 
        strip.text.y = element_blank(), 
        legend.title = element_blank(), 
        legend.position = "none") 
recs
ggsave("descriptives/tcs_ref_types_perc.png", plot = recs, width = 4, height = 6)

reports_yr |>
  select(year, issued, public) |>
  mutate(issued = issued - public) |> 
  pivot_longer(cols = !year) |>
  mutate(name = case_when(
    name == "issued" ~ "final report not public",
    name == "public" ~ "public final report"
  )) |>
  ggplot() +
  geom_bar(aes(x = year, y = value, fill = name), stat = "identity") +
  geom_line(
    data = reports_yr |>
      select(year, recommendations) |>
      filter(!is.na(recommendations)) ,
    aes(x = year, y = zoo::rollmean(recommendations, k = 5, fill = NA)), 
    # aes(x = year, y = recommendations)
    col = "#565AB6"
  ) +
  scale_fill_manual(values = c("final report not public" = "darkgray", 
                               "public final report" = "lightgray")) +
  theme_bw() +
  ylab("number of reports") +
  theme(
    legend.title = element_blank(),
    legend.position = "inside",
    legend.position.inside = c(0.1, 0.99),
    legend.justification = c(0.1, 0.99),
    legend.background = element_rect(fill = "white"),
    axis.title.x = element_blank(),
    legend.key.size = unit(10, "pt"), # change legend key size
    legend.key.height = unit(10, "pt"), # change legend key height
    legend.key.width = unit(10, "pt")
  ) +
  scale_y_continuous(
    breaks = c(0, 2, 4, 6, 8),
    sec.axis = sec_axis(
      # transform = ~ . / 20,
      transform = ~ .,
      name = "rolling average reform recommendations index"
    )
  )
### data release Figure 2?
ggsave("descriptives/tcs_ref_recs.png", plot = last_plot(), width = 6, height = 4)

reports_decade |>
  select(decade, issued, public) |>
  mutate(perc = public/issued * 100) 

reps <- reports_decade |>
  select(decade, issued, public) |>
  mutate(issued = issued - public) |> 
  pivot_longer(cols = !decade) |>
  mutate(name = case_when(
    name == "issued" ~ "final report not public",
    name == "public" ~ "public final report"
  )) |>
  ggplot() +
  geom_bar(aes(x = decade, y = value, fill = name), stat = "identity") +
  scale_fill_manual(values = c("final report not public" = "#565AB6", 
                               "public final report" = "#089A82")) +
  theme_bw() + 
  ylab("number of reports") +
  theme(
    legend.title = element_blank(),
    legend.position = "inside",
    legend.position.inside = c(0.1, 0.99),
    legend.justification = c(0.1, 0.99),
    legend.background = element_rect(fill = "white"),
    axis.title.x = element_blank(),
    legend.key.size = unit(10, "pt"), # change legend key size
    legend.key.height = unit(10, "pt"), # change legend key height
    legend.key.width = unit(10, "pt")
  ) 
reps
ggsave("descriptives/tcs_reports.png", plot = reps, width = 6, height = 4)

layout <- c(
  area(t = 1, l = 1, b = 1, r = 1),
  area(t = 2, l = 1, b = 4, r = 1)
)
tcs <- reps + recs +
  plot_layout(design = layout)
tcs
ggsave("descriptives/tcs.png", plot = tcs, width = 6, height = 8)

## Reparations

db[["Reparations"]] |>
  mutate(
    individual = case_when(
      individualReparations == "yes" ~ 1,
      TRUE ~ 0
    ),
    collective = case_when(
      collectiveReparations == "yes" ~ 1,
      TRUE ~ 0
    ),
    compensation = case_when(
      str_detect(individualReparationsForm, "compensation") | str_detect(collectiveReparationsForm, "compensation") ~ 1,
      TRUE ~ 0
    ),
    symbolic = case_when(
      str_detect(individualReparationsForm, "symbolic") | str_detect(collectiveReparationsForm, "symbolic") ~ 1,
      TRUE ~ 0
    )
  ) %>%
  select(yearCreated, individual, collective, compensation, symbolic) %>%
  mutate(onlyIndividual = ifelse(individual == 1 & collective == 0, 1, 0), 
         onlyCompensation = ifelse(compensation == 1 & symbolic == 0, 1, 0)) |>
  reframe(individual = sum(individual), 
          onlyIndividual = sum(onlyIndividual),
          collective = sum(collective), 
          compensation = sum(compensation), 
          symbolic = sum(symbolic), 
          onlyCompensation = sum(onlyCompensation)) 

### Amnesties

peace <- db[["Amnesties"]] %>%
  select(amnestyYear, peaceSettlement) %>%
  mutate(peaceSettlement = case_when(
    peaceSettlement == 0 ~ "no",
    peaceSettlement == 1 ~ "yes"
  )) %>%
  group_by(peaceSettlement) %>%
  reframe(n = n()) %>%
  ggplot(aes(x = reorder(peaceSettlement, n), y = n, fill = peaceSettlement)) +
  geom_bar(position = position_dodge(), stat = "identity") +
  scale_fill_manual(values = c("yes" = "#565AB6", "no"= "#089A82")) +
  # scale_fill_manual(values = c("yes" = "#F8766D", "no"= "darkgray")) +
  # scale_fill_manual(values = c("yes" = "darkgray", "no" = "lightgray")) +
  expand_limits(y = 850) +
  geom_text(aes(label = n), vjust = -0.2, position = position_dodge(0.9), size = 2, color = "black") +
  theme_bw() +
  theme(
    panel.border = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(hjust = 1),
    title = element_text(size = 10),
    legend.position = "none"
  ) +
  ggtitle("Was the amnesty part\nof a peace agreement?")

hrs <- db[["Amnesties"]] %>%
  select(amnestyYear, hrCrimes) %>%
  mutate(
    hrCrimes = ifelse(hrCrimes == "human rights crimes were included in amnesty", 1, 0),
    hrCrimes = case_when(
      hrCrimes == 1 ~ "yes",
      TRUE ~ "no"
    )
  ) %>%
  group_by(hrCrimes) %>%
  reframe(n = n()) %>%
  ggplot(aes(x = reorder(hrCrimes, n), y = n, fill = hrCrimes)) +
  geom_bar(position = position_dodge(), stat = "identity") +
  scale_fill_manual(values = c("yes" = "#565AB6", "no"= "#089A82")) +
  # scale_fill_manual(values = c("yes" = "#F8766D", "no"= "darkgray")) +
  # scale_fill_manual(values = c("yes" = "darkgray", "no" = "lightgray")) +
  expand_limits(y = 750) +
  geom_text(aes(label = n), vjust = -0.2, position = position_dodge(0.9), size = 2, color = "black") +
  theme_bw() +
  theme(
    panel.border = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(hjust = 1),
    title = element_text(size = 10),
    legend.position = "none"
  ) +
  ggtitle("Were human rights crimes\ninlcuded in the amnesty?")

who <- db[["Amnesties"]] %>%
  select(amnestyYear, whoWasAmnestied) %>%
  arrange(amnestyYear) %>%
  mutate(whoWasAmnestied = str_split(whoWasAmnestied, "; ")) %>%
  unnest(whoWasAmnestied) %>%
  mutate(
    whoWasAmnestied = str_replace(whoWasAmnestied, "collaborators", "other"),
    whoWasAmnestied = str_replace(whoWasAmnestied, "draft dodgers / deserters", "other"),
    whoWasAmnestied = str_replace(whoWasAmnestied, "regular convicts", "other"),
    whoWasAmnestied = str_replace(whoWasAmnestied, "armed opposition", "armed\nopposition"),
    whoWasAmnestied = str_replace(whoWasAmnestied, "state agents", "state\nagents"),
    whoWasAmnestied = str_replace(whoWasAmnestied, " / ", " &\n")
  ) %>%
  group_by(whoWasAmnestied) %>%
  reframe(n = n()) %>%
  ggplot(aes(y = reorder(whoWasAmnestied, n), x = n)) +
  # geom_bar(position = position_dodge(), stat = "identity", fill = "#F8766D") +
  geom_bar(position = position_dodge(), stat = "identity", fill = "#565AB6") +
  geom_text(aes(label = n), hjust = -0.1, position = position_dodge(0.9), size = 2, color = "black") +
  expand_limits(x = 600) +
  theme_bw() +
  theme(
    panel.border = element_blank(),
    legend.position = "bottom",
    legend.title = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major.y = element_blank(),
    plot.title = element_text(hjust = 1),
    title = element_text(size = 10)
  ) +
  ggtitle("Who was amnestied?")
  
# amnesty <- multi_panel_figure(columns = 2, rows = 2, panel_label_type = "none")
# amnesty %<>%
#   fill_panel(hrs, column = 1, row = 1) %<>%
#   fill_panel(who, column = 2, row = 1:2) %<>%
#   fill_panel(peace, column = 1, row = 2)
# amnesty

amnesty <- (hrs / peace) | who
amnesty
#### data release Figure 3
ggsave("descriptives/amnesty.png", plot = amnesty, width = 6, height = 4)






### transitional trials

df %>%
  select(year, tran_trs_dom_dtj_ctj_sta, tran_trs_dom_dtj_ctj_sta_hi) %>%
  group_by(year) %>%
  reframe(
    tran_trs_dom_dtj_ctj_sta = sum(tran_trs_dom_dtj_ctj_sta),
    tran_trs_dom_dtj_ctj_sta_hi = sum(tran_trs_dom_dtj_ctj_sta_hi)
  ) %>%
  ggplot() +
  geom_line(aes(x = year, y = tran_trs_dom_dtj_ctj_sta), linewidth = 1) +
  geom_area(aes(x = year, y = tran_trs_dom_dtj_ctj_sta_hi, fill = "high-ranking")) +
  scale_colour_manual(
    values = c("high-ranking" = "#565AB6"),
    aesthetics = c("colour", "fill")
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 1),
    axis.title = element_blank(), 
    legend.position = "inside", 
    legend.position.inside = c(0.01, 0.99),
    legend.justification = c(0.01, 0.99),
    legend.background = element_rect(fill = "white"),
    legend.title = element_blank()
  ) +
  ggtitle("Transitional human rights trials of state agents")
ggsave("descriptives/trials.png", plot = last_plot(), width = 6, height = 3)

### convictions

df %>%
  select(year, tran_cce_dom_dtj_ctj_sta, tran_cce_dom_dtj_ctj_sta_hi) %>%
  group_by(year) %>%
  reframe(
    tran_cce_dom_dtj_ctj_sta = sum(tran_cce_dom_dtj_ctj_sta),
    tran_cce_dom_dtj_ctj_sta_hi = sum(tran_cce_dom_dtj_ctj_sta_hi)
  ) %>%
  ggplot() +
  geom_line(aes(x = year, y = tran_cce_dom_dtj_ctj_sta), linewidth = 0.5) +
  geom_area(aes(x = year, y = tran_cce_dom_dtj_ctj_sta_hi, fill = "high-ranking")) +
  scale_colour_manual(
    values = c("high-ranking" = "#565AB6"),
    aesthetics = c("colour", "fill")
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 1),
    axis.title = element_blank(), 
    legend.position = "inside", 
    legend.position.inside = c(0.01, 0.99),
    legend.justification = c(0.01, 0.99),
    legend.background = element_rect(fill = "white"),
    legend.title = element_blank()
  ) +
  ggtitle("State agents convicted in transitional human rights trials")


df %>%
  select(year, tran_cce_dom_dtj_ctj_sta, tran_cce_dom_dtj_ctj_sta_hi) %>%
  group_by(year) %>%
  reframe(
    tran_cce_dom_dtj_ctj_sta = sum(tran_cce_dom_dtj_ctj_sta),
    tran_cce_dom_dtj_ctj_sta_hi = sum(tran_cce_dom_dtj_ctj_sta_hi)
  ) %>%
  ggplot() +
  geom_area(aes(x = year, y = tran_cce_dom_dtj_ctj_sta), fill = "darkgrey") +
  geom_area(aes(x = year, y = tran_cce_dom_dtj_ctj_sta_hi, fill = "high-ranking")) +
  scale_colour_manual(
    values = c("high-ranking" = "#565AB6"),
    aesthetics = c("colour", "fill")
  ) +
  theme_bw() +
  theme(
    plot.title = element_blank(),
    axis.title = element_blank(), 
    legend.position = "inside", 
    legend.position.inside = c(0.01, 0.99),
    legend.justification = c(0.01, 0.99),
    legend.background = element_rect(fill = "white"),
    legend.title = element_blank()
  ) +
  ggtitle("State agents convicted in transitional human rights trials")
ggsave("descriptives/convictions.png", plot = last_plot(), width = 6, height = 3)







df %>%
  select(year, tran_cce_dom_dtj_ctj_sta, tran_cce_dom_dtj_ctj_sta_hi) %>%
  group_by(year) %>%
  reframe(
    tran_cce_dom_dtj_ctj_sta = sum(tran_cce_dom_dtj_ctj_sta),
    tran_cce_dom_dtj_ctj_sta_hi = sum(tran_cce_dom_dtj_ctj_sta_hi)
  ) %>%
  mutate(prop = ifelse(tran_cce_dom_dtj_ctj_sta != 0, tran_cce_dom_dtj_ctj_sta_hi / tran_cce_dom_dtj_ctj_sta * 100, 0)) %>%
  ggplot() +
  geom_line(aes(x = year, y = prop), linewidth = 1) +
  xlab("") +
  ylab("Percentage of all convictions") +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 1),
    legend.position  = "inside", 
    legend.position.inside = c(0.01, 0.99),
    legend.justification = c(0.01, 0.99),
    legend.background = element_rect(fill = "white"),
    legend.title = element_blank()
  )
  # ggtitle("High-ranking state agents convicted in transitional human rights trials")
### data release Figure 4
ggsave("descriptives/highrankconvictions.png", plot = last_plot(), width = 6, height = 3)


### simultaneity

df %>%
  arrange(country, year) %>%
  mutate(
    trials = trs_int_sta + trs_int_opp + tran_trs_dom_dtj_ctj_sta,
    tcs = tcs_dtj_ctj_created + lag(tcs_dtj_ctj_created, 1) + lag(tcs_dtj_ctj_created, 2),
    amnesties = amnesty_dtj_ctj_sta + lag(amnesty_dtj_ctj_sta, 1) + lag(amnesty_dtj_ctj_sta, 2),
    reparations = rep_created + lag(rep_created, 1) + lag(rep_created, 2),
    post_tc = ifelse(tcs > 0, tcs, 0),
    post_amnesty = ifelse(amnesties > 0, amnesties, 0),
    post_reparation = ifelse(reparations > 0, reparations, 0),
    trials_post_tc = ifelse(post_tc > 0, trials, 0),
    trials_post_amnesty = ifelse(post_amnesty > 0, trials, 0),
    trials_post_reparation = ifelse(post_reparation > 0, trials, 0)
  ) %>%
  select(country, year, trials, trials_post_tc, trials_post_amnesty, trials_post_reparation) %>%
  group_by(year) %>%
  reframe(
    trials_post_tc = sum(trials_post_tc),
    trials_post_amnesty = sum(trials_post_amnesty),
    trials_post_reparation = sum(trials_post_reparation)
  ) %>%
  pivot_longer(cols = !year) %>%
  filter(!is.na(value)) %>%
  mutate(name = case_when(
    name == "trials_post_tc" ~ "trials within two years of truth commissions",
    name == "trials_post_amnesty" ~ "trials within two years of amnesties",
    name == "trials_post_reparation" ~ "trials within two years of reparations policies"
  )) %>%
  ggplot() +
  geom_line(aes(x = year, y = value, col = name), linewidth = 1) +
  scale_colour_manual(
    values = c("trials within two years of reparations policies" = "#089A82", 
               "trials within two years of truth commissions" = "#565AB6", 
               "trials within two years of amnesties" = "black"),
    aesthetics = c("colour", "fill")
  ) +
  ylab("Number of trials") +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 1), ,
    axis.title.x = element_blank(),
    legend.position = "inside", 
    legend.position.inside = c(0.01, 0.99),
    legend.justification = c(0.01, 0.99),
    legend.title = element_blank(),
    # legend.key.height = unit(10, 'pt') )
    legend.background = element_rect(fill = "white")
  ) +
  ggtitle("Human rights trials and other transitional justice")
ggsave("descriptives/simultaneity.png", plot = last_plot(), width = 6, height = 3)



### Syria comparison: needs to be recoded to use new CY measures

df |> 
  select(country_case, ccode_cow, year, outcome_victory_reb, v2regendtypems_5, 
         tcs_all_created, rep_paidout_created, 
         tran_trs_dom_dtj_ctj_sta, tran_trs_dom_dtj_ctj_sta_hi
         ) |>
  arrange(country_case, year) |> 
  group_by(country_case, ccode_cow) |>  
  mutate(
    rebvictory = ifelse(
      outcome_victory_reb > 0 | 
        (v2regendtypems_5 == 1 & (lead(v2regendtypems_5) == 0 | year == max(year))), 
      1, NA), 
    rebvictory = ifelse(
     (country_case == "Chile" & year == 1973) |
       (country_case == "Comoros" & year == 1989) |
       (country_case == "Ghana" & year == 1981) |
       (country_case == "Guinea-Bissau" & year == 1999) | 
       (country_case == "Haiti" & year == 1991) |
       (country_case == "Liberia" & year == 1980) |
       (country_case == "Paraguay" & year == 1989), 
     NA, rebvictory) 
    ) |> 
  fill(rebvictory, .direction = "down") |>
  mutate(rebvictory = ifelse(is.na(rebvictory), 0, rebvictory)) |> 
  filter(rebvictory == 1) |> 
  # select(country_case, year) |> 
  # reframe(beg = min(year), 
  #         end = max(year) ) |> 
  # print(n = Inf)
  reframe(tcs_all_created = sum(tcs_all_created), 
          rep_paidout_created = sum(rep_paidout_created), 
          tran_trs_dom_dtj_ctj_sta = sum(tran_trs_dom_dtj_ctj_sta),
          tran_trs_dom_dtj_ctj_sta_hi = sum(tran_trs_dom_dtj_ctj_sta_hi)) |> 
  left_join(db[["Trials"]] |> 
              filter(trialType == "international (hybrid)") |> 
              select(trialID, country_Accused, country_Trial) |> 
              group_by(country_Accused) |> 
              reframe(n = n()), 
            by = c("country_case" = "country_Accused") 
  ) |> 
  rename(hybrid = n) |> 
  left_join(db[["Trials"]] |> 
              filter(trialType == "international") |> 
              select(trialID, country_Accused, country_Trial) |> 
              group_by(country_Accused) |> 
              reframe(n = n()), 
            by = c("country_case" = "country_Accused") 
  ) |> 
  rename(intl = n) |> 
  left_join(db[["Vettings"]] |> 
              filter(is.na(alterationOf)) |> 
              filter(type_dismissal == 1 | type_ban == 1 | type_declassification == 1) |> 
              rename(year = yearStart) |> 
              group_by(ccode_cow) |> 
              reframe(vet = n()), 
            by = "ccode_cow") |>
  mutate(hybrid = ifelse(is.na(hybrid), 0, hybrid), 
         intl = ifelse(is.na(intl), 0, intl), 
         vet = ifelse(is.na(vet), 0, vet) 
         ) |> 
  # write_csv("~/Desktop/list_of_cases.csv") 
  reframe(hybrid = sum(hybrid, na.rm = TRUE),
          intl = sum(intl, na.rm = TRUE),
          vet = sum(vet, na.rm = TRUE),
          sum_tcs_all_created = sum(tcs_all_created), 
          sum_rep_paidout_created = sum(rep_paidout_created), 
          avg_tran_trs_dom_dtj_ctj_sta = mean(tran_trs_dom_dtj_ctj_sta),
          avg_tran_trs_dom_dtj_ctj_sta_hi = mean(tran_trs_dom_dtj_ctj_sta_hi)) |> 
  pivot_longer(everything()) |> 
  write_csv("~/Desktop/descriptives.csv") 

### global: needs to be recoded to use new CY measures
  
  df |> 
    filter(dtr == 1 | pco_25 == 1) |>  
    select(country_case, ccode_cow, year, 
           tcs_all_created, rep_paidout_created, 
           tran_trs_dom_dtj_ctj_sta, tran_trs_dom_dtj_ctj_sta_hi
    ) |>
    arrange(country_case, year) |> 
    group_by(country_case, ccode_cow) |>  
    reframe(tcs_all_created = sum(tcs_all_created), 
            rep_paidout_created = sum(rep_paidout_created), 
            tran_trs_dom_dtj_ctj_sta = sum(tran_trs_dom_dtj_ctj_sta),
            tran_trs_dom_dtj_ctj_sta_hi = sum(tran_trs_dom_dtj_ctj_sta_hi)) |> 
    left_join(db[["Trials"]] |> 
                filter(trialType == "international (hybrid)") |> 
                select(trialID, country_Accused, country_Trial) |> 
                group_by(country_Accused) |> 
                reframe(n = n()), 
              by = c("country_case" = "country_Accused") 
    ) |> 
    rename(hybrid = n) |> 
    left_join(db[["Trials"]] |> 
                filter(trialType == "international") |> 
                select(trialID, country_Accused, country_Trial) |> 
                group_by(country_Accused) |> 
                reframe(n = n()), 
              by = c("country_case" = "country_Accused") 
    ) |> 
    rename(intl = n) |> 
    left_join(db[["Vettings"]] |> 
                filter(is.na(alterationOf)) |> 
                filter(type_dismissal == 1 | type_ban == 1 | type_declassification == 1) |> 
                rename(year = yearStart) |> 
                group_by(ccode_cow) |> 
                reframe(vet = n()), 
              by = "ccode_cow") |>
    mutate(hybrid = ifelse(is.na(hybrid), 0, hybrid), 
           intl = ifelse(is.na(intl), 0, intl), 
           vet = ifelse(is.na(vet), 0, vet) 
    ) |> 
    # print(n = Inf)
    reframe(hybrid = sum(hybrid, na.rm = TRUE),
            intl = sum(intl, na.rm = TRUE),
            vet = sum(vet, na.rm = TRUE),
            sum_tcs_all_created = sum(tcs_all_created), 
            sum_rep_paidout_created = sum(rep_paidout_created), 
            avg_tran_trs_dom_dtj_ctj_sta = mean(tran_trs_dom_dtj_ctj_sta),
            avg_tran_trs_dom_dtj_ctj_sta_hi = mean(tran_trs_dom_dtj_ctj_sta_hi)) |> 
    pivot_longer(everything()) |> 
  write_csv("~/Desktop/global.csv") 
  
  
  
  
  
  
  
  
  
  
  
  
  
  library(tidyverse)
  library(pglm)
  library(survival)
  library(bife)
  library(sandwich)
  library(lmtest)
  
  df <- read_csv("~/Documents/GitHub/tjet-db/tjet_datasets/tjet_cy_analyses.csv") %>% 
    mutate(vet_count = vet_dismiss_created + vet_ban_created + vet_declass_created,
           vet_dv = ifelse(vet_count > 0, 1, 0),
           amn_dv = ifelse(amnesty_dtj_ctj_sta > 0, 1, 0)
    ) |>
    group_by(country_case) |>
    mutate(lag_gdppc_log = lag(latent_gdppc_wdi_mean_log),
           trials_cumu = cumsum(lag(tran_trs_dom_dtj_ctj_sta)),
           tcs_cumu = cumsum(lag(tcs_all_created)),
           rep_cumu = cumsum(lag(rep_paidout_created)),
           vet_cumu = cumsum(lag(vet_count)),
           amn_cumu = cumsum(lag(amnesty_dtj_ctj_sta)),
           inv_cumu = cumsum(lag(uninv_beg)),
           tcs_dv = ifelse(tcs_all_created > 0, 1, 0),
           tcs_iv = lag(tcs_all_binary),
           rep_dv = ifelse(rep_paidout_created > 0, 1, 0),
           rep_iv = lag(rep_paidout),
           amn_iv = ifelse(amn_cumu > 0, 1, 0),
           vet_iv = ifelse(vet_cumu > 0, 1, 0),
           inv_dv = ifelse(uninv_beg > 0, 1, 0),
           inv_iv = ifelse(inv_cumu > 0, 1, 0)
    ) |>
    ungroup() |>
    mutate() |>
    filter(dtr == 1 | aco_25 == 1) |>
    filter(year < 2021) |>
    select(country_case, year, tran_trs_dom_dtj_ctj_sta, trials_cumu, tcs_dv, 
           tcs_iv, tcs_cumu, rep_dv, rep_iv, rep_cumu, amn_dv, amn_iv, 
           amn_cumu, vet_dv, vet_iv, vet_cumu, inv_dv, inv_iv, inv_cumu)
  
  df |>
    summary()
  
  ### trials
  pglm(
    tran_trs_dom_dtj_ctj_sta ~ amn_iv + rep_iv + tcs_iv + vet_iv + inv_iv,
    data = df, effect = "individual", model = "within",
    family = poisson, index = c("country_case", "year")
  ) |>
    summary()
  
  mod_poisson <- glm(tran_trs_dom_dtj_ctj_sta ~ amn_iv + rep_iv + tcs_iv + vet_iv + inv_iv,
                     data = na.omit(df), family = poisson)
  mod_poisson_cl <- coeftest(mod_poisson, vcov. = vcovCL(mod_poisson, cluster = na.omit(df)$country_case, type = "HC0"))
  mod_poisson_cl
  
  mod2 <- pglm(
    tran_trs_dom_dtj_ctj_sta ~ amn_iv + rep_iv + tcs_iv + vet_iv + inv_iv,
    data = df, effect = "individual", model = "within",
    family = negbin, index = c("country_case", "year")
  ) |>
    summary()
  
  mod_nb <- MASS::glm.nb(tran_trs_dom_dtj_ctj_sta ~ amn_iv + rep_iv + tcs_iv + vet_iv + inv_iv,
                         data = na.omit(df))
  mod_nb_cl <- coeftest(mod_nb, vcov. = vcovCL(mod_nb, cluster = na.omit(df)$country_case, type = "HC0"))
  mod_nb_cl
  pchisq(2 * (as.numeric(logLik(mod_nb)) - as.numeric(logLik(mod_poisson))), df = 1, lower.tail = FALSE)
  
  ### TCs
  mod_tcs <- glm(tcs_dv ~ trials_cumu + rep_iv + vet_iv + amn_iv + inv_iv,
                 data = na.omit(df), family = binomial)
  mod_tcs_cl <- coeftest(mod_tcs, vcov. = vcovCL(mod_tcs, cluster = na.omit(df)$country_case, type = "HC0"))
  mod_tcs_cl
  
  bife(
    tcs_dv ~ trials_cumu + rep_iv + vet_iv + amn_iv + inv_iv | country_case,
    data  = na.omit(df),
    model = "logit"
  ) |> summary()
  
  clogit(
    tcs_dv ~ trials_cumu + rep_iv + vet_iv + amn_iv + inv_iv + strata(country_case),
    data = na.omit(df)
  )
  
  ### reparations
  mod_rep <- glm(rep_dv ~ trials_cumu + tcs_iv + vet_iv + amn_iv + inv_iv,
                 data = na.omit(df), family = binomial)
  mod_rep_cl <- coeftest(mod_rep, vcov. = vcovCL(mod_rep, cluster = na.omit(df)$country_case, type = "HC0"))
  mod_rep_cl
  
  bife(
    rep_dv ~ trials_cumu + tcs_iv + vet_iv + amn_iv + inv_iv | country_case,
    data  = na.omit(df),
    model = "logit"
  ) |> summary()
  
  clogit(
    rep_dv ~ trials_cumu + tcs_iv + vet_iv + amn_iv + inv_iv + strata(country_case),
    data = na.omit(df)
  )
  
  ### amnesties
  mod_amn <- glm(amn_dv ~ trials_cumu + rep_iv + tcs_iv + vet_iv + inv_iv,
                 data = na.omit(df), family = binomial)
  mod_amn_cl <- coeftest(mod_amn, vcov. = vcovCL(mod_amn, cluster = na.omit(df)$country_case, type = "HC0"))
  mod_amn_cl
  
  bife(
    amn_dv ~ trials_cumu + rep_iv + tcs_iv + vet_iv + inv_iv | country_case,
    data  = na.omit(df),
    model = "logit"
  ) |> summary()
  
  clogit(
    amn_dv ~ trials_cumu + rep_iv + tcs_iv + vet_iv + inv_iv + strata(country_case),
    data = na.omit(df)
  )
  
  ### vetting
  mod_vet <- glm(vet_dv ~ trials_cumu + rep_iv + tcs_iv + amn_iv + inv_iv,
                 data = na.omit(df), family = binomial)
  mod_vet_cl <- coeftest(mod_vet, vcov. = vcovCL(mod_vet, cluster = na.omit(df)$country_case, type = "HC0"))
  mod_vet_cl
  
  bife(
    vet_dv ~ trials_cumu + rep_iv + tcs_iv + amn_iv + inv_iv | country_case,
    data  = na.omit(df),
    model = "logit"
  ) |> summary()
  
  clogit(
    vet_dv ~ trials_cumu + rep_iv + tcs_iv + amn_iv + inv_iv + strata(country_case),
    data = na.omit(df)
  )
  
  ### UN investigations 
  
  mod_inv <- glm(inv_dv ~ trials_cumu + rep_iv + tcs_iv + amn_iv + vet_iv,
                 data = na.omit(df), family = binomial)
  mod_inv_cl <- coeftest(mod_inv, vcov. = vcovCL(mod_inv, cluster = na.omit(df)$country_case, type = "HC0"))
  mod_inv_cl
  
  bife(
    inv_dv ~ trials_cumu + rep_iv + tcs_iv + amn_iv + vet_iv | country_case,
    data  = na.omit(df),
    model = "logit"
  ) |> summary()
  
  clogit(
    inv_dv ~ trials_cumu + rep_iv + tcs_iv + amn_iv + vet_iv + strata(country_case),
    data = na.omit(df)
  )
  
  
### 
  
  results <- list(
    "Amnesties" = mod_amn_cl,
    "Prosecutions" = mod_nb_cl,
    "Reparations" = mod_rep_cl,
    "Truth Commissions" = mod_tcs_cl,
    "UN Investigations" = mod_inv_cl,
    "Vetting" = mod_vet_cl)
  
  names(results) |>
    map(function(modname) {
      tibble(
        Variable = rownames(results[[modname]]),
        est = format(results[[modname]][, 1], justify = "right", digits = 1, nsmall = 3, scientific = FALSE),
        se = format(results[[modname]][, 2], justify = "right", digits = 1, nsmall = 3, scientific = FALSE),
        pvalue = format(results[[modname]][, 4], scientific = FALSE) ) |>
        mutate(
          stars = case_when(
            pvalue > 0.05 ~ "   ",
            pvalue <= 0.05 & pvalue > 0.01 ~ "*  ",
            pvalue <= 0.01 & pvalue > 0.001 ~ "** ",
            pvalue <= 0.001 ~ "***"
          ),
          result = paste(est, " (", se, ")", stars, sep = "")
        ) |>
        select(Variable, result) |>
        rename(!!modname := result)
    }) |>
    reduce(full_join, by = "Variable") |>
    mutate(Variable = case_when(
      Variable == "amn_iv" ~ "Amnesties",
      Variable == "rep_iv" ~ "Reparations policies",
      Variable == "vet_iv" ~ "Vetting policies",
      Variable == "tcs_iv" ~ "Truth commissions",
      Variable == "trials_cumu" ~ "Prosecutions",
      Variable == "inv_iv" ~ "UN Investigations",
      Variable == "(Intercept)" ~ "Intercept"
    )  ) |>
    arrange(Variable) |>
    filter(Variable != "Intercept") |>
    rename("Prior TJ" = "Variable") |>
    write_csv("~/Desktop/correlations.csv", na = "")

df |> 
  select(country_case, year, tcs_cumu, rep_dv) |> 
  filter(rep_dv > 0 & tcs_cumu > 0) |> 
  reframe(.by = country_case, 
          rep_year = min(year)) |> 
  print(n = Inf) 
