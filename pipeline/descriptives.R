library(tidyverse)
library(gt)
library(coefplot)
library(multipanelfigure)
library(cowplot)
library(patchwork)

# base <- "~/Documents/GitHub/tjet-db/tjet_datasets"
# base <- "../analyses_datasets"
base <- "tjet_datasets"

db <- list(Amnesties = read_csv(here::here(base, "tjet_amnesties.csv")),
           Reparations = read_csv(here::here(base, "tjet_reparations.csv")),
           TruthCommissions = read_csv(here::here(base, "tjet_tcs.csv")),
           Trials = read_csv(here::here(base, "tjet_trials.csv")),
           Accused = read_csv(here::here(base, "tjet_accused.csv")),
           Vettings = read_csv(here::here(base, "tjet_vettings.csv")),
           Investigations = read_csv(here::here(base, "tjet_un_investigations.csv")))

n_records <- nrow(db$Amnesties) + nrow(db$Trials) +
  nrow(db$Reparations) + nrow(db$TruthCommissions) +
  nrow(db$Accused) + nrow(db$Vettings)

db$Trials %>%
  summarize(finalConviction = sum(finalConviction))

db$Accused %>% select(lastSentencingTime) %>% 
  table() %>% 
  sum()

db$Amnesties %>% 
  select(hrCrimes) %>% 
  table() 

db$Amnesties %>%
  summarise(fitsConflictTJ = sum(fitsConflictTJ))

df <- read_csv(here::here(base, "tjet_cy_analyses.csv")) %>%
  mutate(gdp_per_capita = rgdpe_pwt / pop_pwt,
         gdp_per_capita_log = log(gdp_per_capita),
         pop_log = log(pop_pwt))

### rescaling
to_put_on_unit_scale <- c("theta_mean_fariss")
df %>%
  select(any_of(to_put_on_unit_scale)) %>%
  summary()
UnitScale <- function(col) {
  col <- col - min(col, na.rm = TRUE)
  col <- col / max(col, na.rm = TRUE)
  return(col)
}
df <- df %>%
  mutate(across(any_of(to_put_on_unit_scale), UnitScale)) %>%
  mutate(trials = trials_domestic + trials_intl + trials_foreign) %>%
  filter(year >= 1970 & year <= 2020)
df %>%
  select(any_of(to_put_on_unit_scale)) %>%
  summary()

### Global mechanisms
totals <- df %>%
  summarize(amnesties = sum(amnesties, na.rm = TRUE),
            trials = sum(trials, na.rm = TRUE),
            tcs = sum(tcs, na.rm = TRUE),
            reparations = sum(reparations, na.rm = TRUE),
            vettings = sum(vettings, na.rm = TRUE)) %>%
  pivot_longer(everything()) %>%
  mutate(name = str_replace(name, "amnesties", "Amnesties"),
         name = str_replace(name, "trials", "Trials"),
         name = str_replace(name, "tcs", "Truth Commissions"),
         name = str_replace(name, "reparations", "Reparations"),
         name = str_replace(name, "vettings", "Vettings")) %>%
  arrange(name) 

totals %>%
  gt() %>%
  tab_options(column_labels.hidden = TRUE) |>
  cols_label(name = "", value = "Total")

df1 <- df %>%
  group_by(region) %>%
  summarize(amnesties = sum(amnesties, na.rm = TRUE),
            trials = sum(trials, na.rm = TRUE),
            tcs = sum(tcs, na.rm = TRUE),
            reparations = sum(reparations, na.rm = TRUE)) %>%
  ungroup()

df1 %>%
  pivot_longer(cols = !region, values_to = "raw")

df1 %>%
  mutate(amnesties = round(amnesties / sum(amnesties) * 100, 1),
         trials = round(trials / sum(trials) * 100, 1),
         tcs = round(tcs / sum(tcs) * 100, 1),
         reparations = round(reparations / sum(reparations) * 100, 1)
         ) %>%
  pivot_longer(cols = !region) %>%
  full_join(df1 %>%
              pivot_longer(cols = !region, values_to = "raw"),
            by = c("region", "name")) %>%
  mutate(name = case_when(name == "amnesties" ~ "Amnesties",
                          name == "trials" ~ "Trials",
                          name == "tcs" ~ "Truth Commissions",
                          name == "reparations" ~ "Reparations") ) %>%
  ggplot(aes(fill = region, y = value, x = name)) +
    geom_bar(position = position_dodge(), stat = "identity", color = "black") +
    geom_text(aes(label= raw), vjust = -0.5, position = position_dodge(0.9), size = 2) +
    # scale_fill_grey(start = 0.8, end = 0.2) +
    # scale_colour_manual(values = c("orange", "black", "red", "blue", "darkgreen"),
    #                     aesthetics = c("colour", "fill")) +
    theme_bw() +
    theme(panel.border = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
    ylab("Percentage")
ggsave("descriptives/regions.png", plot = last_plot(), width = 6, height = 4)

### TRENDS

df %>%
  group_by(year) %>%
  summarize(amnesties = sum(amnesties, na.rm = TRUE),
            trials = sum(trials, na.rm = TRUE),
            tcs = sum(tcs, na.rm = TRUE),
            reparations = sum(reparations, na.rm = TRUE)) %>%
  pivot_longer(all_of(c("amnesties", "trials", "tcs", "reparations")),
               names_to = "key", values_to = "value") %>%
  mutate(key = str_replace(key, "amnesties", "Amnesties"),
         key = str_replace(key, "trials", "Trials"),
         key = str_replace(key, "tcs", "Truth Commissions"),
         key = str_replace(key, "reparations", "Reparations")) %>%
  ggplot() +
  geom_line(aes(x = as.integer(as.character(year)), y = value, col = key, group = key), linewidth = 1) +
  facet_grid(key ~ ., scales = "free_y") +
  scale_y_continuous(breaks = function(x) floor(max(x)), minor_breaks = NULL #,
                     # expand = expansion(mult = c(0, .1))
                     )  +
  scale_colour_manual(values = c("orange", "black", "red", "blue"),
                      limits = c("Reparations",
                                 "Amnesties",
                                 "Trials",
                                 "Truth Commissions"),
                      aesthetics = c("colour", "fill")) +
  ylab("") + xlab("") +
  theme_bw() +
  theme(legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(size = 20),
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 20),
        strip.text.y = element_blank(),
        plot.margin = margin(t = 8, r = 6, b = 0, l = 0, unit = "pt")
        )
# guides(col = guide_legend(ncol = 3))
### data release Figure 1
ggsave("descriptives/trends.png", plot = last_plot(), width = 6, height = 4)

### Table 1

table1 <- list() 

table1[["Amnesties"]] <- db$Amnesties %>%
  filter(amnestyYear %in% 1970:2020) %>%
  arrange(country) %>%
  group_by(country) %>%
  reframe(n = n()) %>%
  # print(n = Inf)
  ungroup() %>%
  reframe(mech = "Amnesties",
          n = sum(n),
          ctry_n = n())

table1[["Reparations"]] <- db$Reparations %>%
  filter(yearCreated %in% 1970:2020) %>%
  arrange(country) %>%
  group_by(country) %>%
  reframe(n = n()) %>%
  # print(n = Inf)
  ungroup() %>%
  reframe(mech = "Reparations",
          n = sum(n),
          ctry_n = n())


table1[["Truth Commissions"]] <- db$TruthCommissions %>%
  filter(yearPassed %in% 1970:2020) %>%
  filter(authorizedByState == 1 & temporaryBodyReport == 1 & focusedPast == 1 & investigatePatternAbuse == 1) %>%
  arrange(country) %>%
  group_by(country) %>%
  reframe(n = n()) %>%
  # print(n = Inf)
  ungroup() %>%
  reframe(mech = "Truth Commissions",
          n = sum(n),
          ctry_n = n())

table1[["Criminal prosecutions"]] <- db$Trials %>%
  filter(yearStart %in% 1970:2020) %>%
  arrange(country_Accused) %>%
  group_by(country_Accused) %>%
  reframe(n = n()) %>%
  # print(n = Inf)
  reframe(mech = "Criminal prosecutions",
          n = sum(n),
          ctry_n = n())

table1[["Trial types"]] <- db$Trials %>%
  filter(yearStart %in% 1970:2020) %>%
  mutate(trialType = ifelse(trialType == "international (hybrid)", "international", trialType) ) %>%
  arrange(country_Accused, trialType) %>%
  group_by(country_Accused, trialType) %>%
  reframe(n = n()) %>%
  ungroup() %>%
  group_by(trialType) %>%
  reframe(n = sum(n),
          ctry_n = n())  %>%
  rename(mech = trialType) 

table1[["Vetting policies"]] <- db$Vettings %>%
  filter(yearStart %in% 1970:2020) %>%
  filter(is.na(alterationOf)) %>%
  arrange(country) %>%
  group_by(country) %>%
  reframe(n = n()) %>%
  # print(n = Inf)
  ungroup() %>%
  reframe(mech = "Vetting policies",
          n = sum(n),
          ctry_n = n())

table1[["UN Investigations"]] <- db$Investigations %>%
  filter(beg %in% 1970:2020) %>%
  arrange(country) %>%
  group_by(country) %>%
  reframe(n = n()) %>%
  ungroup() %>%
  # print(n = Inf)
  reframe(mech = "UN Investigations",
          n = sum(n),
          ctry_n = n())

table1 <- table1 |> 
  bind_rows() |> 
  write_csv("descriptives/table1.csv") 
  
### Table 3 

table3 <- list() 

table3[["domestic"]] <- df %>%
  filter(year >= 1970 & year <= 2020) |>  
  reframe(regu_trs_dom_sta = sum(regu_trs_dom_sta, na.rm = TRUE), 
          tran_trs_dom_dtj = sum(tran_trs_dom_dtj, na.rm = TRUE), 
          tran_trs_dom_ctj = sum(tran_trs_dom_ctj, na.rm = TRUE), 
          oppo_trs_dom_sta_opp = sum(oppo_trs_dom_sta_opp, na.rm = TRUE)) |> 
  pivot_longer(everything(), names_to = "subtype", values_to = "n") |> 
  mutate(type = "domestic",
    subtype = case_when(
      subtype == "regu_trs_dom_sta" ~ "Regular human rights prosecutions", 
      subtype == "tran_trs_dom_dtj" ~ "Transitional human rights prosecutions", 
      subtype == "tran_trs_dom_ctj" ~ "Intrastate conflict prosecutions" , 
      subtype == "oppo_trs_dom_sta_opp" ~ "Opposition prosecutions") )


sum(table3[["domestic"]]$n)

table3[["foreign"]] <- db$Trials |> 
  filter(trialType == "foreign" & yearStart >= 1970 & yearStart <= 2020) |> 
  rename(subtype = jurisdiction) |> 
  mutate(subtype = ifelse(subtype == "territorial", "Territorial jurisdiction", subtype), 
         subtype = ifelse(subtype == "passive personality", "Passive personality jurisdiction", subtype), 
         subtype = str_to_sentence(subtype), 
         subtype = ifelse(is.na(subtype), "NOT CODED", subtype)) |> 
  group_by(subtype) |> 
  reframe(type = "foreign", 
          n = n()) 
  
table3[["international"]] <- db$Trials |> 
  filter(trialType %in% c("international (hybrid)", "international") & yearStart >= 1970 & yearStart <= 2020) |> 
  mutate(
    subtype = case_when(
      legalSystem %in% c("ICTR", "ICTY") ~ "Ad Hoc Tribunals", 
      legalSystem == "ICC" ~ "International Criminal Court", 
      legalSystem %in% c("ECCC", "Iraqi High Tribunal", "SCSL", "STL", "UNMIK/EULEX", "SPSC, Timor Leste") ~ "Hybrid Tribunals"), 
    ) |> 
  group_by(subtype) |> 
  reframe(type = "international", 
          n = n()) 

table3 <- table3 |> 
  bind_rows() |> 
  select(type, subtype, n) |> 
  write_csv("descriptives/table3.csv") 

### FROM HERE > 









# in figure, 6470, check against table 6513




### data release trends

df2 <- df %>% 
  group_by(year) %>%
  reframe(amnesties = sum(amnesties, na.rm = TRUE),
          trials = sum(trials, na.rm = TRUE),
          trials_domestic = sum(trials_domestic, na.rm = TRUE),
          trials_intl = sum(trials_intl, na.rm = TRUE),
          trials_foreign = sum(trials_foreign, na.rm = TRUE),
          tcs = sum(tcs, na.rm = TRUE),
          reparations = sum(reparations, na.rm = TRUE),
          vettings = sum(vettings, na.rm = TRUE),
          uninv = sum(uninv, na.rm = TRUE))

totals <- df2 %>%
  reframe(amnesties = sum(amnesties, na.rm = TRUE),
          trials = sum(trials, na.rm = TRUE),
          trials_domestic = sum(trials_domestic, na.rm = TRUE),
          trials_intl = sum(trials_intl, na.rm = TRUE),
          trials_foreign = sum(trials_foreign, na.rm = TRUE),
          tcs = sum(tcs, na.rm = TRUE),
          reparations = sum(reparations, na.rm = TRUE),
          vettings = sum(vettings, na.rm = TRUE),
          uninv = sum(uninv, na.rm = TRUE))

df2 %>%
  pivot_longer(all_of(c("amnesties", "trials", "tcs", "reparations", "vettings", "uninv")),
               names_to = "key", values_to = "value") %>%
  mutate(key = str_replace(key, "amnesties",
                           paste("Amnesties\n(n = ", totals$amnesties, ")", sep = "")),
         key = str_replace(key, "trials", paste("Prosecutions\n(n = ", totals$trials, ")", sep = "")),
         key = str_replace(key, "tcs", paste("Truth Commissions\n(n = ", totals$tcs, ")", sep = "")),
         key = str_replace(key, "reparations", paste("Reparations\n(n = ", totals$reparations, ")", sep = "")),
         key = str_replace(key, "vettings", paste("Vettings\n(n = ", totals$vettings, ")", sep = "")),
         key = str_replace(key, "uninv", paste("UN Investigations\n(n = ", totals$uninv, ")", sep = ""))) %>%
  ggplot() +
  # geom_line(aes(x = as.integer(as.character(year)), y = value, col = key, group = key), linewidth = 1) +
  geom_line(aes(x = as.integer(as.character(year)), y = value, group = key), linewidth = 1) +
  facet_grid(key ~ ., scales = "free_y") +
  scale_y_continuous(breaks = function(x) floor(max(x)), minor_breaks = NULL #,
                     # expand = expansion(mult = c(0, .1))
  ) +
  ylab("") + xlab("") +
  theme_bw() +
  theme(legend.position = "none",
        legend.title = element_blank(),
        # strip.text.y = element_blank()
        strip.text.y = element_text(angle = 0),
        # strip.background = element_rect(fill = "white"),
        plot.margin = margin(t = 8, r = 2, b = 0, l = 0, unit = "pt")
  )
ggsave("descriptives/trends_all.png", plot = last_plot(), width = 6, height = 4)

df %>%
  group_by(year) %>%
  summarize(trials = sum(tran_trs_dom_dtj_ctj_sta, na.rm = TRUE)) %>%
  ggplot() +
  geom_line(aes(x = as.integer(as.character(year)), y = trials), linewidth = 1) +
  theme_bw() +
  theme(legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(size = 20),
        axis.text = element_text(size = 20),
        axis.title = element_blank())
# ggsave("descriptives/tran_trs_dom_dtj_ctj_sta.pdf", plot = last_plot(), width = 6, height = 4)

# Trials over time

to_plot <- db$Trials %>%
  mutate(trialType = ifelse(trialType == "international (hybrid)", "other", trialType),
         trialType = ifelse(trialType == "foreign", "other", trialType),
         trialType = ifelse(trialType == "international", "other", trialType)) %>%
  select(yearStart, trialType)

to_plot %>%
  filter(trialType %in% c("domestic", "don't know")) %>%
  group_by(yearStart) %>%
  mutate(count = n()) %>%
  ggplot() +
  geom_line(aes(x = yearStart, y = count), linewidth = 1) +
  xlab("") + ylab("") +
  theme_bw() +
  theme(legend.position = "none",
        legend.title = element_blank(),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        title = element_text(size = 20) ) +
  ggtitle("Domestic Trials")
# ggsave("descriptives/domestic.pdf", plot = last_plot(), width = 6, height = 4)

to_plot %>%
  filter(trialType == "other") %>%
  group_by(yearStart) %>%
  mutate(count = n()) %>%
  ggplot() +
  geom_line(aes(x = yearStart, y = count), linewidth = 1) +
  xlab("") + ylab("") +
  theme_bw() +
  theme(legend.position = "none",
        legend.title = element_blank(),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        title = element_text(size = 20)) +
  ggtitle("International (incl Hybrid) & Foreign Trials")
# ggsave("descriptives/intl.pdf", plot = last_plot(), width = 6, height = 4)

# Trial Contexts and Types

db$Trials %>%
  mutate(HRs = ifelse(humanRights > 0 | HRs_charges > 0, 1, 0),
         trialType = ifelse(trialType == "foreign", "other", trialType),
         trialType = ifelse(trialType == "international (hybrid)", "other", trialType),
         trialType = ifelse(trialType == "international", "other", trialType)) %>%
  select(yearStart, HRs, fitsPostAutocraticTJ, fitsConflictTJ, trialType) %>%
  group_by(yearStart, trialType) %>%
  reframe(trials_HRs = sum(HRs, na.rm = TRUE),
            trials_trans = sum(fitsPostAutocraticTJ, na.rm = TRUE),
            trials_confl = sum(fitsConflictTJ, na.rm = TRUE)) %>%
  pivot_longer(all_of(c("trials_HRs", "trials_trans", "trials_confl")),
               names_to = "key", values_to = "value") %>%
  mutate(key = str_replace(key, "trials_HRs", "all human rights trials"),
         key = str_replace(key, "trials_trans", "transitional trials"),
         key = str_replace(key, "trials_confl", "conflict trials")) %>%
  filter(trialType == "domestic") %>%
  ggplot() +
  geom_line(aes(x = yearStart, y = value, col = key), linewidth = 1) +
  scale_colour_manual(values = c("orange", "red", "black"),
                      limits = c("all human rights trials",
                                 "transitional trials",
                                 "conflict trials"),
                      aesthetics = c("colour", "fill")) +
  xlab("") + ylab("") +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 20),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        title = element_text(size = 20)) +
  ggtitle("Domestic Trials")
# ggsave("descriptives/domestic_nexus.pdf", plot = last_plot(), width = 6, height = 4)

db$Trials %>%
  filter(trialType != "domestic") %>%
  mutate(trialType = ifelse(trialType == "international (hybrid)", "international", trialType)) %>%
  select(yearStart, trialType) %>%
  group_by(yearStart, trialType) %>%
  reframe(count = n()) %>%
  ggplot() +
  geom_line(aes(x = yearStart, y = count, col = trialType), linewidth = 1) +
  scale_colour_manual(values = c("orange", "red", "black"),
                      limits = c("international",
                                 "foreign"),
                      aesthetics = c("colour", "fill")) +
  xlab("") + ylab("") +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 20),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        title = element_text(size = 20)) +
  ggtitle("International (incl Hybrid) & Foreign Trials")
# ggsave("descriptives/intl_foreign.pdf", plot = last_plot(), width = 6, height = 4)

# Trials addressing SGBV over time

to_plot <- db$Trials %>%
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
  summarise(count = n(),
            SGBV = sum(SGBV)) %>%
  ggplot() +
  geom_line(aes(x = yearStart, y = count), linewidth = 1) +
  geom_area(aes(x = yearStart, y = SGBV), fill = "red") +
  xlab("") + ylab("") +
  theme_bw() +
  # scale_colour_manual(values = c("SGBV" = "red")) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 20),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        title = element_text(size = 20)) +
  ggtitle("Domestic addressing SGBV")
# ggsave("descriptives/domestic_SGBV.pdf", plot = last_plot(), width = 6, height = 4)

to_plot %>%
  filter(trialType == "other") %>%
  group_by(yearStart) %>%
  summarise(count = n(),
            SGBV = sum(SGBV)) %>%
  ggplot() +
  geom_line(aes(x = yearStart, y = count), linewidth = 1) +
  geom_area(aes(x = yearStart, y = SGBV, fill = SGBV), fill = "red") +
  xlab("") + ylab("") +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 20),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        title = element_text(size = 16)) +
  ggtitle("International (incl Hybrid), Foreign Trials addressing SGBV")
# ggsave("descriptives/intl_foreign_SGBV.pdf", plot = last_plot(), width = 6, height = 4)

# Extra: International & Hybrid Trials

db$Trials %>%
  filter(trialType %in% c("international (hybrid)", "international")) %>%
  mutate(trialType = ifelse(trialType == "international (hybrid)", "hybrid", trialType)) %>%
  select(yearStart, trialType) %>%
  group_by(yearStart, trialType) %>%
  reframe(count = n()) %>%
  ggplot() +
  geom_line(aes(x = yearStart, y = count, col = trialType), linewidth = 1) +
  scale_colour_manual(values = c("orange", "red", "black"),
                      limits = c("international",
                                 "hybrid"),
                      aesthetics = c("colour", "fill")) +
  xlab("") + ylab("") +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 20),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        title = element_text(size = 20)) +
ggtitle("International & Hybrid Trials")
# ggsave("descriptives/intl_hybrid.pdf", plot = last_plot(), width = 6, height = 4)

# Extra: Defendants

db$Trials %>%
  filter(fitsConflictTJ > 0 | humanRights > 0 | HRs_charges > 0) %>%
  mutate(rank = case_when(anyHighRank == 1 ~ "high",
                          anyHighRank == 0 ~ "low"),
         affiliation = case_when(anyStateAgent == 1 ~ "state agents",
                                 anyOpposedToGov == 1 ~ "opposition",
                                 anyStateAgent == 0 & anyOpposedToGov == 0 ~ "unknown") )  %>%
  filter(affiliation != "unknown") %>%
  group_by(rank, affiliation) %>%
  tally() %>%
  spread(affiliation, n) %>%
  ungroup() %>%
  gt()

## TCs

df %>%
  mutate(across(any_of(c("tcs_victim_process", "tcs_powers", "tcs_recommendations")), UnitScale)) %>%
  select(year, tcs_metcriteria, tcs_public_process, tcs_victim_process, tcs_powers) %>%
  group_by(year) %>%
  reframe(across(everything(), ~ mean(.x, na.rm = TRUE))) %>%
  mutate(across(all_of(c("tcs_public_process", "tcs_victim_process", "tcs_powers")),
                ~ ifelse(tcs_metcriteria > 0, .x/tcs_metcriteria, 0))) %>%
  select(year, tcs_public_process, tcs_victim_process, tcs_powers) %>%
  filter(year > 1971) %>%
  pivot_longer(cols = -year) %>%
  mutate(name = case_when(name == "tcs_public_process" ~ "Public process index",
                          name == "tcs_victim_process" ~ "Victim-centered process index",
                          name == "tcs_powers" ~ "Powers index") ) %>%
  ggplot() +
  geom_line(aes(x = year, y = value, color = name)) +
  theme_bw() +
  ylab("Average index value") +
  theme(axis.title.x = element_blank(),
        legend.title = element_blank(),
        legend.position.inside = c(0.99, 0.01),
        legend.justification = c(0.99, 0.01),
        legend.background = element_rect(fill = "white"),
        legend.key.height = unit(10, 'pt') )
ggsave("descriptives/process.png", plot = last_plot(), width = 6, height = 4)

reports <- db$TruthCommissions %>%
  filter(authorizedByState == 1 & temporaryBodyReport == 1 & ## met criteria
           focusedPast == 1 & investigatePatternAbuse == 1) %>%
  filter(!is.na(yearCompleteOperation)) %>%
  select(yearCompleteOperation,
         finalReportIssued, reportPubliclyAvailable) %>%
  rename(year = yearCompleteOperation,
         report = finalReportIssued,
         public = reportPubliclyAvailable) %>%
  mutate(report = case_when(report == "Yes" ~ 1,
                                       TRUE ~ 0),
         public = case_when(public == "Yes" ~ 1,
                                       TRUE ~ 0)) %>%
  group_by(year) %>%
  reframe(report = sum(report),
          public = sum(public)) %>%
  filter(year < 2021) %>%
  full_join(expand_grid(year = 1970:2020), by = "year") %>%
  arrange(year) %>%
  mutate(report = ifelse(is.na(report), 0, report),
         public = ifelse(is.na(public), 0, public)) %>%
  full_join(df %>%
              mutate(tcs_recommendations = UnitScale(tcs_recommendations)) %>%
              select(country, year, tcs_recommendations) %>%
              group_by(year) %>%
              reframe(recommendations = sum(tcs_recommendations)),
            by = "year") %>% 
  mutate(reports = cumsum(report)) %>%
  mutate(recommendations = ifelse(reports > 0, recommendations / reports, NA) * 20)

reports %>%
  select(year, report, public) %>%
  pivot_longer(cols = !year) %>%
  mutate(name = case_when(name == "report" ~ "final report not public",
                          name == "public" ~ "public final report") ) %>%
  ggplot() +
  geom_bar(aes(x = year, y = value, fill = name), stat = "identity") +
  geom_line(data = reports %>%
              select(year, recommendations),
            aes(x = year, y = recommendations)) +
  scale_fill_manual(values = c("final report not public" = "darkgray", "public final report"= "lightgray")) +
  theme_bw() +
  ylab("number of reports") +
  ylim(c(0, 10)) +
  theme(legend.title = element_blank(),
        legend.position.inside = c(0.33, 0.99),
        legend.justification = c(0.33, 0.99),
        legend.background = element_rect(fill = "white"),
        axis.title.x = element_blank(),
        legend.key.size = unit(10, 'pt'), # change legend key size
        legend.key.height = unit(10, 'pt'), # change legend key height
        legend.key.width = unit(10, 'pt')) +
  scale_y_continuous(breaks = c(0, 2, 4, 6, 8),
                     sec.axis = sec_axis(transform = ~ . / 20,
                                         name = "average reform recommendations index"))
### data release Figure 2
ggsave("descriptives/reports.png", plot = last_plot(), width = 6, height = 4)

# could later use 3 point scale for policy recommendation

## Reparations

# compensatory or symbolic, or whether they are collective or individual. Also important is the scope of the policy, as well as the number of victims that are registered.

db$Reparations %>%
  mutate(individual = case_when(individualReparations == "yes" ~ 1,
                                TRUE ~ 0),
         collective = case_when(collectiveReparations == "yes" ~ 1,
                                TRUE ~ 0),
         compensation = case_when(str_detect(individualReparationsForm, "compensation") | str_detect(collectiveReparationsForm, "compensation") ~ 1,
                                  TRUE~ 0),
         symbolic = case_when(str_detect(individualReparationsForm, "symbolic") | str_detect(collectiveReparationsForm, "symbolic") ~ 1,
                                  TRUE~ 0)) %>%
  select(yearCreated, individual, collective, compensation, symbolic)

df %>%
  select(year, reparations, rep_binary, rep_compensation, rep_symbolic, rep_collective, rep_victim_centered) %>%
  group_by(year) %>%
  reframe(across(everything(), ~ sum(.x, na.rm = TRUE))) %>%
  print(n = Inf)

## Amnesties

peace <- db$Amnesties %>%
  select(amnestyYear, peaceSettlement) %>%
  mutate(peaceSettlement = case_when(peaceSettlement == 0 ~ "no",
                                     peaceSettlement == 1 ~ "yes") ) %>%
  group_by(peaceSettlement) %>%
  reframe(n = n()) %>%
  ggplot(aes(x = reorder(peaceSettlement, n), y = n, fill = peaceSettlement)) +
  geom_bar(position = position_dodge(), stat = "identity") +
  # scale_fill_manual(values = c("yes" = "#F8766D", "no"= "darkgray")) +
  scale_fill_manual(values = c("yes" = "darkgray", "no"= "lightgray")) +
  expand_limits(y = 850) +
  geom_text(aes(label= n), vjust = -0.2, position = position_dodge(0.9), size = 2, color = "black") +
  theme_bw() +
  theme(panel.border = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.title = element_text(hjust = 1),
        title = element_text(size = 10),
        legend.position = "none") +
  ggtitle("Was the amnesty part\nof a peace agreement?")

hrs <- db$Amnesties %>%
  select(amnestyYear, hrCrimes) %>%
  mutate(
    hrCrimes = ifelse(hrCrimes == "human rights crimes were included in amnesty", 1, 0),
    hrCrimes = case_when(hrCrimes == 1 ~ "yes",
                         TRUE ~ "no"))  %>%
  group_by(hrCrimes) %>%
  reframe(n = n()) %>%
  ggplot(aes(x = reorder(hrCrimes, n), y = n, fill = hrCrimes)) +
  geom_bar(position = position_dodge(), stat = "identity") +
  # scale_fill_manual(values = c("yes" = "#F8766D", "no"= "darkgray")) +
  scale_fill_manual(values = c("yes" = "darkgray", "no"= "lightgray")) +
  expand_limits(y = 750) +
  geom_text(aes(label= n), vjust = -0.2, position = position_dodge(0.9), size = 2, color = "black") +
  theme_bw() +
  theme(panel.border = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.title = element_text(hjust = 1),
        title = element_text(size = 10),
        legend.position = "none") +
  ggtitle("Were human rights crimes\ninlcuded in the amnesty?")

who <- db$Amnesties %>%
  select(amnestyYear, whoWasAmnestied) %>%
  arrange(amnestyYear) %>%
  mutate(whoWasAmnestied = str_split(whoWasAmnestied, "; ") ) %>%
  unnest(whoWasAmnestied) %>%
  mutate(whoWasAmnestied = str_replace(whoWasAmnestied, "collaborators", "other"),
         whoWasAmnestied = str_replace(whoWasAmnestied, "draft dodgers / deserters", "other"),
         whoWasAmnestied = str_replace(whoWasAmnestied, "regular convicts", "other"),
         whoWasAmnestied = str_replace(whoWasAmnestied, "armed opposition", "armed\nopposition"),
         whoWasAmnestied = str_replace(whoWasAmnestied, "state agents", "state\nagents"),
         whoWasAmnestied = str_replace(whoWasAmnestied, " / ", " &\n") ) %>%
  group_by(whoWasAmnestied) %>%
  reframe(n = n()) %>%
  ggplot(aes(y = reorder(whoWasAmnestied, n), x = n)) +
  # geom_bar(position = position_dodge(), stat = "identity", fill = "#F8766D") +
  geom_bar(position = position_dodge(), stat = "identity", fill = "darkgray") +
  geom_text(aes(label= n), hjust = -0.1, position = position_dodge(0.9), size = 2, color = "black") +
  expand_limits(x = 600) +
  theme_bw() +
  theme(panel.border = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major.y = element_blank(),
        plot.title = element_text(hjust = 1),
        title = element_text(size = 10)) +
  ggtitle("Who was amnestied?")

# amnesty <- multi_panel_figure(columns = 2, rows = 2, panel_label_type = "none")
# amnesty %<>%
#   fill_panel(hrs, column = 1, row = 1) %<>%
#   fill_panel(who, column = 2, row = 1:2) %<>%
#   fill_panel(peace, column = 1, row = 2)
# amnesty

amnesty <- (hrs / peace) | who
### data release Figure 3
ggsave("descriptives/amnesty.png", plot = amnesty, width = 6, height = 4)

## trials and convictions

df %>%
  select(year, tran_trs_dom_dtj_ctj_sta, tran_trs_dom_dtj_ctj_sta_hi) %>%
  group_by(year) %>%
  reframe(tran_trs_dom_dtj_ctj_sta = sum(tran_trs_dom_dtj_ctj_sta),
          tran_trs_dom_dtj_ctj_sta_hi = sum(tran_trs_dom_dtj_ctj_sta_hi)) %>%
  ggplot() +
  geom_line(aes(x = year, y = tran_trs_dom_dtj_ctj_sta), linewidth = 1) +
  geom_area(aes(x = year, y = tran_trs_dom_dtj_ctj_sta_hi, fill = "high-ranking")) +
  scale_colour_manual(values = c("high-ranking" = "red"),
                      limits = "red") +
  xlab("") + ylab("") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 1),
        legend.position.inside = c(0.01, 0.99),
        legend.justification = c(0.01, 0.99),
        legend.background = element_rect(fill = "white"),
        legend.title = element_blank()) +
  ggtitle("Transitional human rights trials of state agents")
ggsave("descriptives/trials.png", plot = last_plot(), width = 6, height = 3)

df %>%
  select(year, tran_cce_dom_dtj_ctj_sta, tran_cce_dom_dtj_ctj_sta_hi) %>%
  group_by(year) %>%
  reframe(tran_cce_dom_dtj_ctj_sta = sum(tran_cce_dom_dtj_ctj_sta),
         tran_cce_dom_dtj_ctj_sta_hi = sum(tran_cce_dom_dtj_ctj_sta_hi)) %>%
  ggplot() +
  geom_line(aes(x = year, y = tran_cce_dom_dtj_ctj_sta), linewidth = 1) +
  geom_area(aes(x = year, y = tran_cce_dom_dtj_ctj_sta_hi, fill = "high-ranking")) +
  scale_colour_manual(values = c("high-ranking" = "red"),
                      limits = "red") +
  xlab("") + ylab("") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 1),
        legend.position = c(0.01, 0.99),
        legend.justification = c(0.01, 0.99),
        legend.background = element_rect(fill = "white"),
        legend.title = element_blank()) +
  ggtitle("State agents convicted in transitional human rights trials")
ggsave("descriptives/convictions.png", plot = last_plot(), width = 6, height = 3)

df %>%
  select(year, tran_cce_dom_dtj_ctj_sta, tran_cce_dom_dtj_ctj_sta_hi) %>%
  group_by(year) %>%
  reframe(tran_cce_dom_dtj_ctj_sta = sum(tran_cce_dom_dtj_ctj_sta),
          tran_cce_dom_dtj_ctj_sta_hi = sum(tran_cce_dom_dtj_ctj_sta_hi)) %>%
  mutate(prop = ifelse(tran_cce_dom_dtj_ctj_sta != 0, tran_cce_dom_dtj_ctj_sta_hi / tran_cce_dom_dtj_ctj_sta * 100, 0)) %>%
  # print(n = Inf)
  ggplot() +
  geom_line(aes(x = year, y = prop), linewidth = 1) +
  xlab("") + ylab("Percentage of all convictions") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 1),
        legend.position.inside = c(0.01, 0.99),
        legend.justification = c(0.01, 0.99),
        legend.background = element_rect(fill = "white"),
        legend.title = element_blank()) +
  ggtitle("High-ranking state agents convicted in transitional human rights trials")
### data release Figure 4
ggsave("descriptives/highrankconvictions.png", plot = last_plot(), width = 6, height = 3)

# simultaneity

df %>%
  arrange(country, year) %>%
  mutate(trials = trs_int_sta + trs_int_opp + tran_trs_dom_dtj_ctj_sta,
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
  reframe(trials_post_tc = sum(trials_post_tc),
          trials_post_amnesty = sum(trials_post_amnesty),
          trials_post_reparation = sum(trials_post_reparation) ) %>%
  pivot_longer(cols = !year) %>%
  filter(!is.na(value) ) %>%
  mutate(name = case_when(name == "trials_post_tc" ~ "trials within two years of truth commissions",
                          name == "trials_post_amnesty" ~ "trials within two years of amnesties",
                          name == "trials_post_reparation" ~ "trials within two years of reparations policies") ) %>%
  ggplot() +
  geom_line(aes(x = year, y = value, col = name), linewidth = 1) +
  # scale_colour_manual(values = c("orange", "red", "black"),
  #                     limits = c("trials following truth commissions",
  #                                "trials following amnesties",
  #                                "trials following reparations policies"),
  #                     aesthetics = c("colour", "fill")) +
  xlab("") + ylab("Number of trials") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 1),,
        axis.title.x = element_blank(),
        legend.position = c(0.01, 0.99),
        legend.justification = c(0.01, 0.99),
        legend.title = element_blank(),
        # legend.key.height = unit(10, 'pt') )
        legend.background = element_rect(fill = "white")
        ) +
  ggtitle("Human rights trials and other transitional justice")
ggsave("descriptives/simultaneity.png", plot = last_plot(), width = 6, height = 3)
