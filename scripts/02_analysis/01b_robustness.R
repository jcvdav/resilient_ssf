################################################################################
# title
################################################################################
#
# Juan Carlos Villaseñor-Derbez
# juancvd@stanford.edu
# date
#
# Description
#
################################################################################

## SET UP ######################################################################

# Load packages ----------------------------------------------------------------
pacman::p_load(
  here,
  lme4,
  modelsummary,
  tidyverse
)

# Lod main model ---------------------------------------------------------------
model <- readRDS(here("data", "output", "mixed_effects_model.rds"))

# Load data --------------------------------------------------------------------
yr_eu <- readRDS(here("data/processed/year_eu.rds")) %>%
  mutate(baseline = 1 * (period == "Baseline"),
         MHW = 1 * (period == "MHW"),
         C19 = 1 * (period == "C19"))|>
  group_by(eu_rnpa) |>
  mutate(n_g = n_distinct(period)) |>
  ungroup() |>
  filter(n_g == 3) |>
  ungroup()

alt_mhw <- yr_eu %>%
  ungroup() %>%
  mutate(period = case_when(year %in% (c(2014:2016)) ~ "MHW",
                            year %in% c(2020:2022) ~ "C19",
                            T ~ "Baseline"),
         period = fct_relevel(period, c("Baseline", "MHW", "C19"))) %>%
  group_by(eu_rnpa) %>%
  mutate(
    std_rev = (revenue - mean(revenue[period == "Baseline"], na.rm = T)) / sd(revenue[period == "Baseline"], na.rm = T)) %>%
  mutate(baseline = 1 * (period == "Baseline"),
         MHW = 1 * (period == "MHW"),
         C19 = 1 * (period == "C19"))

alt_c19 <- yr_eu %>%
  ungroup() %>%
  mutate(period = case_when(year %in% (c(2015:2016)) ~ "MHW",
                            year %in% c(2020:2021) ~ "C19",
                            T ~ "Baseline"),
         period = fct_relevel(period, c("Baseline", "MHW", "C19"))) %>%
  group_by(eu_rnpa) %>%
  mutate(
    std_rev = (revenue - mean(revenue[period == "Baseline"], na.rm = T)) / sd(revenue[period == "Baseline"], na.rm = T)) %>%
  mutate(baseline = 1 * (period == "Baseline"),
         MHW = 1 * (period == "MHW"),
         C19 = 1 * (period == "C19"))

alt_mhw_c19 <- yr_eu %>%
  ungroup() %>%
  mutate(period = case_when(year %in% (c(2014:2016)) ~ "MHW",
                            year %in% c(2020:2021) ~ "C19",
                            T ~ "Baseline"),
         period = fct_relevel(period, c("Baseline", "MHW", "C19"))) %>%
  group_by(eu_rnpa) %>%
  mutate(
    std_rev = (revenue - mean(revenue[period == "Baseline"], na.rm = T)) / sd(revenue[period == "Baseline"], na.rm = T)) %>%
  mutate(baseline = 1 * (period == "Baseline"),
         MHW = 1 * (period == "MHW"),
         C19 = 1 * (period == "C19"))

post_04 <- yr_eu %>%
  ungroup() %>%
  filter(!year <= 2003) %>%
  group_by(eu_rnpa) %>%
  mutate(
    std_rev = (revenue - mean(revenue[period == "Baseline"], na.rm = T)) / sd(revenue[period == "Baseline"], na.rm = T)) %>%
  mutate(baseline = 1 * (period == "Baseline"),
         MHW = 1 * (period == "MHW"),
         C19 = 1 * (period == "C19"))

HMC <- yr_eu %>%
  ungroup() %>%
  mutate(period = case_when(year == 2008 ~ "HMC",
                            year %in% (c(2015:2016)) ~ "MHW",
                            year %in% c(2020:2022) ~ "C19",
                            T ~ "Baseline"),
         period = fct_relevel(period, c("Baseline", "MHW", "C19"))) %>%
  group_by(eu_rnpa) %>%
  mutate(
    std_rev = (revenue - mean(revenue[period == "Baseline"], na.rm = T)) / sd(revenue[period == "Baseline"], na.rm = T)) %>%
  mutate(baseline = 1 * (period == "Baseline"),
         HMC = 1 * (period == "HMC"),
         MHW = 1 * (period == "MHW"),
         C19 = 1 * (period == "C19"))

## PROCESSING ##################################################################
free_intercept <- lmer(std_rev ~ MHW + C19 + (0 + MHW | eu_rnpa) + (0 + C19 | eu_rnpa),
                data = yr_eu)

alt_mhw_model <- lmer(std_rev ~ 0 + MHW + C19 + (0 + MHW | eu_rnpa) + (0 + C19 | eu_rnpa),
                      data = alt_mhw)
alt_c19_model <- lmer(std_rev ~ 0 + MHW + C19 + (0 + MHW | eu_rnpa) + (0 + C19 | eu_rnpa),
                      data = alt_c19)
alt_mhw_c19_model <- lmer(std_rev ~ 0 + MHW + C19 + (0 + MHW | eu_rnpa) + (0 + C19 | eu_rnpa),
                          data = alt_mhw_c19)

post_04_model <- lmer(std_rev ~ 0 + MHW + C19 + (0 + MHW | eu_rnpa) + (0 + C19 | eu_rnpa),
                          data = post_04)
HMC_model <- lmer(std_rev ~ 0 + HMC + MHW + C19 + (0 + HMC | eu_rnpa) + (0 + MHW | eu_rnpa) + (0 + C19 | eu_rnpa),
                  data = HMC)

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
list(model, free_intercept, alt_mhw_model, alt_c19_model, alt_mhw_c19_model, post_04_model, HMC_model) %>%
  set_names(c("Main model", "Free intercept", "MHW (2014-2016)", "C19 (2020-2021)", "MHW (2014-2016) & C19 (2020-2021)", "Post '04 data", "HMC '08")) %>%
  modelsummary(gof_omit = c("IC|Adj|Std|FE|MSE"), coef_omit = "(Intercept)|HMC",
               output = here("results", "tab", "tabS2_robustness_checks.tex"),
               threeparttable = T,
               stars = panelsummary:::econ_stars(),
               escape = F,
               title = "\\label{tab:robustness}Table S2 - Main effects of Marine Heatwaves (MHW) and COVID-19 (C19) disruptions on normalized landings by 245 economic units. Numbers in parentheses are standard errors.
               The first column shows the main-text estimates, for reference. The second column uses the same variable deffinitions but allows for a free-varying y-intercept.
               Column 3 redefines the periods based the marine heatwave regime ocurring between 2014-2016.
               Column 4 redefines the periods beasd on the Pandemic ranging from 2020-2021.
               Column 5 combines the marine heatwave and COVID-19 period deffinitions in columns three and four.
               Column 6 excludes years 2001-2003, a period of time associated with the dot-com crash and economic uncertainty.")


list(model, free_intercept, alt_mhw_model, alt_c19_model, alt_mhw_c19_model, post_04_model, HMC_model) %>%
  map_dfr(~as_tibble(coef(.x)$eu_rnpa, rownames = "eu_rnpa"), .id = "source") %>%
  select(1:3) %>%
  pivot_wider(names_from = source, values_from = MHW)%>%
  select(-eu_rnpa) %>%
  plot(main = "Pairwise comparisons of eu-fixed effects for MHW under different speciications")


list(model, free_intercept, alt_mhw_model, alt_c19_model, alt_mhw_c19_model, post_04_model, HMC_model) %>%
  map_dfr(~as_tibble(coef(.x)$eu_rnpa, rownames = "eu_rnpa"), .id = "source") %>%
  select(1:4) %>%
  select(-3) %>%
  pivot_wider(names_from = source, values_from = C19)%>%
  select(-eu_rnpa) %>%
  plot(main = "Pairwise comparisons of eu-fixed effects for C19 under different speciications")
