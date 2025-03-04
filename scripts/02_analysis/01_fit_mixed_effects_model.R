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

source(here("scripts/00_set_up.R"))

# Load data --------------------------------------------------------------------
yr_eu <- readRDS(here("data/processed/year_eu.rds")) %>%
  mutate(baseline = 1 * (period == "Baseline"),
         MHW = 1 * (period == "MHW"),
         C19 = 1 * (period == "C19")) |>
  group_by(eu_rnpa) |>
  mutate(n_g = n_distinct(period)) |>
  ungroup() |>
  filter(n_g == 3) |>
  ungroup()

## PROCESSING ##################################################################
model <- lmer(std_rev ~ 0+ MHW + C19 + (0 + MHW | eu_rnpa) + (0 + C19 | eu_rnpa),
              data = yr_eu)
# X ----------------------------------------------------------------------------
coefs <- coef(model)$eu_rnpa %>%
  as_tibble(rownames = "eu_rnpa")

ranefs <- ranef(model)$eu_rnpa %>%
  as_tibble()


## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
modelsummary(model,
             stars = panelsummary:::econ_stars(),
             gof_omit = c("IC|Adj|Std|FE|MSE"),
             output = here("results", "tab", "tab1_main_model.tex"),
             escape = F,
             title = "\\label{tab:main_reg}Regression results for reductions in standard-normalized revenues during Marine heatwaves (MHW) and COVID-19 (C19) periods for 244 economic units in Baja California. Numbers in parentheses indicate standard errors and asterisks show statistical significance. Standard deviations of the random effects are also included at the bottom of the table.")

saveRDS(object = model,
        file = here("data", "output", "mixed_effects_model.rds"))

saveRDS(object = coefs,
        file = here("data", "output", "coefs.rds"))








