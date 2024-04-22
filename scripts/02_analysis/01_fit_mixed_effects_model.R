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

# Load data --------------------------------------------------------------------
yr_eu <- readRDS(here("data/processed/year_eu.rds")) %>%
  mutate(baseline = 1 * (period == "Baseline"),
         MHW = 1 * (period == "MHW"),
         C19 = 1 * (period == "C19"))

## PROCESSING ##################################################################
model <- lmer(std_rev ~ 0 + MHW + C19 + (0 + MHW | eu_rnpa) + (0 + C19 | eu_rnpa),
              data = yr_eu)
alt_mod <- lmer(std_rev ~ MHW + C19 + (0 + MHW | eu_rnpa) + (0 + C19 | eu_rnpa),
                data = yr_eu)

# X ----------------------------------------------------------------------------
coefs <- coef(model)$eu_rnpa %>%
  as_tibble()

ranefs <- ranef(model)$eu_rnpa %>%
  as_tibble()


## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
list(model, alt_mod) %>%
  set_names(c("Main model", "Free y-intercept")) %>%
  modelsummary(gof_omit = "ICC|BIC",
               stars = panelsummary:::econ_stars(),
               title = "Main effects of Marine Heatwaves (MHW) and COVID-19 (C19) disruptions on normalized landings by 266 economic units. Numbers in parentheses are standard errors.")

saveRDS(object = model,
        file = here("data", "output", "mixed_effects_model.rds"))


