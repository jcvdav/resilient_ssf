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
  equatiomatic,
  modelsummary,
  performance,
  tidyverse
)

source(here("scripts/00_set_up.R"))

# Load data --------------------------------------------------------------------
yr_eu <- readRDS(here("data/processed/year_eu.rds")) %>%
  mutate(baseline = 1 * (period == "Baseline"),
         MHW = 1 * (period == "MHW"),
         C19 = 1 * (period == "C19"))

## PROCESSING ##################################################################
# 1) Fit the model -------------------------------------------------------------
model <- lmer(std_rev ~ 0 + MHW + C19 + (0 + MHW +C19 | eu_rnpa),
              data = yr_eu)

# Get model equation in LaTeX format
# extract_eq(model,
#            align_env = "split")

# 2) Get coefficients ----------------------------------------------------------
coefs <- coef(model)$eu_rnpa %>%
  as_tibble(rownames = "eu_rnpa")

ranefs <- ranef(model)$eu_rnpa %>%
  as_tibble()

## 3) Calculate delta-marginal R2s ---------------------------------------------
# Estimate sub-models, dropping the fixed and random effects of each term
m_noMHW <- lmer(std_rev ~ 0 + C19 + (0 + C19 | eu_rnpa), data = yr_eu)
m_noC19 <- lmer(std_rev ~ 0 + MHW + (0 + MHW | eu_rnpa), data = yr_eu)

# Marginal R2 for the full model
r2_full   <- r2_nakagawa(model)$R2_marginal

# Marginal r2 for sub-models
r2_noMHW <- r2_nakagawa(m_noMHW)$R2_marginal
r2_noC19 <- r2_nakagawa(m_noC19)$R2_marginal

# Calculate change in marginal R
r2_part_MHW <- r2_full - r2_noMHW
r2_part_C19 <- r2_full - r2_noC19

# Check that the sum of the delta marginalRs approaches the marginal R of the main model
# Are they within 0.01?
near((r2_part_MHW + r2_part_C19), r2_full, tol = 0.01)

# Calculate CIs via bootstrap
# Define a random seed
set.seed(1)

bootstrap <- function() {
  # Simulate outcomes
  std_rev_sim <- simulate(model, nsim = 1)[[1]]

  # Update the data
  dB <- yr_eu
  dB$std_rev <- std_rev_sim

  # Update the models
  full_bootstrap <- update(model,  data = dB)
  no_MHW_bootstrap <- update(m_noMHW, data = dB)
  no_C19_bootstrap <- update(m_noC19, data = dB)

  # If the any model was singular, ignore that bootstrap
  singular <- (1 * any(isSingular(full_bootstrap),
                       isSingular(no_MHW_bootstrap),
                       isSingular(no_C19_bootstrap)))

  c(r2_nakagawa(full_bootstrap)$R2_marginal - r2_nakagawa(no_MHW_bootstrap)$R2_marginal,
    r2_nakagawa(full_bootstrap)$R2_marginal - r2_nakagawa(no_C19_bootstrap)$R2_marginal,
    singular)
}

# Call the function 1100 times.
Bmat <- replicate(1100, bootstrap())

# Hoa many had singular fits?
length(Bmat[3,]) - sum(Bmat[3,]) # We'll discard the ones with singular fits and then retain the top 1000

# Calculate CI for each
ci_MHW <- quantile(head(Bmat[1, Bmat[3,] == 0], 1000), c(.025,.975))
ci_C19 <- quantile(head(Bmat[2, Bmat[3,] == 0], 1000), c(.025,.975))

## EXPORT ######################################################################

# Export model table -----------------------------------------------------------
modelsummary(model,
             stars = panelsummary:::econ_stars(),
             gof_omit = c("IC|Adj|Std|FE|MSE"),
             coef_rename = c("MHW" = "\\mu_{\\gamma_{1i}}",
                             "C19" = "\\mu_{\\gamma_{2i}}",
                             "SD (MHW eu_rnpa)" = "\\sigma_{\\gamma_{1i}}",
                             "SD (C19 eu_rnpa)" = "\\sigma_{\\gamma_{2i}}",
                             "SD (Observations)" = "\\sigma",
                             "Cor (MHW~C19 eu_rnpa)" = "\\rho"),
             output = here("results", "tab", "tab1_main_model.tex"),
             escape = F,
             title = "\\label{tab:main_reg}Regression results for reductions in standard-normalized revenues during Marine heatwaves (MHW) and COVID-19 (C19) periods for 237 economic units in Baja California. Numbers in parentheses indicate standard errors and asterisks show statistical significance. Standard deviations of the random effects are also included at the bottom of the table.")

# Export model object and coefficients -----------------------------------------
saveRDS(object = model,
        file = here("data", "output", "mixed_effects_model.rds"))

saveRDS(object = coefs,
        file = here("data", "output", "coefs.rds"))








