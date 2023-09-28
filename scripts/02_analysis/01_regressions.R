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
  modelr,
  magrittr,
  tidyverse
)

fit_c19 <- function(data) {
  lm(c19_shock ~ taxa_simpson + mkt_simpson, data = data) %>%
    broom::tidy()
}

fit_mhw <- function(data) {
  lm(mhw_shock ~  taxa_simpson + mkt_simpson, data = data) %>%
    broom::tidy()
}

# Load data --------------------------------------------------------------------
shock <- readRDS(file = here("data", "output", "shock_estimates.rds")) %>%
  mutate(taxa_bin = case_when(between(taxa_simpson, 0, 1/3) ~ "Low",
                              between(taxa_simpson, 1/3, 2/3) ~ "Med",
                              between(taxa_simpson, 2/3, 1) ~ "High"),
         taxa_bin = fct_reorder(taxa_bin, taxa_simpson),
         mkt_bin = case_when(between(mkt_simpson, 0, 1/3) ~ "Low",
                             between(mkt_simpson, 1/3, 2/3) ~ "Med",
                             between(mkt_simpson, 2/3, 1) ~ "High"),
         mkt_bin = fct_reorder(mkt_bin, mkt_simpson)) %>%
  mutate(c19_shock = -1 * c19_shock,
         mhw_shock = -1 * mhw_shock)

## PROCESSING ##################################################################

# X ----------------------------------------------------------------------------
bootstraped_c19 <- shock %>%
  bootstrap(n = 100, id = "split") %$%
  map_dfr(strap, fit_c19, .id = "split") %>%
  group_by(term) %>%
  summarize(std.error = sd(estimate),
            estimate = mean(estimate)) %>%
  mutate(dep = "C19")

bootstraped_mhw <- shock %>%
  bootstrap(n = 100, id = "split") %$%
  map_dfr(strap, fit_mhw, .id = "split") %>%
  group_by(term) %>%
  summarize(std.error = sd(estimate),
            estimate = mean(estimate)) %>%
  mutate(dep = "MHW")

terms <- bind_rows(bootstraped_c19, bootstraped_mhw)

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
saveRDS(object = terms,
        file = here("data", "processed", "coefficient_estimates.rds"))
