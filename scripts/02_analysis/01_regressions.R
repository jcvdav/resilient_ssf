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
  lm(c19_shock_r ~ cv_revenue, weights = n, data = data) %>%
    broom::tidy()
}

fit_mhw <- function(data) {
  lm(mhw_shock_r ~ cv_revenue, weights = n, data = data) %>%
    broom::tidy()
}

# Load data --------------------------------------------------------------------

shock <- readRDS(file = here("data", "processed", "cv_and_shocks.rds"))

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
  map_dfr(strap, fit_c19, .id = "split") %>%
  group_by(term) %>%
  summarize(std.error = sd(estimate),
            estimate = mean(estimate)) %>%
  mutate(dep = "MHW")

terms <- bind_rows(bootstraped_c19, bootstraped_mhw)

lm(mhw_shock_r ~ cv_revenue, weights = n, data = shock) %>% summary()

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
saveRDS(object = terms,
        file = here("data", "processed", "coefficient_estimates.rds"))
