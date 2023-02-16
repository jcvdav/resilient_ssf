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
  tidyverse)

# Load data --------------------------------------------------------------------
# Landings data
landings <- readRDS(
  file = file.path(
    "/Users/juancarlosvillasenorderbez/GitHub/",
    "data_mex_fisheries",
    "data",
    "mex_landings",
    "clean",
    "mex_monthly_landings_by_eu.rds"
  )
)

eu_rnpas <-
  sf::st_read(dsn = here("../ssf_shocks", "data", "processed", "turf_polygons.gpkg")) %>%
  sf::st_drop_geometry() %>%
  pull(eu_rnpa) %>%
  unique()

cpi <- readRDS(here("../ssf_shocks", "data", "processed", "cpi_t_rates.rds"))


# coop_numbers <- read_excel("/Users/juancarlosvillasenorderbez/Downloads/CooperativePoints2022.xlsx") %>%
#   janitor::clean_names() %>%
#   drop_na(emp_planta)
## PROCESSING ##################################################################

# Filter for cooperatives we are focusing on -----------------------------------
baja_landings <- landings %>%
  filter(eu_rnpa %in% eu_rnpas) %>%
  group_by(year, eu_rnpa) %>%
  summarize(revenue = sum(value),
            n_spp = n_distinct(main_species_group)) %>%
  ungroup() %>%
  left_join(cpi, by = "year") %>%
  mutate(revenue = revenue * rate)

# Calculate coefficient of variation up to 2013 --------------------------------
cv_i <- baja_landings %>%
  filter(year <= 2013) %>%
  group_by(eu_rnpa) %>%
  summarize(mean_revenue = mean(revenue),
            sd_revenue = sd(revenue),
            n = n(),
            n_spp = max(n_spp)) %>%
  ungroup() %>%
  mutate(cv_revenue = sd_revenue / mean_revenue) %>%
  filter(n >= 5)

# Calculate simpson's diversity index ------------------------------------------
simpson <- landings %>%
  filter(eu_rnpa %in% eu_rnpas,
         year <= 2013) %>%
  group_by(year, eu_rnpa, main_species_group) %>%
  summarize(revenue = sum(value)) %>%
  ungroup() %>%
  group_by(eu_rnpa, main_species_group) %>%
  summarize(revenue = mean(revenue)) %>%
  group_by(eu_rnpa) %>%
  summarize(simpson = sum((revenue / sum(revenue)) ^ 2))

# Calculate dim during covid ---------------------------------------------------
covid_data <- baja_landings %>%
  filter(year == 2020) %>%
  select(-c(year, n_spp)) %>%
  drop_na()

# Calcualte dip during MHW events ----------------------------------------------
heatwave_data <- baja_landings %>%
  filter(between(year, 2014, 2017)) %>%
  select(-c(year, n_spp)) %>%
  drop_na() %>%
  group_by(eu_rnpa) %>%
  summarize(mhw_revenue = mean(revenue),
            n = n()) %>%
  filter(n >= 3) %>%
  select(-n)

# CBuild panel -----------------------------------------------------------------
shock <- cv_i %>%
  left_join(covid_data, by = "eu_rnpa") %>%
  left_join(heatwave_data, by = "eu_rnpa") %>%
  left_join(simpson, by = "eu_rnpa") %>%
  mutate(c19_shock_r = (revenue - mean_revenue) / sd_revenue,
         mhw_shock_r = (mhw_revenue - mean_revenue) / sd_revenue) %>%
  drop_na()


## EXPORT ######################################################################

saveRDS(object = shock,
        file = here("data", "processed", "cv_and_shocks.rds"))

