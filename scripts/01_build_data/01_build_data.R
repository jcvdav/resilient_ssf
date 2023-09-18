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
  tidyverse
)

coop_numbers <- readxl::read_excel("/Users/juancarlosvillasenorderbez/Downloads/CooperativePoints2022.xlsx") %>%
  janitor::clean_names() %>%
  filter(estado %in% c("BAJA CALIFORNIA", "BAJA CALIFORNIA SUR"),
         municipio %in% c("SAN QUINTIN", "PLAYAS DE ROSARITO", "TIJUANA", "ENSENADA",
                          "COMONDU", "MULEGE", "LA PAZ", "LOS CABOS"),
         oficina %in% oficinas) %>%
  drop_na(lat, long, emp_planta) %>%
  select(eu_name = unidad_economica,
         state = estado,
         municipality = municipio,
         location = localidad,
         office = oficina,
         memebrs = emp_planta,
         interns = emp_eventual,
         lat, long) %>%
  distinct() %>%
  mutate(eu_name = clean_eu_names(eu_name)) %>%
  filter(!eu_name %in% c("X",
                         "ACUICPESCADORES DE LA HERRADURA SC L",
                         "ISLA DEL ANGEL",
                         "PESQUERA DON JAVIER S L MI",
                         "SC NATIVOS DEL DATIL SC L DE CV",
                         "BUZOS ORILLEROS DEL MAR DE CORTEZ",
                         "LA TRIESTE SC L DE CV",
                         "BUZOS DE BAHIA",
                         "GOLFO REALMAR",
                         "MS FISHERIES SA DE CV",
                         "PESCADORES HERMANOS FUERTE",
                         "SARDINEROS BAJACALIFORNIANOS SCL"
                         ))

pts <- coop_numbers %>% select(eu_name, lat, long) %>% distinct() %>% st_as_sf(coords = c("long", "lat"), crs = 4326)
mapview(pts %>% mutate(a = 1), zcol = "a")

# Define data ------------------------------------------------------------------
oficinas <- c(
  "BAHIA ASUNCION", "BAHIA TORTUGAS",
  "CD. CONSTITUCION", "EL ROSARIO",
  "ENSENADA", "GUERRERO NEGRO",
  "ISLA CEDROS", "PTO. ADOLFO LOPEZ MATEOS",
  "PUNTA ABREOJOS", "SAN CARLOS",
  "SAN JUANICO", "SAN QUINTIN",
  "TIJUANA", "VILLA DE JESUS MARIA"
)

# Load data --------------------------------------------------------------------
# CPI
cpi <- readRDS(here("../ssf_shocks", "data", "processed", "cpi_t_rates.rds"))

# Landings data
landings_raw <- readRDS(
  file = file.path(
    "/Users/juancarlosvillasenorderbez/GitHub/",
    "data_mex_fisheries",
    "data",
    "mex_landings",
    "clean",
    "mex_landings_2000_2022.rds"
  )
) %>%
  filter(state %in% c("BAJA CALIFORNIA", "BAJA CALIFORNIA SUR"),
         !acuaculture_production == "SÍ" | is.na(acuaculture_production),
         live_weight > 0,
         value > 0)



## PROCESSING ##################################################################

# counts to report filters
# Number  of SSF in Baja
landings_raw %>%
  filter(fleet == "small_scale") %>%
  pull(eu_rnpa) %>%
  unique() %>%
  length()

spp_remove <- c("BESUGO", "BAGRE", "LOBINA", "RUBIA Y VILLAJAIBA", "CARPA", "LANGOSTINO", "ANCHOVETA", "ESMEDREGAL", "OSTION", "PECES DE ORNATO", "BARRILETE", "PETO")

# EUs that report in offices along the pacific coastline
eus_reporting_in <- landings_raw %>%
  filter(office_name %in% oficinas,
         !main_species_group %in% spp_remove,
         fleet == "small_scale") %>%
  pull(eu_rnpa) %>%
  unique()

# SSF eus that report in other offices in Baja
eus_reporting_out <- landings_raw %>%
  filter(!office_name %in% oficinas,
         !main_species_group %in% spp_remove,
         fleet == "small_scale") %>%
  pull(eu_rnpa) %>%
  unique()

# Keep EUS that ONLY report in the offices identified (i.e. they don't report in the Gulf)
eus_only_in <- eus_reporting_in[!(eus_reporting_in %in% eus_reporting_out)]
length(eus_only_in)

eus_with_10 <- landings_raw %>%
  filter(office_name %in% oficinas,
         !main_species_group %in% spp_remove,
         eu_rnpa %in% eus_only_in,
         year >= 2005) %>%
  mutate(
    period = case_when(year %in% c(2014:2016) ~ "MHW",
                       year %in% c(2020, 2021) ~ "C19",
                       T ~ "Baseline"),
    period = fct_relevel(period, c("Baseline", "MHW", "C19"))
  ) %>%
  group_by(eu_rnpa) %>%
  summarize(n = n_distinct(year),
            n_period = n_distinct(period)) %>%
  ungroup() %>%
  filter(n >= 10,
         n_period == 3) %>%
  pull(eu_rnpa) %>%
  unique()

length(eus_with_10)

eus_with_5_baseline <- landings_raw %>%
  filter(office_name %in% oficinas,
        !main_species_group %in% spp_remove,
        eu_rnpa %in% eus_only_in,
        year >= 2005,
        year <= 2013) %>%
  group_by(eu_rnpa) %>%
  summarize(n_years = n_distinct(year)) %>%
  ungroup() %>%
  filter(n_years >= 3) %>%
  pull(eu_rnpa) %>%
  unique()

length(eus_with_5_baseline)

# Build a panel of landings by species, year, and economic unit
yr_eu_spp <- landings_raw %>%
  # Some filters are redundant, but good for consistency and guard rails
  filter(year >= 2005,
         office_name %in% oficinas,
         eu_rnpa %in% eus_reporting_in,
         !eu_rnpa %in% eus_reporting_out,
         !main_species_group %in% spp_remove,
         eu_rnpa %in% eus_only_in,
         eu_rnpa %in% eus_with_10,
         eu_rnpa %in% eus_with_5_baseline,
         !eu_rnpa %in% c("0203008107", "0203013891", "0203014162", "0203014717", "0203015607", "0203015656",
                         "0203015714", "0303000673", "0308000231", "0308000447", "0311001077", "0311001200",
                         "0311001549")
  ) %>%
  mutate(taxa = case_when(
    main_species_group == "ABULON" ~ "Haliotis",
    main_species_group == "ALGAS" ~ "Algae",
    main_species_group == "ALMEJA" ~ "Bivalvia",
    main_species_group == "ATUN" ~ "Thunnus",
    main_species_group == "BANDERA" ~ "Gerreidae",
    main_species_group == "BAQUETA" ~ "Serranidae",
    main_species_group == "BERRUGATA" ~ "Sciaenidae",
    main_species_group == "BONITO" ~ "Scombridae",
    main_species_group == "CABRILLA" ~ "Serranidae",
    main_species_group == "CALAMAR" ~ "Squid",
    main_species_group == "CAMARON" ~ "Shrimp",
    main_species_group == "CARACOL" ~ "Gastropoda",
    main_species_group == "CAZON" ~ "Mustelus",
    main_species_group == "CORVINA" ~ "Sciaenidae",
    main_species_group == "ERIZO" ~ "Echinoida",
    main_species_group == "GUACHINANGO" ~ "Lutjanidae",
    main_species_group == "JAIBA" ~ "Callinected",
    main_species_group == "JUREL" ~ "Carangidae",
    main_species_group == "LANGOSTA" ~ "Palinuridae",
    main_species_group == "LEBRANCHA" ~ "Mugilidae",
    main_species_group == "LENGUADO" ~ "Pleuronectiformes",
    main_species_group == "LISA" ~ "Mugilidae",
    main_species_group == "MACARELA" ~ "Scombridae",
    main_species_group == "MERO" ~ "Serranidae",
    main_species_group == "MOJARRA" ~ "Gerreidae",
    main_species_group == "OTRAS" ~ "Others",
    main_species_group == "PAMPANO" ~ "Carangidae",
    main_species_group == "PARGO" ~ "Lutjanidae",
    main_species_group == "PEPINO DE MAR" ~ "Stichopodidae",
    main_species_group == "PIERNA" ~ "Malacanthidae",
    main_species_group == "PULPO" ~ "Octopodidae",
    main_species_group == "RAYA Y SIMILARES" ~ "Batoidea",
    main_species_group == "ROBALO" ~ "Centropomidae",
    main_species_group == "RONCO" ~ "Haemulidae",
    main_species_group == "SARDINA" ~ "Clupeidae",
    main_species_group == "SARGAZO" ~ "Algae",
    main_species_group == "SIERRA" ~ "Scombridae",
    main_species_group == "TIBURON" ~ "Sharks"
  ),
  period = case_when(year %in% c(2014:2016) ~ "MHW",
                     year %in% c(2020, 2021) ~ "C19",
                            T ~ "Baseline"),
         period = fct_relevel(period, c("Baseline", "MHW", "C19"))
  ) %>%
  group_by(year, period, eu_rnpa, eu_name, taxa) %>%
  summarize(revenue = sum(value, na.rm = T),
            landings = sum(live_weight, na.rm = T)) %>%
  ungroup() %>%
  left_join(cpi, by = "year") %>%
  mutate(revenue = revenue * rate * 0.052) %>%
  select(-rate)

length(unique(yr_eu_spp$eu_rnpa))

# Calculate simpson's diversity index ------------------------------------------
yr_eu <- yr_eu_spp %>%
  group_by(year, period, eu_rnpa, eu_name) %>%
  summarize(revenue = sum(revenue, na.rm = T),
            landings = sum(landings, na.rm = T)) %>%
  ungroup() %>%
  group_by(eu_rnpa) %>%
  mutate(
    std_rev = (revenue - mean(revenue[period == "Baseline"], na.rm = T)) / sd(revenue[period == "Baseline"], na.rm = T),
    std_land = (landings - mean(landings[period == "Baseline"], na.rm = T)) / sd(landings[period == "Baseline"], na.rm = T))

length(unique(yr_eu$eu_rnpa))

# EU charcteristics ------------------------------------------------------------
simpson <- yr_eu_spp %>%
  filter(year <= 2013) %>%
  group_by(eu_rnpa, taxa) %>%
  summarize(revenue = mean(revenue)) %>%
  group_by(eu_rnpa) %>%
  summarize(simpson = sum((revenue / sum(revenue)) ^ 2)) %>%
  mutate(inv_simp = 1 - simpson) %>%
  ungroup()

richness <- yr_eu_spp %>%
  filter(year <= 2013) %>%
  group_by(eu_rnpa) %>%
  summarize(n_spp = n_distinct(taxa)) %>%
  ungroup()

cv <- yr_eu %>%
  filter(year <= 2013) %>%
  group_by(eu_rnpa) %>%
  summarize(
    mean_rev = mean(revenue ,na.rm = T),
    mean_land = mean(landings, na.rm = T),
    cv_rev = sd(revenue, na.rm = T) / mean(revenue, na.rm = T),
    cv_land = sd(landings, na.rm = T) / mean(landings, na.rm = T)
  ) %>%
  ungroup()

characteristics <- cv %>%
  left_join(simpson, by = "eu_rnpa") %>%
  left_join(richness, by = "eu_rnpa")



# Summary tables
datasummary((`Species Group` = main_species_group) * ((`Revenue (USD[2019])` = revenue) + (`Landings (Kg)` = landings)) ~ mean + median + sd + max + min ,
            data = yr_eu_spp)

datasummary((`Revenue (USD[2019])` = revenue) + (`Landings (Kg)` = landings) ~ mean + sd + median + max + min,
            data = yr_eu)




## EXPORT ######################################################################
saveRDS(object = coefs,
        file = here("data", "processed", "cv_and_shocks.rds"))

