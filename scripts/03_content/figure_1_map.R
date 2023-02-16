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
  sf,
  rnaturalearth,
  tidyverse
)

# Load data --------------------------------------------------------------------
shock <- readRDS(file = here("data", "processed", "cv_and_shocks.rds"))

turfs <- st_read(dsn = here("../ssf_shocks", "data", "processed", "turf_polygons.gpkg"))

mex <- rnaturalearth::ne_countries(country = c("Mexico", "United States of America"), returnclass = "sf", scale = "large") %>%
  st_crop(st_bbox(st_buffer(turfs, 150000)))

## PROCESSING ##################################################################
centroids <- turfs %>%
  select(eu_rnpa) %>%
  distinct() %>%
  st_centroid() %>%
  left_join(schok, by = "eu_rnpa") %>%
  drop_na()

## VISUALIZE ###################################################################

# X ----------------------------------------------------------------------------
p1 <- ggplot() +
  geom_sf(data = mex) +
  geom_sf(data = centroids, size = 0.5) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0))

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
ggsave(plot = p1,
       filename = here("results", "img", "figure1_map.png"),
       width = 3,
       height = 3.5)
