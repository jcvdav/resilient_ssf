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
  smoothr,
  cowplot,
  ggspatial,
  ggrepel,
  tidyverse
)

source(here("scripts/00_set_up.R"))

# Load data --------------------------------------------------------------------
shock <- readRDS(file = here("data", "processed", "cv_and_shocks.rds"))

# states <- ne_states(country = "Mexico", returnclass = "sf") %>%
#   filter(name %in% c("Baja California", "Baja California Sur", "Sonora", "Sinaloa", "Chihuahua", "Durango")) %>%
#   mutate(baja = name %in% c("Baja California", "Baja California Sur")) %>%
#   st_crop(xmin = -118, xmax = -108, ymin = 21, ymax = 34)

mex <- ne_countries(country = c("Mexico", "United States of America"), returnclass = "sf", scale = "large") %>%
  st_crop(xmin = -118, xmax = -108, ymin = 21, ymax = 34)

world <- ne_countries(returnclass = "sf", scale = "small")

offices <- tribble(~"office", ~"lat", ~"lon",
                   "BAHIA ASUNCION", 27.1368789, -114.3057131,
                   "BAHIA TORTUGAS", 27.6929632, -114.9071388,
                   "CD. CONSTITUCION", 25.0322641, -111.7363293,
                   "EL ROSARIO", 30.0595656, -115.7452369,
                   "ENSENADA", 31.8433852, -116.6700999,
                   "GUERRERO NEGRO", 27.9705889, -114.0487772,
                   "ISLA CEDROS", 28.1154271, -115.1950819,
                   "PTO. ADOLFO LOPEZ MATEOS", 25.1928214, -112.1173405,
                   "PUNTA ABREOJOS", 26.7175959, -113.5824839,
                   "SAN CARLOS", 24.8000128, -112.124893,
                   "SAN JUANICO", 26.2564261, -112.487555,
                   "SAN QUINTIN", 30.5711518, -115.9498472,
                   "TIJUANA", 32.4572039, -117.0338849,
                   "VILLA DE JESUS MARIA", 28.2824755, -114.0106611) %>%
  st_as_sf(coords = c("lon", "lat"),
           crs = 4326)

communities <- tribble(~"community", ~"lat", ~"lon",
                       "El Rosario", 30.0595656, -115.7452369,
                       "Bahía Asunción", 27.141132, -114.295047,
                       "Isla Cedros", 28.1154271, -115.1950819,
                       "Isla Natividad", 27.8723398,-115.2067512) %>%
  st_as_sf(coords = c("lon", "lat"),
           crs = 4326)

## VISUALIZE ###################################################################

# X ----------------------------------------------------------------------------
p1 <- ggplot(data = world) +
  geom_sf(fill = "black",
          color = "black",
          linewidth = 0.1) +
  geom_rect(xmin = -118, xmax = -108, ymin = 21, ymax = 34,
            color = "red",
            fill = "transparent",
            linewidth = 0.5,
            inherit.aes = F) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_void() +
  theme(panel.border = element_rect(linewidth = 1, fill = "transparent"),
        plot.background = element_rect(linewidth = 1, fill = "white"))


p2 <- ggplot() +
  geom_sf(data = mex,
          color = "black",
          linewidth = 0.1) +
  geom_sf(data = offices, size = 3, fill = "steelblue") +
  geom_sf(data = communities, size = 1, fill = "red") +
  ggrepel::geom_text_repel(
    data = communities,
    aes(label = community, geometry = geometry),
    stat = "sf_coordinates",
    min.segment.length = 0,
    colour = "black",
    size = 2,
    segment.colour = "black",
    nudge_x = -1
  ) +
  scale_fill_manual(values = c("transparent", "gray50")) +
  scale_linewidth_manual(values = c(0.1, 0.5)) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(legend.position = "None",
        axis.title = element_blank(),
        axis.ticks = element_line(color = "black")) +
  annotation_scale(location = 'tr')

p <- ggdraw(p2) +
  draw_plot(p1, hjust = 0, vjust = 0, x = -0.19, y = -0.335, scale = 0.45)

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
ggsave(plot = p,
       filename = here("results", "img", "fig1_map.png"),
       width = 4,
       height = 5)

ggsave(plot = p,
       filename = here("results", "img", "fig1_map.pdf"),
       width = 4,
       height = 5)

