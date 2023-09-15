pt <- 0.176389
ggplot2::theme_set(
  ggplot2::theme_bw()
)
ggplot2::theme_update(
  line = ggplot2::element_line(color = "black",
                               linewidth = pt),
  panel.grid = ggplot2::element_blank(),
  legend.background = ggplot2::element_blank(),
  legend.key = ggplot2::element_blank(),
  strip.background = ggplot2::element_blank(),
  strip.text = ggplot2::element_text(hjust = 0),
  text = ggplot2::element_text(size = 7,
                               color = "black")
)

ggplot2::update_geom_defaults(geom = "point",
                              new = list(shape = 21,
                                         color = "black",
                                         fill = "navyblue",
                                         alpha = 0.8,
                                         size = 3))

ggplot2::update_geom_defaults(geom = "smooth",
                              new = list(linetype = "dotted",
                                         color = "black",
                                         linewidth = 1))

ggplot2::update_geom_defaults(geom = "line",
                              new = list(color = "black",
                                         linewidth = 1))

ggplot2::update_geom_defaults(geom = "abline",
                              new = list(color = "black",
                                         linewidth = 1))

ggplot2::update_geom_defaults(geom = "hline",
                              new = list(color = "black",
                                         linewidth = 1,
                                         linetype = "dashed"))

ggplot2::update_geom_defaults(geom = "vline",
                              new = list(color = "black",
                                         linewidth = 1,
                                         linetype = "dashed"))


period_palette <- c("Baseline" = "gray10",
                    "Blob" = "red",
                    "Pandemic" = "steelblue")
