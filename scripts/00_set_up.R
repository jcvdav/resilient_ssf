pt <- 0.176389
ggplot2::theme_set(
  ggplot2::theme_minimal(base_size = 7)
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
                                         linewidth = 1,
                                         linetype = "dotted"))

ggplot2::update_geom_defaults(geom = "hline",
                              new = list(color = "black",
                                         linewidth = 0.5,
                                         linetype = "dashed"))

ggplot2::update_geom_defaults(geom = "vline",
                              new = list(color = "black",
                                         linewidth = 0.5,
                                         linetype = "dashed"))


period_palette <- c("Baseline" = "gray10",
                    "MHW" = "red",
                    "C19" = "steelblue")

# Helper functions
clean_eu_names <- function(x) {

  out <-  x %>%
    str_to_upper() %>%
    str_squish() %>%
    str_remove_all(pattern= ",") %>%
    str_replace_all(pattern= "Á", replacement = "A") %>%
    str_replace_all(pattern= "É", replacement = "E") %>%
    str_replace_all(pattern= "Í", replacement = "I") %>%
    str_replace_all(pattern= "Ó", replacement = "O") %>%
    str_replace_all(pattern= "Ú", replacement = "U") %>%
    str_replace_all(pattern= "Ñ", replacement = "N") %>%
    # Common abbreviations
    str_replace_all(pattern = "BCS\\.?\\s", replacement = "BAJA CALIFORNIA SUR ") %>%
    str_replace_all(pattern = "BC\\.?\\s?", replacement = "BAJA CALIFORNIA ") %>%
    str_replace_all(pattern = "OCC\\.?\\s", replacement = "OCCIDENTAL ") %>%
    str_replace_all(pattern = "CALIF\\.?\\s", replacement = "CALIFORNIA ") %>%
    # Prefixes
    str_remove(pattern = "SCPP") %>%
    str_remove(pattern = "UPP") %>%
    str_remove(pattern = "S\\.E\\.P\\.P\\.E\\.?") %>%
    str_remove(pattern = "S\\.C\\.P\\.P\\.?") %>%
    str_remove(pattern = "U\\.P\\.P\\.E\\.?") %>%
    str_remove(pattern = "U\\.P\\.P\\.?") %>%
    str_remove(pattern = "SOCIEDAD COOPERATIVA DE PRODUCCION PESQUERA") %>%
    str_remove(pattern = "UNIDAD DE PRODUCCION PESQUERA EJIDAL") %>%
    str_remove(pattern = "UNIDAD DE PRODUCCION PESQUERA") %>%
    str_squish() %>%
    # Suffixes
    str_remove(pattern = "S\\.C\\.L\\.?") %>%
    str_remove(pattern = "S\\.?\\s?P\\.?\\s?R\\.? DE R\\.?\\s?L\\.") %>%
    str_remove(pattern = "S\\.?\\s?C\\.? DE R\\.?\\s?L\\.") %>%
    str_remove(pattern = "S\\.P\\.R\\. DE R\\.I\\.") %>%
    str_remove(pattern = "S\\.P\\.R\\. DE R\\.") %>%
    str_remove(pattern = "S\\.P\\.R\\.?") %>%
    str_remove(pattern = "S\\.A\\.?") %>%
    str_remove(pattern = "S\\.") %>%
    str_remove(pattern = "DE R\\.L\\.") %>%
    str_remove(pattern = "DE R\\.I\\.") %>%
    str_remove(pattern = "DE C\\.V\\.") %>%
    str_remove_all(pattern = "\\.\\s") %>%
    str_remove_all(pattern = "P.R DE R.L.") %>%
    str_squish() %>%
    # Suffixes without points in them
    str_remove(pattern = "SPR DE RL") %>%
    str_remove(pattern = "DE R") %>%
    # str_remove_all(pattern = "[:punct:]") %>%
    str_squish() %>%
    # Random things
    str_remove_all(pattern = "ERIZO ROJO Y PEPINO") %>%
    str_remove_all(pattern = "PEPINO Y CANGREJO") %>%
    str_remove_all(pattern = "NUM[:digit:]") %>%
    str_remove_all(pattern = '"') %>%
    str_squish()

  return(out)
}


fix_rnpa <- function(rnpa, length = 8){
  rnpa[is.na(rnpa)] <- "_"
  lengths <- stringr::str_length(rnpa)
  missing <- pmax(length - lengths, 0)
  zeroes <- purrr::map_chr(missing, ~paste(numeric(length = .x), collapse = ""))
  out <- paste0(zeroes, rnpa)
  return(out)
}


tidy_lme4 <- function(model) {
  # Extract degrees of freedom
  df <- insight::get_df(model)

  # Extract fixed effects
  fixefs <- lme4::fixef(model)

  # Extract variance-covariance matrix
  vcov <- as.matrix(vcov(model))

  # Calculate standard errors
  ses <- sqrt(diag(vcov))

  # Combine into a tibble
  tidy_model <- tibble(
    term = names(fixefs),
    estimate = fixefs,
    std.error = ses) %>%
    # Calculate t.statistic, p.values, and 95% confidence intervals
    mutate(statistic = estimate / std.error,
           p.value = pt(statistic, df = df) * 2,
           conf.low = estimate - (1.96 * std.error),
           conf.high = estimate + (1.96 * std.error),
           term = fct_relevel(term, "MHW", "C19"))

  return(tidy_model)
}
