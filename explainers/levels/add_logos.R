librarian::shelf(ggplot2, magick, ragg, ggbrookings)

path <- here::here("explainers/levels/becca-charts/")


# Figure 1 ----------------------------------------------------------------
gdp_effect <- add_logo(glue("{path}/gdp_effect_wbrackets-01.png"), logo_path = 'hc', 'bottom right', logo_scale = 5, height_padding = 0.02)

image_write(gdp_effect, glue("{path}/gdp_effect.png"))

# Figure 2 ----------------------------------------------------------------
gdp_effect_components <- add_logo(glue("{path}/gdp_effect_components_wbrackets-01.png"), logo_path = 'hc', 'bottom right', logo_scale = 5, height_padding = 0.01)

image_write(gdp_effect_components, glue("{path}/gdp_effect_components.png"))

# Figure 3 -----------------------------------------------------------------


fim_components <- add_logo(glue("{path}/fim_components_wbrackets-01.png"), logo_path = 'hc', 'bottom right', logo_scale = 5, height_padding = 0.025)

image_write(fim_components, glue("{path}/fim_components.png"))

