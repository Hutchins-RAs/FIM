library('magick')
library('ggbrookings')
path <- here::here("explainers/levels/charts.Rmd")
rmarkdown::render(path)

## TRANSFERS
net_transfers <- 'explainers/levels/figures/net-transfers-1.png'
net_transfers_logo <- add_logo(net_transfers, 'hc', height_padding = 0.01)

magick::image_write(net_transfers_logo, net_transfers)

## GDP
gdp <- "explainers/levels/figures/gdp-1.png"
gdp_logo <- add_logo(gdp, 'hc', height_padding = 0.01)

magick::image_write(gdp_logo, gdp)
