# ------------------------------setup-------------------------------------------

# libraries
library(tidyverse)
library(riekelib)
library(patchwork)
library(ggiraph)
library(reactable)
library(htmltools)

# setup themes
site_color <- "#F7F7F7"
sysfonts::font_add_google("Roboto Slab")
showtext::showtext_auto()
ggplot2::theme_set(
  ggplot2::theme_minimal(base_family = "Roboto Slab") +
    ggplot2::theme(plot.title.position = "plot",
                   plot.background = ggplot2::element_rect(fill = site_color, color = site_color),
                   plot.title = ggtext::element_markdown(size = 14, hjust = 0.5),
                   plot.subtitle = ggtext::element_markdown(size = 10, hjust = 0.5),
                   axis.text.x = ggtext::element_markdown())
)

# assign global colors
dem_blu <- MetBrewer::MetPalettes$Benedictus[[1]][13]
rep_red <- MetBrewer::MetPalettes$Benedictus[[1]][1]
purple <- MetBrewer::MetPalettes$Renoir[[1]][3]

