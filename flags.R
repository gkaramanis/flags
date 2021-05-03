library(tidyverse)
library(imager)
library(cowplot)
library(jsonlite)

# Create data frame with country codes and flag URLs
# from https://github.com/hampusborgos/country-flags
# by hampusborgos
countries <- stack(fromJSON("https://raw.githubusercontent.com/hampusborgos/country-flags/main/countries.json")) %>% 
  select(code = ind, country = values) %>% 
  mutate(flag = paste0("https://raw.githubusercontent.com/hampusborgos/country-flags/main/png1000px/", tolower(code), ".png"))

# Uncomment to plot one flag
# countries <- countries %>% 
#   filter(code == "GB")

# Loop through countries
for (i in 1:nrow(countries)) {
  
  # load flag
  fl <- load.image(countries$flag[i])
  
  # calculate flag aspect ratio
  r = width(fl) / height(fl)
  
  # get pixel hex colors in image
  fl_hex <- fl %>% 
    # uncomment for faster plotting (and scaling artifacts!)
    # resize(size_y = 200, size_x = 200 * r) %>%
    as.data.frame() %>% 
    pivot_wider(id_cols = c(x, y), names_from = cc, names_prefix = "c") %>% 
    mutate(hex_col = rgb(c1, c2, c3))
  
  # make histogram of colors along the y axis
  py <- ggplot(fl_hex) +
    geom_histogram(aes(x = y, fill = hex_col), binwidth = 1) +
    scale_fill_identity() +
    coord_flip() +
    scale_x_reverse() +
    theme_void() +
    theme(
      plot.background = element_rect(fill = "grey60", color = NA)
    )
  
  # make histogram of colors along the x axis
  px <- ggplot(fl_hex) +
    geom_histogram(aes(x = x, fill = hex_col), binwidth = 1) +
    scale_fill_identity() +
    theme_void() +
    theme(
      plot.background = element_rect(fill = "grey60", color = NA)
    )
  
  # plot the flag
  p <- ggplot(fl_hex, aes(x, y, fill = hex_col)) +
    geom_tile() +
    scale_y_reverse() +
    scale_fill_identity() +
    theme_void() +
    theme(
      plot.background = element_rect(fill = "grey60", color = NA)
    )
  
  # place histograms along the axes 
  p1 <- insert_xaxis_grob(p, px, grid::unit(.2, "null"), position = "top")
  p2 <- insert_yaxis_grob(p1, py, grid::unit(.2, "null"), position = "right")
  
  # plot everything and save image
  ggdraw(p2) +
    ggsave(here::here("plots", paste0(countries$code[i], ".png")), dpi = 320, height = 10, width = 10 * r)
  
  }