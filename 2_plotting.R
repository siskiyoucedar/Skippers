library(tidyverse)
library(sf)
library(tmap)
library(viridis)

# run after 1_mergy.R, do not clean environment

# you'll need to download regions from the open geo portal
regions <- st_read("_Spatial_data/European_Electoral_Regions_Dec_2018_FCB_UK.shp")

# unfortunately there's just one element that doesn't match.
# this is a fudge, sorry!
regions[6, 2] = "East of England"

# mergy
plot_regions <- regions |> merge(
  breakdown_regions,
  by.x = "eer18nm",
  by.y = "Region",
  all.x = TRUE
) 

# plotty

breaks = c(0, 5, 10, 20, 30, 40)

tm_shape(plot_regions) +
  tm_fill("%_raw", 
          title = " ",
          title.color = "grey15",
          breaks = breaks,
          palette = "viridis",
          alpha = 0.9
          ) +
  
  tm_borders(col = "gray95") +
  
  # tm_compass(position = c("left", "top")) +
  
  tm_layout(main.title = "% of stations with ticket barriers",
            main.title.size = 1.2,
            main.title.fontfamily = "Arial",
            main.title.fontface = "bold",
            main.title.position = "center",
            legend.position = c("right", "top"),
            legend.text.color = "#fffffc",
            bg.color = "grey15"
            )
