library(tidyverse)
library(sf)
library(tmap)
library(viridis)

### A plot of ticket barriers per region

# run after 1_dataset.R, do not clean environment

# you'll need to download regions from the open geo portal (link provided in the README.md)
regions <- st_read("_Spatial_data/European_Electoral_Regions_Dec_2018_FCB_UK.shp")

# unfortunately, there's just one element that doesn't match.
# East of England is named Eastern in this data even though they constitute the same area
# so I am manually correcting this.
# this is a fudge, sorry!
regions[6, 2] = "East of England"

# merge information about stations per region with the shape
plot_regions <- regions |> merge(
  breakdown_regions,
  by.x = "eer18nm",
  by.y = "Region",
  all.x = TRUE
) 

# plot the merged shape

breaks = c(0, 5, 10, 20, 30, 40)
labels = c("0 to 5%", "5 to 10%", "10 to 20%", "20 to 30%", "30 to 40%")

tm_shape(plot_regions) +
  tm_fill("%_raw", 
          title = " ",
          breaks = breaks,
          palette = "viridis",
          alpha = 0.9,
          labels = labels
          ) +
  
  tm_borders(col = "gray5") +
  
  # tm_compass(position = c("left", "top")) +
  
  tm_layout(main.title = "% of stations with ticket barriers",
            main.title.size = 1.35,
            main.title.fontfamily = "Arial",
            main.title.fontface = "bold",
            main.title.position = "center",
            legend.position = c("right", "top"),
            legend.text.color = "#fffffc",
            bg.color = "grey15"
            )
