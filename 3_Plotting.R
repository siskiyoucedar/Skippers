library(tidyverse)
library(sf)
library(tmap)
library(viridis)
library(extrafont)
loadfonts(device = "win", quiet = TRUE) 
# run after 2_Wrangling.R, do not clean environment

# you'll need to download regions from the open geo portal: https://geoportal.statistics.gov.uk/datasets/81dd0b26640b45b0bd873372355e29e8_0/explore
regions <- st_read("_Spatial_data/European_Electoral_Regions_Dec_2018_FCB_UK.shp")

# unfortunately, there's just one element that doesn't match in the region data...
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

# plot the region shape

for_map <- stations_counties |>
  mutate(
    "Ticket Barriers?" = ifelse(`TRUE.` == 1, "Ticket Barrier", "No Ticket Barrier")
  )

breaks = c(0, 6, 11, 26, 31, 60)
labels = c("0 to 5%", "5 to 10%", "10 to 25%", "25 to 30%", "Over 50%")

tmap_mode("plot")

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
            main.title.fontfamily = "Accidental Presidency",
            main.title.position = "center",
            legend.position = c("right", "top"),
            legend.text.color = "#fffffc",
            bg.color = "grey15"
  ) +
  
  tm_shape(for_map) +
  tm_symbols(size = 0.2, 
             col = "Ticket Barriers?",
             
  ) +
  tm_shape(train_lines) +
  tm_lines(col = "black",
           alpha = 0.5)

# plot the county shape

breaks2 = c(0, 6, 11, 21, 31, 41, 51, 60, 101)
labels2 = c("0 to 5%", "5 to 10%", "10 to 20%", "20 to 30%", "30 to 40%", "40 to 50%","50 to 60%", "100%")

tm_shape(ceremonial_barriers) +
  tm_fill("%_Raw", 
          title = " ",
          breaks = breaks2,
          palette = "viridis",
          alpha = 0.9,
          labels = labels2
  ) +
  
  tm_borders(col = "gray5") +
  
  # tm_compass(position = c("left", "top")) +
  
  tm_layout(main.title = "% of stations with ticket barriers",
            main.title.size = 2,
            main.title.fontfamily = "Accidental Presidency",
            main.title.position = "center",
            legend.position = c("right", "top"),
            legend.text.color = "#fffffc",
            bg.color = "grey15"
  )