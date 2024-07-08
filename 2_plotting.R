library(tidyverse)
library(sf)
library(tmap)
library(viridis)
library(extrafont)
loadfonts(device = "win", quiet = TRUE) 

### A plot of ticket barriers per region

# run after 1_Dataset.R, do not clean environment

# you'll need to download regions from the open geo portal: https://geoportal.statistics.gov.uk/datasets/81dd0b26640b45b0bd873372355e29e8_0/explore
regions <- st_read("_Spatial_data/European_Electoral_Regions_Dec_2018_FCB_UK.shp")

# then download the OS ceremonial counties: https://www.ordnancesurvey.co.uk/products/boundary-line
OS_pre_shape <- (
  "_Spatial_data\\bdline_gb.gpkg"
)
OS_layers <- st_layers(
  OS_pre_shape
)
ceremonials <- st_read(
  OS_pre_shape, "boundary_line_ceremonial_counties"
)
rm(OS_layers,OS_pre_shape)

# you'll also need lat / long data - comes from this FOI on car parking: https://dataportal.orr.gov.uk/media/1924/ad-hoc-station-car-parking.csv
lat_long <- read.csv("_Spatial_data/stations_lat_long.csv") |> 
  rename(
    "TLC" = CrsCode
  )

# unfortunately, there's just one element that doesn't match in the region data...
# East of England is named Eastern in this data even though they constitute the same area
# so I am manually correcting this.
# this is a fudge, sorry!
regions[6, 2] = "East of England"

## MERGING DATA

# merge information about stations per region with the shape
plot_regions <- regions |> merge(
  breakdown_regions,
  by.x = "eer18nm",
  by.y = "Region",
  all.x = TRUE
) 

# turn stations into a point geometry

joined <- left_join(stations_barriers, lat_long)

# we lose 20 stations here - I need a more up to date dataset... but for now this will do

stations_geo <- joined |> filter(!(is.na(Latitude))) |>
  st_as_sf(coords = c("Longitude", "Latitude")) |>
  st_set_crs(4326) |>
  st_transform(27700) 

joined_test <- joined |> filter(is.na(Latitude))
rm(joined)

# A buffer was added for two stations in the sea, but this did not really work. Solution TBC
# Three stations (Newcourt, Oxford Parkway, Apperley Bridge) had their co-ordinates manually changed.

stations_counties <- stations_geo |> st_intersection(ceremonials)

# seems 5 have disappeared...

lost_stations <- stations_geo |> filter(
  !(TLC %in% (station_counties$TLC))
)

# Ryde - in the sea
# Stranraer - in the sea


# get ticket gate data per county...

county_barriers <- stations_counties |>
  mutate(
    "Value" = 1
  ) |>
  pivot_wider(
    names_from = Ticket_Gates,
    values_from = Value,
    values_fill = 0
  ) |> 
  select(
    Name, `TRUE`
  ) |>
  group_by(
    Name
    ) |>
  summarise(
    "N" = n(),
    "Ticket_Gate_N"= sum(`TRUE`),
    "%_with_Gates" = paste0((Ticket_Gate_N / N * 100)+"%")
  )

### PLOTTING

# plot the region shape

breaks = c(0, 6, 11, 26, 31, 60)
labels = c("0 to 5%", "5 to 10%", "10 to 25%", "25 to 30%", "Over 50%")

tmap_mode("view")

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

  tm_shape(stations_geo) +
  tm_symbols(size = 0.05, col = "red")
