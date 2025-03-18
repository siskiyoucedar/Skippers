library(tidyverse)
library(sf)
library(tmap)

# run after 1_Dataset.R, do not clean environment

### WRANGLING

# The aim here is to append some more data to the ticket barrier stuff.
# I want counties, I want routes...

## COUNTIES

# Read my homespun BFC counties:

ceremonials <- st_read("_Spatial_data\\Counties_BFC.gpkg") |> rename("County_Name" = Name)

# If you don't have that, download the OS ceremonial counties (BFE): https://www.ordnancesurvey.co.uk/products/boundary-line

# commented out because mine's better

# OS_pre_shape <- (
#   "_Spatial_data\\bdline_gb.gpkg"
# )
# OS_layers <- st_layers(
#   OS_pre_shape
# )
# ceremonials <- st_read(
#   OS_pre_shape, "boundary_line_ceremonial_counties"
# )
# 
# 
# rm(OS_layers,OS_pre_shape)

# you'll also need lat / long data - weirdly comes from this FOI on car parking: https://dataportal.orr.gov.uk/media/1924/ad-hoc-station-car-parking.csv

lat_long <- read.csv("_Spatial_data/stations_lat_long.csv") |> 
  rename(
    "TLC" = CrsCode
  )

# note for later - replace this with easting / northing from all stations dataset (this also removes heritage stations)

### MERGING DATA

# turn stations into a point geometry

joined <- left_join(stations_barriers, lat_long)

# we lose 20 stations here - I need a more up to date dataset... but for now this will do

stations_geo <- joined |> filter(!(is.na(Latitude))) |>
  st_as_sf(coords = c("Longitude", "Latitude")) |>
  st_set_crs(4326) |>
  st_transform(27700) 

# check for lost stations: seems it's new stations and the lizzy line

joined_test <- joined |> filter(is.na(Latitude))

rm(joined, joined_test, lat_long)

# A buffer was added for two stations in the sea, but this did not really work. Solution TBC
# Three stations (Newcourt, Oxford Parkway, Apperley Bridge) had their co-ordinates manually changed.

stations_counties <- stations_geo |> st_intersection(ceremonials)

# seems 5 have disappeared...

lost_stations <- stations_geo |> filter(
  !(TLC %in% (stations_counties$TLC))
)

rm(stations_geo, lost_stations) # if not needed

# Ryde - in the sea
# Stranraer - in the sea

# get ticket gate data per county...

county_barriers <- as.data.frame(stations_counties) |>
  group_by(
    County_Name
    ) |>
  summarise(
    "N" = n(),
    "Ticket_Gate_N"= sum(`TRUE.`),
    "%_with_Gates" = paste0(format(
      round(
        (Ticket_Gate_N / N * 100), 0
        ),
      nsmall = 0), "%"),
    "%_Raw" = as.integer(format(
      round(
        (Ticket_Gate_N / N * 100), 0
      ),
    nsmall = 0))
  ) 

# make the shape (and remove missing vals, i.e. counties with no stations)

plot_ceremonials <- left_join(ceremonials, county_barriers) |>
  mutate(
    `%_Raw` = ifelse(is.na(`%_Raw`), 0, `%_Raw`)
  )

# just rework the stations dataset a touch

stations_counties <- stations_counties |>
  mutate(
    "Ticket Barriers" = ifelse(`TRUE.` == 1, TRUE, FALSE)
  ) |>
  select(
    -`TRUE.`, - `FALSE.`,-`NA.`,-Area_Description
  ) 

rm(ceremonials, county_barriers)
