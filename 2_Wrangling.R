library(tidyverse)
library(sf)
library(tmap)
# run after 1_Dataset.R, do not clean environment

### WRANGLING

# The aim here is to append some more data to the ticket barrier stuff.
# I want counties, I want routes...

## COUNTIES

# Read my homespun GB BFC shape (not currently used:)

# outline <- st_read("_Spatial_data/GB_BFC.gpkg")

# Download the OS ceremonial counties: https://www.ordnancesurvey.co.uk/products/boundary-line

# Read ceremonial counties shape:

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

rm(joined, joined_test, lat_long, stations_barriers)

# A buffer was added for two stations in the sea, but this did not really work. Solution TBC
# Three stations (Newcourt, Oxford Parkway, Apperley Bridge) had their co-ordinates manually changed.

stations_counties <- stations_geo |> st_intersection(ceremonials)

# seems 5 have disappeared...

lost_stations <- stations_geo |> filter(
  !(TLC %in% (stations_counties$TLC))
)

rm(stations_geo)

# Ryde - in the sea
# Stranraer - in the sea

# get ticket gate data per county...

county_barriers <- as.data.frame(stations_counties) |>
  group_by(
    Name
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

ceremonial_barriers <- left_join(ceremonials, county_barriers) |>
  mutate(
    `%_Raw` = ifelse(is.na(`%_Raw`), 0, `%_Raw`)
  )
rm(ceremonials, county_barriers)

## LINING

# too much information on one map, but useful for future reference:

# read railway lines

train_lines <- st_read("_Spatial_data/gis_osm_railways_free_1.shp") 

# read train stations

transport_all <- st_read("_Spatial_data/gis_osm_transport_free_1.shp")
train_stations <- transport_all |> filter(fclass == "railway_station")
rm(transport_all)

# make sure it's all british national grid...

train_lines <- st_transform(train_lines, 27700) 
train_stations <- st_transform(train_stations, 27700)

# get just the datapoints we need (plus geometries):

train_lines <- train_lines |> select("Line" = name)
train_stations <- train_stations |> select("Name" = name)

#try intersecting the train lines

test <- st_intersection(train_stations, train_lines)

# seems we lost some train lines. Maybe a buffer?

help_buff1 <- st_buffer(train_lines, 1)
help_buff2 <- st_buffer(train_lines, 2)
help_buff5 <- st_buffer(train_lines, 5)
help_buff10 <- st_buffer(train_lines, 10)

# seems 1 isn't enough

test1 <- st_intersection(train_stations, help_buff)
test2 <- st_intersection(train_stations, help_buff2)
test5 <- st_intersection(train_stations, help_buff5)
test10 <- st_intersection(train_stations, help_buff10)

# maybe better to buff around stations? circularity would help prevent errors
stations_buffed <- st_buffer(train_stations, 30)
test_buff_stat <- st_intersection(stations_buffed, help_buff)

# check exactly how many stations are repeating, and by how much
multiples <- test_buff_stat |>
  group_by(Name) |>
  summarise(
    "Count" = n()
  )

# seems Little Ruddington is the winner: how dire is it?
Little_Ruddington <- test_buff_stat |> filter(Name == "Little Ruddington")

# seems the lines with NA are causing issues... how do we remove them?
lines_no_NA <- help_buff |> na.omit()

test_no_NA <- st_intersection(stations_buffed, lines_no_NA)

# check exactly how many stations are repeating, and by how much (with no NAs)
multiples_no_NA <- test_no_NA |>
  group_by(Name) |>
  summarise(
    "Count" = n()
  )

# Aldgate still weird... let's see

Aldgate <- test_no_NA |> filter(Name == "Aldgate")

# interesting - Aldgate is correct!

# simply need to do a unique()

stations_routes <- as.data.frame(test_no_NA) |> select(Name, Line) |> unique()

# this number is larger than the number of stations in the UK as some carry multiple routes. this can be fixed...

# the below doesn't work - need to find a way that spreads the data per station

stations_routes <- pivot_wider(stations_routes, names_from = c("Line1", "Line2",), values_from = Line)

outliers <- filter(train_stations, !(Name %in% test_no_NA$Name))

#_however_ we are still losing some large stations, like Birmingham Snow Hill

# would consider making the station buffers slightly larger, as the odds of incorporating incorrect data is sufficiently low


### THEN we can plot ticket barriers per route by merging to original dataset.

# need to clear off HS1 underwater - this is crashing R though

# train_lines <-  st_intersection(train_lines, outline)





### FACTUALS

# building a couple of fun functions with the data

no_barriers <- stations_counties |>
  filter(
    `FALSE.` == 1
  )

simple_NB_list <- as.data.frame(no_barriers) |>
  select(Station_Name, TLC)

# a nice device for checking journeys

journey_checker <- function(x,y) {
  ifelse(
    x %in% simple_NB_list$Station_Name, 
    
    # if X was in the list:
    ifelse(
      y %in% simple_NB_list$Station_Name, 
      
      # if Y was in the list too:
      print(
        "This journey involves no ticket barriers."
        ), 
      
      # if X was in the list but Y wasn't:
      print(
          paste0(
            "This journey involves ticket barriers at ", y, " but not ", x, "."
            )
          )
      ), 
    
    # if X wasn't in the list:
    ifelse(
        y %in% simple_NB_list$Station_Name, 
        
        # if Y was in the list but X wasn't:
        print(
          paste0(
            "This journey involves ticket barriers at ", x, " but not ", y, "."
          )
        ), 
        
        # if neither X nor Y were in the list:
        print(
          "This journey involves ticket barriers at both ends."
        )
      )
    )
}
journey_checker("Ipswich", "Norwich")

# other things to consider:
# busiest O-D pairs with no ticket barriers?
# longest journey with no ticket barriers? (needs OTP)
# incorporate table 6329 (more recent station list with eastings, northings)
# find info on which stations on which routes (manual routing but also % no ticket barriers on each route)
