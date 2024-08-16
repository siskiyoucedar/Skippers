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

test1 <- st_intersection(train_stations, help_buff1)
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
lines_no_NA <- help_buff1 |> na.omit()

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

stations_buffed2 <- st_buffer(train_stations, 50)
test_buff_stat2 <- st_intersection(stations_buffed2, lines_no_NA)
outliers <- filter(train_stations, !(Name %in% test_buff_stat2$Name))

########## still getting Snow Hill problems - may plot again soon

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
