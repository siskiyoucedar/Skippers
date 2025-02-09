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

# clear off NAs

train_lines <- train_lines |> na.omit()

#try intersecting the train lines

# test <- st_intersection(train_stations, train_lines)

# seems we lost some train lines. Maybe a buffer?

train_lines_buffed <- st_buffer(train_lines, 1)

# help_buff1 <- st_buffer(train_lines, 1)
# help_buff2 <- st_buffer(train_lines, 2)
# help_buff5 <- st_buffer(train_lines, 5)
# help_buff10 <- st_buffer(train_lines, 10)

# # seems 1 isn't enough
# 
# test1 <- st_intersection(train_stations, help_buff1)
# test2 <- st_intersection(train_stations, help_buff2)
# test5 <- st_intersection(train_stations, help_buff5)
# test10 <- st_intersection(train_stations, help_buff10)

# maybe better to buff around stations? circularity would help prevent errors
stations_buffed <- st_buffer(train_stations, 50)

stations_intersect <- st_intersection(stations_buffed, train_lines_buffed)
rm(stations_buffed, train_lines_buffed)

# # check exactly how many stations are repeating, and by how much
# multiples <- stations_intersect |>
#   group_by(Name) |>
#   summarise(
#     "Count" = n()
#   )

# seems Little Ruddington is the winner: how dire is it?
# Little_Ruddington <- test_buff_stat |> filter(Name == "Little Ruddington")

# check exactly how many stations are repeating, and by how much (with no NAs)
# multiples_no_NA <- stations_intersect |>
#   group_by(Name) |>
#   summarise(
#     "Count" = n()
#   )

# Aldgate still weird... let's see

Aldgate <- stations_intersect |> filter(Name == "Aldgate")
Aldgate_OG <- train_stations |> filter(Name == "Aldgate")

# interesting - Aldgate is correct!

# simply need to do a unique()

stations_routes <- as.data.frame(stations_intersect) |> select(Name, Line) |> unique()

stations_routes <- arrange(stations_routes, Line)

# this below is fine but it doesn't achieve loads
# it also illustrates we need a baseline clean on station names

stations_lists <- group_by(stations_routes, Line) |>
  summarise(
    "Count" = n(),
    "Stations" = list(Name)
  )

to_remove <- stations_lists |> filter(
  Count == 1
)

# the below cuts out all routes with only one station as they're likely bollox

stations_routes <- stations_routes |> filter(
  !(Line %in% to_remove$Line)
)

# what about stations with brackets?

# let's try and get them perf based on the original stations csv

stations_TLC <- stations_barriers |>
  rename(
  "Name" = Station_Name
  ) |>
  select(
    Name, TLC
  )

stations_routes_test <- stations_routes |>
  merge(
    stations_TLC, all.y = TRUE
  )

# could do a cut down for parentheses based on the above...

stations_routes_test <- stations_routes_test |>
  mutate(Line_test = 
           
    # remove everything after "lines"
           
    ifelse(str_detect(Line, "lines"),
    str_replace(Line, "lines.*", "lines"),
                  
          # remove everything after "Lines"
                  
          ifelse(str_detect(Line, "Lines"),
          str_replace(Line, "Lines.*", "Lines"),
          
                  # remove everything after "line"
                  
                  ifelse(str_detect(Line, "line"),
                         str_replace(Line, "line.*", "line"),
                         
                         # remove everything after "Line"
                                
                                ifelse(str_detect(Line, "Line"),
                                       str_replace(Line, "Line.*", "Line"),
                                       Line))))
        ) |>

# group_by names to get ids to make pivoting possible
  
  select(-Line) |>
  unique() |>
  group_by(Name) |>
  mutate(Line_id = paste0("Line_", row_number())) |>
  ungroup() |>
  
  # do the pivot
  
  pivot_wider(
    names_from = Line_id,
    values_from = Line_test
  )

stations_output <- stations_barriers |>
  merge(
    stations_routes_test, by.x = "TLC"
  ) |>
  tail(-1)

test_ECML <- stations_output |>
  filter(Line_1 == "East Coast Main Line" | Line_2 == "East Coast Main Line" | Line_3 == "East Coast Main Line" | Line_4 == "East Coast Main Line"| Line_5 == "East Coast Main Line" )






# outliers <- filter(train_stations, !(Name %in% test_no_NA$Name))
# 
# # NOTE we are still losing some large stations, like Birmingham Snow Hill
# 
# # would consider making the station buffers slightly larger, as the odds of incorporating incorrect data is sufficiently low
# 
# stations_buffed2 <- st_buffer(train_stations, 50)
# test_buff_stat2 <- st_intersection(stations_buffed2, lines_no_NA)
# outliers <- filter(train_stations, !(Name %in% test_buff_stat2$Name))

########## still getting Snow Hill problems - may plot again soon

### THEN we can plot ticket barriers per route by merging to original dataset.

# need to clear off HS1 underwater - this is crashing R though

# train_lines <-  st_intersection(train_lines, outline)