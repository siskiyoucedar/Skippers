## LINING

# too much information on one map, but useful for future reference:

# read railway lines

train_lines_eng <- st_read("_Spatial_data/gis_osm_railways_free_eng.shp") 
train_lines_scot <- st_read("_Spatial_data/gis_osm_railways_free_scot.shp") 
train_lines_wales <- st_read("_Spatial_data/gis_osm_railways_free_wales.shp") 

train_lines <- bind_rows(train_lines_eng, train_lines_scot,train_lines_wales)
rm(train_lines_eng,train_lines_scot,train_lines_wales)

# read train stations

lat_long <- read.csv("_Spatial_data/stations_lat_long.csv") |> 
  rename(
    "TLC" = CrsCode
  )
rm(transport_all)

train_stations <- lat_long |> filter(!(is.na(Latitude))) |>
  st_as_sf(coords = c("Longitude", "Latitude")) |>
  st_set_crs(4326)

# make sure it's all british national grid...

train_lines <- st_transform(train_lines, 7801) 
train_stations <- st_transform(train_stations, 7801) # 2770 to go back to britmap

# get just the datapoints we need (plus geometries):

train_lines <- train_lines |> select("Line" = name)

# train_stations <- train_stations |> select("Name" = name)

# clear off NAs

train_lines <- train_lines |> na.omit()

#try intersecting the train lines

# test <- st_intersection(train_stations, train_lines)

# seems we lost some train lines. Maybe a buffer?

train_lines_buffed <- train_lines |> 
  # test to get a buffer we can comprehend (7801, uses metres as distance)
  #st_transform(crs = 7801) |>
  st_buffer(100)

# test crop to configure around York

# test <- st_crop(train_lines_buffed, xmin = 458000, xmax = 462000, ymin = 450000, ymax = 452000)
# test2 <- st_crop(train_stations, xmin = 458000, xmax = 462000, ymin = 450000, ymax = 452000)
# 
# ggplot(data = test) + geom_sf() + 
# geom_sf(data = test2) +
#   coord_sf()
# 

# maybe better to buff around stations? circularity would help prevent errors
stations_buffed <- st_buffer(train_stations, 500)

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

# Aldgate <- stations_intersect |> filter(Name == "Aldgate")
# Aldgate_OG <- train_stations |> filter(Name == "Aldgate")

# interesting - Aldgate is correct!

# simply need to do a unique()

stations_routes <- as.data.frame(stations_intersect) |> select(TLC, Line) |> unique()

stations_routes <- arrange(stations_routes, Line)

# this below is fine but it doesn't achieve loads

stations_lists <- group_by(stations_routes, Line) |>
  summarise(
    "Count" = n(),
    "Stations" = list(TLC)
  )

# the below cuts out all routes with only one station as they're likely bollox

to_remove <- stations_lists |> filter(
  Count == 1
)

stations_routes <- stations_routes |> filter(
  !(Line %in% to_remove$Line)
)

rm(stations_lists, to_remove)

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

outliers <- filter(stations_TLC, !(Name %in% stations_output$Name))

# ok so um... 1 station *from the ticket dataset* missing, that's actually no biggie

rm(stations_routes, stations_routes_test, stations_TLC, outliers)

### challenge becomes - the lines that were missing from stations... how to configure?

# pivot to tidy - really happy with this!

stations_output_pivot <- pivot_longer(
  stations_output, 
  cols = starts_with("Line"),
  names_to = "REMOVE", # we will not need this
  values_to = "Route_Name"
) |>
  # remove some rogue columns while we're here 
  select(-`NA`, -Name, -REMOVE) |>
  unique() 

# want something that isn't NA signifying no route found, so it's easier to play about with

stations_output_pivot$Route_Name <- replace_na(stations_output_pivot$Route_Name, "None_found")

# let's sort out some easy catches for things that aren't real lines:

stations_output_pivot <- stations_output_pivot |>
  mutate(
    "Route_Name" = ifelse(
      str_detect(Route_Name, paste(c("Depot","depot",
                                     "siding","Siding",
                                     "Back Road", "Platform",
                                     "Tunnel", "Up SloW",
                                     "Down Slow", "Up Fast",
                                     "Down Fast", "Up Goods Loop",
                                     "Up London","DC Electric",
                                     "TMD", "Diamond Crossover",
                                     "Down Hendon", "CTRL Up",
                                     "Down ", "Up Slow",
                                     "Loop"), collapse = "|")
      ),
      "None_found",
      Route_Name
    )
  )

# need a list of all the stations that *did* get attached

stations_output_just_working <- stations_output_pivot |>
  filter(
    Route_Name != "None_found"
  )

# then seek the stations that are not in the "did get attached" list

outliers <- stations_output_pivot |>
  filter(!(TLC %in% stations_output_just_working$TLC))

rm(stations_output_just_working)

# looks about right! I'm going to export this to examine in QGIS to see what's going wrong

lat_long <- read.csv("_Spatial_data/stations_lat_long.csv") |> 
  rename(
    "TLC" = CrsCode
  )

outliers <- left_join(outliers, lat_long)
rm(lat_long)

outliers <- outliers |> filter(!(is.na(Latitude))) |>
  st_as_sf(coords = c("Longitude", "Latitude")) |>
  st_set_crs(4326) |>
  st_transform(27700) 

st_write(outliers, "_Spatial_data/My_Outliers3.gpkg", append = FALSE)

# export all the stations - eliminate 'none found' when data was found.

stations_output_pivot <- stations_output_pivot |>
  mutate("Route_Name" = ifelse(
    (TLC %in% outliers$TLC),
    Route_Name,
    ifelse(
      Route_Name == "None_found", 
      NA, 
      Route_Name
    )
  )
  ) |>
  na.omit() |>
  filter()

write.csv(stations_output_pivot, "_Spatial_data/stations_routes.csv", append = FALSE)

### the dataset is correct when there are 52 stations in the below.

test_ECML <- stations_output |>
  filter(Line_1 == "East Coast Main Line" | Line_2 == "East Coast Main Line" | Line_3 == "East Coast Main Line" | Line_4 == "East Coast Main Line"| Line_5 == "East Coast Main Line" )

rm(test_ECML)

# # NOTE we are still losing some large stations, like Birmingham Snow Hill
# 
# # would consider making the station buffers slightly larger, as the odds of incorporating incorrect data is sufficiently low
# 

########## still getting Snow Hill problems - may plot again soon

### THEN we can plot ticket barriers per route by merging to original dataset.

# need to clear off HS1 underwater - this is crashing R though

# train_lines <-  st_intersection(train_lines, outline)