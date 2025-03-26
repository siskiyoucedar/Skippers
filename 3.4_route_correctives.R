# this dataset can run independently of the others

library(tidyverse)

# first read in our station data - this dataset contains modified entries for otherwise borked stations

stations <- read.csv("stations_routes_modified.csv", row.names = 1) 
# 
# stations_raw <- read.csv("_Spatial_data/stations_routes.csv", row.names = 1)
# 
# clipped <- stations_raw |>
#   filter(
#     Route_Name == "None_found"
#   )
# 
# stations_corrected <- stations |>
#   filter(
#     TLC %in% clipped$TLC
#   )
# 
# stations <- stations_raw |>
#   rbind(stations_corrected) |>
#   filter(
#     !(Route_Name == "None_found")
#   )

stations <- stations |>
  mutate(
    "Route_Name" = str_to_title(Route_Name)
  ) |>
  unique()

# up barry, up llandaff, "Line"... need to modify more
  
# get rid of metro services exc lizzy line

# LINES TO REMOVE:
to_remove <- c(
  # "Elizabeth Line",
  "Central Line",
  "Northern Line",
  "Jubilee Line",
  "Bakerloo Line",
  "Victoria Line",
  "Circle, Hammersmith & City and Metropolitan Lines",
  "Circle and District Lines",
  "Circle and Hammersmith & City Lines",
  "Docklands Light Railway",
  "Metropolitan Line",
  "Picadilly Line",
  "District Line",
  "Hammersmith & City Line",
  "Circle Line",
  "Waterloo & City Line",
  "Hammersmith & City and Metropolitan Lines",
  "None_found" # essential
)

stations <- stations |>
  filter(!(Route_Name %in% to_remove))

# let's summarise

# first I'll need a stat for the number of passengers travelling from stations w no ticket barriers (dearth)

distinguishing <- stations |>
  mutate(
    "dearth" = ifelse(
      `FALSE.` == 1, Tickets_Sold, 0
    )
  ) |>
  group_by(Route_Name) |>
  summarise(
    "Count" = n(),
    "Total Tickets" = sum(Tickets_Sold),
    "% Ticket Barriers" = sum(`TRUE.`)/(sum(`TRUE.`)+sum(`FALSE.`)),
    "% Journeys from Ticket Barrier-Less Stations" = round((sum(dearth) / sum(Tickets_Sold) *100), digits = 2) 
  ) |>
  arrange(desc(`Total Tickets`))

# might be interesting to investigate the routes with the greatest no. of passengers but no ticket barriers

biggest_shortfall <- distinguishing |>
  filter(
    `% Journeys from Ticket Barrier-Less Stations`> 99
  ) |>
  arrange(desc(`Total Tickets`)) |>
  
  # clear off branch lines etc.
  
  filter(
    Count > 2
  )

# and the inverse - routes with the highest % ticket barriers but fewest passengers

lowest_benefit <- distinguishing |>
  filter(
    `% Journeys from Ticket Barrier-Less Stations` <= 5
  ) |>
  arrange(`Total Tickets`) |>
  
  # clear off branch lines etc.
  
  filter(
    Count > 2
  )

ECML_stations <- stations |>
  filter(
    Route_Name == "East Coast Main Line"
  )

# hurrah! that one at least is correct now

print(ECML_stations$Station_Name)

distinguished_stations <- stations |>
  merge(distinguishing)


