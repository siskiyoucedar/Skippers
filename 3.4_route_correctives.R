
# first read in our station data

stations <- read.csv("_Spatial_data/stations_routes_modified.csv", row.names = 1) 

# needs modifying again to clear off 'up slow'

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
  arrange(desc(`Total Tickets`))

# and the inverse - routes with the highest % ticket barriers but fewest passengers

lowest_benefit <- distinguishing |>
  filter(
    `% Journeys from Ticket Barrier-Less Stations` <= 5
  ) |>
  arrange(`Total Tickets`)

ECML_stations <- stations |>
  filter(
    Route_Name == "East Coast Main Line"
  )

# hurrah! that one at least is correct now

print(ECML_stations$Station_Name)

distinguished_stations <- stations |>
  merge(distinguishing)


