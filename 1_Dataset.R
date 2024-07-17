library(tidyverse)

# read in your data

# stations csv via table 1410 ods: https://dataportal.orr.gov.uk/statistics/usage/estimates-of-station-usage/
# (irrelevant columns were removed in addition to removing formatting)
# barriers .csv via a parse of this xml: https://internal.nationalrail.co.uk/4.0/stations.zip

stations <- read.csv("_Source_data/stations_data.csv")
barriers <- read.csv("_Source_data/barrier_data.csv")

# clean - I really don't like these periods.

barriers <- barriers |> select(
  "TLC" = CRS.Code,
  "Ticket_Gates" = TicketGates
)

stations_barriers <- stations |>
  rename(
    "Station_Name" = `Station.name`,
    "NLC" = `National.Location.Code..NLC.`,
    "Tickets_Sold" = `Entries.and.exits...All.tickets`,
    "Rank_Tickets" = `Entries.and.exits.Rank`,
    "TLC" = `Three.Letter.Code..TLC.`,
    "Station_Owner" = `Station.facility.owner`,
    "Main_OD" = `Main.origin.or.destination.station`
  ) |>
  
# join barriers and stations
  
  left_join(
    barriers
  ) |>
  mutate(
    "Value" = 1
  ) |>
  pivot_wider(
    names_from = Ticket_Gates,
    values_from = Value,
    values_fill = 0
  )

# I want to see the stations excluded. 

station_check <- barriers |>
  filter(
    !(TLC %in% stations_barriers$TLC)
  )

# seems they are either new stations in Scotland or stations recently removed from service. 
# That's fine - this year's December data will provide a brief corrective

# let's have an owner breakdown!

breakdown_owners <- stations_barriers |>
  group_by(
    Station_Owner
  ) |>
  summarise(
    "Count" = n(),
    "No. with Ticket Barriers" = sum(`TRUE`),
    "% with Ticket Barriers" =  paste0(format(round(
      ((sum(`TRUE`))/Count*100), 0), nsmall = 0), "%"),
    "%_raw" = ((sum(`TRUE`))/Count*100),
    "% All Stations" = paste0(format(round((Count/2579)*100, 0), nsmall = 0), "%")
    ) |>
  
# remove operators responsible for only one station to simplify results
  
  filter(
    Count > 1
  ) |>
  arrange(
    desc(`%_raw`)
  )

# figures will not sum to 100 because of the removed small operators

# a regional output would be cool too.

breakdown_regions <- stations_barriers |>
  group_by(
    Region
  ) |>
  summarise(
    "Count" = n(),
    "No. with Ticket Barriers" = sum(`TRUE`),
    "% with Ticket Barriers" =  paste0(format(round(
      ((sum(`TRUE`))/Count*100), 0), nsmall = 0), "%"),
    "%_raw" = ((sum(`TRUE`))/Count*100),
    "% All Stations" = paste0(format(round((Count/2579)*100, 0), nsmall = 0), "%")
  ) |>
# remove that one null
  filter(
    Count > 1
  ) |>
  arrange(
    desc(`%_raw`)
  ) 

write.csv(stations_barriers, "stations_barriers.csv")

# 53% in London seems a bit low. Let me flick through...

london_check <- stations_barriers |>
  filter(
    Region == "London"
  )

rm(london_check, station_check, stations, barriers)


