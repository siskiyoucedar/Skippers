library(tidyverse)

# read in your data
# no changes were made to the barrier .ods, except removing formatting
# for the station entry / exits .ods, irrelevant columns were removed in addition to removing formatting
stations <- read.csv("_Source_data/stations_data.csv")
barriers <- read.csv("_Source_data/barrier_data.csv")

# clean - I really don't like these periods.

barriers <- barriers |> 
  rename(
    "Station_Name" = `Station.Name`,
    "NLC" = `National.Location.Code..NLC.`,
    "ATG_Count" = `Number.of.ATGs.installed`
  ) |>
  
# add a simple Y/N for ticket barriers
  
  mutate(
  "Ticket_Barriers" = (ifelse((ATG_Count != 0), "Yes", "No")) 
) |> 
  arrange(
    desc(ATG_Count)
  )
  
# clean 2 electric boogaloo

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
  )

# let's have an owner breakdown!

breakdown_owners <- stations_barriers |>
  mutate(
    "Value" = 1
  ) |>
  pivot_wider(
    names_from = Ticket_Barriers,
    values_from = Value,
    values_fill = 0
  ) |>
  group_by(
    Station_Owner
  ) |>
  summarise(
    "Count" = n(),
    "No. with Ticket Barriers" = sum(Yes),
    "% with Ticket Barriers" =  paste0(format(round(
      ((sum(Yes))/Count*100), 0), nsmall = 0), "%"),
    "%_raw" = ((sum(Yes))/Count*100),
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
  mutate(
    "Value" = 1
  ) |>
  pivot_wider(
    names_from = Ticket_Barriers,
    values_from = Value,
    values_fill = 0
  ) |>
  group_by(
    Region
  ) |>
  summarise(
    "Count" = n(),
    "No. with Ticket Barriers" = sum(Yes),
    "% with Ticket Barriers" =  paste0(format(round(
      ((sum(Yes))/Count*100), 0), nsmall = 0), "%"),
    "%_raw" = ((sum(Yes))/Count*100),
    "% All Stations" = paste0(format(round((Count/2579)*100, 0), nsmall = 0), "%")
  ) |>
# remove that one null
  filter(
    Count > 1
  ) |>
  arrange(
    desc(`%_raw`)
  ) 

# that Southeastern result seems wrong. Let's check the output....

write.csv(stations_barriers, "stations_barriers.csv", append = TRUE)

# yeah, it's definitely wrong

