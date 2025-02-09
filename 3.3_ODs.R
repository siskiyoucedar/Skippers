library(tidyverse)
library(sf)
library(tmap)

# run after 2_Wrangling.R, do not clean environment

# create a frame of all the stations WITHOUT barriers

subset <- stations_barriers |> 
  filter(`FALSE` == 1) |>
  select(
    Station_Name, Tickets_Sold, Main_OD, Station_Owner, Region
    ) |>
  arrange(desc(Tickets_Sold))

# filter for stations where the main origin / destination is *also* a station with no barriers

subsubset <- subset |>
  filter(Main_OD %in% subset$Station_Name) |>
  arrange(desc(Tickets_Sold))