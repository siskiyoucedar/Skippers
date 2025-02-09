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
# comparing exactly which statiosn have ticket barriers with % of stations cut / kept post beeching (a la PK) 

