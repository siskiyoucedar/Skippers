### I HAVE NO IDEA WHAT I'M DOING: GTFS EDITION

# let's start by getting a locally hosted routing engine working.

# R5R (https://ipeagit.github.io/r5r/articles/r5r.html)

# basic settings

library(r5r)
rJava::.jinit()
options(java.parameters = '-Xmx2G')

# R5R demands a pbf of roads. if we're looking UK wide, unfortunately we need that...

library(osmextract)
library(sf)
test_pbf <- oe_get(
  "England",
  quiet = FALSE,
  query = "SELECT * FROM 'lines' WHERE highway = 'primary'"
)

# hopefully, shouldn't have to grab that again

par(mar = rep(0.1, 4))
plot(st_geometry(test_pbf))

# this is great but it's just for roads

# I'll need something else for railway lines

# I don't feel I need timetable data - I just need to know how a station is reached from 
# a given other station in order to generate a routemap

# OpenTripPlanner (https://docs.ropensci.org/opentripplanner/articles/opentripplanner.html)

library(opentripplanner)

path_data <- file.path(tempdir(), "OTP")
dir.create(path_data) 
path_otp <- otp_dl_jar(path_data)

otp_dl_demo(path_data)

# server setup

log1 <- otp_build_graph(otp = path_otp, dir = path_data) 

serv <- otp_setup(otp = path_otp, dir = path_data)

# connection

otpcon <- otp_connect(timezone = "Europe/London")

otpcon <- otp_connect(hostname =  "localhost",
                      router = "default",
                      port = 8080)

route2 <- otp_plan(otpcon = otpcon, 
                  fromPlace = c(0.9054279, 51.8865086), 
                  toPlace = c(1.1447878, 52.0504188),
                  mode = "RAIL")
otp_stop()

