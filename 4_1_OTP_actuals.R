
# OpenTripPlanner 

library(opentripplanner) # https://docs.ropensci.org/opentripplanner/articles/opentripplanner.html
library(osmdata)
library(osmextract) # https://www.rdocumentation.org/packages/osmextract/versions/0.5.1
library(sf)
library(tmap)

# set up directory for OTP files

path_data <- file.path("_OTP")
dir.create(path_data) 

# set up OTP path

path_otp <- otp_dl_jar(path_data)

# Build the graph for the trip to be conducted

log1 <- otp_build_graph(otp = path_otp, dir = path_data) 

# (this will take a long time)

serv <- otp_setup(otp = path_otp, dir = path_data)

# connection

otpcon <- otp_connect(timezone = "Europe/London")

otpcon <- otp_connect(hostname =  "localhost",
                      router = "default",
                      port = 8080)

route2 <- otp_plan(otpcon = otpcon, 
                   fromPlace = c(0.9054279, 51.8865086), 
                   toPlace = c(1.1447878, 52.0504188)
                   #,
                   #mode = "RAIL"
)
otp_stop()
