# this code only needs to be run once - it generates the .pbf for the work in 4_1

library(osmdata) # just in case, not used at present
library(osmextract) # https://www.rdocumentation.org/packages/osmextract/versions/0.5.1
library(tmap)

# set up directory for OTP files

path_data <- file.path("_OTP")
dir.create(path_data) 

# set up OTP path

path_otp <- otp_dl_jar(path_data)

# otp_dl_demo(path_data)

### in lieu of the demo data we need to set up new path data, of format:

#/ OTP                 # Your top folder for storing all OTP data
# /graphs                     
# /default             # Subfolder with the name of the router
# osm.pbf              # Required OSM road map
# router-config.json   # Optional config file
# build-config.json    # Optional config file
# gtfs.zip             # Optional GTFS data
# dem.tif              # Optional Elevation data

# tom's means of pulling that data from OSM (currently not used)

bbox = getbb("England")

# query <- opq(bbox = bbox) |>
#   add_osm_feature(key = "railway",
#                   value = c("rail","subway","tram","light_rail"))

# https://wiki.openstreetmap.org/wiki/Map_features#Railway

# this is a big request, so...
options(timeout = max(1000, getOption("timeout")))

# the bit that tests the comp
railway_data <- osmdata_sf(query)

# oe_get() documentation: https://www.rdocumentation.org/packages/osmextract/versions/0.5.1/topics/oe_get 

 osm_rail_pbf <- oe_get("England", stringsAsFactors = FALSE, quiet = TRUE, extra_tags = "railway",
                    query = "SELECT * FROM 'lines' WHERE railway = 'rail'"
                    ) 

# plot to make sure it's the right thing

# qtm(osm_rail_pbf)

# consider removing all irrelevant railway lines (heritage etc.)

