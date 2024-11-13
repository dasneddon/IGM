library(tigris)
options(tigris_use_cache = TRUE)
library(sf)
library(dplyr)
library(censusapi)
library(units)
library(geosphere)

###IMPORT GEOGRAPHIC DATA

counties <- counties()
counties$centroid <- st_centroid(counties$geometry)
counties$lon <- sf::st_coordinates(counties$centroid)[,1]
counties$lat <- sf::st_coordinates(counties$centroid)[,2]
counties <- counties[,c("GEOID", "lon", "lat")]
cbsa <- read.csv("csa-counties.csv")
cbsa$GEOID <- paste0(formatC(cbsa$FIPS.State.Code, width=2, flag=0),
                     formatC(cbsa$FIPS.County.Code, width=3, flag=0))

###IMPORT COUNTY POPUATIONS

county_pop <- getCensus(
  "dec/dhc",
  vintage = 2020,
  vars = c("NAME", "P1_001N"),
  region = "county:*")

county_pop$GEOID <- paste0(county_pop$state, county_pop$county)
county_pop <- merge(counties,county_pop, by="GEOID")
county_pop$area <- set_units(st_area(county_pop$geometry), "mi^2")
county_pop$density <- county_pop$P1_001N/county_pop$area

##MERGE POPULATIONS AND GEOGRAPHY
cbsa <- merge(cbsa,county_pop, by="GEOID")
cbsa$densest <- 0
cbsa$distance <- set_units(0, "m")

##FIND THE DENSEST COUNTY

densecounty <- function(combstatarea){
  cbsaframe <- cbsa[cbsa$CBSA.Code==combstatarea,]
  highest <- max(cbsaframe$density)
  return(cbsaframe$GEOID[cbsaframe$density==highest])
}

for (i in unique(cbsa$CBSA.Code)){
  flag <- densecounty(i)
  cbsa$densest[cbsa$CBSA.Code==i] <- flag
}

for (i in 1:nrow(cbsa)){
  flag <- cbsa$densest[i]  
  cbsa$distance[i] <- distHaversine(c(cbsa[i,"lon"],
                                      cbsa[i,"lat"]),
                                    c(cbsa$lon[cbsa$GEOID==flag],
                                      cbsa$lat[cbsa$GEOID==flag]))
}

cbsa$distance <- set_units(cbsa$distance, "mi")



