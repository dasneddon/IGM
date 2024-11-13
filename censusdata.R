library(censusapi)
library(readr)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(tidyverse)
library(tidygeocoder)


#DATA IMPORTS
county_pop <- getCensus(
  "dec/dhc",
  vintage = 2020,
  vars = c("NAME", "P1_001N"),
  region = "county:*")

state_info <- fips_info()[c(1,2)]
colnames(state_info) <- c("st", "FIPS.State.Code")

principal <- read.csv("principal.csv")

csa_counties <- read.csv("csa-counties.csv")

worldz <- ne_countries(scale = "large", returnclass = "sf")
us_states <- map_data("state")
us_counties <- map_data("county")
us_csa <- map_data("csa")
states <- st_as_sf(us_states, coords = c("long", "lat"), crs=st_crs("ESRI:102010"), agr = "constant") 
counties <- st_as_sf(us_counties, coords = c("long", "lat"), crs=st_crs("ESRI:102010"), agr = "constant")                                                                 

#DATA CLEANING

county_pop$county <- as.numeric(county_pop$county)
county_pop$state <- as.numeric(county_pop$state)
county_pop$sc_fips <- paste0(county_pop$state,
                             "-", 
                             county_pop$county)
county_pop <- county_pop[,c(5, 4)]


csa_counties$CBSA.Code <- as.numeric(csa_counties$CBSA.Code)                              
csa_counties <- csa_counties[!is.na(csa_counties$CBSA.Code),]
csa_counties$sc_fips <- paste0(csa_counties$FIPS.State.Code,
                               "-", 
                               csa_counties$FIPS.County.Code)

csa_counties <- merge(county_pop,
                      csa_counties,
                      by = "sc_fips")

csa_counties <- merge(csa_counties, state_info, by="FIPS.State.Code")
