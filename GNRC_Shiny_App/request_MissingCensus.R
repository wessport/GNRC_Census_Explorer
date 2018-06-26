# Greater Nashville Regional Council
# Project: GNRC Census Explorer
# Creation Date: 26-JUNE-2018
# Script Author: Wes Porter
# Script Summary: Formatting ACS data with missing years
# Last Updated: 26-JUNE-2018

library(tidycensus)
library(leaflet)
library(rgdal)
library(magrittr)
library(sf)
library(tigris)
library(tidyverse)
library(lettercase)
library(lwgeom)
library(htmltools)
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)
census_api_key(Sys.getenv("CENSUS_API_KEY"))

source("C:/Users/wPorter/GitHub/GNRC_Census_Explorer/GNRC_Shiny_App/format_CensusTable.R")

# Define GNRC counties
counties <- c('Cheatham','Davidson','Dickson','Houston','Humphreys','Montgomery','Maury','Robertson','Rutherford','Stewart','Sumner',
              'Trousdale','Williamson','Wilson')
state <- 'TN'

# Health Insurance

tableID <- "B27001"

county_data_16 <- format_Census('county',tableID,2016,state,counties)
tract_data_16 <- format_Census('tract',tableID,2016,state,counties)
bg_data_16 <- format_Census('block group',tableID,2016,state,counties)

county_data_15 <- format_Census('county',tableID,2015,state,counties)
tract_data_15 <- format_Census('tract',tableID,2015,state,counties)
bg_data_15 <- format_Census('block group',tableID,2015,state,counties)

county_data_14 <- format_Census('county',tableID,2014,state,counties)
tract_data_14 <- format_Census('tract',tableID,2014,state,counties)
bg_data_14 <- format_Census('block group',tableID,2014,state,counties)

county_data_13 <- format_Census('county',tableID,2013,state,counties)
tract_data_13 <- format_Census('tract',tableID,2013,state,counties)
bg_data_13 <- format_Census('block group',tableID,2013,state,counties)

county_data_12 <- format_Census('county',tableID,2012,state,counties)
tract_data_12 <- format_Census('tract',tableID,2012,state,counties)
bg_data_12 <- format_Census('block group',tableID,2012,state,counties)

# Missing for 2011
# county_data_11 <- format_Census('county',tableID,2011,state,counties)
# tract_data_11 <- format_Census('tract',tableID,2011,state,counties)
# bg_data_11 <- format_Census('block group',tableID,2011,state,counties)

county_data_12 %>%
  select(GEOID,NAME)%>%
  mutate(Vintage = 2011, Level = 'county') -> county_data_11

tract_data_12 %>%
  select(GEOID,NAME)%>%
  mutate(Vintage = 2011, Level = 'tract') -> tract_data_11

bg_data_12 %>%
  select(GEOID,NAME)%>%
  mutate(Vintage = 2011, Level = 'block group') -> bg_data_11

a <-
  rbind(
    county_data_16,
    tract_data_16,
    bg_data_16,
    county_data_15,
    tract_data_15,
    bg_data_15
  )

b <-
  rbind(
    county_data_14,
    tract_data_14,
    bg_data_14,
    county_data_13,
    tract_data_13,
    bg_data_13,
    county_data_12,
    tract_data_12,
    bg_data_12,
    county_data_11,
    tract_data_11,
    bg_data_11
  )

# Prepare census data for row binding by filling mismatched columns with NA
a[setdiff(names(b), names(a))] <- NA
b[setdiff(names(a), names(b))] <- NA

health_insr_cov_dt <- rbind(a,b)

saveRDS(health_insr_cov_dt,"./data/Health insurance coverage status by sex by age.rds")


# Means of transportation to work white alone
# Appears to have data, just didn't work on first try
tableID <- "B08105A"

county_data_16 <- format_Census('county',tableID,2016,state,counties)
tract_data_16 <- format_Census('tract',tableID,2016,state,counties)
bg_data_16 <- format_Census('block group',tableID,2016,state,counties)

county_data_15 <- format_Census('county',tableID,2015,state,counties)
tract_data_15 <- format_Census('tract',tableID,2015,state,counties)
bg_data_15 <- format_Census('block group',tableID,2015,state,counties)

county_data_14 <- format_Census('county',tableID,2014,state,counties)
tract_data_14 <- format_Census('tract',tableID,2014,state,counties)
bg_data_14 <- format_Census('block group',tableID,2014,state,counties)

county_data_13 <- format_Census('county',tableID,2013,state,counties)
tract_data_13 <- format_Census('tract',tableID,2013,state,counties)
bg_data_13 <- format_Census('block group',tableID,2013,state,counties)

county_data_12 <- format_Census('county',tableID,2012,state,counties)
tract_data_12 <- format_Census('tract',tableID,2012,state,counties)
bg_data_12 <- format_Census('block group',tableID,2012,state,counties)

county_data_11 <- format_Census('county',tableID,2011,state,counties)
tract_data_11 <- format_Census('tract',tableID,2011,state,counties)
bg_data_11 <- format_Census('block group',tableID,2011,state,counties)

a <-
  rbind(
    county_data_16,
    tract_data_16,
    bg_data_16,
    county_data_15,
    tract_data_15,
    bg_data_15
  )

b <-
  rbind(
    county_data_14,
    tract_data_14,
    bg_data_14,
    county_data_13,
    tract_data_13,
    bg_data_13,
    county_data_12,
    tract_data_12,
    bg_data_12,
    county_data_11,
    tract_data_11,
    bg_data_11
  )

# Prepare census data for row binding by filling mismatched columns with NA
a[setdiff(names(b), names(a))] <- NA
b[setdiff(names(a), names(b))] <- NA

means_trans_white_dt <- rbind(a,b)

saveRDS(means_trans_white_dt,"./data/Means of transportation to work white alone.rds")


# Medicare Coverage by sex by age

tableID <- "C27006"

county_data_16 <- format_Census('county',tableID,2016,state,counties)
tract_data_16 <- format_Census('tract',tableID,2016,state,counties)
bg_data_16 <- format_Census('block group',tableID,2016,state,counties)

county_data_15 <- format_Census('county',tableID,2015,state,counties)
tract_data_15 <- format_Census('tract',tableID,2015,state,counties)
bg_data_15 <- format_Census('block group',tableID,2015,state,counties)

county_data_14 <- format_Census('county',tableID,2014,state,counties)
tract_data_14 <- format_Census('tract',tableID,2014,state,counties)
bg_data_14 <- format_Census('block group',tableID,2014,state,counties)

county_data_13 <- format_Census('county',tableID,2013,state,counties)
tract_data_13 <- format_Census('tract',tableID,2013,state,counties)
bg_data_13 <- format_Census('block group',tableID,2013,state,counties)

county_data_12 <- format_Census('county',tableID,2012,state,counties)
tract_data_12 <- format_Census('tract',tableID,2012,state,counties)
bg_data_12 <- format_Census('block group',tableID,2012,state,counties)

# Missing for 2011
# county_data_11 <- format_Census('county',tableID,2011,state,counties)
# tract_data_11 <- format_Census('tract',tableID,2011,state,counties)
# bg_data_11 <- format_Census('block group',tableID,2011,state,counties)

county_data_12 %>%
  select(GEOID,NAME)%>%
  mutate(Vintage = 2011, Level = 'county') -> county_data_11

tract_data_12 %>%
  select(GEOID,NAME)%>%
  mutate(Vintage = 2011, Level = 'tract') -> tract_data_11

bg_data_12 %>%
  select(GEOID,NAME)%>%
  mutate(Vintage = 2011, Level = 'block group') -> bg_data_11

a <-
  rbind(
    county_data_16,
    tract_data_16,
    bg_data_16,
    county_data_15,
    tract_data_15,
    bg_data_15
  )

b <-
  rbind(
    county_data_14,
    tract_data_14,
    bg_data_14,
    county_data_13,
    tract_data_13,
    bg_data_13,
    county_data_12,
    tract_data_12,
    bg_data_12,
    county_data_11,
    tract_data_11,
    bg_data_11
  )

# Prepare census data for row binding by filling mismatched columns with NA
a[setdiff(names(b), names(a))] <- NA
b[setdiff(names(a), names(b))] <- NA

medicare_cov_by_sex_dt <- rbind(a,b)

saveRDS(medicare_cov_by_sex_dt,"./data/Medicare coverage by sex by age.rds")


# Poverty status in the past 12 months by age

tableID <- "B17020"

county_data_16 <- format_Census('county',tableID,2016,state,counties)
tract_data_16 <- format_Census('tract',tableID,2016,state,counties)
bg_data_16 <- format_Census('block group',tableID,2016,state,counties)

county_data_15 <- format_Census('county',tableID,2015,state,counties)
tract_data_15 <- format_Census('tract',tableID,2015,state,counties)
bg_data_15 <- format_Census('block group',tableID,2015,state,counties)

county_data_14 <- format_Census('county',tableID,2014,state,counties)
tract_data_14 <- format_Census('tract',tableID,2014,state,counties)
bg_data_14 <- format_Census('block group',tableID,2014,state,counties)

county_data_13 <- format_Census('county',tableID,2013,state,counties)
tract_data_13 <- format_Census('tract',tableID,2013,state,counties)
bg_data_13 <- format_Census('block group',tableID,2013,state,counties)

# Missing for 2012
# county_data_12 <- format_Census('county',tableID,2012,state,counties)
# tract_data_12 <- format_Census('tract',tableID,2012,state,counties)
# bg_data_12 <- format_Census('block group',tableID,2012,state,counties)

# Missing for 2011
# county_data_11 <- format_Census('county',tableID,2011,state,counties)
# tract_data_11 <- format_Census('tract',tableID,2011,state,counties)
# bg_data_11 <- format_Census('block group',tableID,2011,state,counties)

county_data_13 %>%
  select(GEOID,NAME)%>%
  mutate(Vintage = 2012, Level = 'county') -> county_data_12

tract_data_13 %>%
  select(GEOID,NAME)%>%
  mutate(Vintage = 2012, Level = 'tract') -> tract_data_12

bg_data_13 %>%
  select(GEOID,NAME)%>%
  mutate(Vintage = 2012, Level = 'block group') -> bg_data_12

county_data_12 %>%
  select(GEOID,NAME)%>%
  mutate(Vintage = 2011, Level = 'county') -> county_data_11

tract_data_12 %>%
  select(GEOID,NAME)%>%
  mutate(Vintage = 2011, Level = 'tract') -> tract_data_11

bg_data_12 %>%
  select(GEOID,NAME)%>%
  mutate(Vintage = 2011, Level = 'block group') -> bg_data_11

a <-
  rbind(
    county_data_16,
    tract_data_16,
    bg_data_16,
    county_data_15,
    tract_data_15,
    bg_data_15
  )

b <-
  rbind(
    county_data_14,
    tract_data_14,
    bg_data_14,
    county_data_13,
    tract_data_13,
    bg_data_13,
    county_data_12,
    tract_data_12,
    bg_data_12,
    county_data_11,
    tract_data_11,
    bg_data_11
  )

# Prepare census data for row binding by filling mismatched columns with NA
a[setdiff(names(b), names(a))] <- NA
b[setdiff(names(a), names(b))] <- NA

poverty_status_dt <- rbind(a,b)

saveRDS(poverty_status_dt,"./data/Poverty status in the past 12 months by age.rds")

