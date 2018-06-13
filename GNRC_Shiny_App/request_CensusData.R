# Greater Nashville Regional Council
# Project: GNRC Census Explorer
# Creation Date: 08-MAY-2018
# Script Author: Wes Porter
# Script Summary: Requesting raw census data for Census Explorer Shiny App
# Last Updated: 13-JUNE-2018

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

# VARIABLES --------------------------------------------------------------

# Define variable TableIDs
contract_rent <- "B25056"
detailed_race <- "C02003"
emp_status <- "B23025"
gross_rent <- "B25063"
health_insr_cov <- "B27001"
household_type_white <- "B11001H"
house_heating <- "B25040"
household_income <- "B19001"
household_type_children <- "B09005"
household_type_alone <- "B11001"
household_type_native <- "B11001C"
household_type_asian <- "B11001D"
household_type_black <- "B11001B"
household_type_hispanic <- "B11001I"
household_type_hawaiian <- "B11001E"
household_type_other <- "B11001F"
household_type_more <- "B11001G"
household_type_white <- "B11001A"
housing_unit <- "B25001"
language_spoken <- "C16001"
living_arrangements <- "B09021"
mean_usual_hours <- "B23020"
means_transportation <- "B08301"
means_trans_native <- "B08105C"
means_trans_asian <- "B08105D"
means_trans_black <- "B08105B"
means_trans_by_age <- "B08101"
means_trans_by_class <- "B08128"
means_trans_by_poverty <- "B08122"
means_trans_hispanic <- "B08105I"
means_trans_hawaiian <- "B08105E"
means_trans_other <- "B08105F"
means_trans_more <- "B08105G"
means_trans_white <- "B08105A"
means_trans_white_noth <- "B08105H"
median_age_trans <- "B08103"
median_age_by_sex <- "B23013"
median_gross_rent <- "B25031"
median_monthly_housing <- "B25105"
median_value_dollars <- "B25077"
medicare_cov_by_sex <- "C27006"
monthly_housing_costs <- "B25104"
mortage_status <- "B25087"
occupancy_status <- "B25002"
pop_under_18 <- "B09001"
pverty_status <- "B17020"
race <- "B02001"
ratio_income_pov <- "C17002"
ratio_income_pov_fam <- "B17026"
shool_enrollment <- "B14001"
sex_by_age <- "B01001"
sex_by_age_by_dis <- "B18101"
sex_by_age_by_edu <- "B15001"
sex_by_age_by_emp <- "B23001"
sex_by_age_by_vet <- "B21001"
sex_by_class <- "B24080"
sex_of_workers_trans <- "B08006"
sex_workers_travel_time <- "B08012"
sex_workers_vehicle_avail <- "B08014"
time_leaving_home <- "B08302"
total_pop <- "B01003"
travel_time_to_work <- "B08303"
vacancy_status <- "B25004"
value <- "B25075"
women_birth <- "B13002"
year_struc_built <- "B25034"

# Define GNRC counties
counties <- c('Cheatham','Davidson','Dickson','Houston','Humphreys','Montgomery','Maury','Robertson','Rutherford','Stewart','Sumner',
              'Trousdale','Williamson','Wilson')
state <- 'TN'

# REQUEST DATA -------------------------------------------------------------
tableID <- "B25056"

request_data <- function(tableID)

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

requested_data <- rbind(a,b)

return(requested_data)
  
object.size(requested_data %>%
  st_set_geometry(NULL))

# WRITE DATA ---------------------------------------------------------------

path = "C:/Users/wPorter/Data/Census/census_shapefiles/tidy_data/"
save(census_format, file = paste(path,"sex_by_age_by_edu_formatted.rda", sep=''))

#load("C:/Users/wPorter/Data/Census/census_shapefiles/tidy_data/sex_by_age_by_edu_formatted.rda")



format_Census <- function(geography,tableID,year,state,counties){
  
  # Request data from Census API
  census_data <- get_acs(geography, table = tableID, year = year, state = state, county = counties, 
                         geometry = TRUE)
  }


bg_data_16 <- format_Census('block group',"B25056",2016,"TN",counties)




