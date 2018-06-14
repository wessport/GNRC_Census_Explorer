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

# GEOMETRY ---------------------------------------------------------------

# Create geometry object for joining geometry to data
# df <- readRDS("./data/default_data.rds")
# 
# df %>%
#   ungroup() %>%
#   select(GEOID) -> geom
# 
# saveRDS(geom, "./data/geometry.rds")

# VARIABLES --------------------------------------------------------------

# acs_variables_16 <-  load_variables(2016, "acs5", cache = FALSE)
# saveRDS(acs_variables_16,"./data/acs_variables_2016.rds")
# acs_variables_15 <-  load_variables(2015, "acs5", cache = FALSE)
# saveRDS(acs_variables_15,"./data/acs_variables_2015.rds")
# acs_variables_14 <-  load_variables(2014, "acs5", cache = FALSE)
# saveRDS(acs_variables_14,"./data/acs_variables_2014.rds")
# acs_variables_13 <-  load_variables(2013, "acs5", cache = FALSE)
# saveRDS(acs_variables_13,"./data/acs_variables_2013.rds")
# acs_variables_12 <-  load_variables(2012, "acs5", cache = FALSE)
# saveRDS(acs_variables_12,"./data/acs_variables_2012.rds")
# acs_variables_11 <-  load_variables(2011, "acs5", cache = FALSE)
# saveRDS(acs_variables_11,"./data/acs_variables_2011.rds")

# Define GNRC counties
counties <- c('Cheatham','Davidson','Dickson','Houston','Humphreys','Montgomery','Maury','Robertson','Rutherford','Stewart','Sumner',
              'Trousdale','Williamson','Wilson')
state <- 'TN'

# REQUEST DATA FUNTION --------------------------------------------------
tableID <- "B25056"

request_data <- function(tableID){

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
  }
  
# object.size(requested_data %>%
#   st_set_geometry(NULL))

# start <- Sys.time()
# contract_rent <- request_data("B25056")
# end <- Sys.time()
# end - start

# REQUEST AND WRITE DATA ----------------------------------------------------

# Define variable TableIDs
contract_rent <- "B25056"
contract_rent_dt <- request_data(contract_rent)
saveRDS(contract_rent_dt,"./data/Contract Rent.rds")

detailed_race <- "C02003"
detailed_race_dt <- request_data(detailed_race)
saveRDS(detailed_race_dt,"./data/Detailed Race.rds")

emp_status <- "B23025"
emp_status_dt <- request_data(emp_status)
saveRDS(detailed_race_dt,"./data/Employment status for the population 16 years and over.rds")

gross_rent <- "B25063"
gross_rent_dt <- request_data(gross_rent)
saveRDS(gross_rent_dt,"./data/Gross Rent.rds")

health_insr_cov <- "B27001"
health_insr_cov_dt <- request_data(health_insr_cov)
saveRDS(health_insr_cov_dt,"./data/Health insurance coverage status by sex by age.rds")

household_type_white <- "B11001H"
household_type_white_dt <- request_data(household_type_white)
saveRDS(household_type_white_dt, "./data/houeshold type including living alone white alone nothispanic or latino.rds")

house_heating <- "B25040"
house_heating_dt <- request_data(house_heating)
saveRDS(house_heating_dt,"./data/House heating fuel.rds")

household_income <- "B19001"
household_income_dt <- request_data(household_income)
saveRDS(household_income_dt,"./data/Household income in the past 12 months.rds")

household_type_children <- "B09005"
household_type_children_dt <- request_data(household_type_children)
saveRDS(household_type_children_dt,"./data/Household type for children under 18 years in households.rds")

household_type_alone <- "B11001"
household_type_alone_dt <- request_data(household_type_alone)
saveRDS(household_type_alone_dt,"./data/Household type including living alone.rds")

household_type_native <- "B11001C"
household_type_native_dt <- request_data(household_type_native)
saveRDS(household_type_native_dt,"./data/Household type including living alone american indian and alaska native alone.rds")

household_type_asian <- "B11001D"
household_type_asian_dt <- request_data(household_type_asian)
saveRDS(household_type_asian_dt,"./data/Household type including living alone asian alone.rds")

household_type_black <- "B11001B"
household_type_black_dt <- request_data(household_type_black)
saveRDS(household_type_black_dt,"./data/Household type including living alone black or african american alone.rds")

household_type_hispanic <- "B11001I"
household_type_hispanic_dt <- request_data(household_type_hispanic)
saveRDS(household_type_hispanic_dt,"./data/Household type including living alone hispanic or latino.rds")

household_type_hawaiian <- "B11001E"
household_type_hawaiian_dt <- request_data(household_type_hawaiian)
saveRDS(household_type_hawaiian_dt,"./data/Household type including living alone native hawaiian and other pacific islander alone.rds")

household_type_other <- "B11001F"
household_type_other_dt <- request_data(household_type_other)
saveRDS(household_type_other_dt,"./data/Household type including living alone some other race alone.rds")

household_type_more <- "B11001G"
household_type_more_dt <- request_data(household_type_more)
saveRDS(household_type_more_dt,"Household type including living alone two or more races.rds")

household_type_white <- "B11001A"
household_type_white_dt <- request_data(household_type_white)
saveRDS(household_type_white_dt,"./data/Household type including living alone white alone.rds")

housing_unit <- "B25001"
housing_unit_dt <- request_data(housing_unit)
saveRDS(housing_unit_dt,"./data/Housing Units.rds")

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







