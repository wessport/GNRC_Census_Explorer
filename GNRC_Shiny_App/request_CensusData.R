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
df <- readRDS("./data/default_data.rds")

df %>%
  ungroup() %>%
  select(NAME,Vintage) -> geom

saveRDS(geom, "./data/geometry.rds")

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

# Move Vintage and Level to front of table

move_vl <- function(data_path){
  
  data_set <- readRDS(data_path)
  
  data_set %>%
    ungroup %>%
    select(GEOID,NAME,Level,Vintage)-> a#,`Total estimate`,`Total moe`)-> a
  
  data_set %>%
    ungroup%>%
    select(-GEOID,-NAME,-Level,-Vintage)-> b#,-`Total estimate`,-`Total moe`) -> b
  
  bind_cols(a,b) -> c
  
  saveRDS(c,data_path)
  
}

# REQUEST AND WRITE DATA ----------------------------------------------------

# Define variable TableIDs
tableID <- "B25056"
contract_rent_dt <- request_data(tableID)
saveRDS(contract_rent_dt,"./data/Contract Rent.rds")
move_vl("./data/Contract Rent.rds")

tableID <- "C02003"
detailed_race_dt <- request_data(tableID)
saveRDS(detailed_race_dt,"./data/Detailed Race.rds")
move_vl("./data/Detailed Race.rds")

tableID <- "B23025"
emp_status_dt <- request_data(tableID)
saveRDS(emp_status_dt,"./data/Employment status for the population 16 years and over.rds")
move_vl("./data/Employment status for the population 16 years and over.rds")

tableID <- "B25063"
gross_rent_dt <- request_data(tableID)
saveRDS(gross_rent_dt,"./data/Gross Rent.rds")
move_vl("./data/Gross Rent.rds")

tableID <- "B27001"
health_insr_cov_dt <- request_data(tableID)
saveRDS(health_insr_cov_dt,"./data/Health insurance coverage status by sex by age.rds")
move_vl("./data/Health insurance coverage status by sex by age.rds")

tableID <- "B11001H"
household_type_white_dt <- request_data(tableID)
saveRDS(household_type_white_dt, "./data/Household type including living alone white alone nothispanic or latino.rds")
move_vl("./data/Household type including living alone white alone nothispanic or latino.rds")

tableID <- "B25040"
house_heating_dt <- request_data(tableID)
saveRDS(house_heating_dt,"./data/House heating fuel.rds")
move_vl("./data/House heating fuel.rds")

tableID <- "B19001"
household_income_dt <- request_data(tableID)
saveRDS(household_income_dt,"./data/Household income in the past 12 months.rds")
move_vl("./data/Household income in the past 12 months.rds")

tableID <- "B09005"
household_type_children_dt <- request_data(tableID)
saveRDS(household_type_children_dt,"./data/Household type for children under 18 years in households.rds")
move_vl("./data/Household type for children under 18 years in households.rds")

tableID <- "B11001"
household_type_alone_dt <- request_data(tableID)
saveRDS(household_type_alone_dt,"./data/Household type including living alone.rds")
move_vl("./data/Household type including living alone.rds")

tableID <- "B11001C"
household_type_native_dt <- request_data(tableID)
saveRDS(household_type_native_dt,"./data/Household type including living alone american indian and alaska native alone.rds")
move_vl("./data/Household type including living alone american indian and alaska native alone.rds")

tableID <- "B11001D"
household_type_asian_dt <- request_data(tableID)
saveRDS(household_type_asian_dt,"./data/Household type including living alone asian alone.rds")
move_vl("./data/Household type including living alone asian alone.rds")

tableID <- "B11001B"
household_type_black_dt <- request_data(tableID)
saveRDS(household_type_black_dt,"./data/Household type including living alone black or african american alone.rds")
move_vl("./data/Household type including living alone black or african american alone.rds")

tableID <- "B11001I"
household_type_hispanic_dt <- request_data(tableID)
saveRDS(household_type_hispanic_dt,"./data/Household type including living alone hispanic or latino.rds")
move_vl("./data/Household type including living alone hispanic or latino.rds")

tableID <- "B11001E"
household_type_hawaiian_dt <- request_data(tableID)
saveRDS(household_type_hawaiian_dt,"./data/Household type including living alone native hawaiian and other pacific islander alone.rds")
move_vl("./data/Household type including living alone native hawaiian and other pacific islander alone.rds")

tableID <- "B11001F"
household_type_other_dt <- request_data(tableID)
saveRDS(household_type_other_dt,"./data/Household type including living alone some other race alone.rds")
move_vl("./data/Household type including living alone some other race alone.rds")

tableID <- "B11001G"
household_type_more_dt <- request_data(tableID)
saveRDS(household_type_more_dt,"Household type including living alone two or more races.rds")
move_vl("Household type including living alone two or more races.rds")

tableID <- "B11001A"
household_type_white_dt <- request_data(tableID)
saveRDS(household_type_white_dt,"./data/Household type including living alone white alone.rds")
move_vl("./data/Household type including living alone white alone.rds")

tableID <- "B25001"
housing_unit_dt <- request_data(tableID)
saveRDS(housing_unit_dt,"./data/Housing Units.rds")
move_vl("./data/Housing Units.rds")

# tableID <- "C16001"
# language_spoken_dt <- request_data(tableID)
# saveRDS(language_spoken_dt,"./data/Language spoken at home for the population 5 years and over.rds")
# 
# tableID <- "B09021"
# living_arrangements_dt <- request_data(tableID)
# saveRDS(living_arrangements_dt,"./data/Living arrangements of adults 18 years and over by age.rds")

tableID <- "B23020"
mean_usual_hours_dt <- request_data(tableID)
saveRDS(mean_usual_hours_dt,"./data/Mean usual hours worked in the past 12 months for workers 16 to 64 years.rds")
move_vl("./data/Mean usual hours worked in the past 12 months for workers 16 to 64 years.rds")

tableID <- "B08301"
means_transportation_dt <- request_data(tableID)
saveRDS(means_transportation_dt,"./data/Means of transportation to work.rds")
move_vl("./data/Means of transportation to work.rds")

tableID <- "B08105C"
means_trans_native_dt <- request_data(tableID)
saveRDS(means_trans_native_dt,"./data/Means of transportation to work american indian and alaska native alone.rds")
move_vl("./data/Means of transportation to work american indian and alaska native alone.rds")

tableID <- "B08105D"
means_trans_asian_dt <- request_data(tableID)
saveRDS(means_trans_asian_dt,"./data/Means of transportation to work asian alone.rds")
move_vl("./data/Means of transportation to work asian alone.rds")

tableID <- "B08105B"
means_trans_black_dt <- request_data(tableID)
saveRDS(means_trans_black_dt,"./data/Means of transportation to work black or african american alone.rds")
move_vl("./data/Means of transportation to work black or african american alone.rds")

tableID <- "B08101"
means_trans_by_age_dt <- request_data(tableID)
saveRDS(means_trans_by_age_dt,"./data/Means of transportation to work by age.rds")
move_vl("./data/Means of transportation to work by age.rds")

tableID <- "B08128"
means_trans_by_class_dt <- request_data(tableID)
saveRDS(means_trans_by_class_dt,"./data/Means of transportation to work by class of worker.rds")
move_vl("./data/Means of transportation to work by class of worker.rds")

tableID <- "B08122"
means_trans_by_poverty_dt <- request_data(tableID)
saveRDS(means_trans_by_poverty_dt,"./data/Means of transportation to work by poverty status in the past 12 mo.rds")
move_vl("./data/Means of transportation to work by poverty status in the past 12 mo.rds")

tableID <- "B08105I"
means_trans_hispanic_dt <- request_data(tableID)
saveRDS(means_trans_hispanic_dt,"./data/Means of transportation to work hispanic or latino.rds")
move_vl("./data/Means of transportation to work hispanic or latino.rds")

tableID <- "B08105E"
means_trans_hawaiian_dt <- request_data(tableID)
saveRDS(means_trans_hawaiian_dt,"./data/Means of transportation to work native hawaiian and other pacific islander alone.rds")
move_vl("./data/Means of transportation to work native hawaiian and other pacific islander alone.rds")

tableID <- "B08105F"
means_trans_other_dt <- request_data(tableID)
saveRDS(means_trans_other_dt,"./data/Means of transportation to work some other race alone.rds")
move_vl("./data/Means of transportation to work some other race alone.rds")

tableID <- "B08105G"
means_trans_more_dt <- request_data(tableID)
saveRDS(means_trans_more_dt,"./data/Means of transportation to work two or more races.rds")
move_vl("./data/Means of transportation to work two or more races.rds")

tableID <- "B08105A"
means_trans_white_dt <- request_data(table_ID)
saveRDS(means_trans_white_dt,"./data/Means of transportation to work white alone.rds")
move_vl("./data/Means of transportation to work white alone.rds")

tableID <- "B08105H"
means_trans_white_noth <- request_data(tableID)
saveRDS(means_trans_white_noth,"./data/Means of transportation to work white alone not hispanic or latino.rds")
move_vl("./data/Means of transportation to work white alone not hispanic or latino.rds")

tableID <- "B08103"
median_age_trans_dt <- request_data(tableID)
saveRDS(median_age_trans_dt,"./data/Median age by means of transportation to work.rds")
move_vl("./data/Median age by means of transportation to work.rds") # -----

tableID <- "B23013"
median_age_by_sex_dt <- request_data(tableID)
saveRDS(median_age_by_sex_dt,"./data/Median age by sex for workers 16 to 64 years.rds")
move_vl("./data/Median age by sex for workers 16 to 64 years.rds")

# tableID <- "B25031"
# median_gross_rent_dt <- request_data(tableID)
# saveRDS(median_gross_rent_dt,"./data/Median gross rent by bedrooms.rds")

tableID <- "B25105"
median_monthly_housing_dt <- request_data(tableID)
saveRDS(median_monthly_housing_dt,"./data/Median monthly housing costs dollars.rds")
move_vl("./data/Median monthly housing costs dollars.rds")

tableID <- "B25077"
median_value_dollars <- request_data(tableID)
saveRDS(median_value_dollars,"./data/Median value dollars.rds")
move_vl("./data/Median value dollars.rds")

tableID <- "C27006"
medicare_cov_by_sex_dt <- request_data(tableID)
saveRDS(medicare_cov_by_sex_dt,"./data/Medicare coverage by sex by age.rds")
move_vl("./data/Medicare coverage by sex by age.rds")

tableID <- "B25104"
monthly_housing_costs_dt <- request_data(tableID)
saveRDS(monthly_housing_costs_dt,"./data/Monthly housing costs.rds")
move_vl("./data/Monthly housing costs.rds")

tableID <- "B25087"
mortage_status_dt <- request_data(tableID)
saveRDS(mortage_status_dt,"./data/Mortage status and selected monthly owner costs.rds")
move_vl("./data/Mortage status and selected monthly owner costs.rds")

tableID <- "B25002"
occupancy_status_dt <- request_data(tableID)
saveRDS(occupancy_status_dt,"./data/Occupancy status.rds")
move_vl("./data/Occupancy status.rds")

tableID <- "B09001"
pop_under_18_dt <- request_data(tableID)
saveRDS(pop_under_18_dt,"./data/Population under 18 years by age.rds")
move_vl("./data/Population under 18 years by age.rds")

tableID <- "B17020"
poverty_status_dt <- request_data(tableID)
saveRDS(poverty_status_dt,"./data/Poverty status in the past 12 months by age.rds")
move_vl("./data/Poverty status in the past 12 months by age.rds")

tableID <- "B02001"
race_dt <- request_data(tableID)
saveRDS(race_dt,"./data/Race.rds")
move_vl("./data/Race.rds")

tableID <- "C17002"
ratio_income_pov_dt <- request_data(tableID)
saveRDS(ratio_income_pov_dt,"Ratio of income to poverty level in the past 12 mo.rds")
move_vl("Ratio of income to poverty level in the past 12 mo.rds")

tableID <- "B17026"
ratio_income_pov_fam_dt <- request_data(tableID)
saveRDS(ratio_income_pov_fam_dt,"./data/Ratio of income to poverty level of families in the past 12 months.rds")
move_vl("./data/Ratio of income to poverty level of families in the past 12 months.rds")

tableID <- "B14001"
shool_enrollment_dt <- request_data(tableID)
saveRDS(shool_enrollment_dt,"./data/School enrollment by level of school for the population 3 years and over.rds")
move_vl("./data/School enrollment by level of school for the population 3 years and over.rds")

tableID <- "B01001"
sex_by_age_dt <- request_data(tableID)
saveRDS(sex_by_age_dt,"./data/Sex by age.rds")
move_vl("./data/Sex by age.rds")

sex_by_age_by_dis <- "B18101"
sex_by_age_by_dis_dt <- request_data(tableID)
saveRDS(sex_by_age_by_dis_dt,"./data/Sex by age by disability status.rds")
move_vl("./data/Sex by age by disability status.rds")

tableID <- "B15001"
sex_by_age_by_edu_dt <- request_data(tableID)
saveRDS(sex_by_age_by_edu_dt,"./data/Sex by age by educational attainment for the population 18 years and over.rds")
move_vl("./data/Sex by age by educational attainment for the population 18 years and over.rds")

tableID <- "B23001"
sex_by_age_by_emp_dt <- request_data(tableID)
saveRDS(sex_by_age_by_emp_dt,"./data/Sex by age by employment status for the population 16 years and over.rds")
move_vl("./data/Sex by age by employment status for the population 16 years and over.rds")

tableID <- "B21001"
sex_by_age_by_vet_dt <- request_data(tableID)
saveRDS(sex_by_age_by_vet_dt,"./data/Sex by age by veteran status for the civilian population 18 years and over.rds")

tableID <- "B24080"
sex_by_class_dt <- request_data(tableID)
saveRDS(sex_by_class_dt,"./data/Sex by class of worker for the civilian employed population 16 years and over.rds")
move_vl("./data/Sex by class of worker for the civilian employed population 16 years and over.rds")

tableID <- "B08006"
sex_of_workers_trans_dt <- request_data(tableID)
saveRDS(sex_of_workers_trans_dt,"./data/Sex of workers by means of transportation to work.rds")
move_vl("./data/Sex of workers by means of transportation to work.rds")

tableID <- "B08012"
sex_workers_travel_time_dt <- request_data(tableID)
saveRDS(sex_workers_travel_time_dt,"./data/Sex of workers by travel time to work.rds")
move_vl("./data/Sex of workers by travel time to work.rds")

tableID <- "B08014"
sex_workers_vehicle_avail_dt <- request_data(tableID)
saveRDS(sex_workers_vehicle_avail_dt,"./data/Sex of workers by vehicles available.rds")
move_vl("./data/Sex of workers by vehicles available.rds")

tableID <- "B08302"
time_leaving_home_dt <- request_data(tableID)
saveRDS(time_leaving_home_dt,"./data/Time leaving home to go to work.rds")
move_vl("./data/Time leaving home to go to work.rds")

tableID <- "B01003"
total_pop_dt <- request_data(tableID)
saveRDS(total_pop_dt,"./data/Total Population.rds")
move_vl("./data/Total Population.rds")

tableID <- "B08303"
travel_time_to_work_dt <- request_data(tableID)
saveRDS(travel_time_to_work_dt,"./data/Travel time to work.rds")
move_vl("./data/Travel time to work.rds")

tableID <- "B25004"
vacancy_status_dt <- request_data(tableID)
saveRDS(vacancy_status_dt,"./data/Vacancy status.rds")
move_vl("./data/Vacancy status.rds")

tableID <- "B25075"
value_dt <- request_data(tableID)
saveRDS(value_dt,"./data/Value.rds")
move_vl("./data/Value.rds")

tableID <- "B13002"
women_birth_dt <- request_data(tableID)
saveRDS(women_birth_dt,"./data/Women 15 to 50 years who had a birth in the past 12 mo by marital status and age.rds")
move_vl("./data/Women 15 to 50 years who had a birth in the past 12 mo by marital status and age.rds")

tableID <- "B25034"
year_struc_built_dt <- request_data(tableID)
saveRDS(year_struc_built_dt,"./data/Year structure built.rds")
move_vl("./data/Year structure built.rds")






