# Greater Nashville Regional Council
# Project: GNRC Census Explorer
# Creation Date: 08-MAY-2018
# Script Author: Wes Porter
# Script Summary: Requesting raw census data for Census Explorer Shiny App
# Last Updated: 08-MAY-2018

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

setwd("C:/Users/wPorter/Data/Census/ACS5_tabular_data/data_dictionary/variableID_tables")

# VARIABLES --------------------------------------------------------------

# Load variables of interest
contract_rent_varid <- read.csv("contract_rent.csv", header = FALSE)
detailed_race_varid <- read.csv("detailed_race.csv", header = FALSE)
emp_status_varid <- read.csv("employment_status_for_the_population_16_years_and_over.csv", header = FALSE)
gross_rent_varid <- read.csv("gross_rent.csv", header = FALSE)
health_insr_cov_varid <- read.csv("health_insurance_coverage_status_by_sex_by_age.csv", header = FALSE)
household_type_white <- read.csv("houeshold_type_including_living_alone_white_alone_not_hispanic_or_latino.csv", header = FALSE)
house_heating_varid <- read.csv("house_heating_fuel.csv", header = FALSE)
household_income_varid <- read.csv("household_income_in_the_past_12_months.csv", header = FALSE)
household_type_children_varid <- read.csv("household_type_for_children_under_18_years_in_households.csv", header = FALSE)
household_type_alone_varid <- read.csv("household_type_including_living_alone.csv", header = FALSE)
household_type_native_varid <- read.csv("household_type_including_living_alone_american_indian_and_alaska_native_alone.csv", header = FALSE)
household_type_asian_varid <-  read.csv("household_type_including_living_alone_asian_alone.csv")
household_type_black_varid <-  read.csv("household_type_including_living_alone_black_or_african_american_alone.csv", header = FALSE)
household_type_hispanic_vaird <- read.csv("household_type_including_living_alone_hispanic_or_latino.csv")
household_type_hawaiian_varid <- read.csv("household_type_including_living_alone_native_hawaiian_and_other_pacific_islander_alone.csv", header = FALSE)
household_type_other_varid <- read.csv("household_type_including_living_alone_some_other_race_alone.csv", header = FALSE)
household_type_more_varid <-  read.csv("household_type_including_living_alone_two_or_more_races.csv", header = FALSE)
household_type_white_varid <- read.csv("household_type_including_living_alone_white_alone.csv", header = FALSE)
housing_unit_varid <-  read.csv("housing_units.csv", header = FALSE)
language_spoken_varid <- read.csv("language_spoken_at_home_for_the_population_5_years_and_over.csv", header = FALSE)
living_arrangements_varid <- read.csv("living_arrangements_of_adults_18_years_and_over_by_age.csv", header = FALSE)
mean_usual_hours_varid <-  read.csv("mean_usual_hours_worked_in_the_past_12_months_for_workers_16_to_64_years.csv", header = FALSE)
means_transportation_varid <- read.csv("means_of_transportation_to_work.csv")
means_trans_native_varid <- read.csv("means_of_transportation_to_work_american_indian_and_alaska_native_alone.csv", header = FALSE)
means_trans_asian_varid <- read.csv("means_of_transportation_to_work_asian_alone.csv", header = FALSE)
means_trans_black_varid <- read.csv("means_of_transportation_to_work_black_or_african_american_alone.csv", header = FALSE)
means_trans_by_age_varid <- read.csv("means_of_transportation_to_work_by_age.csv", header = FALSE)
means_trans_by_class_varid <- read.csv("means_of_transportation_to_work_by_class_of_worker.csv", header = FALSE)
means_trans_by_poverty_varid <- read.csv("means_of_transportation_to_work_by_poverty_status_in_the_past_12_mo.csv", header = FALSE)
means_trans_hispanic_vaird <- read.csv("means_of_transportation_to_work_hispanic_or_latino.csv", header = FALSE)
means_trans_hawaiian_varid <- read.csv("means_of_transportation_to_work_native_hawaiian_and_other_pacific_islander_alone.csv", header = FALSE)
means_trans_other_varid <- read.csv("means_of_transportation_to_work_some_other_race_alone.csv", header = FALSE)
means_trans_more_varid <- read.csv("means_of_transportation_to_work_two_or_more_races.csv", header = FALSE)
means_trans_white_varid  <- read.csv("means_of_transportation_to_work_white_alone.csv", header = FALSE)
means_trans_white_noth_varid <- read.csv("means_of_transportation_to_work_white_alone_not_hispanic_or_latino.csv", header = FALSE)
median_age_trans_varid <- read.csv("median_age_by_means_of_transportation_to_work.csv", header = FALSE)
median_age_by_sex_varid <- read.csv("median_age_by_sex_for_workers_16_to_64_years.csv", header = FALSE)
median_gross_rent_varid <- read.csv("median_gross_rent_by_bedrooms.csv", header = FALSE)
median_monthly_housing_varid <- read.csv("median_monthly_housing_costs_dollars.csv", header = FALSE)
median_value_dollars_varid <- read.csv("median_value_dollars.csv", header = FALSE)
medicare_cov_by_sex_varid <- read.csv("medicare_coverage_by_sex_by_age.csv", header = FALSE)
monthly_housing_costs_varid <- read.csv("monthly_housing_costs.csv", header = FALSE)
mortage_status_vaird <- read.csv("mortage_status_and_selected_monthly_owner_costs.csv", header = FALSE)
occupancy_status_varid <- read.csv("occupancy_status.csv", header = FALSE)
pop_under_18_varid <-  read.csv("population_under_18_years_by_age.csv", header = FALSE)
pverty_status_varid <-  read.csv("poverty_status_in_the_past_12_months_by_age.csv", header = FALSE)
race_varid <- read.csv("race.csv", header = FALSE)
ratio_income_pov_varid <- read.csv("ratio_of_income_to_poverty_level_in_the_past_12_mo.csv", header = FALSE)
ratio_income_pov_fam_varid <- read.csv("ratio_of_income_to_poverty_level_of_families_in_the_past_12_months.csv", header = FALSE)
shool_enrollment_varid <- read.csv("school_enrollment_by_level_of_school_for_the_population_3_years_and_over.csv", header=FALSE)
sex_by_age_varid <-  read.csv("sex_by_age.csv", header = FALSE)
sex_by_age_by_dis_varid <- read.csv("sex_by_age_by_disability_status.csv", header = FALSE)
sex_by_age_by_edu_varid <- read.csv("sex_by_age_by_educational_attainment_for_the_population_18_years_and_over.csv", header = FALSE)
sex_by_age_by_emp_female_varid <- read.csv("sex_by_age_by_employment_status_for_the_population_16_years_and_over_female.csv", header = FALSE)
sex_by_age_by_emp_male_varid <- read.csv("sex_by_age_by_employment_status_for_the_population_16_years_and_over_male.csv", header = FALSE)
sex_by_age_by_vet_varid <- read.csv("sex_by_age_by_veteran_status_for_the_civilian_population_18_years_and_over.csv", header = FALSE)
sex_by_class <- read.csv("sex_by_class_of_worker_for_the_civilian_employed_population_16_years_and_over.csv", header = FALSE)
sex_of_workers_trans_varid <- read.csv("sex_of_workers_by_means_of_transportation_to_work.csv", header = FALSE)
sex_workers_travel_time_varid <-  read.csv("sex_of_workers_by_travel_time_to_work.csv", header = FALSE)
sex_workers_vehicle_avail_varid <-  read.csv("sex_of_workers_by_vehicles_available.csv", header = FALSE)
time_leaving_home_varid <-  read.csv("time_leaving_home_to_go_to_work.csv", header = FALSE)
total_pop_varid <- read.csv("total_pop.csv", header = FALSE)
travel_time_to_work_varid <- read.csv("travel_time_to_work.csv", header = FALSE)
vacancy_status_varid <- read.csv("vacancy_status.csv", header = FALSE)
value_varid <- read.csv("value.csv", header = FALSE)
women_birth <- read.csv("women_15_to_50_years_who_had_a_birth_in_the_past_12_mo_by_marital_status_and_age.csv", header = FALSE)
year_struc_built_varid <- read.csv("year_structure_built.csv", header = FALSE)

# COUNTY -----------------------------------------------------------------
# Request county level data including the following geographic boundaries:

# County
# Tract
# Block Group

# Define GNRC counties
counties <- c('Cheatham','Davidson','Dickson','Houston','Humphreys','Montgomery','Maury','Robertson','Rutherford','Stewart','Sumner',
              'Trousdale','Williamson','Wilson')



# WRITE DATA ---------------------------------------------------------------

path = "C:/Users/wPorter/Data/Census/census_shapefiles/tidy_data/"
save(census_format, file = paste(path,"sex_by_age_by_edu_formatted.rda", sep=''))

#load("C:/Users/wPorter/Data/Census/census_shapefiles/tidy_data/sex_by_age_by_edu_formatted.rda")









