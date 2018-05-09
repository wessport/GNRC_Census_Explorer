# Greater Nashville Regional Council
# Project: GNRC Census Explorer
# Creation Date: 07-MAY-2018
# Script Author: Wes Porter
# Script Summary: Function for formatting tidycensus data
# Last Updated: 09-MAY-2018

library(tidycensus)
library(magrittr)
library(sf)
library(tidyverse)
library(lettercase)
library(lwgeom)
library(stringr)

# Currently works for ACS 2016-2015
# Will need to update to handle 2014-2011

census_api_key(Sys.getenv("CENSUS_API_KEY"))

state = 'TN'
counties <- c('Cheatham','Davidson','Dickson','Houston','Humphreys','Montgomery','Maury','Robertson','Rutherford','Stewart','Sumner',
              'Trousdale','Williamson','Wilson')
geo <- 'tract'
tableID <- 'B25063'
yr <- 2016

format_Census <- function(state, counties, geo, tableID, yr){

  if (is.na(counties)){

    # Request data from Census API
    census_data <- get_acs(state = state, geography = geo,
                           table = tableID, geometry = TRUE)

  } else {

    # Request data from Census API
    census_data <- get_acs(state = state, county = counties, geography = geo, 
                    table = tableID, year = yr, geometry = TRUE)
}

  # Complete list of variable names and their respective IDs
  acs_variables <-  load_variables(yr, "acs5", cache = FALSE)
  acs_variables %>% mutate(table_name = str_split_fixed(name, "_", 2)[,1]) -> acs_variables
  
  # Subset variable names based on variables of interest 
  a <- acs_variables[acs_variables$table_name %in% tableID,]
  
  # Prep variable IDs for joining
  a %>% mutate(name_mod = substr(name,1,nchar(as.character(name))-1))%>% 
    mutate(label_mod = substr(label,11,nchar(as.character(label)))) %>% 
    mutate(label_mod = gsub("!!"," ",label_mod)) %>%
    select(-table_name) -> a
  
  # Join variable names and variable IDs together
  # Be careful to preserve sf class for the geometry attribute
  census_data %>% 
    left_join(a, by = c("variable" = "name_mod")) %>% 
    select(-name,-label,-concept)-> census_data
  
  # Formatting Census table in familiar format for users
  # Long to wide format with variable names
  census_data %>% 
    gather(measurement, value, estimate, moe) %>%
    unite(variable_combined, label_mod, measurement, sep = " ") %>%
    group_by(NAME) %>%
    arrange(GEOID,variable) %>%
    select(-variable) %>%
    spread(variable_combined, value) %>%
    mutate(Area_m2 = st_area(geometry)) -> census_format
  
  # Reproject to WGS 84 EPSG 4326 A.K.A. Google's Projection
  census_format <- st_transform(census_format, 4326, use_gdal = T)
  
  return(census_format)
}

test <-  format_Census(state, counties, geo, tableID, 2015)


# 2011-2014 ------------------------------------------------------------
yr <- 2014

# Request data from Census API
census_data <- get_acs(state = state, county = counties, geography = geo, 
                       table = tableID, year = yr, geometry = TRUE)

# Complete list of variable names and their respective IDs
acs_variables <-  load_variables(yr, "acs5", cache = FALSE)
acs_variables %>% mutate(table_name = str_split_fixed(name, "_", 2)[,1]) -> acs_variables

# Subset variable names based on variables of interest 
b <- acs_variables[acs_variables$table_name %in% tableID,]

b %>% 
  filter(grepl("*E$",b$name)) %>%
  select(-table_name) %>%
  mutate(label = gsub(":!!"," ",label)) %>%
  mutate(label = gsub(":"," ",label)) %>%
  mutate(name_mod = substr(name,1,nchar(as.character(name))-1)) -> b

a %>% 
  mutate(label_mod = str_split_fixed(label,"!!",2))

t <- cbind.data.frame(a, c('a','b','c') = str_split_fixed(a$label,"!!",3))

# NEED TO FIGURE OUT HOWTO SPLIT AND THEN ADD IN TOTAL 
t %>%
  mutate(t)

# Prep variable IDs for joining
# a %>% mutate(name_mod = substr(name,1,nchar(as.character(name))-1))%>% 
#   mutate(label_mod = substr(label,11,nchar(as.character(label)))) %>% 
#   mutate(label_mod = gsub("!!"," ",label_mod)) %>%
#   select(-table_name) -> a

# Join variable names and variable IDs together
# Be careful to preserve sf class for the geometry attribute
census_data %>% 
  left_join(a, by = c("variable" = "name_mod")) %>% 
  select(-name,-concept)-> census_data

# Formatting Census table in familiar format for users
# Long to wide format with variable names
census_data %>% 
  gather(measurement, value, estimate, moe) %>%
  unite(variable_combined, label, measurement, sep = " ") %>%
  group_by(NAME) %>%
  arrange(GEOID,variable) %>%
  select(-variable) %>%
  spread(variable_combined, value) %>%
  mutate(Area_m2 = st_area(geometry)) -> census_format

# Reproject to WGS 84 EPSG 4326 A.K.A. Google's Projection
census_format <- st_transform(census_format, 4326, use_gdal = T)
  






