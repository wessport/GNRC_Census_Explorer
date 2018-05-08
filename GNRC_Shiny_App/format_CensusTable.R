# Greater Nashville Regional Council
# Project: GNRC Census Explorer
# Creation Date: 07-MAY-2018
# Script Author: Wes Porter
# Script Summary: Function for formatting tidycensus data
# Last Updated: 08-MAY-2018

library(tidycensus)
library(magrittr)
library(sf)
library(tidyverse)
library(lettercase)
library(lwgeom)

census_api_key(Sys.getenv("CENSUS_API_KEY"))

format_Census <- function(state, counties, geo, vars){

  if (is.na(counties)){
    
    # Request data from Census API
    census_data <- get_acs(state = state, geography = geo, 
                           variables = vars, geometry = TRUE)
    
  } else {
    
    # Request data from Census API
    census_data <- get_acs(state = state, county = counties, geography = geo, 
                    variables = vars, geometry = TRUE)
  }
  
  # Complete list of variable names and their respective IDs
  svar <- read.csv("C:/Users/wPorter/Data/Census/ACS5_tabular_data/data_dictionary/selected_variableID.csv")
  
  # Subset variable names based on variables of interest 
  a <- svar[svar$name %in% vars,]
  
  # Prep variable IDs for joining
  a %>% mutate(name_mod = substr(name,1,nchar(as.character(name))-1))%>% 
    mutate(label_mod = substr(label,11,nchar(as.character(label)))) %>% 
    mutate(label_mod = gsub("!!"," ",label_mod)) -> a
  
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


