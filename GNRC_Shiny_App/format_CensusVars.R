# Greater Nashville Regional Council
# Project: GNRC Census Explorer
# Creation Date: 11-MAY-2018
# Script Author: Wes Porter
# Script Summary: Formatting Census data for individual variables
# Last Updated: 11-MAY-2018

library(tidycensus)
library(magrittr)
library(sf)
library(tidyverse)
library(lettercase)
library(lwgeom)
library(stringr)

format_census_vars <- function(state, counties, geo, var_ids, yr){
  
  if (is.na(counties)){
    
    # Request data from Census API
    census_data <- get_acs(state = state, geography = geo,
                           variables = var_ids, geometry = TRUE)
    
  } else {
    
    # Request data from Census API
    census_data <- get_acs(state = state, county = counties, geography = geo, 
                           variables = var_ids, year = yr, geometry = TRUE)
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
  
  # Subset variable names based on variables of interest 
  a <- acs_variables[acs_variables$name %in% var_ids,]
  
  # Prep variable IDs for joining
  a %>% mutate(name_mod = substr(name,1,nchar(as.character(name))-1))%>% 
    mutate(label_mod = substr(label,11,nchar(as.character(label)))) %>% 
    mutate(label_mod = gsub("!!"," ",label_mod)) %>%
    mutate(label_mod = paste(str_title_case(str_lower(concept)), label_mod, sep =" ")) -> a
  
  # Join variable names and variable IDs together
  # Be careful to preserve sf class for the geometry attribute
  census_data %>% 
    left_join(a, by = c("variable" = "name_mod")) %>% 
    select(-name,-label,-concept,-table_name)-> census_data
  
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

test <- format_census_vars('TN',counties,'county',DI_vars,2016)
