# Greater Nashville Regional Council
# Project: GNRC Census Explorer
# Creation Date: 07-MAY-2018
# Script Author: Wes Porter
# Script Summary: Function for formatting tidycensus data
# Last Updated: 09-MAY-2018

library(plyr)
library(dplyr)
library(tidycensus)
library(magrittr)
library(sf)
library(tidyverse)
library(lettercase)
library(lwgeom)
library(stringr)

# Currently works for ACS 2016-2011

# CENSUS KEY ---------------------------------------------------

# census_api_key(Sys.getenv("CENSUS_API_KEY"))
census_api_key("6999d8d1e472e95e754d605f9a5646beec7eede5")


# state = 'TN'
# counties <- c('Cheatham','Davidson','Dickson','Houston','Humphreys','Montgomery','Maury','Robertson','Rutherford','Stewart','Sumner',
#               'Trousdale','Williamson','Wilson')
# geo <- 'tract'
# tableID <- 'B25063'
# yr <- 2016

# DOWNLOAD AND FORMAT FUNCTION ----------------------------------

format_Census <- function(geography,tableID,year,state,counties){

  # if (is.na(counties)){
  # 
  #   # Request data from Census API
  #   census_data <- get_acs(state = state, geography = geo,
  #                          table = tableID, geometry = TRUE)
  # 
  # } else {

  # # Request data from Census API
  # census_data <- get_acs(geography,table = tableID,year = yr,state = state,county = counties, 
  #                   geometry = TRUE)
  # Request data from Census API
  census_data <- get_acs(geography, table = tableID, year = year, state = state, county = counties, 
                         geometry = FALSE)

# }
  
  yr <- year
  geo <- geography

  # Complete list of variable names and their respective IDs
  # acs_variables <-  load_variables(yr, "acs5", cache = FALSE)
  acs_variables <- readRDS(paste("./data/acs_variables_",yr,".rds",sep=""))
  acs_variables %>% mutate(table_name = str_split_fixed(name, "_", 2)[,1]) -> acs_variables
  
  # Subset variable names based on variables of interest 
  a <- acs_variables[acs_variables$table_name %in% tableID,]
  
  # Prep variable IDs for joining
  if (yr > 2014){
  
    a %>% mutate(name_mod = substr(name,1,nchar(as.character(name))-1))%>% 
      mutate(label_mod = substr(label,11,nchar(as.character(label)))) %>% 
      mutate(label_mod = gsub("Total!!","",label_mod)) %>%
      mutate(label_mod = gsub("!!"," ",label_mod)) %>%
      select(-table_name) -> a
    
    } else {
      
    a %>% 
    filter(grepl("*E$",a$name)) %>%
    select(-table_name) %>%
    mutate(label = gsub(":!!"," ",label)) %>%
    mutate(label = gsub(":"," ",label)) %>%
    mutate(label = ifelse(grepl(' --!!Total estimate',label)|grepl(' --!!Total moe',label),
                          gsub(" --!!"," ",label),
                          gsub(" --!!Total","",label))) %>%
    mutate(label = gsub("Median age--!!","",label)) %>%
    mutate(label = gsub("Median age","Total",label)) %>%
    mutate(label_mod = label) %>%
    mutate(name_mod = substr(name,1,nchar(as.character(name))-1)) %>%
    mutate(concept = str_extract(concept,"^*\\s([^ ]).*$")) -> a
    }
  
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
    # mutate(Area_m2 = st_area(geometry))%>%
    mutate(Level = geo) %>%
    mutate(Vintage = yr) -> census_format
  
  # if (yr == 2014){
  #   census_format %>%
  #     st_zm(drop=TRUE, what ="ZM") -> census_format
  # }
  
  # Find and replace extra spaces
  names(census_format) <- str_replace(names(census_format), "  "," ")
  
  # # Reproject to WGS 84 EPSG 4326 A.K.A. Google's Projection
  # census_format <- st_transform(census_format, 4326, use_gdal = T)
  
  return(census_format)
}


# start <- Sys.time()
# county_data_16 <- format_Census('block group',tableID,2016,state,counties)
# end <- Sys.time()
# end - start


