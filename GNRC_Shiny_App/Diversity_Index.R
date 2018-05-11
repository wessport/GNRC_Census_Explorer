# Greater Nashville Regional Council
# Project: GNRC Census Explorer
# Creation Date: 10-MAY-2018
# Script Author: Wes Porter
# Script Summary: Downloading DI variables
# Last Updated: 11-MAY-2018

library(tidycensus)
library(magrittr)
library(sf)
library(tidyverse)
library(lettercase)
library(lwgeom)
library(stringr)

counties <- c('Cheatham','Davidson','Dickson','Houston','Humphreys','Montgomery','Maury','Robertson','Rutherford','Stewart','Sumner',
              'Trousdale','Williamson','Wilson')

geo <- 'county'
state <-  'TN'
yr <- 2016

generate_DI <- function(geo, counties, state, yr){

  DI_vars <- c('C02003_002E','B03002_002E','B03002_003E','B03002_004E','B03002_005E','B03002_006E','B03002_007E','B03002_008E','C02003_009E')    
  
  not_hisp_vars <- c('B03002_003','B03002_004','B03002_005','B03002_006','B03002_007','B03002_008')
  
  di_data <- get_acs(state = state, geography = 'tract', county = counties, variables = DI_vars,  year = yr, geometry = TRUE)
  
  if (is.na(counties)){
  
    # Request data from Census API
    di_data <- get_acs(state = state, geography = geo, variables = DI_vars, year = yr, geometry = TRUE)
  
  } else {
  
    # Request data from Census API
    di_data <- get_acs(state = state, geography = geo, county = counties, variables = DI_vars,  year = yr, geometry = TRUE)
  }

  # Shannon's diversity index -----------------

  di_data %>%
    st_set_geometry(NULL) %>%
    group_by(NAME) %>%
    summarise(total_pop_one_race = sum(estimate[variable == 'C02003_002']),
              total_not_hisp_one_race = sum(estimate[variable %in% not_hisp_vars]),
              total_not_hisp_white = sum(estimate[variable == 'B03002_003']),
              total_not_hisp_black = sum(estimate[variable == 'B03002_004']),
              total_not_hisp_amer_ind = sum(estimate[variable == 'B03002_005']),
              total_not_hisp_asian = sum(estimate[variable == 'B03002_006']),
              total_not_hisp_pacific = sum(estimate[variable == 'B03002_007']),
              total_not_hisp_other = sum(estimate[variable == 'B03002_008']),
              total_two_or_more = sum(estimate[variable == 'C02003_009'])
              )%>%
    mutate(total_hispanic_pop = total_pop_one_race - total_not_hisp_one_race)-> summary_di
    
  summary_di %>% 
    gather(variable, value, -NAME) %>%
    group_by(NAME) %>%
    summarise(perc_overall_pop_white = value[variable == 'total_not_hisp_white']/value[variable == 'total_pop_one_race'],
              perc_overall_pop_black = value[variable == 'total_not_hisp_black']/value[variable == 'total_pop_one_race'],
              perc_overall_pop_amer_ind = value[variable == 'total_not_hisp_amer_ind']/value[variable == 'total_pop_one_race'],
              perc_overall_pop_asian = value[variable == 'total_not_hisp_asian']/value[variable == 'total_pop_one_race'],
              perc_overall_pop_pacific = value[variable == 'total_not_hisp_pacific']/value[variable == 'total_pop_one_race'],
              perc_overall_pop_hispanic = value[variable == 'total_hispanic_pop']/value[variable == 'total_pop_one_race'],
              perc_overall_pop_other = value[variable == 'total_not_hisp_other']/value[variable == 'total_pop_one_race'],
              perc_overall_pop_two_or_more = value[variable == 'total_two_or_more']/value[variable == 'total_pop_one_race']
              ) -> perc_overall_pop
  
  # Handle NA values 
  perc_overall_pop[is.na(perc_overall_pop)] <- 0
  
  perc_overall_pop %>%
    gather(variable,value,-NAME) %>%
    group_by(NAME)%>%
    summarise(ln_perc_overall_pop_white = if(value[variable == 'perc_overall_pop_white'] != 0){log(value[variable == 'perc_overall_pop_white'])}else{0},
              ln_perc_overall_pop_black = if(value[variable == 'perc_overall_pop_black'] != 0){log(value[variable == 'perc_overall_pop_black'])}else{0},
              ln_perc_overall_pop_amer_ind = if(value[variable == 'perc_overall_pop_amer_ind'] != 0){log(value[variable == 'perc_overall_pop_amer_ind'])}else{0},
              ln_perc_overall_pop_asian = if(value[variable == 'perc_overall_pop_asian'] != 0){log(value[variable == 'perc_overall_pop_asian'])}else{0},
              ln_perc_overall_pop_pacific = if(value[variable == 'perc_overall_pop_pacific'] != 0){log(value[variable == 'perc_overall_pop_pacific'])}else{0},
              ln_perc_overall_pop_hispanic = if(value[variable == 'perc_overall_pop_hispanic'] != 0){log(value[variable == 'perc_overall_pop_hispanic'])}else{0},
              ln_perc_overall_pop_other = if(value[variable == 'perc_overall_pop_other'] != 0){log(value[variable == 'perc_overall_pop_other'])}else{0},
              ln_perc_overall_pop_two_or_more = if(value[variable == 'perc_overall_pop_two_or_more'] != 0){log(value[variable == 'perc_overall_pop_two_or_more'])}else{0}
              ) -> ln_perc_overall_pop
  
  perc_overall_pop %>%
    select(-NAME) -> a
  
  ln_perc_overall_pop %>%
    select(-NAME) -> b
  
  c <-  a*b
  
  colnames(c) <- c('diversification_white','diversification_black','diversification_amer_ind','diversification_asian','diversification_pacific',
                    'diversification_hispanic','diversification_other','diversification_two_or_more')
               
  perc_overall_pop %>%
    select(NAME) %>%
    cbind(c) %>%
    rowwise() %>%
    mutate(Shannons_DI = -1*(sum(diversification_white,diversification_black,diversification_amer_ind,diversification_asian,diversification_pacific,
               diversification_hispanic,diversification_other,diversification_two_or_more))) -> Shannons_DI
  
  di_data %>%
    select(NAME,geometry)%>%
    group_by(NAME)%>%
    distinct() %>%
    right_join(Shannons_DI, by = c("NAME" = "NAME"))%>%
    select(NAME,Shannons_DI,geometry)-> shannons_DI_geom
    
  return(shannons_DI_geom)

}

test2016 <- generate_DI(geo,counties,state,2016)
test2015 <- generate_DI(geo,counties,state,2015)
test2014 <- generate_DI(geo,counties,state,2014)
test2013 <- generate_DI(geo,counties,state,2013)
test2012 <- generate_DI(geo,counties,state,2012)
test2011 <- generate_DI(geo,counties,state,2011)

tract2016 <- generate_DI('tract',counties,state,2016)
tract2015 <- generate_DI('tract',counties,state,2015)
tract2014 <- generate_DI('tract',counties,state,2014)
tract2013 <- generate_DI('tract',counties,state,2013)
tract2012 <- generate_DI('tract',counties,state,2012)
tract2011 <- generate_DI('tract',counties,state,2011)
