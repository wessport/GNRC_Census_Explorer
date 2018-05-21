# Greater Nashville Regional Council
# Project: GNRC Census Explorer
# Creation Date: 11-MAY-2018
# Script Author: Wes Porter
# Script Summary: Calculate and Store DI Indices
# Last Updated: 11-MAY-2018

library(magrittr)
library(sf)
library(tidycensus)
library(tidyverse)

source("Diversity_Index.R")

# Define ACS variables

state <-  'TN'
counties <-
  c(
    'Cheatham',
    'Davidson',
    'Dickson',
    'Houston',
    'Humphreys',
    'Montgomery',
    'Maury',
    'Robertson',
    'Rutherford',
    'Stewart',
    'Sumner',
    'Trousdale',
    'Williamson',
    'Wilson'
  )
DI_vars <-
  c(
    'B01003_001E',
    'C02003_002E',
    'B03002_002E',
    'B03002_003E',
    'B03002_004E',
    'B03002_005E',
    'B03002_006E',
    'B03002_007E',
    'B03002_008E',
    'C02003_009E'
  )    

# Calculating Diversity indices

# County -----
di_census_data <-
  get_acs(
    state = state,
    geography = 'county',
    county = counties,
    variables = DI_vars,
    year = 2016,
    geometry = TRUE
  )
county_di_2016 <- generate_DI(di_census_data)
di_census_data <-
  get_acs(
    state = state,
    geography = 'county',
    county = counties,
    variables = DI_vars,
    year = 2015,
    geometry = TRUE
  )
county_di_2015 <- generate_DI(di_census_data)
di_census_data <-
  get_acs(
    state = state,
    geography = 'county',
    county = counties,
    variables = DI_vars,
    year = 2014,
    geometry = TRUE
  )
county_di_2014 <- generate_DI(di_census_data)
di_census_data <-
  get_acs(
    state = state,
    geography = 'county',
    county = counties,
    variables = DI_vars,
    year = 2013,
    geometry = TRUE
  )
county_di_2013 <- generate_DI(di_census_data)
di_census_data <-
  get_acs(
    state = state,
    geography = 'county',
    county = counties,
    variables = DI_vars,
    year = 2012,
    geometry = TRUE
  )
county_di_2012 <- generate_DI(di_census_data)
di_census_data <-
  get_acs(
    state = state,
    geography = 'county',
    county = counties,
    variables = DI_vars,
    year = 2011,
    geometry = TRUE
  )
county_di_2011 <- generate_DI(di_census_data)

# Tract -----
di_census_data <-
  get_acs(
    state = state,
    geography = 'tract',
    county = counties,
    variables = DI_vars,
    year = 2016,
    geometry = TRUE
  )
tract_di_2016 <- generate_DI(di_census_data)
di_census_data <-
  get_acs(
    state = state,
    geography = 'tract',
    county = counties,
    variables = DI_vars,
    year = 2015,
    geometry = TRUE
  )
tract_di_2015 <- generate_DI(di_census_data)
di_census_data <-
  get_acs(
    state = state,
    geography = 'tract',
    county = counties,
    variables = DI_vars,
    year = 2014,
    geometry = TRUE
  )
tract_di_2014 <- generate_DI(di_census_data)
di_census_data <-
  get_acs(
    state = state,
    geography = 'tract',
    county = counties,
    variables = DI_vars,
    year = 2013,
    geometry = TRUE
  )
tract_di_2013 <- generate_DI(di_census_data)
di_census_data <-
  get_acs(
    state = state,
    geography = 'tract',
    county = counties,
    variables = DI_vars,
    year = 2012,
    geometry = TRUE
  )
tract_di_2012 <- generate_DI(di_census_data)
di_census_data <-
  get_acs(
    state = state,
    geography = 'tract',
    county = counties,
    variables = DI_vars,
    year = 2011,
    geometry = TRUE
  )
tract_di_2011 <- generate_DI(di_census_data)

# Block group -----
di_census_data <-
  get_acs(
    state = state,
    geography = 'block group',
    county = counties,
    variables = DI_vars,
    year = 2016,
    geometry = TRUE
  )
bg_di_2016 <- generate_DI(di_census_data)
di_census_data <-
  get_acs(
    state = state,
    geography = 'block group',
    county = counties,
    variables = DI_vars,
    year = 2015,
    geometry = TRUE
  )
bg_di_2015 <- generate_DI(di_census_data)
di_census_data <-
  get_acs(
    state = state,
    geography = 'block group',
    county = counties,
    variables = DI_vars,
    year = 2014,
    geometry = TRUE
  )
bg_di_2014 <- generate_DI(di_census_data)
di_census_data <-
  get_acs(
    state = state,
    geography = 'block group',
    county = counties,
    variables = DI_vars,
    year = 2013,
    geometry = TRUE
  )
bg_di_2013 <- generate_DI(di_census_data)
di_census_data <-
  get_acs(
    state = state,
    geography = 'block group',
    county = counties,
    variables = DI_vars,
    year = 2012,
    geometry = TRUE
  )
bg_di_2012 <- generate_DI(di_census_data)
di_census_data <-
  get_acs(
    state = state,
    geography = 'block group',
    county = counties,
    variables = DI_vars,
    year = 2011,
    geometry = TRUE
  )
bg_di_2011 <- generate_DI(di_census_data)

save(
  county_di_2016,
  county_di_2015,
  county_di_2014,
  county_di_2013,
  county_di_2012,
  county_di_2011,
  file = "data/county_di.RData"
)

save(
  tract_di_2016,
  tract_di_2015,
  tract_di_2014,
  tract_di_2013,
  tract_di_2012,
  tract_di_2011,
  file = "data/tract_di.RData"
)

save(
  bg_di_2016,
  bg_di_2015,
  bg_di_2014,
  bg_di_2013,
  bg_di_2012,
  bg_di_2011,
  file = "data/bg_di.RData"
)



county_di_2016 %>%
  mutate(Vintage = 2016) %>%
  mutate(Level = "county") -> county_di_2016

county_di_2015 %>%
  mutate(Vintage = 2015)%>%
  mutate(Level = "county") -> county_di_2015

county_di_2014 %>%
  mutate(Vintage = 2014)%>%
  st_zm(drop=TRUE, what ="ZM")%>%
  mutate(Level = "county") -> county_di_2014

county_di_2013 %>%
  mutate(Vintage = 2013)%>%
  mutate(Level = "county") -> county_di_2013

county_di_2012 %>%
  mutate(Vintage = 2012)%>%
  mutate(Level = "county") -> county_di_2012

county_di_2011 %>%
  mutate(Vintage = 2011)%>%
  mutate(Level = "county") -> county_di_2011


county_di_2016 %>%
  rbind(county_di_2015) %>%
  rbind(county_di_2014) %>%
  rbind(county_di_2013) %>%
  rbind(county_di_2012) %>%
  rbind(county_di_2011) -> county_di

tract_di_2016 %>%
  mutate(Vintage = 2016)%>%
  mutate(Level = "tract")-> tract_di_2016

tract_di_2015 %>%
  mutate(Vintage = 2015)%>%
  mutate(Level = "tract") -> tract_di_2015

tract_di_2014 %>%
  mutate(Vintage = 2014)%>%
  st_zm(drop=TRUE, what ="ZM")%>%
  mutate(Level = "tract") -> tract_di_2014

tract_di_2013 %>%
  mutate(Vintage = 2013)%>%
  mutate(Level = "tract") -> tract_di_2013

tract_di_2012 %>%
  mutate(Vintage = 2012)%>%
  mutate(Level = "tract") -> tract_di_2012

tract_di_2011 %>%
  mutate(Vintage = 2011)%>%
  mutate(Level = "tract") -> tract_di_2011


tract_di_2016 %>%
  rbind(tract_di_2015) %>%
  rbind(tract_di_2014) %>%
  rbind(tract_di_2013) %>%
  rbind(tract_di_2012) %>%
  rbind(tract_di_2011) -> tract_di
  

bg_di_2016 %>%
  mutate(Vintage = 2016)%>%
  mutate(Level = "block group")-> bg_di_2016

bg_di_2015 %>%
  mutate(Vintage = 2015)%>%
  mutate(Level = "block group") -> bg_di_2015

bg_di_2014 %>%
  mutate(Vintage = 2014)%>%
  st_zm(drop=TRUE, what ="ZM")%>%
  mutate(Level = "block group") -> bg_di_2014

bg_di_2013 %>%
  mutate(Vintage = 2013)%>%
  mutate(Level = "block group") -> bg_di_2013

bg_di_2012 %>%
  mutate(Vintage = 2012)%>%
  mutate(Level = "block group") -> bg_di_2012

bg_di_2011 %>%
  mutate(Vintage = 2011)%>%
  mutate(Level = "block group") -> bg_di_2011


bg_di_2016 %>%
  rbind(bg_di_2015) %>%
  rbind(bg_di_2014) %>%
  rbind(bg_di_2013) %>%
  rbind(bg_di_2012) %>%
  rbind(bg_di_2011) -> bg_di


county_di %>%
  rbind(tract_di) %>%
  rbind(bg_di) -> di

save(di, file = "data/di.RData")
