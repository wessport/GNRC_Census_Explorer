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

di <- st_transform(di, 4326, use_gdal = T)

save(di, file = "data/di.RData")


di <- readRDS("data/Diversity Indices.rds")

gross_rent <- readRDS("data/Gross Rent.rds")

di %>%
  filter(Level == 'county', Vintage == 2016)-> c_16
di %>%
  filter(Level == 'county', Vintage == 2015)-> c_15
di %>%
  filter(Level == 'county', Vintage == 2014)-> c_14
di %>%
  filter(Level == 'county', Vintage == 2013)-> c_13
di %>%
  filter(Level == 'county', Vintage == 2012)-> c_12
di %>%
  filter(Level == 'county', Vintage == 2011)-> c_11

di %>%
  filter(Level == 'tract', Vintage == 2016)-> t_16
di %>%
  filter(Level == 'tract', Vintage == 2015)-> t_15
di %>%
  filter(Level == 'tract', Vintage == 2014)-> t_14
di %>%
  filter(Level == 'tract', Vintage == 2013)-> t_13
di %>%
  filter(Level == 'tract', Vintage == 2012)-> t_12
di %>%
  filter(Level == 'tract', Vintage == 2011)-> t_11

di %>%
  filter(Level == 'block group', Vintage == 2016)-> b_16
di %>%
  filter(Level == 'block group', Vintage == 2015)-> b_15
di %>%
  filter(Level == 'block group', Vintage == 2014)-> b_14
di %>%
  filter(Level == 'block group', Vintage == 2013)-> b_13
di %>%
  filter(Level == 'block group', Vintage == 2012)-> b_12
di %>%
  filter(Level == 'block group', Vintage == 2011)-> b_11

test <- rbind(c_16,t_16,b_16,c_15,t_15,b_15,c_14,t_14,b_14,c_13,t_13,b_13,c_12,t_12,b_12,c_11,t_11,b_11)

gross_rent %>%
  ungroup() %>%
  select(GEOID) %>%
  mutate(
    NAME = test$NAME,
    Shellys_DI = round(test$Shellys_DI,3),
    Simpsons_DI = round(test$Simpsons_DI,3),
    Shannons_DI = round(test$Shannons_DI,3),
    Vintage = test$Vintage,
    Level = test$Level
  ) -> diversity_indices

saveRDS(diversity_indices,"./data/Diversity Indices.rds")
