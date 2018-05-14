# Greater Nashville Regional Council
# Project: GNRC Census Explorer
# Creation Date: 10-MAY-2018
# Script Author: Wes Porter
# Script Summary: Function for Calculating DI Indices
# Last Updated: 11-MAY-2018

library(magrittr)
library(tidycensus)
library(tidyverse)
library(sf)
library(lwgeom)


generate_DI <- function(di_data){
  
  # This function takes tidycensus sf objects
  # and calculates Simpsons/Shannon Diversity Indices.
  # Works for ACS 2011 - 2016 County, Tract, BG geoms.
  # The following ACS variables are necessary for DI
  # calculations:
  #
  # 'B01003_001E',
  # 'C02003_002E',
  # 'B03002_002E',
  # 'B03002_003E',
  # 'B03002_004E',
  # 'B03002_005E',
  # 'B03002_006E',
  # 'B03002_007E',
  # 'B03002_008E',
  # 'C02003_009E'
  
  not_hisp_vars <- c('B03002_003','B03002_004','B03002_005','B03002_006','B03002_007','B03002_008')
  
  di_data %>%
    st_set_geometry(NULL) %>%
    group_by(NAME) %>%
    summarise(
      total_pop = sum(estimate[variable == 'B01003_001']),
      total_pop_one_race = sum(estimate[variable == 'C02003_002']),
      total_not_hisp_one_race = sum(estimate[variable %in% not_hisp_vars]),
      total_not_hisp_white = sum(estimate[variable == 'B03002_003']),
      total_not_hisp_black = sum(estimate[variable == 'B03002_004']),
      total_not_hisp_amer_ind = sum(estimate[variable == 'B03002_005']),
      total_not_hisp_asian = sum(estimate[variable == 'B03002_006']),
      total_not_hisp_pacific = sum(estimate[variable == 'B03002_007']),
      total_not_hisp_other = sum(estimate[variable == 'B03002_008']),
      total_two_or_more = sum(estimate[variable == 'C02003_009'])
    ) %>%
    mutate(total_hispanic_pop = total_pop_one_race - total_not_hisp_one_race) -> summary_di
  
  summary_di %>%
    rowwise() %>%
    mutate(Shellys_DI = ( 1 - 
      sum(
        (total_not_hisp_white - 1)*total_not_hisp_white,
        (total_not_hisp_black - 1)*total_not_hisp_black,
        (total_not_hisp_amer_ind - 1)*total_not_hisp_amer_ind,
        (total_not_hisp_asian - 1)*total_not_hisp_asian,
        (total_not_hisp_pacific - 1)*total_not_hisp_pacific,
        (total_not_hisp_other - 1)*total_not_hisp_other,
        (total_two_or_more - 1)*total_two_or_more,
        (total_hispanic_pop - 1)*total_hispanic_pop
      ) / ((total_pop - 1)*total_pop)
    ))%>% 
    select(NAME, Shellys_DI) -> Shellys_DI
  
  summary_di %>%
    gather(variable, value,-NAME) %>%
    group_by(NAME) %>%
    summarise(
      perc_overall_pop_white = value[variable == 'total_not_hisp_white'] / value[variable == 'total_pop'],
      perc_overall_pop_black = value[variable == 'total_not_hisp_black'] /
        value[variable == 'total_pop'],
      perc_overall_pop_amer_ind = value[variable == 'total_not_hisp_amer_ind'] /
        value[variable == 'total_pop'],
      perc_overall_pop_asian = value[variable == 'total_not_hisp_asian'] /
        value[variable == 'total_pop'],
      perc_overall_pop_pacific = value[variable == 'total_not_hisp_pacific'] /
        value[variable == 'total_pop'],
      perc_overall_pop_hispanic = value[variable == 'total_hispanic_pop'] /
        value[variable == 'total_pop'],
      perc_overall_pop_other = value[variable == 'total_not_hisp_other'] /
        value[variable == 'total_pop'],
      perc_overall_pop_two_or_more = value[variable == 'total_two_or_more'] /
        value[variable == 'total_pop']
    ) -> perc_overall_pop
  
  # Handle NA values
  perc_overall_pop[is.na(perc_overall_pop)] <- 0
  
  perc_overall_pop %>%
    gather(variable, value, -NAME) %>%
    group_by(NAME) %>%
    summarise(
      ln_perc_overall_pop_white = if (value[variable == 'perc_overall_pop_white'] != 0) {
        log(value[variable == 'perc_overall_pop_white'])
      } else{
        0
      },
      ln_perc_overall_pop_black = if (value[variable == 'perc_overall_pop_black'] != 0) {
        log(value[variable == 'perc_overall_pop_black'])
      } else{
        0
      },
      ln_perc_overall_pop_amer_ind = if (value[variable == 'perc_overall_pop_amer_ind'] != 0) {
        log(value[variable == 'perc_overall_pop_amer_ind'])
      } else{
        0
      },
      ln_perc_overall_pop_asian = if (value[variable == 'perc_overall_pop_asian'] != 0) {
        log(value[variable == 'perc_overall_pop_asian'])
      } else{
        0
      },
      ln_perc_overall_pop_pacific = if (value[variable == 'perc_overall_pop_pacific'] != 0) {
        log(value[variable == 'perc_overall_pop_pacific'])
      } else{
        0
      },
      ln_perc_overall_pop_hispanic = if (value[variable == 'perc_overall_pop_hispanic'] != 0) {
        log(value[variable == 'perc_overall_pop_hispanic'])
      } else{
        0
      },
      ln_perc_overall_pop_other = if (value[variable == 'perc_overall_pop_other'] != 0) {
        log(value[variable == 'perc_overall_pop_other'])
      } else{
        0
      },
      ln_perc_overall_pop_two_or_more = if (value[variable == 'perc_overall_pop_two_or_more'] != 0) {
        log(value[variable == 'perc_overall_pop_two_or_more'])
      } else{
        0
      }
    ) -> ln_perc_overall_pop
  
  perc_overall_pop %>%
    gather(variable, value, -NAME) %>%
    group_by(NAME) %>%
    summarise(
      sq_perc_overall_pop_white = if (value[variable == 'perc_overall_pop_white'] != 0) {
        (value[variable == 'perc_overall_pop_white']) ^ 2
      } else{
        0
      },
      sq_perc_overall_pop_black = if (value[variable == 'perc_overall_pop_black'] != 0) {
        (value[variable == 'perc_overall_pop_black']) ^ 2
      } else{
        0
      },
      sq_perc_overall_pop_amer_ind = if (value[variable == 'perc_overall_pop_amer_ind'] != 0) {
        (value[variable == 'perc_overall_pop_amer_ind']) ^ 2
      } else{
        0
      },
      sq_perc_overall_pop_asian = if (value[variable == 'perc_overall_pop_asian'] != 0) {
        (value[variable == 'perc_overall_pop_asian']) ^ 2
      } else{
        0
      },
      sq_perc_overall_pop_pacific = if (value[variable == 'perc_overall_pop_pacific'] != 0) {
        (value[variable == 'perc_overall_pop_pacific'] ^ 2)
      } else{
        0
      },
      sq_perc_overall_pop_hispanic = if (value[variable == 'perc_overall_pop_hispanic'] != 0) {
        (value[variable == 'perc_overall_pop_hispanic']) ^ 2
      } else{
        0
      },
      sq_perc_overall_pop_other = if (value[variable == 'perc_overall_pop_other'] != 0) {
        (value[variable == 'perc_overall_pop_other']) ^ 2
      } else{
        0
      },
      sq_perc_overall_pop_two_or_more = if (value[variable == 'perc_overall_pop_two_or_more'] != 0) {
        (value[variable == 'perc_overall_pop_two_or_more']) ^ 2
      } else{
        0
      }
    ) -> sq_perc_overall_pop
  
  perc_overall_pop %>%
    select(-NAME) -> a
  
  ln_perc_overall_pop %>%
    select(-NAME) -> b
  
  c <-  a * b
  
  colnames(c) <-
    c(
      'diversification_white',
      'diversification_black',
      'diversification_amer_ind',
      'diversification_asian',
      'diversification_pacific',
      'diversification_hispanic',
      'diversification_other',
      'diversification_two_or_more'
    )
  
  perc_overall_pop %>%
    select(NAME) %>%
    cbind(c) %>%
    rowwise() %>%
    mutate(Shannons_DI = -1 * (
      sum(
        diversification_white,
        diversification_black,
        diversification_amer_ind,
        diversification_asian,
        diversification_pacific,
        diversification_hispanic,
        diversification_other,
        diversification_two_or_more
      )
    )) -> Shannons_DI
  
  sq_perc_overall_pop %>%
    rowwise() %>%
    mutate(
      Simpsons_DI = 1 / sum(
        sq_perc_overall_pop_white,
        sq_perc_overall_pop_black,
        sq_perc_overall_pop_amer_ind,
        sq_perc_overall_pop_asian,
        sq_perc_overall_pop_pacific,
        sq_perc_overall_pop_hispanic,
        sq_perc_overall_pop_other,
        sq_perc_overall_pop_two_or_more
      )
    ) -> Simpsons_DI
  
  Simpsons_DI$Simpsons_DI[Simpsons_DI$Simpsons_DI == Inf] <- 1.0
  
  di_data %>%
    select(NAME, geometry) %>%
    group_by(NAME) %>%
    distinct() %>%
    right_join(Shellys_DI, by = c("NAME" = "NAME")) %>%
    right_join(Shannons_DI, by = c("NAME" = "NAME")) %>%
    right_join(Simpsons_DI, by = c("NAME" = "NAME")) %>%
    select(NAME, Shellys_DI, Simpsons_DI, Shannons_DI, geometry) -> DI_geom
  
  return(DI_geom)

}

