# Formatting census data for end users
library(tidycensus)
library(leaflet)
library(rgdal)
library(magrittr)
library(sf)
library(tigris)
library(tidyverse)
library(lettercase)
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)
census_api_key("6999d8d1e472e95e754d605f9a5646beec7eede5", install = TRUE)
Sys.getenv("CENSUS_API_KEY")

# Define variables of interest
gross_rent <- c('B25063_001E','B25063_002E','B25063_003E','B25063_004E','B25063_005E','B25063_006E','B25063_007E','B25063_008E','B25063_009E',
                'B25063_010E','B25063_011E','B25063_012E','B25063_013E','B25063_014E','B25063_015E','B25063_016E','B25063_017E','B25063_018E',
                'B25063_019E','B25063_020E','B25063_021E','B25063_022E','B25063_023E','B25063_024E','B25063_025E','B25063_026E','B25063_027E')

# Define GNRC counties
counties <- c('Cheatham','Davidson','Dickson','Houston','Humphreys','Montgomery','Maury','Robertson','Rutherford','Stewart','Sumner',
              'Trousdale','Williamson','Wilson')

# Request data from Census API
census_data <- get_acs(state = "TN", county = counties, geography = "tract", 
                variables = gross_rent, geometry = TRUE)

# Complete list of variable names and their respective IDs
svar <- read.csv("C:/Users/wPorter/Data/Census/ACS5_tabular_data/data_dictionary/selected_variableID.csv")

# Subset variable names based on variables of interest 
a <- svar[svar$name %in% gross_rent,]

# Prep variable IDs for joining
a %>% mutate(name_mod = substr(name,1,nchar(as.character(name))-1))%>% 
  mutate(label_mod = substr(label,11,nchar(as.character(label)))) %>% 
  mutate(label_mod = gsub("!!"," ",label_mod)) %>%
  mutate(label_mod = paste(str_title_case(str_lower(concept)), label_mod, sep =" ")) -> a

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
  spread(variable_combined, value)-> census_format

# Reproject to WGS 84 EPSG 4326 A.K.A. Google's Projection
census_format <- st_transform(census_format, 4326, use_gdal = T)

# User selected variable
map_var <- census_format[[3]]

leaflet(census_format) %>%
  addProviderTiles(providers$CartoDB.PositronNoLabels,
                   options = providerTileOptions(noWrap =TRUE, zIndex = 1)
  ) %>%
  addPolygons(group = "polygons",
              fillColor = ~colorQuantile("YlOrRd", map_var)(map_var), 
              fillOpacity = 0.5, 
              weight = 2, 
              stroke = T, 
              color = "grey", 
              opacity = 1,
              dashArray = "3",
              highlight = highlightOptions(color = "white", weight = 3, bringToFront = TRUE),
              options=list(zIndex = 2)
  ) %>%
  addProviderTiles("CartoDB.PositronOnlyLabels", group="labels", 
                   options=providerTileOptions(zIndex = 3, pane = 'markerPane')) %>% 
  addLayersControl(overlayGroups = c("polygons", "labels"))



