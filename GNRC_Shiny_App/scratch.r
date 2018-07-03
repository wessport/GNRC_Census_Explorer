# A sandbox for working with census data
library(dplyr)
library(ggplot2)
library(tidycensus)
library(leaflet)
library(rgdal)
library(magrittr)
library(sf)
library(stringr)
library(tigris)
library(tidyverse)
library(lettercase)
library(plotly)
library(viridis)
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)
census_api_key("6999d8d1e472e95e754d605f9a5646beec7eede5", install = TRUE)
Sys.getenv("CENSUS_API_KEY")

path = "C:/Users/wPorter/Data/Census/census_shapefiles/GNR/counties"

gnr_2017 <- readOGR(path, layer = 'cb_2017_gnr_county')

plot(gnr_2017)

test <-  st_read(path, layer = 'cb_2017_gnr_county')


plot(test$NAME, test$ALAND)

barplot(test$ALAND, main="Area Distribution", 
        names.arg=test$NAME)


labels <- test$NAME

c <- paste("<strong>", labels, "</strong>", sep ='')

leaflet(test) %>%
  addProviderTiles(providers$Stamen.TonerLite,
                   options = providerTileOptions(noWrap =TRUE)
  ) %>%
  addPolygons(fillColor = ~colorQuantile("YlOrRd", ALAND)(ALAND), 
              fillOpacity = 0.5, 
              weight = 2, 
              stroke = T, 
              color = "grey", 
              opacity = 1,
              dashArray = "3",
              highlight = highlightOptions(color = "white", weight = 3, bringToFront = TRUE),
              #label = labels
              label = lapply(c,HTML)
              )


lableOptions = labelOptions(
  style = list("font-weight" = "normal", padding = "3px 8px"),
  textsize = "15px",
  direction = "auto")


# Testing to see which geographies Tidycensus supports

# COUNTY
# county
# tract
# block group

# STATE 
# place - no geometry via tidycensus
# combined statistical area

# NATION
# urban area

tn <- get_acs(geography = "place", 
              variables = c(medincome = "B19013_001"), 
              state = "TN")

tn <- get_acs(geography = "county", variables = c(medincome = "B19013_001", medincome2 = "B19013_002"), state = "TN", endyear = 2015)

nation <- get_acs(geography = 'urban area', variables = c(medincome = "B19013_001"))


acs_variables_16 <-  load_variables(2016, "acs5", cache = TRUE)
acs_variables_15 <-  load_variables(2015, "acs5", cache = TRUE)
acs_variables_14 <-  load_variables(2014, "acs5", cache = TRUE)
acs_variables_13 <-  load_variables(2013, "acs5", cache = TRUE)
acs_variables_12 <-  load_variables(2012, "acs5", cache = TRUE)
acs_variables_11 <-  load_variables(2011, "acs5", cache = TRUE)


v <- read.csv("C:/Users/wPorter/Data/Census/ACS5_tabular_data/data_dictionary/variables.csv", stringsAsFactors = FALSE)

write.table(unique(v),"C:/Users/wPorter/Data/Census/ACS5_tabular_data/data_dictionary/unique_variables.csv", sep=",", col.names = T, row.names = F)

nrow(unique(v))

test <- get_acs(state = "TN", county = c("Davidson","Rutherford"), geography = "tract", 
                  variables = "B19013_001", geometry = TRUE)


gross_rent <- c('B25063_001E','B25063_002E','B25063_003E','B25063_004E','B25063_005E','B25063_006E','B25063_007E','B25063_008E','B25063_009E',
                'B25063_010E','B25063_011E','B25063_012E','B25063_013E','B25063_014E','B25063_015E','B25063_016E','B25063_017E','B25063_018E',
                'B25063_019E','B25063_020E','B25063_021E','B25063_022E','B25063_023E','B25063_024E','B25063_025E','B25063_026E','B25063_027E')

gsub("E", "",gross_rent) -> gross_rent

test <- get_acs(table = "B25063", state = "TN", county = c("Davidson", "Rutherford"), geography = "tract",geometry = TRUE, year = 2017)

# Formatting Census table in familiar format for users
# * Still need to figure out how to add variable names elegantly
test %>% 
  gather(measurement, value, estimate, moe) %>% 
  unite(variable_combined, variable, measurement )%>%
  group_by(NAME) %>%
  arrange(GEOID,variable_combined)%>%
  spread(variable_combined, value)-> t


# Leaflet works with wide format, expects only 1 variable at a time
leaflet(t) %>%
  addProviderTiles(providers$Stamen.TonerLite,
                   options = providerTileOptions(noWrap =TRUE)
  ) %>%
  addPolygons(fillColor = ~colorQuantile("YlOrRd", B25063_001_estimate)(B25063_001_estimate), 
              fillOpacity = 0.5, 
              weight = 2, 
              stroke = T, 
              color = "grey", 
              opacity = 1,
              dashArray = "3",
              highlight = highlightOptions(color = "white", weight = 3, bringToFront = TRUE)
  )

# Reproject to WGS 84 EPSG 4326 A.K.A. Google's Projection
t <- st_transform(t, 4326, use_gdal = T)


svar <- read.csv("C:/Users/wPorter/Data/Census/ACS5_tabular_data/data_dictionary/selected_variableID.csv")

a <- svar[svar$name %in% gross_rent,]


a %>% mutate(name_mod = substr(name,1,nchar(as.character(name))-1)) -> a

# Handle upper case
str_lower("GROSS RENT") %>% str_title_case()

a %>% mutate(label_mod = substr(label,11,nchar(as.character(label)))) %>% 
  mutate(label_mod = gsub("!!"," ",label_mod)) %>%
  mutate(label_mod = paste(str_title_case(str_lower(concept)), label_mod, sep =" ")) -> a

colnames(t)

col_labels <- colnames(t)



##########

gross_rent <- c('B25063_001E','B25063_002E','B25063_003E','B25063_004E','B25063_005E','B25063_006E','B25063_007E','B25063_008E','B25063_009E',
                'B25063_010E','B25063_011E','B25063_012E','B25063_013E','B25063_014E','B25063_015E','B25063_016E','B25063_017E','B25063_018E',
                'B25063_019E','B25063_020E','B25063_021E','B25063_022E','B25063_023E','B25063_024E','B25063_025E','B25063_026E','B25063_027E')

test <- get_acs(state = "TN", county = c("Rutherford"), geography = "tract", 
                variables = gross_rent, geometry = TRUE, year = 2014)


a %>% select(name_mod, label_mod) %>%
  right_join(test, by = c("name_mod" = "variable")) -> test2


# Formatting Census table in familiar format for users
test2 %>% 
  gather(measurement, value, estimate, moe) %>%
  unite(variable_combined, label_mod, measurement, sep = " ") %>%
  group_by(NAME) %>%
  arrange(GEOID,name_mod) %>%
  select(-name_mod) %>%
  spread(variable_combined, value)-> t2



attr <- colnames(county_di_2016)


attr[!attr %in% c("NAME","geometry")] 

c("Please select an option below" = "","a","b","c")

a <- c("Please select an option below" = "")

b <- c("a","b","c")

c(a,b)

attr <- colnames(county_di_2016)
attr[!attr %in% c("NAME","geometry")]

c("Please select an option below" = "", attr[!attr %in% c("NAME","geometry")])



# Removing geometry for labeling purposes

county_di_2016 %>%
  st_set_geometry(NULL) %>% 
  pull("NAME") -> label

county_di_2016 %>%
  st_set_geometry(NULL) %>% 
  ungroup() %>%
  pull("Shellys_DI") -> map_var


test %>%
  filter(Vintage == 2015)-> test2


leaflet(test2) %>%
  addProviderTiles(providers$Stamen.TonerLite,
                   options = providerTileOptions(noWrap =TRUE)
  ) %>%
  addPolygons(fillColor = ~colorQuantile("YlOrRd", Shellys_DI)(Shellys_DI), 
              fillOpacity = 0.5, 
              weight = 2, 
              stroke = T, 
              color = "grey", 
              opacity = 1,
              dashArray = "3",
              highlight = highlightOptions(color = "white", weight = 3, bringToFront = TRUE)
  )


st_zm(county_di_2014,drop=TRUE, what ="ZM") -> county_di_2014


##########

geom <- readRDS("./data/geometry.rds")

test <- readRDS("./data/Contract Rent.rds")

test %>%
  left_join(geom, by = c("NAME" = "NAME", "Vintage" = "Vintage")) -> t

t <- st_as_sf(t)

t %>%
  filter(Vintage == 2016 & Level == "county") -> f_cnty_data



tryCatch(colorQuantile("YlOrRd", f_cnty_data[["With cash rent $2,000 to $2,499 estimate"]])(f_cnty_data[["With cash rent $2,000 to $2,499 estimate"]]), 
         error=function(e) colorBin("YlOrRd", f_cnty_data[["With cash rent $2,000 to $2,499 estimate"]])(f_cnty_data[["With cash rent $2,000 to $2,499 estimate"]]))

leaflet(f_cnty_data) %>%
  
  addProviderTiles(providers$CartoDB.PositronNoLabels,
                   options = providerTileOptions(noWrap = TRUE, zIndex = 1)) %>%
  addPolygons(
    group = "county",
    fillColor = ~ colorBin("YlOrRd", f_cnty_data[["With cash rent $2,000 to $2,499 estimate"]])(f_cnty_data[["With cash rent $2,000 to $2,499 estimate"]]),
    fillOpacity = 0.5,
    weight = 2,
    stroke = T,
    color = "grey",
    # opacity = 1,
    #dashArray = "3",
    highlight = highlightOptions(color = "white",weight = 3,bringToFront = TRUE),
    options = list(zIndex = 2),
    label = lapply(c, HTML)
  ) %>%
  addProviderTiles(
    "CartoDB.PositronOnlyLabels",
    group = "labels",
    options = providerTileOptions(zIndex = 3, pane = 'markerPane')
  ) %>%
  addLayersControl(overlayGroups = c("polygons", "labels")) %>%
  addMarkers(-86.7849,36.16947)




f_cnty_data %>%
  mutate(perc=(get("With cash rent $2,000 to $2,499 estimate")/get("Total estimate"))*100) -> f_cnty_data

f_cnty_data %>% ungroup() %>% summarise(sum(`Total estimate`)) %>% st_set_geometry(NULL) -> total

f_cnty_data %>%
  mutate(perc=(get("With cash rent $2,000 to $2,499 estimate")/total[[1]])*100) -> f_cnty_data

pal <- tryCatch(colorQuantile("YlOrRd", f_cnty_data[["perc"]])(f_cnty_data[["perc"]]), 
         error=function(e) colorBin("YlOrRd", f_cnty_data[["perc"]])(f_cnty_data[["perc"]]))



leaflet(f_cnty_data) %>%
  
  addProviderTiles(providers$CartoDB.PositronNoLabels,
                   options = providerTileOptions(noWrap = TRUE, zIndex = 1)) %>%
  addPolygons(
    group = "county",
    fillColor = colorBin("YlOrRd", f_cnty_data[["perc"]])(f_cnty_data[["perc"]]),
    fillOpacity = 0.5,
    weight = 2,
    stroke = T,
    color = "grey",
    # opacity = 1,
    #dashArray = "3",
    highlight = highlightOptions(color = "white",weight = 3,bringToFront = TRUE),
    options = list(zIndex = 2),
    label = lapply(c, HTML)
  ) %>%
  addProviderTiles(
    "CartoDB.PositronOnlyLabels",
    group = "labels",
    options = providerTileOptions(zIndex = 3, pane = 'markerPane')
  ) %>%
  addLegend("bottomright", pal = 'grey', values = ~perc,
            title = "Count",
            opacity = 0.5
  )%>%
  addLayersControl(overlayGroups = c("polygons", "labels"))

#####
di %>%
  filter(Vintage == 2016 & Level == "county") 

di %>% 
  st_set_geometry(NULL) %>%
  filter(NAME == 'Davidson County, Tennessee') %>%
  select(Vintage, Shellys_DI)-> test

# Plotly

di %>%
  st_set_geometry(NULL) %>%
  filter(Level == "county")%>%
  group_by(NAME)%>% 
  plot_ly(x = ~Vintage, y = ~Shellys_DI, ylab = 'DI', color = ~NAME, colors = viridis_pal(option = "D")(3), name = ~NAME, type = 'scatter', mode = 'lines+markers')

contract_rent_dt %>% filter(Vintage == 2016 & Level == 'county') -> temp

sum(is.na(temp$`With cash rent $2,000 to $2,499 estimate`))

####


places <- st_read("C:/Users/wPorter/Data/Census/census_shapefiles/boundaries/GNR/places/tl_2017_gnr_places_slim.shp")

places %>% select(NAME) -> places

saveRDS(places,"./data/places.rds")

####

contract_rent_dt %>% filter(Level == 'county') -> test
contract_rent_dt %>% filter(Level == 'tract' & grepl('Davidson County, Tennessee',NAME, fixed = TRUE)) -> test
contract_rent_dt %>% filter(Level == 'block group' & grepl('Davidson County, Tennessee',NAME, fixed = TRUE)) -> test

# Bar Plot

y <-  2016

y_axis <- list(
  title = "")

x_axis <- list(
  title = "Count")

grep(" .*", test$NAME, value = TRUE)

gsub("^(.*?),.*", "\\1", test$NAME)




test %>%
  filter(Vintage == 2016) %>%
  select(NAME) -> bar_names

# BG
word(bar_names$NAME,1,2,sep=',') -> bar_names

# Tract
gsub(",.*$", "", bar_names$NAME) %>% word(2,3) -> bar_names  

# County
sub(" .*", '', bar_names$NAME) -> bar_names

test %>%
  filter(Vintage == y) %>%
  plot_ly(
    x = ~ get('Total estimate'),
    y = bar_names,
    type = 'bar',
    orientation = 'h',
    marker = list(
      # color = 'rgba(50, 171, 96, 0.6)',
      color = 'rgb(154, 208, 245, 0.6)',
      line = list(color = 'rgba(50, 171, 96, 1.0)', width = 1)
    )
  ) %>%
  layout(
    title = 'Horizontal bar Chart',
    xaxis = x_axis,
    yaxis = y_axis,
    margin = list(
      l = 100,
      r = 20,
      t = 70,
      b = 70
    )
  )

#### Fighting with bicycle legend

`Means of transportation to work`$`Bicycle estimate`

quantile(`Means of transportation to work`$`Bicycle estimate`)

unique(quantile(`Means of transportation to work`$`Bicycle estimate`))

q_length <- length(quantile(`Means of transportation to work`$`Bicycle estimate`))

unique_q_length <- length(unique(quantile(`Means of transportation to work`$`Bicycle estimate`)))

if(q_length>unique_q_length){
  print('Need to use colorBin')
} else{
  'Need to use colorQuant'
}


health_insr_cov_dt %>%
  filter(Level == 'block group' & Vintage == 2016) -> test 


quantile(test$`Female estimate`,na.rm=TRUE)

#### DCS

tableID <- "B25056"
year <- 2010
state <- 'TN'
counties <- c('Cheatham','Davidson','Dickson','Houston','Humphreys','Montgomery','Maury','Robertson','Rutherford','Stewart','Sumner',
              'Trousdale','Williamson','Wilson')

# Doesn't work because  decennial table IDs are different from ACS
census_data <- get_decennial('county', table = tableID, year = year, state = state, county = counties, 
                       geometry = FALSE)

# Data Table Headers -----

colnames(`Detailed Race`)

colnames(`Detailed Race`)[(str_count(colnames(`Detailed Race`), ' ')>=10)]

which(str_count(colnames(`Detailed Race`), ' ')>=10)


# Move Vintage and Level to front of table

move_vl <- function(data_path){
  
  data_set <- readRDS(data_path)
  
  data_set %>%
    ungroup %>%
    select(GEOID,NAME,Level,Vintage,`Total estimate`,`Total moe`)-> a
  
  data_set %>%
    ungroup%>%
    select(-GEOID,-NAME,-Level,-Vintage,-`Total estimate`,-`Total moe`) -> b
  
  bind_cols(a,b) -> c
  
  saveRDS(c,data_path)
  
}

move_vl("./data/Gross Rent.rds")

# # Convert GEOID to character
# 
# as.character(`Detailed Race`$GEOID)
# `Detailed Race` %>% mutate_at("GEOID",as.character())


colnames(median_age_trans_dt)

grep(' --!!Total estimate',colnames(median_age_trans_dt),value = TRUE)

grepl(' --!!Total estimate',colnames(median_age_trans_dt))

labs <- as.data.frame(colnames(median_age_trans_dt))
colnames(labs) <- 'label'

labs %>%
  mutate(label_mod = ifelse(grepl(' --!!Total estimate',label)|grepl(' --!!Total moe',label),gsub(" --!!"," ",label),gsub(" --!!Total","",label)))



# Centroid

geom %>%
  filter(Vintage == 2016 & NAME == 'Davidson County, Tennessee') %>% 
  st_centroid()
  

row.names(places)

places$NAME[row.names(places) == 1]

# Find Min where h[5] is not null

subset(h, (!is.na(h[,5])))

j <- subset(h, (!is.na(h[,5])))

min(j$Vintage)
