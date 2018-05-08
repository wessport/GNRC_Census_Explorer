# GREATER NASHVILLE CENSUS EXPLORER
#
# A Shiny web application for visualizing American
# Community Census Data
#
# Developed by WES PORTER
# 20 APR 2018
#

library(shiny)
library(leaflet)
library(rgdal)
library(magrittr)
library(sf)
library(tidyverse)
library(htmltools)

path = "C:/Users/wPorter/Data/Census/census_shapefiles/boundaries/GNR/counties"
gnr_2017 <- readOGR(path, layer = 'cb_2017_gnr_county')

test <-  st_read(path, layer = 'cb_2017_gnr_county')


# UI ----------------------------------------------------------------

ui <- fluidPage(
  titlePanel(h1("GNRC Census Explorer")), 
  
  sidebarLayout(
    
    sidebarPanel(
      
      helpText("Create demographic maps with 
               information from the US Census."),
      
      sliderInput("yr", "Vintage:", min = 2011, max = 2017, value = c(2014, 2015), sep = '', dragRange = TRUE)
      ),
    
    mainPanel(
      leafletOutput("mymap")
    )
  ),
  
  
  hr(),
  
  fluidRow(
    
    column(3,
           h4("Plot/Graph 1")
    ),
    
    column(4, offset = 1,
           h4("Plot/Graph 2")
    ),
    
    column(4,
           h4("Plot/Graph 3")
    ),
    plotOutput("myplot")
  ),
  
  hr(),
  
  fluidRow(
    column(2,
           "Table Sidebar"
    ),
    column(10,
           h4("Table"))
  )
)


# SERVER -----------------------------------------------------------

server <-  function(input, output, session){
  
  labels <- test$NAME
  
  c <- paste("<strong>", labels, "</strong>", sep ='')
  
  output$mymap <- renderLeaflet({
    
    map_var <- census_format[[3]]
    
    labels <- census_format$NAME
    
    c <- paste("<strong>", labels, "</strong>", sep ='')
    
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
                  options=list(zIndex = 2),
                  label = lapply(c,HTML)
      ) %>%
      addProviderTiles("CartoDB.PositronOnlyLabels", group="labels", 
                       options=providerTileOptions(zIndex = 3, pane = 'markerPane')) %>% 
      addLayersControl(overlayGroups = c("polygons", "labels"))
  })
  
  # output$myplot <- barplot(test$ALAND, main="Area Distribution", 
  #                          names.arg=test$NAME)
}

shinyApp(ui, server)

