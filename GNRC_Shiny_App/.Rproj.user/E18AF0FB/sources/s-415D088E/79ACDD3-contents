# GREATER NASHVILLE CENSUS EXPLORER
#
# A Shiny web application for visualizing American
# Community Census Data
#
# Developed by WES PORTER
#

library(shiny)
library(leaflet)
library(rgdal)
library(magrittr)

path = "C:/Users/wPorter/Data/Census/census_shapefiles/GNR/counties"
gnr_2017 <- readOGR(path, layer = 'cb_2017_gnr_county')

r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

# UI ----------------------------------------------------------------

ui <- fluidPage(
  titlePanel("GNRC Census Explorer"),
  
  sidebarLayout(
    sidebarPanel("Hello im a sidebar"),
    mainPanel(leafletOutput("mymap"),
              p()
                )
              )
)


# SERVER -----------------------------------------------------------

server <-  function(input, output, session){
  
  points <- eventReactive(input$recalc, {
    cbind(rnorm(40) * 2 + 13, rnorm(40) + 48)
  }, ignoreNULL = FALSE)
  
  output$mymap <- renderLeaflet({
    leaflet(gnr_2017) %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap =TRUE)
                       ) %>%
      addPolygons(fillColor = ~colorQuantile("YlOrRd", ALAND)(ALAND), 
                  fillOpacity = 0.5, 
                  weight = 2, 
                  stroke = T, 
                  color = "grey", 
                  opacity = 1,
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                        bringToFront = TRUE))
  })
}

shinyApp(ui, server)