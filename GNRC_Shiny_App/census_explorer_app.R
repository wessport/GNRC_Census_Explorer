###############################################################    
#                                                             #
#             GREATER NASHVILLE CENSUS EXPLORER               #
#                  Developed by Wes Porter                    #
#                       20 APR 2018                           #
#                                                             #
#          A Shiny web application for visualizing            #
#              American Community Census data                 #
#                                                             #
# Find out more about building applications with Shiny here:  #
#                                                             #
#                http://shiny.rstudio.com/                    #
#                                                             #
#               Last Updated: 14-MAY-2018                     #
#                                                             #
###############################################################

library(shiny)
library(leaflet)
library(rgdal)
library(magrittr)
library(sf)
library(tidyverse)
library(htmltools)

# Import data
load("./data/county_di.RData")
load("./data/tract_di.RData")
load("./data/bg_di.RData")

# UI ----------------------------------------------------------------

ui <- fluidPage(
  titlePanel(h1("GNRC Census Explorer")), 
  
  sidebarLayout(
    
    sidebarPanel(
      
      helpText("Create demographic maps with 
               information from the US Census."),
      
      selectInput(
        "select_category",
        label = h3("Category:"),
        choices = c(
          "Please select an option below" = "",
          "Population" = "Pop",
          "Transportation" = "Tran",
          "Housing" = "Housing",
          "Health" = "Health",
          "Employment" = "Emp"
        )
      ),
      
      uiOutput("secondSelection"),
      
      uiOutput("thirdSelection")
      
      
      # sliderInput("yr", "Vintage:", min = 2011, max = 2017, value = c(2014, 2015), sep = '', dragRange = TRUE)
      
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
  
  output$secondSelection <- renderUI({
    
    if (input$select_category == ''){}
    
    else if (input$select_category == 'Pop') {
      selectInput("select_var", label = h3("Variable:"), c("Please select an option below" = "", 'Diversity Indices', 'b', 'c'))
    }
    
    else if (input$select_category == 'Tran') {
      selectInput("select_var", label = h3("Variable:"), c("Please select an option below" = "", 'd', 'e', 'f'))
    }

    else if (input$select_category == 'Housing') {
      selectInput("select_var", label = h3("Variable:"), c("Please select an option below" = "", 'g', 'h', 'i'))
    }

    else if (input$select_category == 'Health') {
      selectInput("select_var", label = h3("Variable:"), c("Please select an option below" = "", 'j', 'k', 'l'))
    }

    else if (input$select_category == 'Emp') {
      selectInput("select_var", label = h3("Variable:"), c("Please select an option below" = "", 'm', 'n', 'o'))
    }
    
  })
  
  output$thirdSelection <- renderUI({
    
    if (input$select_category == ''){}
    
    else if (input$select_var == 'Diversity Indices') {
      
      attr <- colnames(county_di_2016)
      attr[!attr %in% c("NAME","geometry")] 
      
      selectInput(
        "select_attr",
        label = h3("Attribute:"),
        c("Please select an option below" = "", attr[!attr %in% c("NAME","geometry")])
      )
    }
  })
  
  
  labels <- county_di_2016$NAME
  
  c <- paste("<strong>", labels, "</strong>", sep ='')
  
  output$mymap <- renderLeaflet({
    
    map_var <- county_di_2016
    
    labels <- county_di_2016$NAME
    
    c <- paste("<strong>", labels, "</strong>", sep ='')
    
    leaflet(county_di_2016) %>%
      addProviderTiles(providers$CartoDB.PositronNoLabels,
                       options = providerTileOptions(noWrap =TRUE, zIndex = 1)
      ) %>%
      addPolygons(group = "polygons",
                  fillColor = ~colorQuantile("YlOrRd", county_di_2016$Shellys_DI)(county_di_2016$Shellys_DI), 
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

