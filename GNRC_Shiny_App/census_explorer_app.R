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

library(DT)
library(htmltools)
library(leaflet)
library(magrittr)
library(rgdal)
library(shiny)
library(sf)
library(tidyverse)


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
               information from the US Census.",
               p(),
               "Begin by selecting a variable."),
      
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
      
      # uiOutput("slider"),
      
      
      #sliderInput("yr", "Vintage:", min = 2011, max = 2017, value = c(2014, 2015), sep = '', dragRange = TRUE)
      
      ),
    
    mainPanel(
      leafletOutput("mymap"),
      
      uiOutput("slider")
      
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
  
  uiOutput("dt")
  
  #textOutput("test")

)


# SERVER -----------------------------------------------------------

server <-  function(input, output, session){
  
  # Dynamic Input Widgets
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
        label = h3("Map Attribute:"),
        c("Please select an option below" = "", attr[!attr %in% c("NAME","geometry")])
      )
    }
  })
  
  output$slider <- renderUI({
    
    if (is.null(input$select_attr)){return(invisible())}
    
    else if (input$select_attr == ''){return(invisible())}
    
    else if(input$select_attr != '') {
      
      sliderInput("yr", "Vintage:", min = 2011, max = 2016, value = 2016, sep = '')
      
      }
  })
  
  # FIX ME ---------
  labels <- county_di_2016$NAME
  
  c <- paste("<strong>", labels, "</strong>", sep ='')
  # ----------------
  
  # Map ------
  
  output$mymap <- renderLeaflet({
    if (is.null(input$select_attr)) {
      leaflet() %>%
        setView(lat = 36.174465,
                lng = -86.767960,
                zoom = 8) %>%
        addProviderTiles(providers$CartoDB.Positron,
                         options = providerTileOptions(noWrap = TRUE, zIndex = 1))
    } else if (input$select_attr == '') {
      leaflet() %>%
        setView(lat = 36.174465,
                lng = -86.767960,
                zoom = 8) %>%
        addProviderTiles(providers$CartoDB.Positron,
                         options = providerTileOptions(noWrap = TRUE, zIndex = 1))
    }
    else
      
    {
      sel_col <- input$select_attr
      
      county_di_2016 %>%
        st_set_geometry(NULL) %>% 
        pull("NAME") -> label
      
      county_di_2016 %>%
        st_set_geometry(NULL) %>% 
        ungroup() %>%
        pull(input$select_attr) %>%
        round(digits = 3) -> map_var
      
      c <- paste("<strong>", labels, "</strong><br>",map_var, sep = '')
      
      leaflet(county_di_2016) %>%
        addProviderTiles(providers$CartoDB.PositronNoLabels,
                         options = providerTileOptions(noWrap = TRUE, zIndex = 1)) %>%
        addPolygons(
          group = "polygons",
          fillColor = ~ colorQuantile("YlOrRd", map_var)(map_var),
          fillOpacity = 0.5,
          weight = 2,
          stroke = T,
          color = "grey",
          opacity = 1,
          dashArray = "3",
          highlight = highlightOptions(
            color = "white",
            weight = 3,
            bringToFront = TRUE
          ),
          options = list(zIndex = 2),
          label = lapply(c, HTML)
        ) %>%
        addProviderTiles(
          "CartoDB.PositronOnlyLabels",
          group = "labels",
          options = providerTileOptions(zIndex = 3, pane = 'markerPane')
        ) %>%
        addLayersControl(overlayGroups = c("polygons", "labels"))
    }
  })
  
  output$data_table <-
    DT::renderDataTable({
      county_di_2016
    }, options = list(columnDefs = list(list(
      targets = ncol(county_di_2016),
      render = JS(
        "function(data, type, row, meta) {",
        "return type === 'display' && data.toString().length > 6 ?",
        "'<span title=\"' + data + '\">' + data.toString().substr(0, 6) + '...</span>' : data;",
        "}"
      )
    ))))
    
  output$dt <- renderUI({
    
    if(is.null(input$select_var)){}
    
    else if (input$select_var == ''){}
    
    else {
      
      fluidRow(column(12), DT::dataTableOutput("data_table"))
      
    }
    
  })
  
# Test / Trouble shooting  
  # output$test <- renderPrint({
  #   county_di_2016 %>%
  #     st_set_geometry(NULL) %>%
  #     ungroup() %>%
  #     select(input$select_attr) -> map_var
  #   
  #   map_var
  # })
  
}

shinyApp(ui, server)

