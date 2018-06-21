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
#               Last Updated: 18-JUNE-2018                    #
#                                                             #
###############################################################

library(DT)
library(htmltools)
library(leaflet)
library(magrittr)
library(plotly)
library(rgdal)
library(shiny)
library(sf)
library(tidyverse)
library(viridis)


# Import data
# load("./data/county_di.RData")
# load("./data/tract_di.RData")
# load("./data/bg_di.RData")
load("./data/di.RData")

places <- readRDS("./data/places.rds")
geom <- readRDS("./data/geometry.rds")

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
        c("Please select an option below" = "",
          "Population" = "Pop",
          "Transportation" = "Tran",
          "Housing" = "Housing",
          "Health" = "Health",
          "Employment" = "Emp"
        )
      ),
      
      uiOutput("secondSelection"),
      
      uiOutput("thirdSelection"),
      
      uiOutput("GeoSelection"),
      
      uiOutput("slider")
      
      
      ),
    
    mainPanel(
      leafletOutput("mymap", height = 675)

    )
  ),
  
  
  hr(),
  
  fluidRow(
    
    column(3,
           uiOutput("plot_filter")
           
    ),
    
    column(8, offset = 1,
           h4(),
           plotlyOutput("plot1")
    ),
    
    plotOutput("myplot")
  ),
  
  hr(),
  
  uiOutput("dt"),
  
  uiOutput("download_dt")
  
  # textOutput("test")

)


# SERVER -----------------------------------------------------------

server <-  function(input, output, session){
  
  # Select box widget for user variable selection
  
  output$secondSelection <- renderUI({
    
    req(input$select_category)
    
    if(input$select_category == ""){}
    
    else if (input$select_category == 'Pop') {
      selectInput(
        "select_var",
        label = h3("Variable:"),
        c(
          "Please select an option below" = "",
          'Detailed Race',
          'Diversity Indices',
          'Population under 18 years by age',
          'Race',
          'School enrollment by level of school for the population 3 years and over',
          'Sex by age',
          'Sex by age by educational attainment for the population 18 years and over',
          'Total Population',
          'Women 15 to 50 years who had a birth in the past 12 mo by marital status and age'
        )
      )
    }

    else if (input$select_category == 'Tran') {
      selectInput(
        "select_var",
        label = h3("Variable:"),
        c(
          "Please select an option below" = "",
          'Means of transportation to work',
          'Means of transportation to work by age',
          'Means of transportation to work by class of worker',
          'Means of transportation to work by poverty status in the past 12 mo',
          'Means of transportation to work american indian and alaska native alone',
          'Means of transportation to work asian alone',
          'Means of transportation to work black or african american alone',
          'Means of transportation to work hispanic or latino',
          'Means of transportation to work native hawaiian and other pacific islander alone',
          #'Means of transportation to work white alone',
          'Means of transportation to work white alone not hispanic or latino',
          'Means of transportation to work some other race alone',
          'Means of transportation to work two or more races',
          'Median age by means of transportation to work',
          'Sex of workers by means of transportation to work',
          'Sex of workers by travel time to work',
          'Sex of workers by vehicles available',
          'Time leaving home to go to work',
          'Travel time to work'
        )
      )
    }

    else if (input$select_category == 'Housing') {
      selectInput(
        "select_var",
        label = h3("Variable:"),
        c(
          "Please select an option below" = "",
          'Contract Rent',
          'Gross Rent',
          'House heating fuel',
          'Household income in the past 12 months',
          'Household type including living alone',
          'Household type for children under 18 years in households',
          'Household type including living alone american indian and alaska native alone',
          'Household type including living alone asian alone',
          'Household type including living alone black or african american alone',
          'Household type including living alone hispanic or latino',
          'Household type including living alone native hawaiian and other pacific islander alone',
          'Household type including living alone some other race alone',
          'Household type including living alone white alone',
          'Household type including living alone white alone nothispanic or latino',
          'Household income in the past 12 months',
          'Housing Units',
          'Median monthly housing costs dollars',
          'Median value dollars',
          'Monthly housing costs',
          'Mortage status and selected monthly owner costs',
          'Occupancy status',
          'Vacancy status',
          'Value',
          'Year structure built'
        )
      )
    }
    
    else if (input$select_category == 'Health') {
      selectInput(
        "select_var",
        label = h3("Variable:"),
        c("Please select an option below" = "", 'Sex by age by disability status')
      )
    }
    
    else if (input$select_category == 'Emp') {
      selectInput(
        "select_var",
        label = h3("Variable:"),
        c(
          "Please select an option below" = "",
          'Employment status for the population 16 years and over',
          'Mean usual hours worked in the past 12 months for workers 16 to 64 years',
          'Median age by sex for workers 16 to 64 years',
          'Ratio of income to poverty level of families in the past 12 months',
          'Sex by age by employment status for the population 16 years and over',
          'Sex by class of worker for the civilian employed population 16 years and over'
        )
      )
    }
    
  })
  
  # Load data by user selection
  
  selected_data <-  reactive({
    
    if(is.null(input$select_var)){readRDS("./data/default_data.rds")}
    
    else if (input$select_var == ""){readRDS("./data/default_data.rds")}
    
    else {
      
      tabular_data <- readRDS(paste("./data/",input$select_var,".rds",sep=""))
      
      tabular_data %>% 
          left_join(geom, by = c("NAME" = "NAME", "Vintage"="Vintage")) -> t

      st_as_sf(t)
      }
    
  })
  
  
  output$thirdSelection <- renderUI({ 
    
    req(input$select_var)
    
    if (input$select_var == ''){}
      
    else {
    
      attr <- colnames(selected_data())
      # attr[!attr %in% c("GEOID","NAME","Vintage","Level","geometry")] 
      
      selectInput(
        "select_attr",
        label = h3("Map Attribute:"),
        c("Please select an option below" = "", attr[!attr %in% c("GEOID","NAME","Vintage","Level","geometry")])
      )
    }
  })
  
  # User switches data category
  
  observeEvent(input$select_category, updateSelectInput(session,input='select_var',selected=''))
  observeEvent(input$select_category, updateSelectInput(session,input='select_attr',selected=''))

  # User switches data variable
  
  eventReactive(input$select_var, updateSelectInput(session,input='select_attr',selected=''))

  
  # VINTAGE SLIDER
  output$slider <- renderUI({

    req(input$select_attr)

    sliderInput("yr", "Map Vintage:", min = 2011, max = 2016, value = 2016, sep = '', animate =
                    animationOptions(interval = 1200, loop = TRUE))
    })

  output$GeoSelection <- renderUI({

    req(input$select_attr)

    if (input$select_attr == ''){}

    else {

      selectInput(
        "select_geo",
        label = h3("Geographic Division:"),
        c("Please select an option below" = "", "Automatic by Zoom Level","County","Tract","Block Group")
      )
    }
  })
  
  
  # Map ------
  
  geo_level <- reactive({

    req(input$mymap_zoom)

    if(is.null(input$select_geo)){

      if (input$mymap_zoom <= 9) {
        'county'
      }
      else if (input$mymap_zoom > 9 & input$mymap_zoom < 13){
        'tract'
      }
      else if (input$mymap_zoom >= 13){
        'block group'
      }
    }

    else if (input$select_geo == "" | input$select_geo == "Automatic by Zoom Level"){

      if (input$mymap_zoom <= 9) {
      'county'
      }
      else if (input$mymap_zoom > 9 & input$mymap_zoom < 13){
      'tract'
      }
      else if (input$mymap_zoom >= 13){
      'block group'
      }
    }

    else if(input$select_geo == "County"){'county'}

    else if(input$select_geo == "Tract"){'tract'}

    else if(input$select_geo == "Block Group"){'block group'}
  })
  # 
  # # Filter User Selected Data for Map 
  # 
  # filtered_data <- reactive({
  # 
  #   req(input$mymap_zoom)
  # 
  #   if (is.null(input$yr)) {
  #     y <- 2016
  #   }
  #   else {
  #     y <- input$yr
  #   }
  #   
  #   selected_data() %>%
  #     filter(Vintage == y & Level == geo_level())
  # 
  # })
  
  filtered_data <- reactive({

    req(input$mymap_zoom)

    if (is.null(input$yr)){y <- 2016}
    else {y <- input$yr}

    if(is.null(input$select_geo)){

      if (input$mymap_zoom <= 9) {
        geolevel <- 'county'
      }
      else if (input$mymap_zoom > 9 & input$mymap_zoom < 13){
        geolevel <- 'tract'
      }
      else if (input$mymap_zoom >= 13){
        geolevel <- 'block group'
      }
    }

    else if (input$select_geo == "" | input$select_geo == "Automatic by Zoom Level"){

      if (input$mymap_zoom <= 9) {
      geolevel <- 'county'
      }
      else if (input$mymap_zoom > 9 & input$mymap_zoom < 13){
        geolevel <- 'tract'
      }
      else if (input$mymap_zoom >= 13){
        geolevel <- 'block group'
      }
    }

    else if(input$select_geo == "County"){geolevel <- 'county'}

    else if(input$select_geo == "Tract"){geolevel <- 'tract'}

    else if(input$select_geo == "Block Group"){geolevel <- 'block group'}

    selected_data() %>%
      filter(Vintage == y & Level == geolevel)

  })
  
  output$mymap <- renderLeaflet({

    leaflet(di) %>%
      addTiles(group = "OSM") %>%
      addProviderTiles(providers$CartoDB.PositronNoLabels, options = providerTileOptions(noWrap = TRUE, zIndex = 1), group = 'Carto DB') %>%
      addProviderTiles(providers$Esri.WorldImagery, group = 'Esri World Imagery') %>%
      addProviderTiles(
        "CartoDB.PositronOnlyLabels",
        group = "Esri World Imagery",
        options = providerTileOptions(zIndex = 3, pane = 'markerPane')
      ) %>% 
      hideGroup('Esri World Imagery') %>%
      setView(lat = 36.174465,lng = -86.767960,zoom = 8) %>%
      addProviderTiles(
        "CartoDB.PositronOnlyLabels",
        group = "Carto DB",
        options = providerTileOptions(zIndex = 3, pane = 'markerPane')
      )
  })
  
  # Fill Color Expression
  fc <- reactive({
    req(input$mymap_zoom)
    
    if(is.null(input$select_attr)){
      
      fc <- 'grey'
    }
    
    else if(input$select_attr==''){
      
      'grey'
      
    } else if (sum(is.na(filtered_data()[[input$select_attr]]))>0){
      
      # fc <- 'grey'
      'grey'
      
    } else {
      
      tryCatch(colorQuantile("YlOrRd", filtered_data()[[input$select_attr]])(filtered_data()[[input$select_attr]]),
               error=function(e) colorBin("YlOrRd", filtered_data()[[input$select_attr]])(filtered_data()[[input$select_attr]]))
    }
   
  })
  
  # Labels
  label_txt <- reactive({
    req(input$mymap_zoom)
    
    if(is.null(input$select_attr)){
      
      filtered_data() %>%
        st_set_geometry(NULL) %>%
        pull("NAME") -> labels
      
      paste("<strong>", labels, "</strong>", sep = '')
    }
    
    else if(input$select_attr==''){
      
      filtered_data() %>%
        st_set_geometry(NULL) %>%
        pull("NAME") -> labels
      
      paste("<strong>", labels, "</strong>", sep = '')
      
    } else if (sum(is.na(filtered_data()[[input$select_attr]]))>0){
      
      filtered_data() %>%
        st_set_geometry(NULL) %>%
        pull("NAME") -> labels
      
      paste("<strong>", labels, "</strong>", sep = '')
      
    } else {
      
      filtered_data() %>%
        st_set_geometry(NULL) %>%
        pull("NAME") -> labels
      
      filtered_data() %>%
        st_set_geometry(NULL) %>%
        ungroup() %>%
        pull(input$select_attr) %>%
        round(digits = 3) -> map_var
      
      paste("<strong>",labels,"</strong><br>",input$select_attr,": ",map_var,sep = '')
    }
  })
  
  places_label_txt <- reactive({
    req(input$mymap_zoom)
    
    places %>%
      st_set_geometry(NULL) %>%
      pull("NAME") -> plabels
    
    paste("<strong>", plabels, "</strong>", sep = '')
    
  })
  
  observe({

    req(input$mymap_zoom,fc())

    leafletProxy("mymap", data = filtered_data()) %>%
      clearShapes() %>%
      addPolygons(
        fillColor = fc(),
        fillOpacity = 0.5,
        weight = 2,
        stroke = T,
        color = "grey",
        opacity = 1,
        highlight = highlightOptions(color = "white",weight = 3,bringToFront = TRUE),
        options = list(zIndex = 2),
        label = lapply(label_txt(), HTML),
        group = 'Boundary'
      ) %>%
      addPolygons(
        data = places,
        fillOpacity = 0,
        weight = 2,
        #stroke = T,
        dashArray = '2',
        color = "purple",
        opacity = 0.5,
        label = lapply(places_label_txt(),HTML),
        group = 'Places'
      ) %>%
      
      addLayersControl(
        baseGroups = c('Carto DB', 'OSM','Esri World Imagery'),
        overlayGroups = c('Places','Boundary'),
        options = layersControlOptions(collapsed = TRUE)
        )      

  })
  
  # Data Table -----

  # output$data_table = DT::renderDataTable( selected_data(), options = list(lengthChange = FALSE),server=TRUE )
  
  table_data <- reactive({
    req(selected_data)
    
    selected_data() %>% 
      st_set_geometry(NULL)
    
  })
  
  output$data_table = renderDT(
    table_data(),
    extensions = 'FixedHeader',
    options = list(scrollX = TRUE, fixedHeader = TRUE)
  )
  
  output$dt <- renderUI({

    req(input$select_var)

    if(is.null(input$select_var)){}

    else if (input$select_var == ''){}

    else {

      fluidRow(column(12), DTOutput("data_table"))

    }

  })

  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$select_var, ".csv", sep = "")
    },
    content = function(file) {
      write.csv((selected_data()%>%st_set_geometry(NULL)), file, row.names = FALSE)
    }
  )

  output$download_dt <- renderUI({

    req(input$select_var)

    fluidRow(

      column(10),downloadButton("downloadData", "Download Spreadsheet"))
  })


  # Plot1 -----
  
  plot_data <- reactive({
    
    req(plot_divisions())
    
    if(input$selected_pfilter ==''){
    
    table_data() %>%
      filter(Level == geo_level()) %>%
      group_by(NAME)
      
    } else if(input$selected_pfilter =='All'){
      
    table_data() %>%
      filter(Level == geo_level()) %>%
      group_by(NAME)
  
    } else {
      
      table_data() %>%
        filter(Level == geo_level() & NAME == input$selected_pfilter) %>%
        group_by(NAME)
      
    }
    
    
  })

  output$plot1 <- renderPlotly({

    req(input$select_attr)
    
    y <- list(
      title = "Count")
    
    plot_data() %>%
      plot_ly(
        x = ~ Vintage,
        y = ~ get(input$select_attr),
        color = ~ NAME,
        colors = viridis_pal(option = "D")(3),
        hovertext = ~ NAME,
        type = 'scatter',
        mode = 'lines+markers'
      ) %>%
      layout(yaxis = y)

  })
  
  plot_divisions <- reactive({
    req(input$select_attr)
    
    table_data() %>%
      filter(Level == geo_level()) %>%
      select(NAME) %>%
      distinct()

  })
  
  output$plot_filter <- renderUI({ 
    req(input$select_attr)
    
    selectInput(
      "selected_pfilter",
      label = h3("Filter Data Points by Division:"),
      c("Please select an option below" = "", "All",plot_divisions())
    )
    
  })

# Test / Trouble shooting
# output$test <- renderPrint({
# 
#   selected_data()
# 
# })

}

shinyApp(ui, server)

