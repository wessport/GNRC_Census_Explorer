###############################################################    
#                                                             #
#             GREATER NASHVILLE CENSUS EXPLORER               #
#                  Developed by Wes Porter                    #
#                       20 APR 2018                           #
#                                                             #
#          A Shiny web application for visualizing            #
#              American Community Survey data                 #
#                                                             #
# Find out more about building applications with Shiny here:  #
#                                                             #
#                http://shiny.rstudio.com/                    #
#                                                             #
#               Last Updated: 03-JULY-2018                    #
#                                                             #
###############################################################

library(DT)
library(htmltools)
library(leaflet)
library(magrittr)
library(plotly)
library(rgdal)
library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(shinyjs)
library(sf)
library(tidyverse)
library(viridis)

# library(ggplot2)
# library(multiscales)
# library(colorspace)

#Global -----

# Import data
places <- readRDS("./data/places.rds")
geom <- readRDS("./data/geometry.rds")
total_pop <- readRDS("./data/Total Population.rds")
colnames(total_pop)[5] <- 'total_est'

# # Display tab content in dashboardBody()with menuItem
# convertMenuItem <- function(mi,tabName) {
#   mi$children[[1]]$attribs['data-toggle']="tab"
#   mi$children[[1]]$attribs['data-value'] = tabName
#   mi
# }

# UI ----------------------------------------------------------------

ui <- dashboardPage(
  
  # UI Header ----
  dashboardHeader(title = "GNRC Census Explorer",
                  titleWidth = 350),
  
  # UI Sidebar ----
  dashboardSidebar(
    width = 350,
    sidebarMenu(
      
      # style = "position: fixed; overflow: visible; overflow-y: scroll",
      
      menuItem("Map & Plot",tabName = "map", icon = icon("map")),
      selectInput(
        "select_category",
        label = h4("Topic:"),
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
      
      uiOutput("GeoSelection")
      
    )
    
    
  ),
  
  # UI Body ----
  dashboardBody(
    useShinyjs(),
    
    tags$head( 
      tags$style(HTML(".main-sidebar { font-size: 14px; }")) #change the font size to 14
    ),
    
    tags$head(
      tags$style(HTML("li { font-size: 24px; }")) #change the font size to 20
    ),
    
    tabItems(
      
      tabItem(tabName = "map",
              fluidRow(box(      
                fluidRow(column(12,
                                absolutePanel(
                                  left = 10, bottom = -20,
                                  width = 330, height = "auto",draggable = FALSE,
                                  
                                  uiOutput("slider"),
                                  
                                  style = "opacity: 1.0; z-index: 1000;" ## z-index modification
                                ),              
                                leafletOutput("mymap", height = 500) %>%
                                  withSpinner(type = getOption("spinner.type", default = 5)))),
                
                verbatimTextOutput("test"),
                status = 'primary',width=12))
   
      )
    )
  )
)


# SERVER -----------------------------------------------------------

server <-  function(input, output, session){
  
  # Render UI elements -----
  
  output$secondSelection <- renderUI({
    
    req(input$select_category)
    
    if (input$select_category == 'Pop') {
      
      selectInput(
        "select_var",
        label = h4("Category:"),
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
        label = h4("Category:"),
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
          'Means of transportation to work white alone',
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
        label = h4("Category:"),
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
        label = h4("Category:"),
        c("Please select an option below" = "", 
          'Health insurance coverage status by sex by age',
          'Medicare coverage by sex by age',
          'Sex by age by disability status')
      )
    }
    
    else if (input$select_category == 'Emp') {
      selectInput(
        "select_var",
        label = h4("Category:"),
        c(
          "Please select an option below" = "",
          'Employment status for the population 16 years and over',
          'Mean usual hours worked in the past 12 months for workers 16 to 64 years',
          'Median age by sex for workers 16 to 64 years',
          'Poverty status in the past 12 months by age',
          'Ratio of income to poverty level of families in the past 12 months',
          'Sex by age by employment status for the population 16 years and over',
          'Sex by class of worker for the civilian employed population 16 years and over'
        )
      )
    }
    
  })
  
  # Load data by user selection ----
  
  selected_data <-  reactive({
    
    req(input$select_var)

    # Read in requested tabular census data
    tabular_data <- readRDS(paste("./data/",input$select_var,".rds",sep=""))

    # Join tabular data with spatial geometry
    tabular_data %>%
      left_join(geom, by = c("NAME" = "NAME", "Vintage"="Vintage")) -> t

    # Define Coordinate Reference System: EPSG: 4326
    # Transfrom joined object into a simple feature spatial object
    st_transform(st_as_sf(t), 4326, use_gdal = T)

  })
  
  # Reset input$select_attr when user changes data topic
  
  observeEvent(input$select_category, priority = 1,{
    
    updateSelectInput(session, "select_attr", selected = '')
    
  })
  
  # Reset input$select_attr when user changes data category
  
  observeEvent(selected_data(), {
    reset_sel_attr()
  }, priority = 10)
  
  # Reset input$select_attr utilizing freezeReactiveValue
  # Based on solution by Joe Cheng [RStudio]
  # https://groups.google.com/forum/#!msg/shiny-discuss/gkeuyPAZndM/QYvgI0WbFAAJ
  reset_sel_attr <- function() {
    updateSelectInput(session, "select_attr", selected = '')
    freezeReactiveValue(input, "select_attr")
  }

  
  # Render Select Variable
  
  output$thirdSelection <- renderUI({ 
    
    req(selected_data())
    
    attr <- colnames(selected_data())
    
    selectInput(
      "select_attr",
      label = h4("Variable:"),
      c("Please select an option below" = "", attr[!attr %in% c("GEOID","NAME","Vintage","Level","geometry")])
    )
  })
  
  # Geographic Boundary
  
  output$GeoSelection <- renderUI({
    
    req(input$select_attr)
    
    selectInput(
      "select_geo",
      label = h4("Geographic Division:"),
      c("Please select an option below" = "", "Automatic by Zoom Level","County","Tract","Block Group")
    )
    
  })
  
  # Map Vintage slider
  output$slider <- renderUI({
    
    req(input$select_attr)
    
    sliderInput("yr", "Map Vintage:", min = 2011, max = 2016, value = 2016, sep = '')
  })
  
  
  
  
  # Map - Zoom Level ------
  values <- reactiveValues(zoom_level = 8,geo_level = 'county')
  
  observeEvent(c(input$mymap_zoom,input$select_geo),{
    
    z_level <- input$mymap_zoom
    
    if(is.null(input$select_geo)){
      
      if(z_level <= 9 & values$geo_level != 'county') {values$geo_level <- 'county'} 
      
      else if(z_level > 9 & z_level < 13 & values$geo_level != 'tract')  {values$geo_level <- 'tract'}
      
      else if(z_level >= 13 & values$geo_level != 'block group') {values$geo_level <- 'block group'}
    }
    
    else if(input$select_geo == "" | input$select_geo == "Automatic by Zoom Level"){
      
      if(z_level <= 9 & values$geo_level != 'county') {values$geo_level <- 'county'} 
      
      else if(z_level > 9 & z_level < 13 & values$geo_level != 'tract')  {values$geo_level <- 'tract'}
      
      else if(z_level >= 13 & values$geo_level != 'block group') {values$geo_level <- 'block group'}
    } 
    
    else if(input$select_geo == "County"){values$geo_level <- 'county'}
    
    else if(input$select_geo == "Tract"){values$geo_level <- 'tract'}
    
    else if(input$select_geo == "Block Group"){values$geo_level <- 'block group'}
  })
  
  # Map - Filter Data -----

  filtered_data <- reactive({
    
    req(selected_data())
    
    if(is.null(input$select_attr)){}
    
    else if(input$select_attr == ''){}
    
    else{

      if (is.null(input$yr)) {
        y <- 2016
      }
      else {
        y <- input$yr
      }
  
      if(is.null(values$geo_level)){
  
        selected_data() %>%
          filter(Vintage == y & Level == 'county')
  
      } else {
  
        selected_data() %>%
          filter(Vintage == y & Level == values$geo_level)
      }
      }
  })

  # # Map - Render Leaflet Tiles -----

  output$mymap <- renderLeaflet({

    leaflet() %>%
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
      ) %>%
      hideGroup('Places')
  })

  # Total Pop

  tp <- reactive({

    if(is.null(values$geo_level)){

      total_pop %>%
        filter(Level == 'county')

    } else {

      total_pop %>%
        filter(Level == values$geo_level)
    }

  })

  f_data <- reactive({
    
    req(selected_data())

    if(is.null(values$geo_level)){

      selected_data() %>%
        filter(Level == 'county')

    } else {

      selected_data() %>%
        filter(Level == values$geo_level)
    }
  })

  perc_pop <- reactive({
    
    req(input$select_attr)

    f_data() %>%
      st_set_geometry(NULL) %>%
      ungroup() %>%
      mutate(pp = (get(input$select_attr))/(tp()$total_est)*100)

  })

  pp_f <- reactive({
    
    req(perc_pop)

    if (is.null(input$yr)) {
      y <- 2016
    }
    else {
      y <- input$yr
    }

    perc_pop()%>%
      filter(Vintage == y & Level == values$geo_level)%>%
      pull(pp)

  })

  min_pp <- reactive({
    
    req(perc_pop())

    perc_pop()%>%
      subset(!is.na(perc_pop()$pp))%>%
      summarise(minpp = min(pp))%>%
      pull(minpp)
  })

  max_pp <- reactive({

    req(perc_pop())
    
    perc_pop()%>%
      subset(!is.na(perc_pop()$pp))%>%
      summarise(maxpp = max(pp))%>%
      pull(maxpp)
  })

  bounds <- reactive({
    
    req(max_pp())

    c(min_pp(),max_pp())

  })

  # Map - Fill Color -----
  fc <- reactive({
    
    req(bounds())

    colorBin("YlOrRd", bounds())(pp_f())

  })


  # Map - Labels -----
  label_txt <- reactive({
    
    req(filtered_data())

    if(is.null(input$select_attr)){

      paste("<strong>", 'Attr NULL', "</strong>", sep = '')

    } else if (input$select_attr ==''){

      paste("<strong>", 'Attr default', "</strong>", sep = '')

    } else {

      filtered_data() %>%
        st_set_geometry(NULL) %>%
        pull("NAME") -> labels

      paste("<strong>",labels,"</strong><br>",input$select_attr,": ",round(pp_f(),2),'%',sep = '')

    }

  })

  places_label_txt <- reactive({

    places %>%
      st_set_geometry(NULL) %>%
      pull("NAME") -> plabels

    paste("<strong>", plabels, "</strong>", sep = '')

  })

  observe(

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
      addLayersControl(
        baseGroups = c('Carto DB', 'OSM','Esri World Imagery'),
        overlayGroups = c('Places','Boundary'),
        options = layersControlOptions(collapsed = TRUE)
      ),
    
    priority = -1000

  )

  # observe({
  # 
  #   leafletProxy("mymap", data = places) %>%
  #     clearGroup('Places') %>%
  #     addPolygons(
  #       data = places,
  #       fillOpacity = 0,
  #       weight = 2,
  #       #stroke = T,
  #       dashArray = '2',
  #       color = "purple",
  #       opacity = 0.5,
  #       label = lapply(places_label_txt(),HTML),
  #       group = 'Places'
  #     )
  # })

  output$test <- renderPrint({

    input$select_attr

  })
  
  
}

shinyApp(ui, server)

