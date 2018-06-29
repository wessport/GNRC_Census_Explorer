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
#               Last Updated: 28-JUNE-2018                    #
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
library(sf)
library(tidyverse)
library(viridis)

#Global -----

# Import data
places <- readRDS("./data/places.rds")
geom <- readRDS("./data/geometry.rds")

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
      
      menuItem("Map & Plot",tabName = "map", icon = icon("map")),
      selectInput(
        "select_category",
        label = h3("Topic:"),
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
  
      uiOutput("slider"),
        
      menuItem("Data Table", tabName = "dt", icon = icon("th")),
      
      uiOutput("download_dt")
      
    )
    
    
  ),
  
  # UI Body ----
  dashboardBody(
    
    tabItems(
    
      tabItem(tabName = "map",
        fluidRow(box(      
          fluidRow(column(12,
            leafletOutput("mymap", height = 500) %>%
                  withSpinner(type = getOption("spinner.type", default = 5)))),status = 'primary',width=12)),

        fluidRow(box(
          
          fluidRow(column(8,column(11,offset=1,htmlOutput('p_title'),
                          tags$head(tags$style("#p_title{color: black;font-size: 16px;font-style: bold;}"))))),
          
          fluidRow(
            column(12,br(),plotlyOutput("plot1") %>% withSpinner(type = getOption("spinner.type", default = 5)))),
          
          fluidRow(
              column(2, uiOutput("plot_type")),
              column(2, offset = 1, uiOutput("plot_filter_county")),
              column(2, offset = 1, uiOutput("plot_filter_tract")),
              column(2, offset = 1, uiOutput("plot_filter_bg"))
            ),
          
          fluidRow(column(12,h2("Regional Period Statistics"))),
          
          fluidRow(
            
            column(6,valueBoxOutput("totalChangeBox")),
            column(6,valueBoxOutput("percChangeBox"))
              
            ),status = 'success', width = 12))
        
        
      ),
      
      tabItem(tabName = "dt",
              box(
              uiOutput("dt"),status = 'warning', width = 12)
              # ,
              # uiOutput("download_dt")
      )
    )
  )
)

# ui <- fluidPage(
#   titlePanel(h1("GNRC Census Explorer")), 
#   
#   sidebarLayout(
#     
#     sidebarPanel(
#       
#       helpText("Visualize and Download ACS data for the Greater Nashville Region.",
#                p(),
#                "Begin by selecting a Census Topic"),
#       
#       selectInput(
#         "select_category",
#         label = h3("Topic:"),
#         c("Please select an option below" = "",
#           "Population" = "Pop",
#           "Transportation" = "Tran",
#           "Housing" = "Housing",
#           "Health" = "Health",
#           "Employment" = "Emp"
#         )
#       ),
#       
#       uiOutput("secondSelection"),
#       
#       uiOutput("thirdSelection"),
#       
#       uiOutput("GeoSelection"),
#       
#       uiOutput("slider")
#       
#       
#       ),
#     
#     mainPanel(
#       leafletOutput("mymap", height = 675) %>% withSpinner(type = getOption("spinner.type", default = 5))
# 
#     )
#   ),
#   
#   
#   hr(),
#   
#   fluidRow(
# 
#     column(3,
#            uiOutput("plot_type"),
#            uiOutput("plot_filter_county"),
#            uiOutput("plot_filter_tract"),
#            uiOutput("plot_filter_bg")
# 
#     ),
# 
#     column(8, offset = 1,
#            h4(),
#            plotlyOutput("plot1") %>% withSpinner(type = getOption("spinner.type", default = 5))
# 
#     )
#   ),
#   
#   verbatimTextOutput("event"),
# 
#   hr(),
# 
#   uiOutput("dt"),
# 
#   uiOutput("download_dt")
#   
#   # textOutput("test")
# 
# )


# SERVER -----------------------------------------------------------

server <-  function(input, output, session){
  
  # Render UI elements -----
  
  output$secondSelection <- renderUI({
    
    req(input$select_category)
    
    if (input$select_category == 'Pop') {
      selectInput(
        "select_var",
        label = h3("Category:"),
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
        label = h3("Category:"),
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
        label = h3("Category:"),
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
        label = h3("Category:"),
        c("Please select an option below" = "", 
          'Health insurance coverage status by sex by age',
          'Medicare coverage by sex by age',
          'Sex by age by disability status')
      )
    }
    
    else if (input$select_category == 'Emp') {
      selectInput(
        "select_var",
        label = h3("Category:"),
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
  

  # Render Select Variable
  
  output$thirdSelection <- renderUI({ 
    
    req(input$select_var)
    
    attr <- colnames(selected_data())
    
    selectInput(
      "select_attr",
      label = h3("Variable:"),
      c("Please select an option below" = "", attr[!attr %in% c("GEOID","NAME","Vintage","Level","geometry")])
    )
  })

  # Geographic Boundary
  
  output$GeoSelection <- renderUI({
    
    req(input$select_attr)
    
      selectInput(
        "select_geo",
        label = h3("Geographic Division:"),
        c("Please select an option below" = "", "Automatic by Zoom Level","County","Tract","Block Group")
      )

  })
  
  # Map Vintage slider
  output$slider <- renderUI({

    req(input$select_attr)

    sliderInput("yr", "Map Vintage:", min = 2011, max = 2016, value = 2016, sep = '')
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
  })
  
  # Map - Render Leaflet Tiles -----
  
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
  
  # Map - Fill Color -----
  fc <- reactive({

    req(input$select_attr)

    if(is.null(input$select_attr)){

      fc <- 'grey'
    }

    else if(input$select_attr==''){

      'grey'

    } 
    
    else {

      tryCatch(colorQuantile("YlOrRd", filtered_data()[[input$select_attr]])(filtered_data()[[input$select_attr]]),
               error=function(e) colorBin("YlOrRd", filtered_data()[[input$select_attr]])(filtered_data()[[input$select_attr]]))
    }

  })
  
  
  # Map - Labels -----
  label_txt <- reactive({

    req(fc())
    
    if(is.null(input$select_attr)){
      
      paste("<strong>", 'Attr NULL', "</strong>", sep = '')
    
    } else if (input$select_attr ==''){
        
      paste("<strong>", 'Attr default', "</strong>", sep = '')
      
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
    
    req(fc())
    
    places %>%
      st_set_geometry(NULL) %>%
      pull("NAME") -> plabels
    
    paste("<strong>", plabels, "</strong>", sep = '')
    
  })
  
  observe({

    # req(fc())

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
        )      

  })
  
  observe({
    
    leafletProxy("mymap", data = places) %>%
      clearGroup('Places') %>%
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
      ) 
  })
  
  # Map - Selected Boundary -----
  
  observe({

    req(selected_boundary())
    
    # Data
    filtered_data()%>%
      filter(NAME == selected_boundary()) -> sd
    
    # Label
    
    sd %>%
      st_set_geometry(NULL) %>%
      ungroup() %>%
      pull(input$select_attr) %>%
      round(digits = 3) -> map_var
    
    label <- paste("<strong>",selected_boundary(),"</strong><br>",input$select_attr,": ",map_var,sep = '')
    

    leafletProxy("mymap", data = sd) %>%
      clearGroup('Selected') %>%
      addPolygons(
        data = sd,
        fillOpacity = 0,
        weight = 3,
        stroke = T,
        color = "#33FFF3",
        opacity = 1,
        highlight = highlightOptions(color = "#33FFF3",weight = 4,bringToFront = TRUE),
        label = lapply(label,HTML),
        group = 'Selected'
      )
  })
  
  # Map Legend -----

  pal <- reactive({

    q_length <- length(quantile(filtered_data()[[input$select_attr]],na.rm=TRUE))

    unique_q_length <- length(unique(quantile(filtered_data()[[input$select_attr]],na.rm=TRUE)))

    if(q_length>unique_q_length){

      colorBin("YlOrRd", filtered_data()[[input$select_attr]])

    } else{

      colorQuantile("YlOrRd", filtered_data()[[input$select_attr]])
    }

  })

  observe({

    if(is.null(input$select_attr)){

      leafletProxy("mymap") %>%
        clearControls()

    } else if (input$select_attr ==''){

      leafletProxy("mymap") %>%
        clearControls()

    } else {

      req(fc())

      leafletProxy("mymap", data = filtered_data()) %>%
        clearControls() %>%
        addLegend("bottomright",
                  pal = pal(),
                  values = ~get(input$select_attr),
                  title = "Count",
                  opacity = 0.5,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0(cuts[-n], " - ", cuts[-1])}
          )
    }
  })
  
  # Data Table -----
  
  table_data <- reactive({
    req(selected_data)
    
    selected_data() %>% 
      st_set_geometry(NULL)
    
  })
  
  output$data_table <- DT::renderDataTable({
    
    DT::datatable(
      table_data(),
      options = list(
        scrollX = TRUE,
        pageLength = 10
      )
    )
  })
  
  output$dt_county <- DT::renderDataTable({
    
    table_data() %>%
      filter(Level == 'county') %>%
      DT::datatable(
        options = list(
          autoWidth = TRUE,
          columnDefs = list(list(width = '300px', targets = c(2,which(str_count(colnames(table_data()), ' ')>6)))),
          scrollX = TRUE,
          pageLength = 10
        )
      )
  })
  
  output$dt_tract <- DT::renderDataTable({
    
    table_data() %>%
      filter(Level == 'tract') %>%
      DT::datatable(
        options = list(
          autoWidth = TRUE,
          columnDefs = list(list(width = '300px', targets = c(2,which(str_count(colnames(table_data()), ' ')>6)))),
          scrollX = TRUE,
          pageLength = 10
        )
      )
  })
  
  output$dt_bg <- DT::renderDataTable({
    
    table_data() %>%
      filter(Level == 'block group') %>%
      DT::datatable(
        options = list(
          autoWidth = TRUE,
          columnDefs = list(list(width = '300px', targets = c(2,which(str_count(colnames(table_data()), ' ')>6)))),
          scrollX = TRUE,
          pageLength = 10
        )
      )
  })
  
  # Render data table
  output$dt <- renderUI({

    # req(input$select_var)

    if(is.null(input$select_var)){
      fluidRow(column(12,h4('Here you can interact with a table of selected census data. To generate a table please begin by selecting a census topic and category.')))
    }

    else if (input$select_var == ''){
      fluidRow(column(12,h4('Here you can interact with a table of selected census data. To generate a table please begin by selecting a census topic and category.')))
    }
    
    else {

     
      tabBox(
           tabPanel(HTML(paste(icon('th-list'),"County",sep=' ')),DT::dataTableOutput("dt_county")),
           tabPanel(HTML(paste(icon('th-list'),"Tract",sep=' ')), DT::dataTableOutput("dt_tract")),
           tabPanel(HTML(paste(icon('th-list'),"Block Group",sep=' ')), DT::dataTableOutput("dt_bg")),
           width=12
     )
    }

  })
  
  # Data Period Statistics -----
  
  total_change <- reactive({
    req(table_data())
    
    table_data() %>%
      select(input$select_attr, Vintage)%>%
      filter(Vintage == 2011) -> vi
    
    table_data() %>%
      select(input$select_attr, Vintage)%>%
      filter(Vintage == 2016) -> vf
    
    vf-vi
    
  })
  
  perc_change <- reactive({
    req(table_data())
    
    table_data() %>%
      select(input$select_attr, Vintage)%>%
      filter(Vintage == 2011) -> vi
    
    table_data() %>%
      select(input$select_attr, Vintage)%>%
      filter(Vintage == 2016) -> vf
    
    round(((vf-vi)/abs(vi)*100),3)
    
  })
  
  output$totalChangeBox <- renderValueBox({
    valueBox(
      total_change(), "Total Change", icon = icon("list"),
      color = "purple"
    )
  })
  
  output$percChangeBox <- renderValueBox({
    valueBox(
      perc_change(), "Percent Change", icon = icon("list"),
      color = "green"
    )
  })
  
  # Download Data ----- 

  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$select_var, ".csv", sep = "")
    },
    content = function(file) {
      write.csv((selected_data()%>% st_set_geometry(NULL)), file, row.names = FALSE)
    }
  )

  # output$download_dt <- renderUI({
  # 
  #   req(input$select_var)
  # 
  #   fluidRow(
  # 
  #     column(10),downloadButton("downloadData", "Download Spreadsheet"))
  # })
  
  output$download_dt <- renderUI({
    
    req(input$select_var)
    
    fluidRow(
      
      column(1),downloadButton("downloadData", "Download Spreadsheet"))
  })


  # Filter Plot Data -----
  # based on user input

  plot_data <- reactive({

    # req(county_names())
    req(input$select_attr)

    # User selects County

    if(values$geo_level == 'county'){

      if(input$selected_cfilter ==''){

      table_data() %>%
        filter(Level == values$geo_level) %>%
        group_by(NAME)

      } else if(input$selected_cfilter =='All'){

      table_data() %>%
        filter(Level == values$geo_level) %>%
        group_by(NAME)

      } else {

        table_data() %>%
          filter(Level == values$geo_level & NAME == input$selected_cfilter) %>%
          group_by(NAME)

      }

    # User selects Tract

    } else if (values$geo_level == 'tract'){

        if(input$selected_cfilter ==''){
          table_data() %>%
            filter(Level == values$geo_level & grepl('Davidson County, Tennessee',NAME, fixed = TRUE)) %>%
            group_by(NAME)

        } else if(input$selected_cfilter !='' & (input$selected_tfilter == ''|input$selected_tfilter == 'All')){

          table_data() %>%
            filter(Level == values$geo_level & grepl(input$selected_cfilter,NAME, fixed = TRUE)) %>%
            group_by(NAME)

       } else{

          table_data() %>%
            filter(Level == values$geo_level & grepl(input$selected_tfilter,NAME, fixed = TRUE)) %>%
            group_by(NAME)
        }

    # User selects block Group

    } else if(values$geo_level == 'block group'){

      if(input$selected_cfilter =='' & input$selected_tfilter =='' & input$selected_bgfilter ==''){
        table_data() %>%
          filter(Level == values$geo_level & grepl('Davidson County, Tennessee',NAME, fixed = TRUE)) %>%
          group_by(NAME)

      } else if (input$selected_cfilter !='' & (input$selected_tfilter =='' | input$selected_tfilter =='All') & (input$selected_bgfilter == ''|input$selected_bgfilter == 'All')){

        table_data() %>%
          filter(Level == values$geo_level & grepl(input$selected_cfilter,NAME, fixed = TRUE)) %>%
          group_by(NAME)

      } else if(input$selected_cfilter !='' & input$selected_tfilter !='' & (input$selected_bgfilter == ''|input$selected_bgfilter == 'All')){

        table_data() %>%
          filter(Level == values$geo_level & grepl(input$selected_tfilter,NAME, fixed = TRUE)) %>%
          group_by(NAME)

      } else{

        table_data() %>%
          filter(Level == values$geo_level & grepl(input$selected_bgfilter,NAME, fixed = TRUE)) %>%
          group_by(NAME)
      }

    }


  })

  # Plot Event Data -----
  
  select_event <- reactive({
    
    event_data("plotly_click", source = "select")
    
  })
  
  selected_boundary <- reactive({
    
    req(select_event())
    
    plot_data()$NAME[row.names(plot_data()) == as.character(select_event()[5])]
    
  })
  
  output$event <- renderPrint({
    
    req(select_event())

    # ed <- event_data("plotly_click", source = "select")
    
    # ed[5]
    
    # row.names(plot_data())
    
    # row.names(plot_data()) == as.character(ed[5])
    
    plot_data()$NAME[row.names(plot_data()) == as.character(select_event()[5])]

    })


  # Plot Controls -----
  
  # Select Plot type
  output$plot_type <- renderUI({
    
    req(input$select_attr)
    
    selectInput(
      "selected_plot",
      NULL,
      c("Scatter Plot","Horizontal Bar Plot")
    )
    
  })
  

  # Collect County, Tract, and Block Group names for plot filtering

  county_names <- reactive({
    req(input$select_attr)

    table_data() %>%
      filter(Level == 'county') %>%
      select(NAME) %>%
      distinct()

  })

  tract_names <- reactive({

    if(input$selected_cfilter == ""){
      table_data() %>%
        filter(Level == 'tract' & grepl('Davidson County, Tennessee',NAME, fixed = TRUE)) %>%
        select(NAME) %>%
        distinct()
    } else if(input$selected_cfilter == "All"){
    table_data() %>%
      filter(Level == 'tract') %>%
      select(NAME) %>%
      distinct()
    } else {
      table_data() %>%
        filter(Level == 'tract' & grepl(input$selected_cfilter,NAME, fixed = TRUE)) %>%
        select(NAME) %>%
        distinct()

    }
  })

  blockgroup_names <- reactive({

    if(input$selected_cfilter == "" & (input$selected_tfilter == "" |input$selected_tfilter == "All")){
      table_data() %>%
        filter(Level == 'block group'& grepl('Davidson County, Tennessee',NAME, fixed = TRUE)) %>%
        select(NAME) %>%
        distinct()
    } else {
      table_data() %>%
        filter(Level == 'block group' & grepl(input$selected_tfilter,NAME, fixed = TRUE)) %>%
        select(NAME) %>%
        distinct()

    }
  })

  # Reactively render selectInput boxes based on selected geo level
  output$plot_filter_county <- renderUI({

    req(input$selected_plot)

    if(values$geo_level=='county'){
      selectInput(
        "selected_cfilter",
        NULL,
        c("County..." = "", "All",county_names())
      )
    } else if(values$geo_level == 'tract' | values$geo_level == 'block group'){
      selectInput(
        "selected_cfilter",
        NULL,
        c("Davidson County, Tennessee" = "", county_names())
      )
    }

  })

  output$plot_filter_tract <- renderUI({

    req(input$select_attr)

    if(values$geo_level == 'tract'){
      selectInput(
        "selected_tfilter",
        NULL,
        c("Tract..." = "", "All",tract_names())
      )
    } else if (values$geo_level == 'block group'){

      selectInput(
        "selected_tfilter",
        NULL,
        c("Tract..." = "", "All",tract_names())
      )

    }

  })

  output$plot_filter_bg <- renderUI({

    req(input$select_attr)

    if(values$geo_level == 'block group'){

      req(!is.null(input$selected_tfilter))

      selectInput(
        "selected_bgfilter",
        NULL,
        c("Block group..." = "", "All",blockgroup_names())
      )
    }

  })

  # Plot Title -----

  plot_title <- reactive({

    req(input$select_attr)

    if(values$geo_level == 'county'){
      if(input$selected_cfilter == '' | input$selected_cfilter == 'All'){

        paste(tags$b(input$select_attr),sep='')

      } else { paste(tags$b(input$select_attr),tags$b(input$selected_cfilter), sep = ' - ')}

    } else if(values$geo_level == 'tract'){

      if(input$selected_cfilter == '' & (input$selected_tfilter == '' |input$selected_tfilter == 'All')){

          paste(tags$b(input$select_attr), tags$b('Davidson County, Tennessee'), sep = ' - ')

      } else if (input$selected_cfilter != '' & (input$selected_tfilter == '' | input$selected_tfilter == 'All')){

        paste(tags$b(input$select_attr), tags$b(input$selected_cfilter), sep = ' - ')

      } else if (input$selected_cfilter == '' & input$selected_tfilter != ''){

          paste(tags$b(input$select_attr), tags$b(input$selected_tfilter), sep = ' - ')
      } else {

        paste(tags$b(input$select_attr), tags$b(input$selected_tfilter), sep = ' - ')
      }

    } else if (values$geo_level == 'block group'){

      if(input$selected_cfilter == '' &
         (input$selected_tfilter == '' | input$selected_tfilter == 'All') &
         (input$selected_bgfilter == '' | input$selected_bgfilter == 'All')) {

          paste(tags$b(input$select_attr), tags$b('Davidson County, Tennessee'), sep = ' - ')

      } else if (input$selected_cfilter != '' &
                 (input$selected_tfilter == '' | input$selected_tfilter == 'All') &
                 (input$selected_bgfilter == '' | input$selected_bgfilter == 'All')){

        paste(tags$b(input$select_attr), tags$b(input$selected_cfilter), sep = ' - ')

      } else if (input$selected_cfilter == '' & input$selected_tfilter != '' & input$selected_bgfilter == ''){

        paste(tags$b(input$select_attr), tags$b(input$selected_tfilter), sep = ' - ')

      } else if (input$selected_cfilter == '' & input$selected_tfilter == '' & input$selected_bgfilter != ''){

        paste(tags$b(input$select_attr), tags$b(input$selected_bgfilter), sep = ' - ')

      } else if (input$selected_cfilter != '' & input$selected_tfilter != '' & (input$selected_bgfilter == '' | input$selected_bgfilter == 'All')){

        paste(tags$b(input$select_attr), tags$b(input$selected_tfilter), sep = ' - ')

      } else {

        paste(tags$b(input$select_attr), tags$b(input$selected_bgfilter), sep = ' - ')

      }

    }

  })

  output$p_title <- renderText({
    
    req(input$selected_plot)
    plot_title()
    
    })
  
  # Plots -----

  output$plot1 <- renderPlotly({
    
    if(values$geo_level == 'county'){req(!is.null(input$selected_cfilter))}
    if(values$geo_level == 'tract'){req(!is.null(input$selected_tfilter))}
    if(values$geo_level == 'block group'){req(!is.null(input$selected_bgfilter))}
    else{req(input$select_attr)}
    
    if(input$selected_plot == 'Scatter Plot'){
      
    key <- row.names(plot_data())

    y <- list(
      title = "Count")

    plot_data() %>%
      plot_ly(
        x = ~ Vintage,
        y = ~ get(input$select_attr),
        key = ~key,
        color = ~ NAME,
        colors = viridis_pal(option = "D")(3),
        hovertext = ~ NAME,
        type = 'scatter',
        mode = 'lines+markers',
        source = "select"
      ) %>%
      # layout(title=plot_title(),yaxis = y)
      layout(title=FALSE,yaxis = y)
    
    } else {
      
        y_axis <- list(
          title = "")

        x_axis <- list(
          title = "Count")

        plot_data() %>%
          filter(Vintage == input$yr) %>%
          select(NAME) -> bar_names

        if(values$geo_level == 'county'){sub(" .*", '', bar_names$NAME) -> bar_names}

        else if (values$geo_level == 'tract'){gsub(",.*$", "", bar_names$NAME) %>% word(2,3) -> bar_names}

        else if (values$geo_level == 'block group'){word(bar_names$NAME,1,2,sep=',') -> bar_names}

        plot_data() %>%
          filter(Vintage == input$yr) %>%
          plot_ly(
            x = ~ get(input$select_attr),
            y = bar_names,
            type = 'bar',
            orientation = 'h',
            marker = list(
              # color = 'rgba(50, 171, 96, 0.6)',
              color = 'rgba(60, 141, 188, 1.0)',
              line = list(color = 'rgba(60, 141, 188, 1.0)', width = 1)
            )
          ) %>%
          layout(
            title = FALSE,
            xaxis = x_axis,
            yaxis = y_axis,
            margin = list(
              l = 100,
              r = 20,
              t = 0,
              b = 70
            )
          )
      
    }

  })

# Test / Trouble shooting
output$test <- renderPrint({

  total_change()

})

}

shinyApp(ui, server)

