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
#               Last Updated: 17-MAY-2018                     #
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
           h4("Plot Controls")
    ),
    
    column(8, offset = 1,
           h4("Plot/Graph 1"),
           plotlyOutput("plot1")
    ),
    
    plotOutput("myplot")
  ),
  
  hr(),
  
  uiOutput("dt"),
  
  textOutput("test")

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
      
      attr <- colnames(di)
      attr[!attr %in% c("NAME","Vintage","Level","geometry")] 
      
      selectInput(
        "select_attr",
        label = h3("Map Attribute:"),
        c("Please select an option below" = "", attr[!attr %in% c("NAME","Vintage","Level","geometry")])
      )
    }
  })
  
  output$slider <- renderUI({
    
    if (is.null(input$select_attr)){return(invisible())}
    
    else if (input$select_attr == ''){return(invisible())}
    
    else if(input$select_attr != '') {
      
      sliderInput("yr", "Vintage:", min = 2011, max = 2016, value = 2016, sep = '',animate =
                    animationOptions(interval = 1200, loop = TRUE))
      
      }
  })
  
  
  # Map ------
  
  filtered_data <- reactive({
    
    req(input$mymap_zoom)
    
    if (is.null(input$yr)){y <- 2016} 
    else {y <- input$yr}
    
    if (input$mymap_zoom <= 8) {
      geolevel <- 'county'
    }
    else if (input$mymap_zoom > 8 & input$mymap_zoom < 13){
      geolevel <- 'tract'
    }
    else if (input$mymap_zoom >= 13){
      geolevel <- 'block group'
    }
    
    di %>%
      filter(Vintage == y & Level == geolevel)
    
  })
  
  output$mymap <- renderLeaflet({
    
    # if (is.null(input$select_attr)) {
    #   leaflet() %>%
    #     setView(lat = 36.174465,
    #             lng = -86.767960,
    #             zoom = 8) %>%
    #     addProviderTiles(providers$CartoDB.Positron,
    #                      options = providerTileOptions(noWrap = TRUE, zIndex = 1))
    # } else if (input$select_attr == '') {
    #   leaflet() %>%
    #     setView(lat = 36.174465,
    #             lng = -86.767960,
    #             zoom = 8) %>%
    #     addProviderTiles(providers$CartoDB.Positron,
    #                      options = providerTileOptions(noWrap = TRUE, zIndex = 1))
    # }
    # else
    #   
    # {
    #   
    #   filtered_data() %>%
    #     st_set_geometry(NULL) %>%
    #     pull("NAME") -> labels
    # 
    #   filtered_data() %>%
    #     st_set_geometry(NULL) %>%
    #     ungroup() %>%
    #     pull(input$select_attr) %>%
    #     round(digits = 3) -> map_var
    # 
    #   c <- paste("<strong>", labels, "</strong><br>", input$select_attr,": ", map_var, sep = '')

    leaflet(di) %>%
      addProviderTiles(providers$CartoDB.PositronNoLabels, options = providerTileOptions(noWrap = TRUE, zIndex = 1)) %>% 
      setView(lat = 36.174465,lng = -86.767960,zoom = 8) 
        # %>%
        # addPolygons(
        #   group = "county",
        #   fillColor = ~ colorQuantile("YlOrRd", map_var)(map_var),
        #   fillOpacity = 0.5,
        #   weight = 2,
        #   stroke = T,
        #   color = "grey",
        #   opacity = 1,
        #   #dashArray = "3",
        #   highlight = highlightOptions(color = "white",weight = 3,bringToFront = TRUE),
        #   options = list(zIndex = 2),
        #   label = lapply(c, HTML)
        # ) %>%
        # addProviderTiles(
        #   "CartoDB.PositronOnlyLabels",
        #   group = "labels",
        #   options = providerTileOptions(zIndex = 3, pane = 'markerPane')
        # ) %>%
        # addLayersControl(overlayGroups = c("polygons", "labels"))
    # }
    
  })
  
  
  observe({
    
    req(input$mymap_zoom)
    
    if(is.null(input$select_attr)){
      
      fc <- 'grey'
      
      filtered_data() %>%
        st_set_geometry(NULL) %>%
        pull("NAME") -> labels
      
      c <- paste("<strong>", labels, "</strong>", sep = '')
      
    } else if(input$select_attr==''){
      
      fc <- 'grey'
      
      filtered_data() %>%
        st_set_geometry(NULL) %>%
        pull("NAME") -> labels
      
      c <- paste("<strong>", labels, "</strong>", sep = '')
      
    } else {
      
      fc <- ~ colorQuantile("YlOrRd", Shellys_DI)(Shellys_DI)
      
      filtered_data() %>%
            st_set_geometry(NULL) %>%
            pull("NAME") -> labels

      filtered_data() %>%
        st_set_geometry(NULL) %>%
        ungroup() %>%
        pull(input$select_attr) %>%
        round(digits = 3) -> map_var
      
      c <- paste("<strong>", labels, "</strong><br>", input$select_attr,": ", map_var, sep = '')

    }
    
    leafletProxy("mymap", data = filtered_data()) %>%
      clearShapes() %>%
      addPolygons(
        fillColor = fc,
        fillOpacity = 0.5,
        weight = 2,
        stroke = T,
        color = "grey",
        opacity = 1,
        highlight = highlightOptions(color = "white",weight = 3,bringToFront = TRUE),
        options = list(zIndex = 2),
        label = lapply(c, HTML)
      ) %>%
      addProviderTiles(
        "CartoDB.PositronOnlyLabels",
        group = "labels",
        options = providerTileOptions(zIndex = 3, pane = 'markerPane')
      )
    
  })
  
  
  # Data Table -----
  
  output$data_table <-
    DT::renderDataTable({
      di
    }, options = list(columnDefs = list(list(
      targets = ncol(di),
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
  
  # Plot1 -----
  
  output$plot1 <- renderPlotly({

    req(input$select_attr)

    di %>%
      st_set_geometry(NULL) %>%
      filter(Level == "county") %>%
      group_by(NAME) %>%
      plot_ly(
        x = ~ Vintage,
        y = ~ Shellys_DI,
        color = ~ NAME,
        colors = viridis_pal(option = "D")(3),
        name = ~ NAME,
        type = 'scatter',
        mode = 'lines+markers'
      ) %>% 
      layout(updatemenus = list(list(
        y = 0.8,
        buttons = list(
          list(
            method = "restyle",
            args = list("type", "scatter"),
            label = "Scatter"
          ),
          
          list(
            method = "restyle",
            args = list("type", "histogram2d"),
            label = "2D Histogram"
          )
        )
      )))
    
  })
  
# Test / Trouble shooting
# output$test <- renderPrint({
#   
#   input$mymap_zoom
#   
#   filtered_data() %>%
#     filter(Level == 'county') -> f_cnty_data
#   
#   f_cnty_data
#   
# })
  
}

shinyApp(ui, server)

