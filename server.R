library(shiny)
library(leaflet)
library(leafpop)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(fontawesome)
library(stringr)
library(ggplot2)
library(tidyverse)
library(fiftystater)
library(scales)
library(usmap)
library(DT)
library(shinycssloaders)
library(mapview)
library(htmltools)
source("utility.R")
library(stringr)
library(mapview)
library(tigris)
library(sf)
library(dplyr)
RV<-reactiveValues(Clicks=list())

# Define server logic required to draw a histogram
function(input, output, session) {
  
  pink2 = colorRampPalette(c('white', 'deeppink'))
  observeEvent(input$westLoopSide_option, {
    option <- input$westLoopSide_option
    
    if(input$westLoopSide_option == "Electricity" || input$westLoopSide_option == "Gas") {
      output$monthList <- renderUI({
        tags$div(class = "filter",
          selectizeInput(
            'month_data', 'Select a Month: ', choices = c("All", "January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"), selected = "All", multiple = FALSE
          )
        )
      })
    }
    else {
      output$monthList <- renderUI({ })
    }
    
    usage_block_df <- getData("Near West Side", input$westLoopSide_option, ifelse(is.null(input$month_data), "All", input$month_data) , input$westLoopSide_buildingType)
    
    output$west_loop_side_map <- renderLeaflet({
      g <- generateMap(input$westLoopSide_option, usage_block_df)
    })
    #output$block_data_plot <- renderPlot({
    #  ggplot(plot_df) + xlim(0, 10) + ylim(0, 100)
    #})
  })
  
  observeEvent(input$month_data, {
    month <- input$month_data
    usage_block_df <- getData("Near West Side", input$westLoopSide_option, input$month_data, input$westLoopSide_buildingType)
    
    output$west_loop_side_map <- renderLeaflet({
      g <- generateMap(input$westLoopSide_option, usage_block_df)
    })
    #output$block_data_plot <- renderPlot({
    #  ggplot(plot_df) + xlim(0, 10) + ylim(0, 100)
    #})
  })
  
  observeEvent(input$westLoopSide_buildingType, {
    bType <- input$westLoopSide_buildingType
    usage_block_df <- getData("Near West Side", input$westLoopSide_option, ifelse(is.null(input$month_data), "All", input$month_data), input$westLoopSide_buildingType)
    
    output$west_loop_side_map <- renderLeaflet({
      g <- generateMap(input$westLoopSide_option, usage_block_df)
    })
    #output$block_data_plot <- renderPlot({
    #  ggplot(plot_df) + xlim(0, 10) + ylim(0, 100)
    #})
  })

  observeEvent(input$reset_btn, {
    usage_block_df <- getData("Near West Side", input$westLoopSide_option, ifelse(is.null(input$month_data), "All", input$month_data), input$westLoopSide_buildingType)
    
    output$west_loop_side_map <- renderLeaflet({
      g <- generateMap(input$westLoopSide_option, usage_block_df)
    })
    #output$block_data_plot <- renderPlot({
    #  ggplot(plot_df) + xlim(0, 10) + ylim(0, 100)
    #})
  })
  
  
  
  
  generateMap <- function(option, df) {
    blue = colorRampPalette(c('white', 'blue'))
    red = colorRampPalette(c('white', 'red'))
    purple = colorRampPalette(c('white', 'purple'))
    orange = colorRampPalette(c('white', 'orange'))
    red2 = colorRampPalette(c('white', 'darkred'))
    pink2 = colorRampPalette(c('white', 'deeppink'))
    
    if(option == "Electricity") {
      mapview(df, zcol = "AMOUNT", layer.name = "Electricity(KWH)", col.regions = blue)@map %>% 
        addMapPane("polygons", zIndex = 999) %>% 
        addCircleMarkers(data = df, 
                             lat = ~as.numeric(INTPTLAT10), 
                             lng = ~as.numeric(INTPTLON10), 
                             fillOpacity=0, 
                             weight = 0, 
                             options = pathOptions(pane = "polygons"), 
                             layerId = ~GEOID10, label = "Generate plot...") 
    }
    else if (option == "Gas") {
      mapview(df, zcol = "AMOUNT", layer.name = 'GAS(THERMS)', col.regions = red)@map %>% 
        addMapPane("polygons", zIndex = 999) %>% 
        addCircleMarkers(data = df, 
                         lat = ~as.numeric(INTPTLAT10), 
                         lng = ~as.numeric(INTPTLON10), 
                         fillOpacity=0, 
                         weight = 0, 
                         options = pathOptions(pane = "polygons"), 
                         layerId = ~GEOID10, label = "Generate plot...") 
    }
    else if (option == "Building Type") {
      #df$AMOUNT_2 <- "Other"
      #df$AMOUNT_2[df$AMOUNT == "Residential"] <- "Residential"
      #df$AMOUNT_2[df$AMOUNT == "Commercial"] <- "Commercial"
      #df$AMOUNT_2[df$AMOUNT == "Industrial"] <- "Industrial"
      print(df)
      #mapview(df, zcol = "AMOUNT_2", layer.name = 'Building Type', col.regions = c("Residential" = "red", "green", "blue", "orange"))@map %>% 
      m <- NULL
      if(nrow(subset(df, AMOUNT == "Residential")) > 0)
        m <- mapview(subset(df, AMOUNT == "Residential"), layer.name = "Residential", zcol = "AMOUNT", col.regions = "#F5793A")
      if(nrow(subset(df, AMOUNT == "Commercial")) > 0)
        m <- m + mapview(subset(df, AMOUNT == "Commercial"), zcol = "AMOUNT", layer.name = "Commercial", col.regions = "#A95AA1") 
      if(nrow(subset(df, AMOUNT == "Industrial")) > 0)
        m <- m + mapview(subset(df, AMOUNT == "Industrial"), zcol = "AMOUNT", layer.name = "Industrial", col.regions = "#85C0F9") 
        
      m@map %>% addMapPane("polygons", zIndex = 999) %>% 
        addCircleMarkers(data = df, 
                         lat = ~as.numeric(INTPTLAT10), 
                         lng = ~as.numeric(INTPTLON10), 
                         fillOpacity=0, 
                         weight = 0, 
                         options = pathOptions(pane = "polygons"), 
                         layerId = ~GEOID10, label = "Generate plot...") 
    }
    else if (option == "Building Age") {
      mapview(df, zcol = "AMOUNT", layer.name = 'Building Age', col.regions = purple)@map %>% 
        addMapPane("polygons", zIndex = 999) %>% 
        addCircleMarkers(data = df, 
                         lat = ~as.numeric(INTPTLAT10), 
                         lng = ~as.numeric(INTPTLON10), 
                         fillOpacity=0, 
                         weight = 0, 
                         options = pathOptions(pane = "polygons"), 
                         layerId = ~GEOID10, label = "Generate plot...") 
    }
    else if (option == "Building Height") {
      mapview(df, zcol = "AMOUNT", layer.name = 'Building Height', col.regions = orange)@map %>% 
        addMapPane("polygons", zIndex = 999) %>% 
        addCircleMarkers(data = df, 
                         lat = ~as.numeric(INTPTLAT10), 
                         lng = ~as.numeric(INTPTLON10), 
                         fillOpacity=0, 
                         weight = 0, 
                         options = pathOptions(pane = "polygons"), 
                         layerId = ~GEOID10, label = "Generate plot...") 
    }
    else if (option == "Total Population") {
      mapview(df, zcol = "AMOUNT", layer.name = 'Total Population', col.regions = red2)@map %>% 
        addMapPane("polygons", zIndex = 999) %>% 
        addCircleMarkers(data = df, 
                         lat = ~as.numeric(INTPTLAT10), 
                         lng = ~as.numeric(INTPTLON10), 
                         fillOpacity=0, 
                         weight = 0, 
                         options = pathOptions(pane = "polygons"), 
                         layerId = ~GEOID10, label = "Generate plot...") 
    }
  }
  
  observe({ 
    p <- input$west_loop_side_map_marker_click  # typo was on this line
    print(p$id)
    if(!is.null(p$id)) {
      #print(subset(plot_df, GEOID10 == p$id))
      plotData <- getPlotData(p$id)
      
      if(!is.null(plotData)) {
        p2 <- ggplot(subset(plotData, GEOID10 == p$id), aes(x = MONTH, y = AMOUNT)) + 
            geom_line(aes(x = MONTH, y = AMOUNT, color = SOURCE)) + 
            labs(title=paste("Annual usage in ", p$id), x="Month", y = "Amount") + 
            scale_y_continuous(labels = scales::comma) + 
            scale_x_continuous(breaks = seq(1, 12, 1),
                             labels = c("1" = "Jan", 
                                        "2" = "Fab",
                                        "3" = "Mar",
                                        "4" = "Apr",
                                        "5" = "May", "5" = "Jun", "7" = "Jul", "8" = "Agu", "9" = "Sept", "10" = "Oct", "11" = "Nov", "12" = "Dec" )) +
            theme(legend.position="bottom") +
           scale_color_manual(values= c("Electricity" = "blue", "Gas" = "red"))
      
        output$block_data_plot <- renderPlot({
          p2
        })
      }
      else {
        output$block_data_plot <- renderPlot({})
      }
      
      Names <- c('', 'Census Block', 'Building Type','January', 'Fabuary', 'March', 'April', 'May', 'June', 'July', 'Augus', 'September', 'October', 'November', 'December')
      NamesFooter <- c('Total', '', '', '', '', '', '', '', '', '', '', '', '', '', '')
      sketch <- htmltools::withTags(table(
        tableHeader(Names),tableFooter(NamesFooter)
      ))
      opts <- list(
        dom = 'Bfrtip',
        footerCallback = JS(
          "function( tfoot, data, start, end, display ) {",
          "var api = this.api(), data;",
          "$( api.column(3).footer()).html(",
          "api.column(3).data().reduce( function ( a, b ) {",
          "return a + b;",
          "} )",
          ");",
          "$( api.column(4).footer()).html(",
          "api.column(4).data().reduce( function ( a, b ) {",
          "return a + b;",
          "} )",
          ");",
          "$( api.column(5).footer()).html(",
          "api.column(5).data().reduce( function ( a, b ) {",
          "return a + b;",
          "} )",
          ");",
          "$( api.column(6).footer()).html(",
          "api.column(6).data().reduce( function ( a, b ) {",
          "return a + b;",
          "} )",
          ");",
          "$( api.column(7).footer()).html(",
          "api.column(7).data().reduce( function ( a, b ) {",
          "return a + b;",
          "} )",
          ");",
          "$( api.column(8).footer()).html(",
          "api.column(8).data().reduce( function ( a, b ) {",
          "return a + b;",
          "} )",
          ");",
          "$( api.column(9).footer()).html(",
          "api.column(9).data().reduce( function ( a, b ) {",
          "return a + b;",
          "} )",
          ");",
          "$( api.column(10).footer()).html(",
          "api.column(10).data().reduce( function ( a, b ) {",
          "return a + b;",
          "} )",
          ");",
          "$( api.column(11).footer()).html(",
          "api.column(11).data().reduce( function ( a, b ) {",
          "return a + b;",
          "} )",
          ");",
          "$( api.column(12).footer()).html(",
          "api.column(12).data().reduce( function ( a, b ) {",
          "return a + b;",
          "} )",
          ");",
          "$( api.column(13).footer()).html(",
          "api.column(13).data().reduce( function ( a, b ) {",
          "return a + b;",
          "} )",
          ");",
          "$( api.column(14).footer()).html(",
          "api.column(14).data().reduce( function ( a, b ) {",
          "return a + b;",
          "} )",
          ");","}")
      )
      output$myTable_elec <- DT::renderDataTable(
        
        DT::datatable(container = sketch,extensions = 'Buttons',options = opts,{ 
          subset(subset(usage_2010_df, GEOID10 == p$id), select = c(GEOID10, BUILDING.TYPE, KWH.JANUARY.2010, KWH.FEBRUARY.2010, KWH.MARCH.2010, 
                                                                    KWH.APRIL.2010, KWH.MAY.2010, KWH.JUNE.2010, KWH.JULY.2010, KWH.AUGUST.2010, 
                                                                    KWH.SEPTEMBER.2010, KWH.OCTOBER.2010, KWH.NOVEMBER.2010, KWH.DECEMBER.2010))
        }, 
        colnames = c('Census Block', 'Building Type','January', 'Fabuary', 'March', 'April', 'May', 'June', 'July', 'Augus', 'September', 'October', 'November', 'December'),
        #options = list(searching = FALSE, pageLength = 19, lengthChange = FALSE, order = list(list(1, 'asc'))), rownames = FALSE 
        ) %>%
          formatCurrency(3, currency = "", interval = 3, mark = ",")#%>% 
          #formatPercentage(3, 1)
      )
      
      output$myTable_gas <- DT::renderDataTable(
        
        DT::datatable(container = sketch,extensions = 'Buttons',options = opts,{ 
          subset(subset(usage_2010_df, GEOID10 == p$id), select = c(GEOID10, BUILDING.TYPE, THERM.JANUARY.2010, THERM.FEBRUARY.2010, THERM.MARCH.2010, 
                                                                    TERM.APRIL.2010, THERM.MAY.2010, THERM.JUNE.2010, THERM.JULY.2010, THERM.AUGUST.2010, 
                                                                    THERM.SEPTEMBER.2010, THERM.OCTOBER.2010, THERM.NOVEMBER.2010, THERM.DECEMBER.2010))
        }, 
        colnames = c('Census Block', 'Building Type','January', 'Fabuary', 'March', 'April', 'May', 'June', 'July', 'Augus', 'September', 'October', 'November', 'December'),
        #options = list(searching = FALSE, pageLength = 19, lengthChange = FALSE, order = list(list(1, 'asc'))), rownames = FALSE 
        ) %>%
          formatCurrency(3, currency = "", interval = 3, mark = ",")#%>% 
        #formatPercentage(3, 1)
      )
    }
    print(p$id)
  })
}



