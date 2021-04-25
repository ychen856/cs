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
  ###################################
  #      part I                     #
  ###################################
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
      g <- generateMap("Set 1", input$westLoopSide_option, "Near West Side", usage_block_df)
    })
  })
  
  observeEvent(input$month_data, {
    month <- input$month_data
    usage_block_df <- getData("Near West Side", input$westLoopSide_option, input$month_data, input$westLoopSide_buildingType)
    
    output$west_loop_side_map <- renderLeaflet({
      g <- generateMap("Set 1", input$westLoopSide_option, "Near West Side", usage_block_df)
    })
  })
  
  observeEvent(input$westLoopSide_buildingType, {
    bType <- input$westLoopSide_buildingType
    usage_block_df <- getData("Near West Side", input$westLoopSide_option, ifelse(is.null(input$month_data), "All", input$month_data), input$westLoopSide_buildingType)
    
    output$west_loop_side_map <- renderLeaflet({
      g <- generateMap("Set 1", input$westLoopSide_option, "Near West Side", usage_block_df)
    })
  })

  observeEvent(input$reset_btn, {
    usage_block_df <- getData("Near West Side", input$westLoopSide_option, ifelse(is.null(input$month_data), "All", input$month_data), input$westLoopSide_buildingType)
    
    output$west_loop_side_map <- renderLeaflet({
      g <- generateMap("Set 1", input$westLoopSide_option, "Near West Side", usage_block_df)
    })
  })
  
  observe({
    plotData <- getPlotData("Near West Side")
    output$block_data_plot <- renderPlot({
      ggplot(plotData, aes(x = MONTH, y = AMOUNT)) + 
        geom_line(aes(x = MONTH, y = AMOUNT, color = SOURCE)) + 
        labs(title="Annual usage in Near West Side", x="Month", y = "Amount") + 
        scale_y_continuous(labels = scales::comma) + 
        scale_x_continuous(breaks = seq(1, 12, 1),
                           labels = c("1" = "Jan", 
                                      "2" = "Fab",
                                      "3" = "Mar",
                                      "4" = "Apr",
                                      "5" = "May", "5" = "Jun", "7" = "Jul", "8" = "Agu", "9" = "Sept", "10" = "Oct", "11" = "Nov", "12" = "Dec" )) +
        theme(legend.position="bottom") +
        scale_color_manual(values= c("Electricity" = "blue", "Gas" = "red"))
    })
    
    elec_df <- aggregate( cbind(KWH.JANUARY.2010, KWH.FEBRUARY.2010, KWH.MARCH.2010, KWH.APRIL.2010,
                                KWH.MAY.2010, KWH.JUNE.2010, KWH.JULY.2010, KWH.AUGUST.2010, KWH.SEPTEMBER.2010, KWH.OCTOBER.2010, 
                                KWH.NOVEMBER.2010, KWH.DECEMBER.2010, TOTAL.KWH) ~ COMMUNITY.AREA.NAME + BUILDING.TYPE, 
                          subset(usage_2010_df, COMMUNITY.AREA.NAME == "Near West Side"), sum)
    gas_df <- gas <- aggregate(cbind(THERM.JANUARY.2010, THERM.FEBRUARY.2010, THERM.MARCH.2010, TERM.APRIL.2010,
                                     THERM.MAY.2010, THERM.JUNE.2010, THERM.JULY.2010, THERM.AUGUST.2010, THERM.SEPTEMBER.2010, THERM.OCTOBER.2010, 
                                     THERM.NOVEMBER.2010, THERM.DECEMBER.2010, TOTAL.THERMS) ~ COMMUNITY.AREA.NAME + BUILDING.TYPE, 
                               subset(usage_2010_df, COMMUNITY.AREA.NAME == "Near West Side"), sum)
    
    Names <- c('', 'Community Area', 'Building Type','January', 'Fabuary', 'March', 'April', 'May', 'June', 'July', 'Augus', 'September', 'October', 'November', 'December')
    NamesFooter <- c('Total', '', '', '', '', '', '', '', '', '', '', '', '', '', '')
    sketch <- htmltools::withTags(table(
      tableHeader(Names),tableFooter(NamesFooter)
    ))
    opts <- list(
      pageLength = 5,
      searching = FALSE,
      lengthChange = TRUE,
      columnDefs = list(list(width = '200px', targets = 1)),
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
        elec_df
      }, 
      colnames = c('Community Area', 'Building Type','January', 'Fabuary', 'March', 'April', 'May', 'June', 'July', 'Augus', 'September', 'October', 'November', 'December'),
      #options = list(searching = FALSE, pageLength = 19, lengthChange = FALSE, order = list(list(1, 'asc'))), rownames = FALSE 
      ) #%>%
      #formatCurrency(3:14, currency = "", interval = 3, mark = ",")#%>% 
      #formatPercentage(3, 1)
    )
    
    output$myTable_gas <- DT::renderDataTable(
      
      DT::datatable(container = sketch,extensions = 'Buttons',options = opts,{ 
        gas_df  
      }, 
      colnames = c('Community Area', 'Building Type','January', 'Fabuary', 'March', 'April', 'May', 'June', 'July', 'Augus', 'September', 'October', 'November', 'December')
      ) #%>%
      #formatCurrency(3:14, currency = "", interval = 3, mark = ",")#%>% 
      #formatPercentage(3, 1)
    )
  })
  
  ###################################
  #      part II                    #
  ###################################
  
  ###########left map################
  observeEvent(input$palette, {
    print(input$palette)
  })
  observeEvent(input$l_area, {
    usage_block_df <- getData(input$l_area, input$l_option, ifelse(is.null(input$l_month_data), "All", input$l_month_data), input$l_buildingType)
    
    output$l_map <- renderLeaflet({
      g <- generateMap(input$palette, input$l_option, input$l_area, usage_block_df)
    })
    
    plotData <- getPlotData(ifelse(is.null(p), "Near West Side", input$l_area))
    output$l_block_data_plot <- renderPlot({
      ggplot(plotData, aes(x = MONTH, y = AMOUNT)) + 
        geom_line(aes(x = MONTH, y = AMOUNT, color = SOURCE)) + 
        labs(title=paste("Annual usage in ", input$l_area), x="Month", y = "Amount") + 
        scale_y_continuous(labels = scales::comma) + 
        scale_x_continuous(breaks = seq(1, 12, 1),
                           labels = c("1" = "Jan", 
                                      "2" = "Fab",
                                      "3" = "Mar",
                                      "4" = "Apr",
                                      "5" = "May", "5" = "Jun", "7" = "Jul", "8" = "Agu", "9" = "Sept", "10" = "Oct", "11" = "Nov", "12" = "Dec" )) +
        theme(legend.position="bottom") +
        scale_color_manual(values= c("Electricity" = "blue", "Gas" = "red"))
    })
  })
  
  observeEvent(input$l_option, {
    option <- input$l_option
    
    if(input$l_option == "Electricity" || input$l_option == "Gas") {
      output$l_monthList <- renderUI({
        tags$div(class = "filter",
                 selectizeInput(
                   'l_month_data', 'Select a Month: ', choices = c("All", "January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"), selected = "All", multiple = FALSE
                 )
        )
      })
    }
    else {
      output$monthList <- renderUI({ })
    }
  
    usage_block_df <- getData(input$l_area, input$l_option, ifelse(is.null(input$l_month_data), "All", input$l_month_data) , input$l_buildingType)
    
    output$l_map <- renderLeaflet({
      g <- generateMap(input$palette, input$l_option, input$l_area, usage_block_df)
    })
  })
  
  observeEvent(input$l_month_data, {
    month <- input$l_month_data
    usage_block_df <- getData(input$l_area, input$l_option, input$l_month_data, input$l_buildingType)
    
    output$l_map <- renderLeaflet({
      g <- generateMap(input$palette, input$l_option, input$l_area, usage_block_df)
    })
  })
  
  observeEvent(input$l_buildingType, {
    bType <- input$l_buildingType
    usage_block_df <- getData(input$l_area, input$l_option, ifelse(is.null(input$l_month_data), "All", input$l_month_data), input$l_buildingType)
    
    output$l_map <- renderLeaflet({
      g <- generateMap(input$palette, input$l_option, input$l_area, usage_block_df)
    })
  })
  
  observeEvent(input$l_reset_btn, {
    usage_block_df <- getData(input$l_area, input$l_option, ifelse(is.null(input$l_month_data), "All", input$l_month_data), input$l_buildingType)
    
    output$l_map <- renderLeaflet({
      g <- generateMap(input$palette, input$l_option, input$l_area, usage_block_df)
    })
  })
  
  ###########right map###############
  observeEvent(input$r_area, {
    usage_block_df <- getData(input$r_area, input$r_option, ifelse(is.null(input$r_month_data), "All", input$r_month_data), input$r_buildingType)
    
    output$rr_map <- renderLeaflet({
      g <- generateMap(input$palette, input$r_option, input$r_area, usage_block_df)
    })
    
    plotData <- getPlotData(ifelse(is.null(p), "Loop", input$r_area))
    output$r_block_data_plot <- renderPlot({
      ggplot(plotData, aes(x = MONTH, y = AMOUNT)) + 
        geom_line(aes(x = MONTH, y = AMOUNT, color = SOURCE)) + 
        labs(title=paste("Annual usage in ", input$r_area), x="Month", y = "Amount") + 
        scale_y_continuous(labels = scales::comma) + 
        scale_x_continuous(breaks = seq(1, 12, 1),
                           labels = c("1" = "Jan", 
                                      "2" = "Fab",
                                      "3" = "Mar",
                                      "4" = "Apr",
                                      "5" = "May", "5" = "Jun", "7" = "Jul", "8" = "Agu", "9" = "Sept", "10" = "Oct", "11" = "Nov", "12" = "Dec" )) +
        theme(legend.position="bottom") +
        scale_color_manual(values= c("Electricity" = "blue", "Gas" = "red"))
    })
  })
  
  observeEvent(input$r_option, {
    
    option <- input$r_option
    
    if(input$r_option == "Electricity" || input$r_option == "Gas") {
      output$r_monthList <- renderUI({
        tags$div(class = "filter",
                 selectizeInput(
                   'r_month_data', 'Select a Month: ', choices = c("All", "January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"), selected = "All", multiple = FALSE
                 )
        )
      })
    }
    else {
      output$r_monthList <- renderUI({ })
    }
    
    usage_block_df <- getData(input$r_area, input$r_option, ifelse(is.null(input$r_month_data), "All", input$r_month_data) , input$r_buildingType)
    
    output$rr_map <- renderLeaflet({
      g <- generateMap(input$palette, input$r_option, input$r_area, usage_block_df)
    })
  })
  
  observeEvent(input$r_month_data, {
    month <- input$r_month_data
    usage_block_df <- getData(input$r_area, input$r_option, input$r_month_data, input$r_buildingType)
    
    output$rr_map <- renderLeaflet({
      generateMap(input$palette, input$r_option, input$r_area, usage_block_df)
    })
  })
  
  observeEvent(input$r_buildingType, {
    bType <- input$r_buildingType
    usage_block_df <- getData(input$r_area, input$r_option, ifelse(is.null(input$r_month_data), "All", input$r_month_data), input$r_buildingType)
    
    output$rr_map <- renderLeaflet({
      g <- generateMap(input$palette, input$r_option, input$r_area, usage_block_df)
    })
  })
  
  observeEvent(input$r_reset_btn, {
    usage_block_df <- getData(input$r_area, input$r_option, ifelse(is.null(input$r_month_data), "All", input$r_month_data), input$r_buildingType)
    
    output$rr_map <- renderLeaflet({
      g <- generateMap(input$palette, input$r_option, input$r_area, usage_block_df)
    })
  })
  
  
  ####################################
  #            part 3                #
  ####################################
  observeEvent(input$t_option, {
    
    option <- input$t_option
    
    if(input$t_option == "Electricity" || input$t_option == "Gas") {
      output$t_monthList <- renderUI({
        tags$div(class = "filter",
                 selectizeInput(
                   't_month_data', 'Select a Month: ', choices = c("All", "January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"), selected = "All", multiple = FALSE
                 )
        )
      })
    }
    else {
      output$t_monthList <- renderUI({ })
    }
    
    #usage_block_df <- getTrackData(input$t_option, ifelse(is.null(input$r_month_data), "All", input$t_month_data) , input$t_buildingType)
    usage_block_df <- getTrackData(input$t_option, ifelse(is.null(input$t_month_data), "All", input$t_month_data), input$t_buildingType)
    
    output$t_map <- renderLeaflet({
      g <- generateMap("Set 1", input$t_option, "Chicago",usage_block_df)
    })
  })
  
  observeEvent(input$t_month_data, {
    month <- input$t_month_data
    usage_block_df <- getTrackData(input$t_option, ifelse(is.null(input$t_month_data), "All", input$t_month_data), input$t_buildingType)
    
    output$t_map <- renderLeaflet({
      g <- generateMap("Set 1", input$t_option, "Chicago", usage_block_df)
    })
  })
  
  observeEvent(input$t_buildingType, {
    bType <- input$t_buildingType
    usage_block_df <- getTrackData(input$t_option, ifelse(is.null(input$t_month_data), "All", input$t_month_data), input$t_buildingType)
    
    output$t_map <- renderLeaflet({
      g <- generateMap("Set 1", input$t_option, "Chicago", usage_block_df)
    })
  })
  
  
  observeEvent(input$t_reset_btn, {
    usage_block_df <- getTrackData(input$t_option, ifelse(is.null(input$t_month_data), "All", input$t_month_data), input$t_buildingType)
    
    output$t_map <- renderLeaflet({
      g <- generateMap("Set 1", input$t_option, "Chicago", usage_block_df)
    })
  })

  observe({
    plotData <- getPlotData_t()
    output$tract_data_plot <- renderPlot({
      ggplot(plotData, aes(x = MONTH, y = AMOUNT)) + 
        geom_line(aes(x = MONTH, y = AMOUNT, color = SOURCE)) + 
        labs(title="Annual usage in Chicago", x="Month", y = "Amount") + 
        scale_y_continuous(labels = scales::comma) + 
        scale_x_continuous(breaks = seq(1, 12, 1),
                         labels = c("1" = "Jan", 
                                    "2" = "Fab",
                                    "3" = "Mar",
                                    "4" = "Apr",
                                    "5" = "May", "5" = "Jun", "7" = "Jul", "8" = "Agu", "9" = "Sept", "10" = "Oct", "11" = "Nov", "12" = "Dec" )) +
        theme(legend.position="bottom") +
        scale_color_manual(values= c("Electricity" = "blue", "Gas" = "red"))
    })
  })

}





