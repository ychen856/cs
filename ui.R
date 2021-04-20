library(shiny)
library(leaflet)
library(mapview)
library(shinyWidgets)
source("utility.R")



#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)
source("global.R")

# Define UI for application that draws a histogram
ui <- fluidPage(class = "p-0 m-0",
    includeCSS("www/dashboard.css"),
    tags$head(
        tags$style("@import url(https://use.fontawesome.com/releases/v5.7.2/css/all.css);")
    ),
    # Application title
    title = "CS424 Project 3",
        tags$nav(class = "head shadow p-0 m-0 pl-0",
            tags$ul(class = "title p-0 mr-auto mt-0 mb-0",
                tags$li(
                    tags$p("CS424 Project 3")
                )
            ),
            tags$ul(class = "name p-0",
                tags$li(class = "text-lg mt-auto mb-auto", "Yi-Chun Chen")
            )
        ),
        tags$div(class = "p-0",
            navbarPage("",
                #energy plants location start
                tabPanel("West Loop Side Census Data 2010", class = "p-0",
                    mainPanel(class = "panel p-0",
                        fluidRow(
                            column(12, class = "p-0",
                                tags$div(class = "card border-title shadow",
                                    #card Start
                                    tags$div(class = "card-body",
                                        tags$div(class = "title",
                                            tags$span("West Loop Side Census 2010")
                                        ),
                                        fluidRow(style = "margin: 2px",
                                            column(2, style = "background-color: white",
                                                tags$div(
                                                    tags$div(class = "subtitle",
                                                        tags$i(class = "fas fa-search"),
                                                            "Data Filter:"
                                                    ),

                                                    #opitons
                                                    tags$div(class = "filter",
                                                        selectizeInput(
                                                            'westLoopSide_option', 'Select a Data Option: ', choices = c("Electricity", "Gas", "Building Type", "Building Age", "Building Height", "Total Population"), selected = "Electricity", multiple = FALSE
                                                        )
                                                    ), #options
                                                    uiOutput("monthList"),
                                                    tags$div(class = "filter",
                                                        selectizeInput(
                                                            'westLoopSide_buildingType', 'Select a Building Type: ', choices = c("All", "Commercial", "Residential", "Industrial"), selected = "All", multiple = TRUE
                                                        )
                                                    ), #options
                                                    actionButton("reset_btn", "Reset")
                                                ),
                                                tags$div(style="margin-top: 10px",
                                                    tags$div(class = "subtitle",
                                                        tags$i(class = "fas fa-search"),
                                                            "Census Plot:"
                                                    ),
                                                    tags$div(
                                                        plotOutput("block_data_plot", height = 300)
                                                    )
                                                )
                                                
                                            ),
                                            column(5,
                                                tags$div(class = "row",
                                                    column(12,
                                                        tags$div(class = "subtitle",
                                                            tags$i(class = "fas fa-map-marked-alt"),
                                                                "Map:"
                                                        ),
                                                        tags$div(style = "height: 650px",
                                                            #shinycssloaders::withSpinner(
                                                                leafletOutput("west_loop_side_map", height = 630),
                                                            #)
                                                        )
                                                    )
                                                )
                                            ),
                                            column(5,
                                                tags$div(class = "row",
                                                    column(12,
                                                        tags$div(class = "subtitle",
                                                            tags$i(class = "fas fa-map-marked-alt"),
                                                                "Electrical Table:"
                                                        ),
                                                        tags$div(style="height: 300px;",
                                                            DT::dataTableOutput("myTable_elec")
                                                        ),

                                                        tags$div(class = "subtitle",
                                                            tags$i(class = "fas fa-map-marked-alt"),
                                                                "Gas Table:"
                                                        ),
                                                        tags$div(style="height:300px;",
                                                            DT::dataTableOutput("myTable_gas")
                                                        )
                                                    )
                                                )
                                            )
                                        ) #End of fluid row
                                    ) #End of card body
                                ) #End of card
                            )
                        )
                    )
                ), #energy plants location end

                #Camparison page start
                tabPanel("Community Area Comparison", class = "p-0",
                    mainPanel(class = "panel p-0",
                        #first map start
                        column(6, class = "p-0",
                            tags$div(class = "card border-title shadow",
                                #card Start
                                tags$div(class = "card-body",
                                    tags$div(
                                        tags$div(class = "title",
                                            tags$span("First Map")
                                        )
                                    ),
                                    fluidRow(style = "margin: 2px",
                                        column(4, style = "background-color: white",
                                                tags$div(
                                                    tags$div(class = "subtitle",
                                                        tags$i(class = "fas fa-search"),
                                                            "Data Filter:"
                                                    ),
                                                    tags$div(class = "filter",
                                                        selectizeInput(
                                                            'l_area', 'Select a Data Option: ', choices = community_area_dist, selected = "Near West Side", multiple = FALSE
                                                        )
                                                    ),
                                                    #opitons
                                                    tags$div(class = "filter",
                                                        selectizeInput(
                                                            'l_option', 'Select a Data Option: ', choices = c("Electricity", "Gas", "Building Type", "Building Age", "Building Height", "Total Population"), selected = "Electricity", multiple = FALSE
                                                        )
                                                    ), #options
                                                    uiOutput("l_monthList"),
                                                    tags$div(class = "filter",
                                                        selectizeInput(
                                                            'l_buildingType', 'Select a Building Type: ', choices = c("All", "Commercial", "Residential", "Industrial"), selected = "All", multiple = TRUE
                                                        )
                                                    ), #options
                                                    actionButton("l_reset_btn", "Reset")
                                                ),
                                                tags$div(style="margin-top: 10px",
                                                    tags$div(class = "subtitle",
                                                        tags$i(class = "fas fa-search"),
                                                            "Census Plot:"
                                                    ),
                                                    tags$div(
                                                        plotOutput("l_block_data_plot", height = 250)
                                                    )
                                                )
                                                
                                            ),
                                            column(8,
                                                tags$div(class = "row",
                                                    column(12,
                                                        tags$div(class = "subtitle",
                                                            tags$i(class = "fas fa-map-marked-alt"),
                                                                "Map:"
                                                        ),
                                                        tags$div(style = "height: 650px",
                                                            #shinycssloaders::withSpinner(
                                                                leafletOutput("l_map", height = 630)
                                                            #)
                                                        )
                                                    )
                                                )
                                            )
                                    ) #End of fluid row
                                ) #End of card
                            )
                        ), #first map end
                        #second map start
                        column(6, class = "p-0",
                            tags$div(class = "card border-title shadow",
                                #card Start
                                tags$div(class = "card-body",
                                    tags$div(
                                        tags$div(
                                            class = "title",
                                            tags$span("Second Map")
                                        )
                                    ),
                                    fluidRow(style = "margin: 2px",
                                        column(4, style = "background-color: white",
                                                tags$div(
                                                    tags$div(class = "subtitle",
                                                        tags$i(class = "fas fa-search"),
                                                            "Data Filter:"
                                                    ),
                                                    tags$div(class = "filter",
                                                        selectizeInput(
                                                            'r_area', 'Select a Data Option: ', choices = community_area_dist, selected = "Loop", multiple = FALSE
                                                        )
                                                    ),
                                                    #opitons
                                                    tags$div(class = "filter",
                                                        selectizeInput(
                                                            'r_option', 'Select a Data Option: ', choices = c("Electricity", "Gas", "Building Type", "Building Age", "Building Height", "Total Population"), selected = "Electricity", multiple = FALSE
                                                        )
                                                    ), #options
                                                    uiOutput("r_monthList"),
                                                    tags$div(class = "filter",
                                                        selectizeInput(
                                                            'r_buildingType', 'Select a Building Type: ', choices = c("All", "Commercial", "Residential", "Industrial"), selected = "All", multiple = TRUE
                                                        )
                                                    ), #options
                                                    actionButton("r_reset_btn", "Reset")
                                                ),
                                                tags$div(style="margin-top: 10px",
                                                    tags$div(class = "subtitle",
                                                        tags$i(class = "fas fa-search"),
                                                            "Census Plot:"
                                                    ),
                                                    tags$div(
                                                        plotOutput("r_block_data_plot", height = 250)
                                                    )
                                                )
                                                
                                            ),
                                            column(8,
                                                tags$div(class = "row",
                                                    column(12,
                                                        tags$div(class = "subtitle",
                                                            tags$i(class = "fas fa-map-marked-alt"),
                                                                "Map:"
                                                        ),
                                                        tags$div(style = "height: 650px",
                                                            #shinycssloaders::withSpinner(
                                                                leafletOutput("rr_map", height = 630),
                                                            #)
                                                        )
                                                    )
                                                )
                                            )
                                    ) #End of fluid row
                                ) #End of card
                            )
                        ),
                    )
                ), #Camparison page end

                #whole us
                tabPanel("Energy Plants in US", class = "p-0",
                    mainPanel(class = "panel p-0",
                        fluidRow(
                            column(12, class = "p-0",
                                tags$div(class = "card border-title shadow",
                                #card Start
                                tags$div(class = "card-body",
                                    tags$div(
                                        tags$div(
                                            class = "title",
                                            tags$span("Energy Plants in US")
                                        )
                                    ),
                                    fluidRow(style = "margin: 2px",
                                            column(2, style = "background-color: white",
                                                tags$div(
                                                    tags$div(class = "subtitle",
                                                        tags$i(class = "fas fa-search"),
                                                            "Data Filter:"
                                                    ),

                                                    #opitons
                                                    tags$div(class = "filter",
                                                        selectizeInput(
                                                            't_option', 'Select a Data Option: ', choices = c("Electricity", "10% Most Electricity", "Gas", "10% Most Gas", "Building Type", "Building Age", "10% Newest Buildings", "10% Oldest Buildings", "Building Height", "10% Tallest Buildings", "Total Population", "10% Most Population", "10% Most Occupied", "10% Highest Renting Rate"), selected = "Electricity", multiple = FALSE
                                                        )
                                                    ), #options
                                                    uiOutput("t_monthList"),
                                                    tags$div(class = "filter",
                                                        selectizeInput(
                                                            't_buildingType', 'Select a Building Type: ', choices = c("All", "Commercial", "Residential", "Industrial"), selected = "All", multiple = TRUE
                                                        )
                                                    ), #options
                                                    actionButton("t_reset_btn", "Reset")
                                                ),
                                                tags$div(style="margin-top: 10px",
                                                    tags$div(class = "subtitle",
                                                        tags$i(class = "fas fa-search"),
                                                            "Census Plot:"
                                                    ),
                                                    tags$div(
                                                        plotOutput("tract_data_plot", height = 300)
                                                    )
                                                )
                                                
                                            ),
                                            column(10,
                                                tags$div(class = "row",
                                                    column(12,
                                                        tags$div(class = "subtitle",
                                                            tags$i(class = "fas fa-map-marked-alt"),
                                                                "Map:"
                                                        ),
                                                        tags$div(style = "height: 650px",
                                                            #shinycssloaders::withSpinner(
                                                                leafletOutput("t_map", height = 630),
                                                            #)
                                                        )
                                                    )
                                                )
                                            )
                                        ) #End of fluid row
                                    ) #End of card
                                )
                            )
                        )
                    )
                ), #whole us

                #idle & new
                tabPanel("Idle & New Plants", class = "p-0",
                    mainPanel(class = "panel p-0",
                        fluidRow(
                            column(12, class = "p-0",
                                tags$div(class = "card border-title shadow",
                                    #card Start
                                    tags$div(class = "card-body",
                                        tags$div(
                                            column(2, class = "p-0",
                                                tags$div(
                                                    class = "title",
                                                    tags$span("Idle & New Plants")
                                                )
                                            ),
                                            column(2,
                                                tags$div(
                                                    tags$div(class = "filter cust-text",
                                                        #selectizeInput(
                                                        #    'yearInput_in', 'Select a year: ', choices = c(2010, 2018), selected = "2018", multiple = FALSE
                                                        #)
                                                    )
                                                )
                                            ),
                                            column(10, tags$div())
                                        ),
                                        fluidRow(style = "margin: 2px",
                                            column(2, style = "background-color: white",
                                                tags$div(
                                                    tags$div(class = "subtitle",
                                                        tags$i(class = "fas fa-search"),
                                                            "Data Filter:"
                                                    ),
                                                    #plants filter start
                                                    tags$div(class = "filter",
                                                        #checkboxGroupInput("sourceInput_in", "Only show: ", choices = c(source_idle_new))
                                                    ), #plants filter end
                                                    tags$div(class = "filter",
                                                        #checkboxGroupInput("energySourceInput_in", "Energy source: ", choices = c(energySource_dist))
                                                    ), #energy source filter end

                                                    #actionButton("reset_in", "Reset view")        
                                                )
                                            ),
                                            column(10,
                                                tags$div(class = "row",
                                                    column(12,
                                                        tags$div(class = "subtitle",
                                                            tags$i(class = "fas fa-map-marked-alt"),
                                                                "Map:"
                                                        ),
                                                        tags$div(style = "height: 630px",
                                                            #shinycssloaders::withSpinner(
                                                            #    leafletOutput("leaf_in", height = 630),
                                                            #)
                                                        )
                                                    )
                                                )
                                            )
                                        ) #End of fluid row
                                    ) #End of card body
                                ) #End of card
                            )
                        )
                    )
                ), #idle & new

                #About page start
                tabPanel("About", class = "p-0",
                    mainPanel(class = "panel p-0",
                        fluidRow(
                            #Total Amount of Energy generation start
                                column(12, class = "p-0",
                                    tags$div(class = "card border-title shadow",
                                        #card Start
                                        tags$div(class = "card-body",
                                            tags$div(class = "title",
                                                tags$span("About")
                                        ),
                                        tags$div(class = "p-5",
                                             tags$div(
                                                tags$span(class = "cust-text-md", "Author: "),
                                                tags$span("Yi-Chun Chen")
                                            ),
                                            tags$div(
                                                tags$span(class = "cust-text-md", "Date: "),
                                                tags$span("03.15.2020")
                                            ),
                                            tags$div(
                                                tags$span(class = "cust-text-md", "Data Source: "),
                                                tags$a(href = "https://www.epa.gov/egrid/download-data", "https://www.epa.gov/egrid/download-data"),
                                                tags$br(),
                                                tags$span(" 2018: eGRID2018v2 Data File (XLSX), eGRID2000_plant.xls file and its EGRDPLNT00 tab"),
                                                tags$br(), 
                                                tags$span(" 2010: Download eGRID historical files (1996-2016) (ZIP), eGRID2010_Data.xls file/EGRDPLNT10 tab"),
                                                tags$br(), 
                                                tags$span(" 2000: Download eGRID historical files (1996-2016) (ZIP), eGRID2000_plant.xls file/EGRDPLNT00 tab")
                                            ),
                                            tags$div(
                                                tags$span(class = "cust-text-md", "Git Repository: "),
                                                tags$a(href = "https://github.com/ychen856/cs424_project_2.git", "https://github.com/ychen856/cs424_project_2.git")
                                            )
                                        ),
                                    )
                                )
                            )
                        )
                    )
                ) #About page end
            )
        )
    )





