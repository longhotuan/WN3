#### Global R ####

library(knitr)
library(rmarkdown)
library(tidyverse)
library(reshape)
library(plotly)
library(flexdashboard)
library(shiny)
library(data.table)
library(lubridate)
library(shinythemes)
library(shinydashboard)
library(tweenr)
library(leaflet)
library(leaflet.minicharts)
library(RColorBrewer)
library(rsconnect)
library(DT)
library(rworldmap)
library(usethis)

load("WN3_v2.RData")

# 3 pages: general info: table + map, journal info: Top 20 journal + open-access , research info: Citation year, top keywords
# + 6 value boxes: Total of number publication, total countries, total document types, total cites, total authors, percentage of open-access

#### ui ####


ui <- dashboardPage(
    # Dashboard header ####
    dashboardHeader(title="Water Nexus dashboard"),
    # Dashboard sidebar #### 
    dashboardSidebar(
        sidebarMenu(id="tab",
                    menuItem("About", 
                             tabName = "about",
                             icon = icon("info")),
                    menuItem("Project info", 
                             tabName = "info",
                             icon = icon("list-ol")), 
                    menuItem("Cooperation", 
                             tabName = "coop",
                             icon = icon("handshake")),
                    selectInput(inputId = "country", label = "Select a country", 
                                choices = c(All = "All",levels(Water_Nexus$COUNTRY))),
                    selectInput(inputId = "year", label = "Select the first year",
                                choices = c(All = "All",levels(as.factor(Water_Nexus$X1st.year.exp))))
        )
    ),
    # Dashboard body #### 
    dashboardBody(
        tabItems(
            # Info tab content ####
            tabItem(tabName = "info",
                    fluidRow(
                        valueBoxOutput("project"),
                        valueBoxOutput("money"),
                        valueBoxOutput("period")
                    ),
                    fluidRow(
                        valueBoxOutput("project"),
                        valueBoxOutput("money"),
                        valueBoxOutput("period")
                    ),
                    fluidRow(
                        box(title = "Project info", width = 12, height = 700,
                            DT::dataTableOutput("table"
                                                ,  width = "100%", height = 700
                            )
                        )
                    ),
                    fluidRow(
                        box(title = "Project map", width = 12, 
                            leafletOutput("map", width = "100%", height = 400)
                        )
                    )
            ), # end tabItem of info tab
            # Cooperation tab content ####
            tabItem(tabName = "coop",
                    fluidRow(
                        valueBoxOutput("project1"),
                        valueBoxOutput("money1"),
                        valueBoxOutput("period1")
                    ),
                    fluidRow(
                        box(title = "Type of cooperation", width = 12,
                            plotlyOutput("coop")
                        )
                    ),
                    fluidRow(
                        box(title = "Contractors", width = 12,
                            plotlyOutput("contractor")
                        )
                    )
            ), # end of cooperation tab content
            # Actor tab content ####
            tabItem(tabName = "actor",
                    fluidRow(
                        valueBoxOutput("project2"),
                        valueBoxOutput("money2"),
                        valueBoxOutput("period2")
                    ),
                    fluidRow(
                        box(title = "Funding Actors", width = 12,
                            plotlyOutput("budgetholder")
                        )
                    ),
                    fluidRow(
                        box(title = "Funding revolution", width = 12,
                            plotlyOutput("allocation")
                        )
                    )
            ), # end tabItem of actor tab
            # Budget-holders tab content ####
            tabItem(tabName = "budget",
                    fluidRow(
                        valueBoxOutput("project3"),
                        valueBoxOutput("money3"),
                        valueBoxOutput("period3")
                    ),
                    fluidRow(
                        box(title = "Aid category", width = 12,
                            plotlyOutput("aid")
                        )
                    ),
                    fluidRow(
                        box(title = "Budget category", width = 12,
                            plotlyOutput("budget")
                        )
                    )
            ), # end tabItem of budget tab
            # About tab content ####
            tabItem(tabName = "about",
                    fluidRow(
                        box(width = 12, 
                            h2("About the dashboard"),
                            hr(),
                            h3("Projects in the Water Sector by Belgian Actors"),
                            br(),
                            h4("The Water Projects Dashboard is an interactive platform centralizing and displaying information about the water-related projects led by Belgian actors. This platform is  dynamic and aim to incorporate upcoming projects. So far, the platform mostly inventories the projects funded by the Belgian Ministry of Foreign Affairs, Development Cooperation and Humanitarian Aid (DGD) from 1998 to present days. However, we are building a broader database that includes projects funded by other funding organisms.")
                        )
                    ),
                    fluidRow(
                        box(width = 12, 
                            h2("About the dataset"),
                            hr(),
                            h3("Dataset of Directorate-General for Development Cooperation and Humanitarian Aid"),
                            br(),
                            h4("The dataset of Directorate-General for Development Cooperation and Humanitarian Aid (DGD) contains 12550 projects in total from 1987 to 2018. The dataset mainly focuses on projects in the water sector. As such, projects related to including environment, agriculture, fisheries, forestry, and hydroelectricity are also included. The dataset includes 191 attributes which are characteristics of any projects that have cooperation with DGD. The attributes cover from basic information of the projects, e.g. title, year, period, etc., to specific properties of the projects, i.e. scale of their involvement with respect to Sustainable Development Goals (SDGs), target groups, reached results, etc."),
                            br(),
                            h4("Besides this dataset, a broader database that includes projects funded by other funding organizations, such as VLIR-UOS, ARES, VPWvO, Enabel, etc., is being developed. If you want to add the information about the projects funded/implemented by your organisation, please send us an email to: ",
                               a("waternexusbelgium@gmail.com",
                                 href = "mailto: waternexusbelgium@gmail.com"))
                        )
                    ),
                    fluidRow(
                        column(6,
                               h1("Funded by"),
                               img(style = "max-width:50%",
                                   src = "Logo2.jpg")
                        ),
                        column(6, 
                               img(align = "left|bottom",
                                   style = "max-width:50%",
                                   src = "Logo.jpg") 
                        )
                    )
            ) # end about tabItem
        ) # end tabItems
    ) # end dashboardbody
) # end dashboardpage
