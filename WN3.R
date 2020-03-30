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
library(ggwordcloud)
library(colorspace)
library(tableHTML)
library(feather)


water_nexus3 <- read_feather("water_nexus.feather") 
first_country <- which(colnames(water_nexus3) == "Albania")
last_country <- which (colnames(water_nexus3) == "Zimbabwe")
first_lat <- which(colnames(water_nexus3) == "lat_Albania")
last_lat <- which (colnames(water_nexus3) == "lat_Zimbabwe")
first_long <- which (colnames(water_nexus3) == "long_Albania")
last_long <- which (colnames(water_nexus3) == "long_Zimbabwe")
first_target <-which(colnames(water_nexus3) == "Target 6.1")
last_target <-which(colnames(water_nexus3) == "Target 6.b")

water_nexus3$`Document Type`[which(is.na(water_nexus3$`Document Type`))] <- levels(water_nexus3$`Document Type`)[1]

water_nexus3$`Document Type` <- as.character(water_nexus3$`Document Type`)
water_nexus3$`Source title` <- as.character(water_nexus3$`Source title`)


# Time divided into three periods 
# add global south into country
# Selection: first select goals and targets by creating new columns 
# three filter: 
# - targets
# - country
# - year 
# 3 pages: general info: table + map, journal info: Top 20 journal + open-access, research info: Citation year, top keywords
# 6 value boxes: Total of number publication, total countries, total document types, total cites, total authors, percentage of open-access

#### ui ####

ui <- dashboardPage(skin = 'red',
    # Dashboard header ####
    dashboardHeader(title="Water Nexus dashboard"),
    # Dashboard sidebar #### 
    dashboardSidebar(
        sidebarMenu(id="tab",
                    menuItem("About", 
                             tabName = "about",
                             icon = icon("info")),
                    menuItem("Overview", 
                             tabName = "info",
                             icon = icon("database")), 
                    menuItem("Journal info", 
                             tabName = "journal",
                             icon = icon("newspaper")),
                    menuItem("Publication info", 
                             tabName = "research",
                             icon = icon("microscope")),
                    selectInput(inputId = "target", label = "Select a target", 
                                choices = c(All = "All", colnames(water_nexus3[, first_target:last_target]))),
                    selectInput(inputId = "country", label = "Select a country", 
                                choices = c(All = "All", "Global South", colnames(water_nexus3[, first_country:last_country]))),
                    selectInput(inputId = "year", label = "Select a year",
                                choices = c(All = "All", "From 2010 to 2019", "From 2000 to 2009", "Before 2000", levels(as.factor(water_nexus3$Year))))
        )
    ),
    # Dashboard body #### 
    dashboardBody(
        tabItems(
            # About tab content ####
            tabItem(tabName = "about",
                    fluidRow(
                        box(width = 12, 
                            h2("About the dashboard"),
                            hr(),
                            h3("Water-Related Research conducted by Belgian Actors"),
                            br(),
                            h4("The Water Research Dashboard is an interactive platform centralizing, displaying and examining information about the water-related research that have involved at least one Belgian actor over the 2009 – 2019 period. 
                               To analyze and evaluate the role of science, technology, and innovation in support of the development of the  water sector in Belgium, a bibliometric analysis was applied to investigate systematically publications targeting water sector with a wide spectrum of complementary expertise, from integrated water management, sanitation and hygiene, policy support for sustainable transitions natural resources governance and legal aspects, private sector development and valorization of research to data analytics and decision support systems for water and environmental management.
                               Bibliometrics was first presented by Pritchard (1969), in which quantitative analyses and statistical measurements were applied on publications in order to gain a systematic, transparent, and reproducible review on the existing knowledge base, from that, allowed advancing research lines.")
                            )
                    ),
                    fluidRow(
                        box(width = 12, 
                            h2("About the dataset"),
                            hr(),
                            h3("Bibliometrix analysis of water research conducted by Belgian Actors"),
                            br(),
                            h4("Bibliographic data was collected on September 24, 2019, on the Scopus website (",
                               a("www.scopus.com", href = "https://www.scopus.com"),
                            "). Scopus database contains the largest international abstract and citation collection of peer-reviewed scientific literature. 
                            Scopus currently indexes 22,800 titles (journals, magazines, reports) from more than 5,000 international publishers 
                               We adapted the list of keywords that can be found in the water-related research. This list was proposed by",
                               a("Mehmood (2019)", href = "https://inweh.unu.edu/bibliometrics-of-water-research/"),
                               "in the United Nations University-INWEH 2019. 
                               The final queries of 248 keywords were applied to download the citations and bibliographies directly in Scopus website as well as in open-source statistical software R using rscopus package. 
                               All types of publications were assessed for the following characteristics: document types and languages, publication outputs, research categories, authors, journals, countries, institutions, and keywords.
                               ")
                        )
                    ),
                    fluidRow(
                        column(6,
                               h2("Funded by"),
                               img(style = "max-width:50%",
                                   src = "Logo2.jpg")
                        ),
                        column(6,
                               img(align = "left|bottom",
                                   style = "max-width:50%",
                                   src = "Logo.jpg") 
                        )
                    ),
                    fluidRow(
                        column(6,
                               h2("Through"),
                               box(
                               img(style = "max-width:100%",
                                   src = "Logo3.jpg")
                               ),
                               box(
                               img(style = "max-width:100%",
                                   src = "Logo4.png")
                               )
                        )
                    )
            ), # end of About tabItem
            # Info tab content ####
            tabItem(tabName = "info",
                    fluidRow(
                        valueBoxOutput("Publication"),
                        valueBoxOutput("TotalCountry"),
                        valueBoxOutput("Citation")
                    ),
                    fluidRow(
                        valueBoxOutput("DocumentType"),
                        valueBoxOutput("OpenAccess"),
                        valueBoxOutput("Totaljournal")
                    ),
                    fluidRow(
                        box(title = "Research info", width = 12, height = 700,
                            DT::dataTableOutput("table"
                                                ,  width = "100%", height = 700
                            )
                        )
                    ),
                    fluidRow(
                        box(title = "Collaboration map (circles' area proportional to the number of publications)", width = 12, 
                            leafletOutput("map", width = "100%", height = 400) # per document types
                        )
                    )
            ), # end of info tabItem
            # Journal tab content ####
            tabItem(tabName = "journal",
                    fluidRow(
                        valueBoxOutput("Publication1"),
                        valueBoxOutput("TotalCountry1"),
                        valueBoxOutput("Citation1")
                    ),
                    fluidRow(
                        valueBoxOutput("DocumentType1"),
                        valueBoxOutput("OpenAccess1"),
                        valueBoxOutput("Totaljournal1")
                    ),
                    fluidRow(
                        box(width = 12,
                            title = "Top most frequent journals",
                            plotlyOutput("topjournal")
                        )
                    ),
                    fluidRow(
                        box(width = 12,
                            title = "Open Access", 
                            plotlyOutput("Openaccess")
                        )
                    )
            ), # end of Journal tabItem
            # Research tab content ####
            tabItem(tabName = "research",
                    fluidRow(
                        valueBoxOutput("Publication2"),
                        valueBoxOutput("TotalCountry2"),
                        valueBoxOutput("Citation2")
                    ),
                    fluidRow(
                        valueBoxOutput("DocumentType2"),
                        valueBoxOutput("OpenAccess2"),
                        valueBoxOutput("Totaljournal2")
                    ),
                    fluidRow(
                        box(title = "Publication Year", width = 12,
                            plotlyOutput("Pubyear")
                        )
                    ),
                    fluidRow(
                        box(title = "Top keywords", width = 12,
                            plotOutput("Topkw")
                        )
                    )
            ) # end tabItem of research tab
        ) # end tabItems
    ) # end dashboardbody
) # end dashboardpage

#### server ####

server <- function(input, output, session) {
    # Setting reactivities ####
    df <- reactive({water_nexus3})
    #** Target #### 
    df_target <- reactive({
        input$target
    })
    #** Country #### 
    
    choose_country <- reactive({
        if(df_target() == "All"){
            colnames(df()[, first_country:last_country])
        } else {
            country_1 <- df()[!is.na(df()[, colnames(df()) == df_target()]), ]
            f_country <- which(colnames(df()) == "Access Type")+1
            l_country <- str_which(colnames(df()), "lat")[1]-1
            colnames(df()[, f_country:l_country])
        }
    })
    
    observe({
        updateSelectInput(session, inputId = "country", label = "Select a country",
                          choices = c(All = "All", "Global South", choose_country()))
    })
    
    df_country <- reactive({
        input$country
    })
    
    #** Year ####
    
    chosen_year <- reactive({
        if(df_target() == "All"){
            target_name <- df()
            if(df_country() == "All"){
                levels(as.factor(target_name$Year))
            } else if(df_country() == "Global South"){
                country_name <- target_name[!is.na(target_name$`Global South`), ]
                year2 <- levels(as.factor(country_name$Year))
                year2
                
            } else {
                year_1 <- target_name[!is.na(target_name[, colnames(target_name) == df_country()]),]
                year_2 <- levels(as.factor(year_1$Year))
                year_2
            }
        } else {
            target_name <- df()[!is.na(df()[, colnames(df()) == df_target()]), ]
            if(df_country() == "All"){
                levels(as.factor(target_name$Year))
            } else if(df_country() == "Global South"){
                country_name <- target_name[!is.na(target_name$`Global South`), ]
                year2 <- levels(as.factor(country_name$Year))
                year2
                
            } else {
                year_1 <- target_name[!is.na(target_name[, colnames(target_name) == df_country()]),]
                year_2 <- levels(as.factor(year_1$Year))
                year_2
            }
        }
    })
    
    observe({
        updateSelectInput(session, inputId = "year", label = "Select a year",
                          choices = c(All = "All", "From 2010 to 2019", "From 2000 to 2009", "Before 2000", chosen_year()))
    })
    
    df_year <- reactive({
        input$year
    })
    
    # Output valuebox in Info tab ####
    output$Publication <- renderValueBox({
        
        if (df_target() == "All"){
            target_chosen <- df()
            if(df_country() == "All"){
                country_chosen <- target_chosen
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            } else if (df_country() == "Global South"){
                country_chosen <- target_chosen[!is.na(target_chosen$`Global South`), ]
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            } else {
                country_chosen <- target_chosen[!is.na(target_chosen[, colnames(target_chosen) == df_country()]),]
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            }
        } else { 
            target_chosen <- df()[!is.na(df()[, colnames(df()) == df_target()]), ]
            if(df_country() == "All"){
                country_chosen <- target_chosen
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            } else if (df_country() == "Global South"){
                country_chosen <- target_chosen[!is.na(target_chosen$`Global South`), ]
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            } else {
                country_chosen <- target_chosen[!is.na(target_chosen[, colnames(target_chosen) == df_country()]),]
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            }
        }
        
        valueBox(
            value = prettyNum(nrow(selecteddata), big.mark = ","),
            subtitle = "Total number of publication",
            icon = icon("newspaper"),
            color = "purple"
        )
    })
    output$TotalCountry <- renderValueBox({
        
        if (df_target() == "All"){
            target_chosen <- df()
            if(df_country() == "All"){
                country_chosen <- target_chosen
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            } else if (df_country() == "Global South"){
                country_chosen <- target_chosen[!is.na(target_chosen$`Global South`), ]
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            } else {
                country_chosen <- target_chosen[!is.na(target_chosen[, colnames(target_chosen) == df_country()]),]
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            }
        } else { 
            target_chosen <- df()[!is.na(df()[, colnames(df()) == df_target()]), ]
            if(df_country() == "All"){
                country_chosen <- target_chosen
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            } else if (df_country() == "Global South"){
                country_chosen <- target_chosen[!is.na(target_chosen$`Global South`), ]
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            } else {
                country_chosen <- target_chosen[!is.na(target_chosen[, colnames(target_chosen) == df_country()]),]
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            }
        }
        selecteddata_v3 <- selecteddata[,first_country:last_country][, colSums(is.na(selecteddata[,first_country:last_country])) < nrow(selecteddata[,first_country:last_country])]
        valueBox(
            value = ncol(selecteddata_v3),
            subtitle = "Total number of country",
            icon = icon("flag"),
            color = "yellow"
        )
    })
    output$Citation <- renderValueBox({
        
        if (df_target() == "All"){
            target_chosen <- df()
            if(df_country() == "All"){
                country_chosen <- target_chosen
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            } else if (df_country() == "Global South"){
                country_chosen <- target_chosen[!is.na(target_chosen$`Global South`), ]
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            } else {
                country_chosen <- target_chosen[!is.na(target_chosen[, colnames(target_chosen) == df_country()]),]
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            }
        } else { 
            target_chosen <- df()[!is.na(df()[, colnames(df()) == df_target()]), ]
            if(df_country() == "All"){
                country_chosen <- target_chosen
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            } else if (df_country() == "Global South"){
                country_chosen <- target_chosen[!is.na(target_chosen$`Global South`), ]
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            } else {
                country_chosen <- target_chosen[!is.na(target_chosen[, colnames(target_chosen) == df_country()]),]
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            }
        }
        
        valueBox(
            value =  prettyNum(sum(selecteddata$`Cited by`, na.rm = TRUE), big.mark = ","),
            subtitle = "Total number of citation",
            icon = icon("file-signature"),
            color = "blue"
        )
    })
    output$DocumentType <- renderValueBox({
        
        if (df_target() == "All"){
            target_chosen <- df()
            if(df_country() == "All"){
                country_chosen <- target_chosen
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            } else if (df_country() == "Global South"){
                country_chosen <- target_chosen[!is.na(target_chosen$`Global South`), ]
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            } else {
                country_chosen <- target_chosen[!is.na(target_chosen[, colnames(target_chosen) == df_country()]),]
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            }
        } else { 
            target_chosen <- df()[!is.na(df()[, colnames(df()) == df_target()]), ]
            if(df_country() == "All"){
                country_chosen <- target_chosen
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            } else if (df_country() == "Global South"){
                country_chosen <- target_chosen[!is.na(target_chosen$`Global South`), ]
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            } else {
                country_chosen <- target_chosen[!is.na(target_chosen[, colnames(target_chosen) == df_country()]),]
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            }
        }
        
        valueBox(
            value = length(levels(as.factor(selecteddata$`Document Type`))),
            subtitle = "Document type",
            icon = icon("folder-open"),
            color = "red"
        )
    })
    output$OpenAccess <- renderValueBox({
        
        if (df_target() == "All"){
            target_chosen <- df()
            if(df_country() == "All"){
                country_chosen <- target_chosen
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            } else if (df_country() == "Global South"){
                country_chosen <- target_chosen[!is.na(target_chosen$`Global South`), ]
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            } else {
                country_chosen <- target_chosen[!is.na(target_chosen[, colnames(target_chosen) == df_country()]),]
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            }
        } else { 
            target_chosen <- df()[!is.na(df()[, colnames(df()) == df_target()]), ]
            if(df_country() == "All"){
                country_chosen <- target_chosen
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            } else if (df_country() == "Global South"){
                country_chosen <- target_chosen[!is.na(target_chosen$`Global South`), ]
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            } else {
                country_chosen <- target_chosen[!is.na(target_chosen[, colnames(target_chosen) == df_country()]),]
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            }
        }
        
        valueBox(
            value = round(sum(!is.na(selecteddata$`Access Type`))*100/(sum(is.na(selecteddata$`Access Type`))+sum(!is.na(selecteddata$`Access Type`))), digits = 2),
            subtitle = "Percentage of open-access publication",
            icon = icon("lock-open"),
            color = "purple"
        )
    })
    output$Totaljournal <- renderValueBox({
        
        if (df_target() == "All"){
            target_chosen <- df()
            if(df_country() == "All"){
                country_chosen <- target_chosen
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            } else if (df_country() == "Global South"){
                country_chosen <- target_chosen[!is.na(target_chosen$`Global South`), ]
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            } else {
                country_chosen <- target_chosen[!is.na(target_chosen[, colnames(target_chosen) == df_country()]),]
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            }
        } else { 
            target_chosen <- df()[!is.na(df()[, colnames(df()) == df_target()]), ]
            if(df_country() == "All"){
                country_chosen <- target_chosen
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            } else if (df_country() == "Global South"){
                country_chosen <- target_chosen[!is.na(target_chosen$`Global South`), ]
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            } else {
                country_chosen <- target_chosen[!is.na(target_chosen[, colnames(target_chosen) == df_country()]),]
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            }
        }
        
        valueBox(
            value = prettyNum(nlevels(as.factor(selecteddata$`Source title`)), big.mark = ","),
            subtitle = "Total number of journal",
            icon = icon("book"),
            color = "maroon"
        )
    })
    # Output valuebox in Journal tab ####
    output$Publication1 <- renderValueBox({

        if (df_target() == "All"){
            target_chosen <- df()
            if(df_country() == "All"){
                country_chosen <- target_chosen
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            } else if (df_country() == "Global South"){
                country_chosen <- target_chosen[!is.na(target_chosen$`Global South`), ]
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            } else {
                country_chosen <- target_chosen[!is.na(target_chosen[, colnames(target_chosen) == df_country()]),]
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            }
        } else { 
            target_chosen <- df()[!is.na(df()[, colnames(df()) == df_target()]), ]
            if(df_country() == "All"){
                country_chosen <- target_chosen
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            } else if (df_country() == "Global South"){
                country_chosen <- target_chosen[!is.na(target_chosen$`Global South`), ]
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            } else {
                country_chosen <- target_chosen[!is.na(target_chosen[, colnames(target_chosen) == df_country()]),]
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            }
        }

        valueBox(
            value = prettyNum(nrow(selecteddata), big.mark = ","),
            subtitle = "Total number of publication",
            icon = icon("newspaper"),
            color = "purple"
        )
    })
    output$TotalCountry1 <- renderValueBox({

        if (df_target() == "All"){
            target_chosen <- df()
            if(df_country() == "All"){
                country_chosen <- target_chosen
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            } else if (df_country() == "Global South"){
                country_chosen <- target_chosen[!is.na(target_chosen$`Global South`), ]
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            } else {
                country_chosen <- target_chosen[!is.na(target_chosen[, colnames(target_chosen) == df_country()]),]
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            }
        } else { 
            target_chosen <- df()[!is.na(df()[, colnames(df()) == df_target()]), ]
            if(df_country() == "All"){
                country_chosen <- target_chosen
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            } else if (df_country() == "Global South"){
                country_chosen <- target_chosen[!is.na(target_chosen$`Global South`), ]
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            } else {
                country_chosen <- target_chosen[!is.na(target_chosen[, colnames(target_chosen) == df_country()]),]
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            }
        }

        selecteddata_v3 <- selecteddata[,first_country:last_country][, colSums(is.na(selecteddata[,first_country:last_country])) < nrow(selecteddata[,first_country:last_country])]
        valueBox(
            value =ncol(selecteddata_v3),
            subtitle = "Total number of country",
            icon = icon("flag"),
            color = "yellow"
        )
    })
    output$Citation1 <- renderValueBox({

        if (df_target() == "All"){
            target_chosen <- df()
            if(df_country() == "All"){
                country_chosen <- target_chosen
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            } else if (df_country() == "Global South"){
                country_chosen <- target_chosen[!is.na(target_chosen$`Global South`), ]
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            } else {
                country_chosen <- target_chosen[!is.na(target_chosen[, colnames(target_chosen) == df_country()]),]
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            }
        } else { 
            target_chosen <- df()[!is.na(df()[, colnames(df()) == df_target()]), ]
            if(df_country() == "All"){
                country_chosen <- target_chosen
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            } else if (df_country() == "Global South"){
                country_chosen <- target_chosen[!is.na(target_chosen$`Global South`), ]
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            } else {
                country_chosen <- target_chosen[!is.na(target_chosen[, colnames(target_chosen) == df_country()]),]
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            }
        }

        valueBox(
            value = prettyNum(sum(selecteddata$`Cited by`, na.rm = TRUE), big.mark = ","),
            subtitle = "Total number of citation",
            icon = icon("file-signature"),
            color = "blue"
        )
    })
    output$DocumentType1 <- renderValueBox({

        if (df_target() == "All"){
            target_chosen <- df()
            if(df_country() == "All"){
                country_chosen <- target_chosen
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            } else if (df_country() == "Global South"){
                country_chosen <- target_chosen[!is.na(target_chosen$`Global South`), ]
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            } else {
                country_chosen <- target_chosen[!is.na(target_chosen[, colnames(target_chosen) == df_country()]),]
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            }
        } else { 
            target_chosen <- df()[!is.na(df()[, colnames(df()) == df_target()]), ]
            if(df_country() == "All"){
                country_chosen <- target_chosen
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            } else if (df_country() == "Global South"){
                country_chosen <- target_chosen[!is.na(target_chosen$`Global South`), ]
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            } else {
                country_chosen <- target_chosen[!is.na(target_chosen[, colnames(target_chosen) == df_country()]),]
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            }
        }

        valueBox(
            value = length(levels(as.factor(selecteddata$`Document Type`))),
            subtitle = "Document type",
            icon = icon("folder-open"),
            color = "red"
        )
    })
    output$OpenAccess1 <- renderValueBox({

        if (df_target() == "All"){
            target_chosen <- df()
            if(df_country() == "All"){
                country_chosen <- target_chosen
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            } else if (df_country() == "Global South"){
                country_chosen <- target_chosen[!is.na(target_chosen$`Global South`), ]
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            } else {
                country_chosen <- target_chosen[!is.na(target_chosen[, colnames(target_chosen) == df_country()]),]
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            }
        } else { 
            target_chosen <- df()[!is.na(df()[, colnames(df()) == df_target()]), ]
            if(df_country() == "All"){
                country_chosen <- target_chosen
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            } else if (df_country() == "Global South"){
                country_chosen <- target_chosen[!is.na(target_chosen$`Global South`), ]
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            } else {
                country_chosen <- target_chosen[!is.na(target_chosen[, colnames(target_chosen) == df_country()]),]
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            }
        }

        valueBox(
            value = round(sum(!is.na(selecteddata$`Access Type`))*100/(sum(is.na(selecteddata$`Access Type`))+sum(!is.na(selecteddata$`Access Type`))), digits = 2),
            subtitle = "Percentage of open-access publication",
            icon = icon("lock-open"),
            color = "purple"
        )
    })
    output$Totaljournal1 <- renderValueBox({

        if (df_target() == "All"){
            target_chosen <- df()
            if(df_country() == "All"){
                country_chosen <- target_chosen
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            } else if (df_country() == "Global South"){
                country_chosen <- target_chosen[!is.na(target_chosen$`Global South`), ]
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            } else {
                country_chosen <- target_chosen[!is.na(target_chosen[, colnames(target_chosen) == df_country()]),]
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            }
        } else { 
            target_chosen <- df()[!is.na(df()[, colnames(df()) == df_target()]), ]
            if(df_country() == "All"){
                country_chosen <- target_chosen
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            } else if (df_country() == "Global South"){
                country_chosen <- target_chosen[!is.na(target_chosen$`Global South`), ]
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            } else {
                country_chosen <- target_chosen[!is.na(target_chosen[, colnames(target_chosen) == df_country()]),]
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            }
        }

        valueBox(
            value = prettyNum(nlevels(as.factor(selecteddata$`Source title`)), big.mark = ","),
            subtitle = "Total number of journal",
            icon = icon("book"),
            color = "maroon"
        )
    })
    # Output valuebox in Research tab ####
    output$Publication2 <- renderValueBox({

        if (df_target() == "All"){
            target_chosen <- df()
            if(df_country() == "All"){
                country_chosen <- target_chosen
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            } else if (df_country() == "Global South"){
                country_chosen <- target_chosen[!is.na(target_chosen$`Global South`), ]
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            } else {
                country_chosen <- target_chosen[!is.na(target_chosen[, colnames(target_chosen) == df_country()]),]
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            }
        } else { 
            target_chosen <- df()[!is.na(df()[, colnames(df()) == df_target()]), ]
            if(df_country() == "All"){
                country_chosen <- target_chosen
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            } else if (df_country() == "Global South"){
                country_chosen <- target_chosen[!is.na(target_chosen$`Global South`), ]
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            } else {
                country_chosen <- target_chosen[!is.na(target_chosen[, colnames(target_chosen) == df_country()]),]
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            }
        }

        valueBox(
            value = prettyNum(nrow(selecteddata), big.mark = ","),
            subtitle = "Total number of publication",
            icon = icon("newspaper"),
            color = "purple"
        )
    })
    output$TotalCountry2 <- renderValueBox({

        if (df_target() == "All"){
            target_chosen <- df()
            if(df_country() == "All"){
                country_chosen <- target_chosen
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            } else if (df_country() == "Global South"){
                country_chosen <- target_chosen[!is.na(target_chosen$`Global South`), ]
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            } else {
                country_chosen <- target_chosen[!is.na(target_chosen[, colnames(target_chosen) == df_country()]),]
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            }
        } else { 
            target_chosen <- df()[!is.na(df()[, colnames(df()) == df_target()]), ]
            if(df_country() == "All"){
                country_chosen <- target_chosen
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            } else if (df_country() == "Global South"){
                country_chosen <- target_chosen[!is.na(target_chosen$`Global South`), ]
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            } else {
                country_chosen <- target_chosen[!is.na(target_chosen[, colnames(target_chosen) == df_country()]),]
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            }
        }

        selecteddata_v3 <- selecteddata[,first_country:last_country][, colSums(is.na(selecteddata[,first_country:last_country])) < nrow(selecteddata[,first_country:last_country])]
        valueBox(
            value =ncol(selecteddata_v3),
            subtitle = "Total number of country",
            icon = icon("flag"),
            color = "yellow"
        )
    })
    output$Citation2 <- renderValueBox({

        if (df_target() == "All"){
            target_chosen <- df()
            if(df_country() == "All"){
                country_chosen <- target_chosen
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            } else if (df_country() == "Global South"){
                country_chosen <- target_chosen[!is.na(target_chosen$`Global South`), ]
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            } else {
                country_chosen <- target_chosen[!is.na(target_chosen[, colnames(target_chosen) == df_country()]),]
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            }
        } else { 
            target_chosen <- df()[!is.na(df()[, colnames(df()) == df_target()]), ]
            if(df_country() == "All"){
                country_chosen <- target_chosen
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            } else if (df_country() == "Global South"){
                country_chosen <- target_chosen[!is.na(target_chosen$`Global South`), ]
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            } else {
                country_chosen <- target_chosen[!is.na(target_chosen[, colnames(target_chosen) == df_country()]),]
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            }
        }

        valueBox(
            value = prettyNum(sum(selecteddata$`Cited by`, na.rm = TRUE), big.mark = ","),
            subtitle = "Total number of citation",
            icon = icon("file-signature"),
            color = "blue"
        )
    })
    output$DocumentType2 <- renderValueBox({

        if (df_target() == "All"){
            target_chosen <- df()
            if(df_country() == "All"){
                country_chosen <- target_chosen
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            } else if (df_country() == "Global South"){
                country_chosen <- target_chosen[!is.na(target_chosen$`Global South`), ]
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            } else {
                country_chosen <- target_chosen[!is.na(target_chosen[, colnames(target_chosen) == df_country()]),]
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            }
        } else { 
            target_chosen <- df()[!is.na(df()[, colnames(df()) == df_target()]), ]
            if(df_country() == "All"){
                country_chosen <- target_chosen
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            } else if (df_country() == "Global South"){
                country_chosen <- target_chosen[!is.na(target_chosen$`Global South`), ]
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            } else {
                country_chosen <- target_chosen[!is.na(target_chosen[, colnames(target_chosen) == df_country()]),]
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            }
        }

        valueBox(
            value = length(levels(as.factor(selecteddata$`Document Type`))),
            subtitle = "Document type",
            icon = icon("folder-open"),
            color = "red"
        )
    })
    output$OpenAccess2 <- renderValueBox({

        if (df_target() == "All"){
            target_chosen <- df()
            if(df_country() == "All"){
                country_chosen <- target_chosen
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            } else if (df_country() == "Global South"){
                country_chosen <- target_chosen[!is.na(target_chosen$`Global South`), ]
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            } else {
                country_chosen <- target_chosen[!is.na(target_chosen[, colnames(target_chosen) == df_country()]),]
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            }
        } else { 
            target_chosen <- df()[!is.na(df()[, colnames(df()) == df_target()]), ]
            if(df_country() == "All"){
                country_chosen <- target_chosen
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            } else if (df_country() == "Global South"){
                country_chosen <- target_chosen[!is.na(target_chosen$`Global South`), ]
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            } else {
                country_chosen <- target_chosen[!is.na(target_chosen[, colnames(target_chosen) == df_country()]),]
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            }
        }

        valueBox(
            value = round(sum(!is.na(selecteddata$`Access Type`))*100/(sum(is.na(selecteddata$`Access Type`))+sum(!is.na(selecteddata$`Access Type`))), digits = 2),
            subtitle = "Percentage of open-access publication",
            icon = icon("lock-open"),
            color = "purple"
        )
    })
    output$Totaljournal2 <- renderValueBox({

        if (df_target() == "All"){
            target_chosen <- df()
            if(df_country() == "All"){
                country_chosen <- target_chosen
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            } else if (df_country() == "Global South"){
                country_chosen <- target_chosen[!is.na(target_chosen$`Global South`), ]
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            } else {
                country_chosen <- target_chosen[!is.na(target_chosen[, colnames(target_chosen) == df_country()]),]
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            }
        } else { 
            target_chosen <- df()[!is.na(df()[, colnames(df()) == df_target()]), ]
            if(df_country() == "All"){
                country_chosen <- target_chosen
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            } else if (df_country() == "Global South"){
                country_chosen <- target_chosen[!is.na(target_chosen$`Global South`), ]
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            } else {
                country_chosen <- target_chosen[!is.na(target_chosen[, colnames(target_chosen) == df_country()]),]
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            }
        }

        valueBox(
            value = prettyNum(nlevels(as.factor(selecteddata$`Source title`)), big.mark = ","),
            subtitle = "Total number of journal",
            icon = icon("book"),
            color = "maroon"
        )
    })
    # # Output table in Info tab ####
    # output$table <- DT::renderDataTable(server = FALSE, {
    #     if (df_target() == "All"){
    #         target_chosen <- df()
    #         if(df_country() == "All"){
    #             country_chosen <- target_chosen
    #             if(df_year() == "All"){
    #                 selecteddata <- country_chosen
    #             } else if (df_year() == "From 2010 to 2019"){
    #                 selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
    #             } else if (df_year() == "From 2000 to 2009"){
    #                 selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
    #             } else if (df_year() == "Before 2000"){
    #                 selecteddata <- country_chosen %>% filter(Year < 2000)
    #             } else {
    #                 # year_chosen <- df_year()
    #                 selecteddata <- country_chosen %>% filter(Year == df_year())
    #             }
    #         } else if (df_country() == "Global South"){
    #             country_chosen <- target_chosen[!is.na(target_chosen$`Global South`), ]
    #             if(df_year() == "All"){
    #                 selecteddata <- country_chosen
    #             } else if (df_year() == "From 2010 to 2019"){
    #                 selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
    #             } else if (df_year() == "From 2000 to 2009"){
    #                 selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
    #             } else if (df_year() == "Before 2000"){
    #                 selecteddata <- country_chosen %>% filter(Year < 2000)
    #             } else {
    #                 # year_chosen <- df_year()
    #                 selecteddata <- country_chosen %>% filter(Year == df_year())
    #             }
    #         } else {
    #             country_chosen <- target_chosen[!is.na(target_chosen[, colnames(target_chosen) == df_country()]),]
    #             if(df_year() == "All"){
    #                 selecteddata <- country_chosen
    #             } else if (df_year() == "From 2010 to 2019"){
    #                 selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
    #             } else if (df_year() == "From 2000 to 2009"){
    #                 selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
    #             } else if (df_year() == "Before 2000"){
    #                 selecteddata <- country_chosen %>% filter(Year < 2000)
    #             } else {
    #                 # year_chosen <- df_year()
    #                 selecteddata <- country_chosen %>% filter(Year == df_year())
    #             }
    #         }
    #     } else { 
    #         target_chosen <- df()[!is.na(df()[, colnames(df()) == df_target()]), ]
    #         if(df_country() == "All"){
    #             country_chosen <- target_chosen
    #             if(df_year() == "All"){
    #                 selecteddata <- country_chosen
    #             } else if (df_year() == "From 2010 to 2019"){
    #                 selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
    #             } else if (df_year() == "From 2000 to 2009"){
    #                 selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
    #             } else if (df_year() == "Before 2000"){
    #                 selecteddata <- country_chosen %>% filter(Year < 2000)
    #             } else {
    #                 # year_chosen <- df_year()
    #                 selecteddata <- country_chosen %>% filter(Year == df_year())
    #             }
    #         } else if (df_country() == "Global South"){
    #             country_chosen <- target_chosen[!is.na(target_chosen$`Global South`), ]
    #             if(df_year() == "All"){
    #                 selecteddata <- country_chosen
    #             } else if (df_year() == "From 2010 to 2019"){
    #                 selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
    #             } else if (df_year() == "From 2000 to 2009"){
    #                 selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
    #             } else if (df_year() == "Before 2000"){
    #                 selecteddata <- country_chosen %>% filter(Year < 2000)
    #             } else {
    #                 # year_chosen <- df_year()
    #                 selecteddata <- country_chosen %>% filter(Year == df_year())
    #             }
    #         } else {
    #             country_chosen <- target_chosen[!is.na(target_chosen[, colnames(target_chosen) == df_country()]),]
    #             if(df_year() == "All"){
    #                 selecteddata <- country_chosen
    #             } else if (df_year() == "From 2010 to 2019"){
    #                 selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
    #             } else if (df_year() == "From 2000 to 2009"){
    #                 selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
    #             } else if (df_year() == "Before 2000"){
    #                 selecteddata <- country_chosen %>% filter(Year < 2000)
    #             } else {
    #                 # year_chosen <- df_year()
    #                 selecteddata <- country_chosen %>% filter(Year == df_year())
    #             }
    #         }
    #     }
    #     
    #     description_df <- selecteddata[, which(colnames(selecteddata) == "Target 6.1"):which(colnames(selecteddata) == "Target 6.b")] %>% 
    #         tidyr::unite(`SDG 6`, remove = TRUE, sep = ". ", na.rm = TRUE)
    #     
    #     data_selected <- selecteddata[,c("Authors", "Title", "Year", "Abbreviated Source Title", "Cited by",
    #                                      "Document Type", "DOI", "Abstract")]
    #     data_selected <- bind_cols(data_selected, description_df)
    #     
    #     DT::datatable({DT::datatable(data_selected)
    #         data_selected$DOI <- paste0("<a href='", "https://doi.org/", data_selected$DOI,"' target='_blank'>", "https://doi.org/", data_selected$DOI,"</a>")
    #         data_selected
    #     }, escape = FALSE,
    #     filter="top",
    #     selection="multiple",
    #     extensions = c('Buttons'),
    #     options = list(sDom  = '<"top"pB>t<"bottom"i>r',
    #                    pageLength = 5,
    #                    buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
    #                    scrollX = TRUE,
    #                    autoWidth = FALSE))
    # })

    # Output map in Info tab ####
    output$map <- renderLeaflet({
        if (df_target() == "All"){
            target_chosen <- df()
            if(df_country() == "All"){
                country_chosen <- target_chosen
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            } else if (df_country() == "Global South"){
                country_chosen <- target_chosen[!is.na(target_chosen$`Global South`), ]
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            } else {
                country_chosen <- target_chosen[!is.na(target_chosen[, colnames(target_chosen) == df_country()]),]
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            }
        } else { 
            target_chosen <- df()[!is.na(df()[, colnames(df()) == df_target()]), ]
            if(df_country() == "All"){
                country_chosen <- target_chosen
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            } else if (df_country() == "Global South"){
                country_chosen <- target_chosen[!is.na(target_chosen$`Global South`), ]
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            } else {
                country_chosen <- target_chosen[!is.na(target_chosen[, colnames(target_chosen) == df_country()]),]
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            }
        }
        
        selecteddata_v2 <- selecteddata[,c(first_country:last_country, 17)] %>%
            gather(key = "Country", value = "value", -`Document Type`, na.rm =TRUE)

        selecteddata_v3 <- selecteddata[,c(first_lat:last_lat, 17)] %>%
            gather(key = "Lat", value = "value", -`Document Type`, na.rm =TRUE)

        selecteddata_v4 <- selecteddata[,c(first_long:last_long, 17)] %>%
            gather(key = "Long", value = "value", -`Document Type`, na.rm =TRUE)


        selecteddata_v2$lat <- selecteddata_v3$value
        selecteddata_v2$long <- selecteddata_v4$value

        selecteddata_v2 <- selecteddata_v2 %>%
            group_by(Country, lat, long, `Document Type`) %>%
            summarise(n=n()) %>%
            spread(key = `Document Type`, value = n)

        selecteddata_v2$Total <- rowSums(subset(selecteddata_v2, select = -c(Country, lat, long)), na.rm = TRUE)
        selecteddata_v2 <- selecteddata_v2[-which(selecteddata_v2$Country == "Belgium"),]


        tilesURL <- '//server.arcgisonline.com/ArcGIS/rest/services/World_Street_Map/MapServer/tile/{z}/{y}/{x}'
        colors <- brewer.pal(n = 12, name = "Paired")
        colors <- colors[c(2:12,1)]
        leaflet() %>%
            addTiles(tilesURL) %>%
            fitBounds(lng1 = -141.152344, lat1 = 55.646599, lng2 = 161.542969, lat2 = -52.194140) %>%
            addMinicharts(selecteddata_v2$lat, selecteddata_v2$long,
                          type = "pie",
                          chartdata = subset(selecteddata_v2, select = -c(Country, lat, long, Total)),
                          colorPalette = colors,
                          width = 80 * sqrt(selecteddata_v2$Total) / sqrt(max(selecteddata_v2$Total)),
                          transitionTime = 0)
    })

    # Output journal in Journal tab ####
    output$topjournal <- renderPlotly({
        if (df_target() == "All"){
            target_chosen <- df()
            if(df_country() == "All"){
                country_chosen <- target_chosen
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            } else if (df_country() == "Global South"){
                country_chosen <- target_chosen[!is.na(target_chosen$`Global South`), ]
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            } else {
                country_chosen <- target_chosen[!is.na(target_chosen[, colnames(target_chosen) == df_country()]),]
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            }
        } else { 
            target_chosen <- df()[!is.na(df()[, colnames(df()) == df_target()]), ]
            if(df_country() == "All"){
                country_chosen <- target_chosen
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            } else if (df_country() == "Global South"){
                country_chosen <- target_chosen[!is.na(target_chosen$`Global South`), ]
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            } else {
                country_chosen <- target_chosen[!is.na(target_chosen[, colnames(target_chosen) == df_country()]),]
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            }
        }

        WN_journal <- selecteddata %>% select(`Source title`) %>%
            dplyr::group_by(`Source title`) %>%
            dplyr::summarise(n=n()) %>%
            dplyr::arrange(desc(n)) %>%
            slice(1:20)

        ggplotly(ggplot(WN_journal, aes(x=reorder(`Source title`, n),y = n)) +
                     geom_bar(stat = "identity",
                              position = position_stack(reverse = TRUE),
                              fill = "tomato") +
                     coord_flip() +
                     theme_bw() +
                     xlab("Journals") +
                     ylab("Number of publications") +
                     theme(text=element_text(family = "Arial")) +
                     theme(axis.title.y = element_blank())
        )
    })
    # Output openaccess in Journal tab ####
    output$Openaccess <- renderPlotly({
        if (df_target() == "All"){
            target_chosen <- df()
            if(df_country() == "All"){
                country_chosen <- target_chosen
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            } else if (df_country() == "Global South"){
                country_chosen <- target_chosen[!is.na(target_chosen$`Global South`), ]
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            } else {
                country_chosen <- target_chosen[!is.na(target_chosen[, colnames(target_chosen) == df_country()]),]
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            }
        } else { 
            target_chosen <- df()[!is.na(df()[, colnames(df()) == df_target()]), ]
            if(df_country() == "All"){
                country_chosen <- target_chosen
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            } else if (df_country() == "Global South"){
                country_chosen <- target_chosen[!is.na(target_chosen$`Global South`), ]
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            } else {
                country_chosen <- target_chosen[!is.na(target_chosen[, colnames(target_chosen) == df_country()]),]
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            }
        }
        
        WN_OA <- water_nexus3 %>% select(`Access Type`, Year) %>%
            dplyr::group_by(`Access Type`,Year) %>%
            dplyr::summarise(n=n())
        WN_OA$`Access Type` <- as.character(WN_OA$`Access Type`)
        WN_OA$`Access Type`[is.na(WN_OA$`Access Type`)] <- "Not OA"
        
        WN_OA <- WN_OA %>% group_by(Year) %>% 
            mutate_at(vars(n), funs("percent" = round(.*100/sum(.), digits = 2)))
        
        for (i in 1:nrow(WN_OA)){
            if (WN_OA$percent[i] == 100){
                WN_OA$`Access Type`[i] <- "Open Access"
                WN_OA$percent[i] <- 0 
                WN_OA$n[i] <- 0 
            }
        }
        # something wrong here
        WN_OA <- WN_OA %>% filter(`Access Type` == "Open Access") %>% arrange(Year)
        attributes(WN_OA) <-NULL
        f_year <- levels(as.factor(WN_OA$Year))[1]
        l_year <- levels(as.factor(WN_OA$Year))[nlevels(as.factor(WN_OA$Year))]
        
        ggplotly(ggplot(WN_OA, aes(x = Year,  y = n, color = 'tomato')) +
                     geom_point(size = 2, color = 'tomato')+
                     geom_line(size = 1.1125, color = 'tomato')+
                     theme_bw() +
                     xlab("Year") +
                     ylab("Open Acess (%)") +
                     scale_x_continuous(name = "Year", limits = c(f_year,l_year), breaks = seq(f_year,l_year, by =10)) +
                     theme(text=element_text(family = "Arial")) +
                     theme(legend.title = element_blank()) +
                     theme(legend.text =  element_blank())
        )
    })
    # Output publication year in Research tab ####
    output$Pubyear <- renderPlotly({
        if (df_target() == "All"){
            target_chosen <- df()
            if(df_country() == "All"){
                country_chosen <- target_chosen
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            } else if (df_country() == "Global South"){
                country_chosen <- target_chosen[!is.na(target_chosen$`Global South`), ]
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            } else {
                country_chosen <- target_chosen[!is.na(target_chosen[, colnames(target_chosen) == df_country()]),]
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            }
        } else { 
            target_chosen <- df()[!is.na(df()[, colnames(df()) == df_target()]), ]
            if(df_country() == "All"){
                country_chosen <- target_chosen
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            } else if (df_country() == "Global South"){
                country_chosen <- target_chosen[!is.na(target_chosen$`Global South`), ]
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            } else {
                country_chosen <- target_chosen[!is.na(target_chosen[, colnames(target_chosen) == df_country()]),]
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            }
        }

        WN_PU_year <- selecteddata %>% select(Year,`Document Type`) %>%
            dplyr::group_by(Year, `Document Type`) %>%
            dplyr::summarise(`Number of publication`=n()) %>%
            dplyr::arrange(Year)
        f_year <- WN_PU_year$Year[1]
        l_year <- WN_PU_year$Year[nrow(WN_PU_year)]

        ggplotly(ggplot(WN_PU_year, aes(x=Year, y=`Number of publication`, color = `Document Type`, group = `Document Type`))+
                     geom_point(size = 2)+
                     geom_line(size = 1.1125)+
                     theme_bw() +
                     xlab("Year") +
                     ylab("Number of Publication") +
                     theme(text=element_text(family = "Arial")) +
                     scale_x_continuous(name = "Year", limits = c(f_year,l_year), breaks = seq(f_year,l_year, by =10)) +
                     theme(legend.title = element_blank())
        )

    })
    # Output top keywords in Research tab ####
    output$Topkw <- renderPlot({
        
        if (df_target() == "All"){
            target_chosen <- df()
            if(df_country() == "All"){
                country_chosen <- target_chosen
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            } else if (df_country() == "Global South"){
                country_chosen <- target_chosen[!is.na(target_chosen$`Global South`), ]
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            } else {
                country_chosen <- target_chosen[!is.na(target_chosen[, colnames(target_chosen) == df_country()]),]
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            }
        } else { 
            target_chosen <- df()[!is.na(df()[, colnames(df()) == df_target()]), ]
            if(df_country() == "All"){
                country_chosen <- target_chosen
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            } else if (df_country() == "Global South"){
                country_chosen <- target_chosen[!is.na(target_chosen$`Global South`), ]
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            } else {
                country_chosen <- target_chosen[!is.na(target_chosen[, colnames(target_chosen) == df_country()]),]
                if(df_year() == "All"){
                    selecteddata <- country_chosen
                } else if (df_year() == "From 2010 to 2019"){
                    selecteddata <- country_chosen %>% filter(Year <= 2019 & Year >= 2010)
                } else if (df_year() == "From 2000 to 2009"){
                    selecteddata <- country_chosen %>% filter(Year <= 2009 & Year >= 2000)
                } else if (df_year() == "Before 2000"){
                    selecteddata <- country_chosen %>% filter(Year < 2000)
                } else {
                    # year_chosen <- df_year()
                    selecteddata <- country_chosen %>% filter(Year == df_year())
                }
            }
        }
        
        KW <- function(x){
            keyword <- strsplit(x, "; ")
            for (i in 1:length(keyword)){
                keyword[i] <- as.data.frame(matrix(as.data.frame(keyword[i])))
            }
            keyword2 <- rbindlist(keyword)
            colnames(keyword2)[1]<- "keyword"
            keyword2<- keyword2[complete.cases(keyword2),]
            keyword2$keyword <- str_to_title(keyword2$keyword)
            keyword3 <- keyword2 %>%
                dplyr::group_by(keyword) %>%
                dplyr::summarise(n=n()) %>%
                dplyr::arrange(desc(n))
            return(keyword3)
        }
        WN_TopKW <- KW(selecteddata$`Author Keywords`)
        ggplot(WN_TopKW[1:50,], aes(label = keyword, size =n,color = rainbow_hcl(50))) +
            geom_text_wordcloud_area(shape = "circle") +
            scale_size_area(max_size = 20) +
            theme_minimal()
    })

}

#### Run the application ####
shinyApp(ui = ui, server = server)
