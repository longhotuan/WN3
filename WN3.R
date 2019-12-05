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
WN3 <- read.csv("WN3_v2.csv", stringsAsFactors = FALSE)
x <- read_csv("colnames.csv")
colnames(WN3) <- x$x
rm(x)

first_country <- which(colnames(WN3) == "Afghanistan")
last_country <- which (colnames(WN3) == "Zimbabwe")
first_lat <- which(colnames(WN3) == "lat_Afghanistan")
last_lat <- which (colnames(WN3) == "lat_Zimbabwe")
first_long <- which (colnames(WN3) == "long_Afghanistan")
last_long <- which (colnames(WN3) == "long_Zimbabwe")


# 3 pages: general info: table + map, journal info: Top 20 journal + open-access , research info: Citation year, top keywords
# + 6 value boxes: Total of number publication, total countries, total document types, total cites, total authors, percentage of open-access

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
                    selectInput(inputId = "country", label = "Select a country", 
                                choices = c(All = "All", "Partner countries", colnames(WN3[, first_country:last_country]))),
                    selectInput(inputId = "year", label = "Select a year",
                                choices = c(All = "All",levels(as.factor(WN3$Year))))
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
                        box(title = "Collaboration map", width = 12, 
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
    df <- reactive({WN3})
    
    df_country <- reactive({
        input$country
    })
    
    chosen_year <- reactive({
        if(df_country() == "All"){
            levels(as.factor(df()$Year))
        } else if(df_country() == "Partner countries"){
            m <-df()[, which(colnames(df()) %in% c("Benin", "Burkina Faso", "Burundi", "DR Congo", "Guinea", "Mali", "Morocco", 
                                                   "Mozambique", "Niger", "Uganda", "Palestine", "Rwanda", "Senegal", "Tanzania"))]
            year1 <- df()[rowSums(is.na(m)) != ncol(m), ]
            year2 <- levels(as.factor(year1$Year))
            year2
            
        } else {
            year_1 <- df()[!is.na(df()[, colnames(df()) == df_country()]),]
            year_2 <- levels(as.factor(year_1$Year))
            year_2
        }
    })
    
    observe({
        updateSelectInput(session, inputId = "year", label = "Select a year",
                          choices = c(All = "All", chosen_year()))
    })
    
    df_year <- reactive({
        input$year
    })
    
    # Output valuebox in Info tab ####
    output$Publication <- renderValueBox({

        if (df_country() == "All"){
            if (df_year() == "All") {
                selectedData <- df()
            } else {
                selectedData <- df() %>% filter(Year == df_year())
            }
        } else if(df_country() == "Partner countries"){
            if (df_year() == "All") {
                m <-df()[, which(colnames(df()) %in% c("Benin", "Burkina Faso", "Burundi", "DR Congo", "Guinea", "Mali", "Morocco", 
                                                       "Mozambique", "Niger", "Uganda", "Palestine", "Rwanda", "Senegal", "Tanzania"))]
                selectedData <- df()[rowSums(is.na(m)) != ncol(m), ]
                selectedData <- selectedData[complete.cases(selectedData[ ,1:3]),]
            } else {
                selectedData3 <- df() %>% filter(Year == df_year())
                m <-selectedData3[, which(colnames(selectedData3) %in% c("Benin", "Burkina Faso", "Burundi", "DR Congo", "Guinea", "Mali", "Morocco", 
                                                       "Mozambique", "Niger", "Uganda", "Palestine", "Rwanda", "Senegal", "Tanzania"))]
                selectedData <- selectedData3[rowSums(is.na(m)) != ncol(m), ]
                selectedData <- selectedData[complete.cases(selectedData[ ,1:3]),]
            }
        } else {
            if (df_year() == "All") {
                selectedData <- df()[!is.na(df()[, colnames(df()) == df_country()]),]
            } else {
                selectedData2 <- df() %>% filter(Year == df_year())
                selectedData <- selectedData2[!is.na(selectedData2[, colnames(selectedData2) == df_country()]),]
            }
        }
        
        valueBox(
            value = prettyNum(nrow(selectedData), big.mark = ","),
            subtitle = "Total number of publication",
            icon = icon("newspaper"),
            color = "purple"
        )
    })
    output$TotalCountry <- renderValueBox({
        
        if (df_country() == "All"){
            if (df_year() == "All") {
                selectedData <- df()
            } else {
                selectedData <- df() %>% filter(Year == df_year())
            }
        } else if(df_country() == "Partner countries"){
            if (df_year() == "All") {
                m <-df()[, which(colnames(df()) %in% c("Benin", "Burkina Faso", "Burundi", "DR Congo", "Guinea", "Mali", "Morocco", 
                                                       "Mozambique", "Niger", "Uganda", "Palestine", "Rwanda", "Senegal", "Tanzania"))]
                selectedData <- df()[rowSums(is.na(m)) != ncol(m), ]
                selectedData <- selectedData[complete.cases(selectedData[ ,1:3]),]
            } else {
                selectedData3 <- df() %>% filter(Year == df_year())
                m <-selectedData3[, which(colnames(selectedData3) %in% c("Benin", "Burkina Faso", "Burundi", "DR Congo", "Guinea", "Mali", "Morocco", 
                                                       "Mozambique", "Niger", "Uganda", "Palestine", "Rwanda", "Senegal", "Tanzania"))]
                selectedData <- selectedData3[rowSums(is.na(m)) != ncol(m), ]
                selectedData <- selectedData[complete.cases(selectedData[ ,1:3]),]
            }
        } else {
            if (df_year() == "All") {
                selectedData <- df()[!is.na(df()[, colnames(df()) == df_country()]),]
            } else {
                selectedData2 <- df() %>% filter(Year == df_year())
                selectedData <- selectedData2[!is.na(selectedData2[, colnames(selectedData2) == df_country()]),]
            }
        }
        selectedData_v3 <- selectedData[,first_country:last_country][, colSums(is.na(selectedData[,first_country:last_country])) < nrow(selectedData[,first_country:last_country])]
        valueBox(
            value = ncol(selectedData_v3),
            subtitle = "Total number of country",
            icon = icon("flag"),
            color = "yellow"
        )
    })
    output$Citation <- renderValueBox({
        if (df_country() == "All"){
            if (df_year() == "All") {
                selectedData <- df()
            } else {
                selectedData <- df() %>% filter(Year == df_year())
            }
        } else if(df_country() == "Partner countries"){
            if (df_year() == "All") {
                m <-df()[, which(colnames(df()) %in% c("Benin", "Burkina Faso", "Burundi", "DR Congo", "Guinea", "Mali", "Morocco", 
                                                       "Mozambique", "Niger", "Uganda", "Palestine", "Rwanda", "Senegal", "Tanzania"))]
                selectedData <- df()[rowSums(is.na(m)) != ncol(m), ]
                selectedData <- selectedData[complete.cases(selectedData[ ,1:3]),]
            } else {
                selectedData3 <- df() %>% filter(Year == df_year())
                m <-selectedData3[, which(colnames(selectedData3) %in% c("Benin", "Burkina Faso", "Burundi", "DR Congo", "Guinea", "Mali", "Morocco", 
                                                       "Mozambique", "Niger", "Uganda", "Palestine", "Rwanda", "Senegal", "Tanzania"))]
                selectedData <- selectedData3[rowSums(is.na(m)) != ncol(m), ]
                selectedData <- selectedData[complete.cases(selectedData[ ,1:3]),]
            }
        } else {
            if (df_year() == "All") {
                selectedData <- df()[!is.na(df()[, colnames(df()) == df_country()]),]
            } else {
                selectedData2 <- df() %>% filter(Year == df_year())
                selectedData <- selectedData2[!is.na(selectedData2[, colnames(selectedData2) == df_country()]),]
            }
        }
        valueBox(
            value =  prettyNum(sum(selectedData$`Cited by`, na.rm = TRUE), big.mark = ","),
            subtitle = "Total number of citation",
            icon = icon("file-signature"),
            color = "blue"
        )
    })
    output$DocumentType <- renderValueBox({
        if (df_country() == "All"){
            if (df_year() == "All") {
                selectedData <- df()
            } else {
                selectedData <- df() %>% filter(Year == df_year())
            }
        } else if(df_country() == "Partner countries"){
            if (df_year() == "All") {
                m <-df()[, which(colnames(df()) %in% c("Benin", "Burkina Faso", "Burundi", "DR Congo", "Guinea", "Mali", "Morocco", 
                                                       "Mozambique", "Niger", "Uganda", "Palestine", "Rwanda", "Senegal", "Tanzania"))]
                selectedData <- df()[rowSums(is.na(m)) != ncol(m), ]
                selectedData <- selectedData[complete.cases(selectedData[ ,1:3]),]
            } else {
                selectedData3 <- df() %>% filter(Year == df_year())
                m <-selectedData3[, which(colnames(selectedData3) %in% c("Benin", "Burkina Faso", "Burundi", "DR Congo", "Guinea", "Mali", "Morocco", 
                                                       "Mozambique", "Niger", "Uganda", "Palestine", "Rwanda", "Senegal", "Tanzania"))]
                selectedData <- selectedData3[rowSums(is.na(m)) != ncol(m), ]
                selectedData <- selectedData[complete.cases(selectedData[ ,1:3]),]
            }
        } else {
            if (df_year() == "All") {
                selectedData <- df()[!is.na(df()[, colnames(df()) == df_country()]),]
            } else {
                selectedData2 <- df() %>% filter(Year == df_year())
                selectedData <- selectedData2[!is.na(selectedData2[, colnames(selectedData2) == df_country()]),]
            }
        }
        valueBox(
            value = length(levels(as.factor(selectedData$`Document Type`))),
            subtitle = "Document type",
            icon = icon("folder-open"),
            color = "red"
        )
    })
    output$OpenAccess <- renderValueBox({
        if (df_country() == "All"){
            if (df_year() == "All") {
                selectedData <- df()
            } else {
                selectedData <- df() %>% filter(Year == df_year())
            }
        } else if(df_country() == "Partner countries"){
            if (df_year() == "All") {
                m <-df()[, which(colnames(df()) %in% c("Benin", "Burkina Faso", "Burundi", "DR Congo", "Guinea", "Mali", "Morocco", 
                                                       "Mozambique", "Niger", "Uganda", "Palestine", "Rwanda", "Senegal", "Tanzania"))]
                selectedData <- df()[rowSums(is.na(m)) != ncol(m), ]
                selectedData <- selectedData[complete.cases(selectedData[ ,1:3]),]
            } else {
                selectedData3 <- df() %>% filter(Year == df_year())
                m <-selectedData3[, which(colnames(selectedData3) %in% c("Benin", "Burkina Faso", "Burundi", "DR Congo", "Guinea", "Mali", "Morocco", 
                                                       "Mozambique", "Niger", "Uganda", "Palestine", "Rwanda", "Senegal", "Tanzania"))]
                selectedData <- selectedData3[rowSums(is.na(m)) != ncol(m), ]
                selectedData <- selectedData[complete.cases(selectedData[ ,1:3]),]
            }
        } else {
            if (df_year() == "All") {
                selectedData <- df()[!is.na(df()[, colnames(df()) == df_country()]),]
            } else {
                selectedData2 <- df() %>% filter(Year == df_year())
                selectedData <- selectedData2[!is.na(selectedData2[, colnames(selectedData2) == df_country()]),]
            }
        }
        valueBox(
            value = round(sum(!is.na(selectedData$`Access Type`))*100/(sum(is.na(selectedData$`Access Type`))+sum(!is.na(selectedData$`Access Type`))), digits = 2),
            subtitle = "Percentage of open-access publication",
            icon = icon("lock-open"),
            color = "purple"
        )
    })
    output$Totaljournal <- renderValueBox({
        if (df_country() == "All"){
            if (df_year() == "All") {
                selectedData <- df()
            } else {
                selectedData <- df() %>% filter(Year == df_year())
            }
        } else if(df_country() == "Partner countries"){
            if (df_year() == "All") {
                m <-df()[, which(colnames(df()) %in% c("Benin", "Burkina Faso", "Burundi", "DR Congo", "Guinea", "Mali", "Morocco", 
                                                       "Mozambique", "Niger", "Uganda", "Palestine", "Rwanda", "Senegal", "Tanzania"))]
                selectedData <- df()[rowSums(is.na(m)) != ncol(m), ]
                selectedData <- selectedData[complete.cases(selectedData[ ,1:3]),]
            } else {
                selectedData3 <- df() %>% filter(Year == df_year())
                m <-selectedData3[, which(colnames(selectedData3) %in% c("Benin", "Burkina Faso", "Burundi", "DR Congo", "Guinea", "Mali", "Morocco", 
                                                       "Mozambique", "Niger", "Uganda", "Palestine", "Rwanda", "Senegal", "Tanzania"))]
                selectedData <- selectedData3[rowSums(is.na(m)) != ncol(m), ]
                selectedData <- selectedData[complete.cases(selectedData[ ,1:3]),]
            }
        } else {
            if (df_year() == "All") {
                selectedData <- df()[!is.na(df()[, colnames(df()) == df_country()]),]
            } else {
                selectedData2 <- df() %>% filter(Year == df_year())
                selectedData <- selectedData2[!is.na(selectedData2[, colnames(selectedData2) == df_country()]),]
            }
        }
        valueBox(
            value = prettyNum(nlevels(as.factor(selectedData$`Source title`)), big.mark = ","),
            subtitle = "Total number of journal",
            icon = icon("book"),
            color = "maroon"
        )
    })
    # Output valuebox in Journal tab ####
    output$Publication1 <- renderValueBox({
        if (df_country() == "All"){
            if (df_year() == "All") {
                selectedData <- df()
            } else {
                selectedData <- df() %>% filter(Year == df_year())
            }
        } else if(df_country() == "Partner countries"){
            if (df_year() == "All") {
                m <-df()[, which(colnames(df()) %in% c("Benin", "Burkina Faso", "Burundi", "DR Congo", "Guinea", "Mali", "Morocco", 
                                                       "Mozambique", "Niger", "Uganda", "Palestine", "Rwanda", "Senegal", "Tanzania"))]
                selectedData <- df()[rowSums(is.na(m)) != ncol(m), ]
                selectedData <- selectedData[complete.cases(selectedData[ ,1:3]),]
            } else {
                selectedData3 <- df() %>% filter(Year == df_year())
                m <-selectedData3[, which(colnames(selectedData3) %in% c("Benin", "Burkina Faso", "Burundi", "DR Congo", "Guinea", "Mali", "Morocco", 
                                                       "Mozambique", "Niger", "Uganda", "Palestine", "Rwanda", "Senegal", "Tanzania"))]
                selectedData <- selectedData3[rowSums(is.na(m)) != ncol(m), ]
                selectedData <- selectedData[complete.cases(selectedData[ ,1:3]),]
            }
        } else {
            if (df_year() == "All") {
                selectedData <- df()[!is.na(df()[, colnames(df()) == df_country()]),]
            } else {
                selectedData2 <- df() %>% filter(Year == df_year())
                selectedData <- selectedData2[!is.na(selectedData2[, colnames(selectedData2) == df_country()]),]
            }
        }
        
        valueBox(
            value = prettyNum(nrow(selectedData), big.mark = ","),
            subtitle = "Total number of publication",
            icon = icon("newspaper"),
            color = "purple"
        )
    })
    output$TotalCountry1 <- renderValueBox({
        
        if (df_country() == "All"){
            if (df_year() == "All") {
                selectedData <- df()
            } else {
                selectedData <- df() %>% filter(Year == df_year())
            }
        } else if(df_country() == "Partner countries"){
            if (df_year() == "All") {
                m <-df()[, which(colnames(df()) %in% c("Benin", "Burkina Faso", "Burundi", "DR Congo", "Guinea", "Mali", "Morocco", 
                                                       "Mozambique", "Niger", "Uganda", "Palestine", "Rwanda", "Senegal", "Tanzania"))]
                selectedData <- df()[rowSums(is.na(m)) != ncol(m), ]
                selectedData <- selectedData[complete.cases(selectedData[ ,1:3]),]
            } else {
                selectedData3 <- df() %>% filter(Year == df_year())
                m <-selectedData3[, which(colnames(selectedData3) %in% c("Benin", "Burkina Faso", "Burundi", "DR Congo", "Guinea", "Mali", "Morocco", 
                                                       "Mozambique", "Niger", "Uganda", "Palestine", "Rwanda", "Senegal", "Tanzania"))]
                selectedData <- selectedData3[rowSums(is.na(m)) != ncol(m), ]
                selectedData <- selectedData[complete.cases(selectedData[ ,1:3]),]
            }
        } else {
            if (df_year() == "All") {
                selectedData <- df()[!is.na(df()[, colnames(df()) == df_country()]),]
            } else {
                selectedData2 <- df() %>% filter(Year == df_year())
                selectedData <- selectedData2[!is.na(selectedData2[, colnames(selectedData2) == df_country()]),]
            }
        }
        selectedData_v3 <- selectedData[,first_country:last_country][, colSums(is.na(selectedData[,first_country:last_country])) < nrow(selectedData[,first_country:last_country])]
        valueBox(
            value =ncol(selectedData_v3),
            subtitle = "Total number of country",
            icon = icon("flag"),
            color = "yellow"
        )
    })
    output$Citation1 <- renderValueBox({
        if (df_country() == "All"){
            if (df_year() == "All") {
                selectedData <- df()
            } else {
                selectedData <- df() %>% filter(Year == df_year())
            }
        } else if(df_country() == "Partner countries"){
            if (df_year() == "All") {
                m <-df()[, which(colnames(df()) %in% c("Benin", "Burkina Faso", "Burundi", "DR Congo", "Guinea", "Mali", "Morocco", 
                                                       "Mozambique", "Niger", "Uganda", "Palestine", "Rwanda", "Senegal", "Tanzania"))]
                selectedData <- df()[rowSums(is.na(m)) != ncol(m), ]
                selectedData <- selectedData[complete.cases(selectedData[ ,1:3]),]
            } else {
                selectedData3 <- df() %>% filter(Year == df_year())
                m <-selectedData3[, which(colnames(selectedData3) %in% c("Benin", "Burkina Faso", "Burundi", "DR Congo", "Guinea", "Mali", "Morocco", 
                                                       "Mozambique", "Niger", "Uganda", "Palestine", "Rwanda", "Senegal", "Tanzania"))]
                selectedData <- selectedData3[rowSums(is.na(m)) != ncol(m), ]
                selectedData <- selectedData[complete.cases(selectedData[ ,1:3]),]
            }
        } else {
            if (df_year() == "All") {
                selectedData <- df()[!is.na(df()[, colnames(df()) == df_country()]),]
            } else {
                selectedData2 <- df() %>% filter(Year == df_year())
                selectedData <- selectedData2[!is.na(selectedData2[, colnames(selectedData2) == df_country()]),]
            }
        }
        valueBox(
            value = prettyNum(sum(selectedData$`Cited by`, na.rm = TRUE), big.mark = ","),
            subtitle = "Total number of citation",
            icon = icon("file-signature"),
            color = "blue"
        )
    })
    output$DocumentType1 <- renderValueBox({
        if (df_country() == "All"){
            if (df_year() == "All") {
                selectedData <- df()
            } else {
                selectedData <- df() %>% filter(Year == df_year())
            }
        } else if(df_country() == "Partner countries"){
            if (df_year() == "All") {
                m <-df()[, which(colnames(df()) %in% c("Benin", "Burkina Faso", "Burundi", "DR Congo", "Guinea", "Mali", "Morocco", 
                                                       "Mozambique", "Niger", "Uganda", "Palestine", "Rwanda", "Senegal", "Tanzania"))]
                selectedData <- df()[rowSums(is.na(m)) != ncol(m), ]
                selectedData <- selectedData[complete.cases(selectedData[ ,1:3]),]
            } else {
                selectedData3 <- df() %>% filter(Year == df_year())
                m <-selectedData3[, which(colnames(selectedData3) %in% c("Benin", "Burkina Faso", "Burundi", "DR Congo", "Guinea", "Mali", "Morocco", 
                                                       "Mozambique", "Niger", "Uganda", "Palestine", "Rwanda", "Senegal", "Tanzania"))]
                selectedData <- selectedData3[rowSums(is.na(m)) != ncol(m), ]
                selectedData <- selectedData[complete.cases(selectedData[ ,1:3]),]
            }
        } else {
            if (df_year() == "All") {
                selectedData <- df()[!is.na(df()[, colnames(df()) == df_country()]),]
            } else {
                selectedData2 <- df() %>% filter(Year == df_year())
                selectedData <- selectedData2[!is.na(selectedData2[, colnames(selectedData2) == df_country()]),]
            }
        }
        valueBox(
            value = length(levels(as.factor(selectedData$`Document Type`))),
            subtitle = "Document type",
            icon = icon("folder-open"),
            color = "red"
        )
    })
    output$OpenAccess1 <- renderValueBox({
        if (df_country() == "All"){
            if (df_year() == "All") {
                selectedData <- df()
            } else {
                selectedData <- df() %>% filter(Year == df_year())
            }
        } else if(df_country() == "Partner countries"){
            if (df_year() == "All") {
                m <-df()[, which(colnames(df()) %in% c("Benin", "Burkina Faso", "Burundi", "DR Congo", "Guinea", "Mali", "Morocco", 
                                                       "Mozambique", "Niger", "Uganda", "Palestine", "Rwanda", "Senegal", "Tanzania"))]
                selectedData <- df()[rowSums(is.na(m)) != ncol(m), ]
                selectedData <- selectedData[complete.cases(selectedData[ ,1:3]),]
            } else {
                selectedData3 <- df() %>% filter(Year == df_year())
                m <-selectedData3[, which(colnames(selectedData3) %in% c("Benin", "Burkina Faso", "Burundi", "DR Congo", "Guinea", "Mali", "Morocco", 
                                                       "Mozambique", "Niger", "Uganda", "Palestine", "Rwanda", "Senegal", "Tanzania"))]
                selectedData <- selectedData3[rowSums(is.na(m)) != ncol(m), ]
                selectedData <- selectedData[complete.cases(selectedData[ ,1:3]),]
            }
        } else {
            if (df_year() == "All") {
                selectedData <- df()[!is.na(df()[, colnames(df()) == df_country()]),]
            } else {
                selectedData2 <- df() %>% filter(Year == df_year())
                selectedData <- selectedData2[!is.na(selectedData2[, colnames(selectedData2) == df_country()]),]
            }
        }
        valueBox(
            value = round(sum(!is.na(selectedData$`Access Type`))*100/(sum(is.na(selectedData$`Access Type`))+sum(!is.na(selectedData$`Access Type`))), digits = 2),
            subtitle = "Percentage of open-access publication",
            icon = icon("lock-open"),
            color = "purple"
        )
    })
    output$Totaljournal1 <- renderValueBox({
        if (df_country() == "All"){
            if (df_year() == "All") {
                selectedData <- df()
            } else {
                selectedData <- df() %>% filter(Year == df_year())
            }
        } else if(df_country() == "Partner countries"){
            if (df_year() == "All") {
                m <-df()[, which(colnames(df()) %in% c("Benin", "Burkina Faso", "Burundi", "DR Congo", "Guinea", "Mali", "Morocco", 
                                                       "Mozambique", "Niger", "Uganda", "Palestine", "Rwanda", "Senegal", "Tanzania"))]
                selectedData <- df()[rowSums(is.na(m)) != ncol(m), ]
                selectedData <- selectedData[complete.cases(selectedData[ ,1:3]),]
            } else {
                selectedData3 <- df() %>% filter(Year == df_year())
                m <-selectedData3[, which(colnames(selectedData3) %in% c("Benin", "Burkina Faso", "Burundi", "DR Congo", "Guinea", "Mali", "Morocco", 
                                                       "Mozambique", "Niger", "Uganda", "Palestine", "Rwanda", "Senegal", "Tanzania"))]
                selectedData <- selectedData3[rowSums(is.na(m)) != ncol(m), ]
                selectedData <- selectedData[complete.cases(selectedData[ ,1:3]),]
            }
        } else {
            if (df_year() == "All") {
                selectedData <- df()[!is.na(df()[, colnames(df()) == df_country()]),]
            } else {
                selectedData2 <- df() %>% filter(Year == df_year())
                selectedData <- selectedData2[!is.na(selectedData2[, colnames(selectedData2) == df_country()]),]
            }
        }
        valueBox(
            value = prettyNum(nlevels(as.factor(selectedData$`Source title`)), big.mark = ","),
            subtitle = "Total number of journal",
            icon = icon("book"),
            color = "maroon"
        )
    })
    # Output valuebox in Research tab ####
    output$Publication2 <- renderValueBox({
        if (df_country() == "All"){
            if (df_year() == "All") {
                selectedData <- df()
            } else {
                selectedData <- df() %>% filter(Year == df_year())
            }
        } else if(df_country() == "Partner countries"){
            if (df_year() == "All") {
                m <-df()[, which(colnames(df()) %in% c("Benin", "Burkina Faso", "Burundi", "DR Congo", "Guinea", "Mali", "Morocco", 
                                                       "Mozambique", "Niger", "Uganda", "Palestine", "Rwanda", "Senegal", "Tanzania"))]
                selectedData <- df()[rowSums(is.na(m)) != ncol(m), ]
                selectedData <- selectedData[complete.cases(selectedData[ ,1:3]),]
            } else {
                selectedData3 <- df() %>% filter(Year == df_year())
                m <-selectedData3[, which(colnames(selectedData3) %in% c("Benin", "Burkina Faso", "Burundi", "DR Congo", "Guinea", "Mali", "Morocco", 
                                                       "Mozambique", "Niger", "Uganda", "Palestine", "Rwanda", "Senegal", "Tanzania"))]
                selectedData <- selectedData3[rowSums(is.na(m)) != ncol(m), ]
                selectedData <- selectedData[complete.cases(selectedData[ ,1:3]),]
            }
        } else {
            if (df_year() == "All") {
                selectedData <- df()[!is.na(df()[, colnames(df()) == df_country()]),]
            } else {
                selectedData2 <- df() %>% filter(Year == df_year())
                selectedData <- selectedData2[!is.na(selectedData2[, colnames(selectedData2) == df_country()]),]
            }
        }
        
        valueBox(
            value = prettyNum(nrow(selectedData), big.mark = ","),
            subtitle = "Total number of publication",
            icon = icon("newspaper"),
            color = "purple"
        )
    })
    output$TotalCountry2 <- renderValueBox({
        
        if (df_country() == "All"){
            if (df_year() == "All") {
                selectedData <- df()
            } else {
                selectedData <- df() %>% filter(Year == df_year())
            }
        } else if(df_country() == "Partner countries"){
            if (df_year() == "All") {
                selectedData <- df()[!is.na(df()[, which(colnames(df()) %in% c("Benin", "Burkina Faso", "Burundi", "Democratic Republic of the Congo", "Guinea", "Mali", "Morocco", 
                                                                               "Mozambique", "Niger", "Uganda", "Palestine", "Rwanda", "Senegal", "Tanzania"))]),]
                selectedData <- selectedData[complete.cases(selectedData[ ,1:3]),]
            } else {
                selectedData3 <- df() %>% filter(Year == df_year())
                selectedData <- selectedData3[!is.na(selectedData3[, which(colnames(selectedData3) %in% c("Benin", "Burkina Faso", "Burundi", "Democratic Republic of the Congo", "Guinea", "Mali", "Morocco", 
                                                                                                          "Mozambique", "Niger", "Uganda", "Palestine", "Rwanda", "Senegal", "Tanzania"))]),]
                selectedData <- selectedData[complete.cases(selectedData[ ,1:3]),]
            }
        } else {
            if (df_year() == "All") {
                selectedData <- df()[!is.na(df()[, colnames(df()) == df_country()]),]
            } else {
                selectedData2 <- df() %>% filter(Year == df_year())
                selectedData <- selectedData2[!is.na(selectedData2[, colnames(selectedData2) == df_country()]),]
            }
        }
        selectedData_v3 <- selectedData[,first_country:last_country][, colSums(is.na(selectedData[,first_country:last_country])) < nrow(selectedData[,first_country:last_country])]
        valueBox(
            value =ncol(selectedData_v3),
            subtitle = "Total number of country",
            icon = icon("flag"),
            color = "yellow"
        )
    })
    output$Citation2 <- renderValueBox({
        if (df_country() == "All"){
            if (df_year() == "All") {
                selectedData <- df()
            } else {
                selectedData <- df() %>% filter(Year == df_year())
            }
        } else if(df_country() == "Partner countries"){
            if (df_year() == "All") {
                m <-df()[, which(colnames(df()) %in% c("Benin", "Burkina Faso", "Burundi", "DR Congo", "Guinea", "Mali", "Morocco", 
                                                       "Mozambique", "Niger", "Uganda", "Palestine", "Rwanda", "Senegal", "Tanzania"))]
                selectedData <- df()[rowSums(is.na(m)) != ncol(m), ]
                selectedData <- selectedData[complete.cases(selectedData[ ,1:3]),]
            } else {
                selectedData3 <- df() %>% filter(Year == df_year())
                m <-selectedData3[, which(colnames(selectedData3) %in% c("Benin", "Burkina Faso", "Burundi", "DR Congo", "Guinea", "Mali", "Morocco", 
                                                       "Mozambique", "Niger", "Uganda", "Palestine", "Rwanda", "Senegal", "Tanzania"))]
                selectedData <- selectedData3[rowSums(is.na(m)) != ncol(m), ]
                selectedData <- selectedData[complete.cases(selectedData[ ,1:3]),]
            }
        } else {
            if (df_year() == "All") {
                selectedData <- df()[!is.na(df()[, colnames(df()) == df_country()]),]
            } else {
                selectedData2 <- df() %>% filter(Year == df_year())
                selectedData <- selectedData2[!is.na(selectedData2[, colnames(selectedData2) == df_country()]),]
            }
        }
        valueBox(
            value = prettyNum(sum(selectedData$`Cited by`, na.rm = TRUE), big.mark = ","),
            subtitle = "Total number of citation",
            icon = icon("file-signature"),
            color = "blue"
        )
    })
    output$DocumentType2 <- renderValueBox({
        if (df_country() == "All"){
            if (df_year() == "All") {
                selectedData <- df()
            } else {
                selectedData <- df() %>% filter(Year == df_year())
            }
        } else if(df_country() == "Partner countries"){
            if (df_year() == "All") {
                m <-df()[, which(colnames(df()) %in% c("Benin", "Burkina Faso", "Burundi", "DR Congo", "Guinea", "Mali", "Morocco", 
                                                       "Mozambique", "Niger", "Uganda", "Palestine", "Rwanda", "Senegal", "Tanzania"))]
                selectedData <- df()[rowSums(is.na(m)) != ncol(m), ]
                selectedData <- selectedData[complete.cases(selectedData[ ,1:3]),]
            } else {
                selectedData3 <- df() %>% filter(Year == df_year())
                m <-selectedData3[, which(colnames(selectedData3) %in% c("Benin", "Burkina Faso", "Burundi", "DR Congo", "Guinea", "Mali", "Morocco", 
                                                       "Mozambique", "Niger", "Uganda", "Palestine", "Rwanda", "Senegal", "Tanzania"))]
                selectedData <- selectedData3[rowSums(is.na(m)) != ncol(m), ]
                selectedData <- selectedData[complete.cases(selectedData[ ,1:3]),]
            }
        } else {
            if (df_year() == "All") {
                selectedData <- df()[!is.na(df()[, colnames(df()) == df_country()]),]
            } else {
                selectedData2 <- df() %>% filter(Year == df_year())
                selectedData <- selectedData2[!is.na(selectedData2[, colnames(selectedData2) == df_country()]),]
            }
        }
        valueBox(
            value = length(levels(as.factor(selectedData$`Document Type`))),
            subtitle = "Document type",
            icon = icon("folder-open"),
            color = "red"
        )
    })
    output$OpenAccess2 <- renderValueBox({
        if (df_country() == "All"){
            if (df_year() == "All") {
                selectedData <- df()
            } else {
                selectedData <- df() %>% filter(Year == df_year())
            }
        } else if(df_country() == "Partner countries"){
            if (df_year() == "All") {
                m <-df()[, which(colnames(df()) %in% c("Benin", "Burkina Faso", "Burundi", "DR Congo", "Guinea", "Mali", "Morocco", 
                                                       "Mozambique", "Niger", "Uganda", "Palestine", "Rwanda", "Senegal", "Tanzania"))]
                selectedData <- df()[rowSums(is.na(m)) != ncol(m), ]
                selectedData <- selectedData[complete.cases(selectedData[ ,1:3]),]
            } else {
                selectedData3 <- df() %>% filter(Year == df_year())
                m <-selectedData3[, which(colnames(selectedData3) %in% c("Benin", "Burkina Faso", "Burundi", "DR Congo", "Guinea", "Mali", "Morocco", 
                                                       "Mozambique", "Niger", "Uganda", "Palestine", "Rwanda", "Senegal", "Tanzania"))]
                selectedData <- selectedData3[rowSums(is.na(m)) != ncol(m), ]
                selectedData <- selectedData[complete.cases(selectedData[ ,1:3]),]
            }
        } else {
            if (df_year() == "All") {
                selectedData <- df()[!is.na(df()[, colnames(df()) == df_country()]),]
            } else {
                selectedData2 <- df() %>% filter(Year == df_year())
                selectedData <- selectedData2[!is.na(selectedData2[, colnames(selectedData2) == df_country()]),]
            }
        }
        valueBox(
            value = round(sum(!is.na(selectedData$`Access Type`))*100/(sum(is.na(selectedData$`Access Type`))+sum(!is.na(selectedData$`Access Type`))), digits = 2),
            subtitle = "Percentage of open-access publication",
            icon = icon("lock-open"),
            color = "purple"
        )
    })
    output$Totaljournal2 <- renderValueBox({
        if (df_country() == "All"){
            if (df_year() == "All") {
                selectedData <- df()
            } else {
                selectedData <- df() %>% filter(Year == df_year())
            }
        } else if(df_country() == "Partner countries"){
            if (df_year() == "All") {
                m <-df()[, which(colnames(df()) %in% c("Benin", "Burkina Faso", "Burundi", "DR Congo", "Guinea", "Mali", "Morocco", 
                                                       "Mozambique", "Niger", "Uganda", "Palestine", "Rwanda", "Senegal", "Tanzania"))]
                selectedData <- df()[rowSums(is.na(m)) != ncol(m), ]
                selectedData <- selectedData[complete.cases(selectedData[ ,1:3]),]
            } else {
                selectedData3 <- df() %>% filter(Year == df_year())
                m <-selectedData3[, which(colnames(selectedData3) %in% c("Benin", "Burkina Faso", "Burundi", "DR Congo", "Guinea", "Mali", "Morocco", 
                                                       "Mozambique", "Niger", "Uganda", "Palestine", "Rwanda", "Senegal", "Tanzania"))]
                selectedData <- selectedData3[rowSums(is.na(m)) != ncol(m), ]
                selectedData <- selectedData[complete.cases(selectedData[ ,1:3]),]
            }
        } else {
            if (df_year() == "All") {
                selectedData <- df()[!is.na(df()[, colnames(df()) == df_country()]),]
            } else {
                selectedData2 <- df() %>% filter(Year == df_year())
                selectedData <- selectedData2[!is.na(selectedData2[, colnames(selectedData2) == df_country()]),]
            }
        }
        valueBox(
            value = prettyNum(nlevels(as.factor(selectedData$`Source title`)), big.mark = ","),
            subtitle = "Total number of journal",
            icon = icon("book"),
            color = "maroon"
        )
    })
    # Output table in Info tab ####
    output$table <- DT::renderDataTable(server = FALSE, {
        if (df_country() == "All"){
            if (df_year() == "All") {
                selectedData <- df()
            } else {
                selectedData <- df() %>% filter(Year == df_year())
            }
        } else if(df_country() == "Partner countries"){
            if (df_year() == "All") {
                m <-df()[, which(colnames(df()) %in% c("Benin", "Burkina Faso", "Burundi", "DR Congo", "Guinea", "Mali", "Morocco", 
                                                       "Mozambique", "Niger", "Uganda", "Palestine", "Rwanda", "Senegal", "Tanzania"))]
                selectedData <- df()[rowSums(is.na(m)) != ncol(m), ]
                selectedData <- selectedData[complete.cases(selectedData[ ,1:3]),]
            } else {
                selectedData3 <- df() %>% filter(Year == df_year())
                m <-selectedData3[, which(colnames(selectedData3) %in% c("Benin", "Burkina Faso", "Burundi", "DR Congo", "Guinea", "Mali", "Morocco", 
                                                       "Mozambique", "Niger", "Uganda", "Palestine", "Rwanda", "Senegal", "Tanzania"))]
                selectedData <- selectedData3[rowSums(is.na(m)) != ncol(m), ]
                selectedData <- selectedData[complete.cases(selectedData[ ,1:3]),]
            }
        } else {
            if (df_year() == "All") {
                selectedData <- df()[!is.na(df()[, colnames(df()) == df_country()]),]
            } else {
                selectedData2 <- df() %>% filter(Year == df_year())
                selectedData <- selectedData2[!is.na(selectedData2[, colnames(selectedData2) == df_country()]),]
            }
        }
        data_selected <- selectedData[,c("Authors", "Title", "Year", "Abbreviated Source Title", "Cited by", 
                                         "Document Type", "DOI")]
        
        DT::datatable({DT::datatable(data_selected)
            data_selected$DOI <- paste0("<a href='", "https://doi.org/", data_selected$DOI,"' target='_blank'>", "https://doi.org/", data_selected$DOI,"</a>")
            data_selected
        }, escape = FALSE,
        filter="top", 
        selection="multiple", 
        extensions = c('Buttons'),
        options = list(sDom  = '<"top"pB>t<"bottom"i>r',
                       pageLength = 5,
                       buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                       scrollX = TRUE,
                       autoWidth = FALSE))
    })
    
    # Output map in Info tab ####
    output$map <- renderLeaflet({
        if (df_country() == "All"){
            if (df_year() == "All") {
                selectedData <- df()
            } else {
                selectedData <- df() %>% filter(Year == df_year())
            }
        } else if(df_country() == "Partner countries"){
            if (df_year() == "All") {
                m <-df()[, which(colnames(df()) %in% c("Benin", "Burkina Faso", "Burundi", "DR Congo", "Guinea", "Mali", "Morocco", 
                                                       "Mozambique", "Niger", "Uganda", "Palestine", "Rwanda", "Senegal", "Tanzania"))]
                selectedData <- df()[rowSums(is.na(m)) != ncol(m), ]
                selectedData <- selectedData[complete.cases(selectedData[ ,1:3]),]
            } else {
                selectedData3 <- df() %>% filter(Year == df_year())
                m <-selectedData3[, which(colnames(selectedData3) %in% c("Benin", "Burkina Faso", "Burundi", "DR Congo", "Guinea", "Mali", "Morocco", 
                                                       "Mozambique", "Niger", "Uganda", "Palestine", "Rwanda", "Senegal", "Tanzania"))]
                selectedData <- selectedData3[rowSums(is.na(m)) != ncol(m), ]
                selectedData <- selectedData[complete.cases(selectedData[ ,1:3]),]
            }
        } else {
            if (df_year() == "All") {
                selectedData <- df()[!is.na(df()[, colnames(df()) == df_country()]),]
            } else {
                selectedData2 <- df() %>% filter(Year == df_year())
                selectedData <- selectedData2[!is.na(selectedData2[, colnames(selectedData2) == df_country()]),]
            }
        }
        
        selectedData_v2 <- selectedData[,c(first_country:last_country, 17)] %>%
            gather(key = "Country", value = "value", -`Document Type`, na.rm =TRUE) 
        
        selectedData_v3 <- selectedData[,c(first_lat:last_lat, 17)] %>%
            gather(key = "Lat", value = "value", -`Document Type`, na.rm =TRUE) 
        
        selectedData_v4 <- selectedData[,c(first_long:last_long, 17)] %>%
            gather(key = "Long", value = "value", -`Document Type`, na.rm =TRUE) 
        
        
        selectedData_v2$lat <- selectedData_v3$value
        selectedData_v2$long <- selectedData_v4$value
        
        selectedData_v2 <- selectedData_v2 %>% 
            group_by(Country, lat, long, `Document Type`) %>%
            summarise(n=n()) %>%
            spread(key = `Document Type`, value = n)
        
        selectedData_v2$Total <- rowSums(subset(selectedData_v2, select = -c(Country, lat, long)), na.rm = TRUE)
        selectedData_v2 <- selectedData_v2[-which(selectedData_v2$Country == "Belgium"),]
        
        
        tilesURL <- '//server.arcgisonline.com/ArcGIS/rest/services/World_Street_Map/MapServer/tile/{z}/{y}/{x}'
        colors <- brewer.pal(n = 12, name = "Paired")
        colors <- colors[c(2:12,1)]
        leaflet() %>%
            addTiles(tilesURL) %>%
            fitBounds(lng1 = -141.152344, lat1 = 55.646599, lng2 = 161.542969, lat2 = -52.194140) %>%
            addMinicharts(selectedData_v2$lat, selectedData_v2$long,
                          type = "pie",
                          chartdata = subset(selectedData_v2, select = -c(Country, lat, long, Total)),
                          colorPalette = colors,
                          width = 80 * sqrt(selectedData_v2$Total) / sqrt(max(selectedData_v2$Total)),
                          transitionTime = 0)
    })
    
    # Output journal in Journal tab ####
    output$topjournal <- renderPlotly({
        if (df_country() == "All"){
            if (df_year() == "All") {
                selectedData <- df()
            } else {
                selectedData <- df() %>% filter(Year == df_year())
            }
        } else if(df_country() == "Partner countries"){
            if (df_year() == "All") {
                m <-df()[, which(colnames(df()) %in% c("Benin", "Burkina Faso", "Burundi", "DR Congo", "Guinea", "Mali", "Morocco", 
                                                       "Mozambique", "Niger", "Uganda", "Palestine", "Rwanda", "Senegal", "Tanzania"))]
                selectedData <- df()[rowSums(is.na(m)) != ncol(m), ]
                selectedData <- selectedData[complete.cases(selectedData[ ,1:3]),]
            } else {
                selectedData3 <- df() %>% filter(Year == df_year())
                m <-selectedData3[, which(colnames(selectedData3) %in% c("Benin", "Burkina Faso", "Burundi", "DR Congo", "Guinea", "Mali", "Morocco", 
                                                       "Mozambique", "Niger", "Uganda", "Palestine", "Rwanda", "Senegal", "Tanzania"))]
                selectedData <- selectedData3[rowSums(is.na(m)) != ncol(m), ]
                selectedData <- selectedData[complete.cases(selectedData[ ,1:3]),]
            }
        } else {
            if (df_year() == "All") {
                selectedData <- df()[!is.na(df()[, colnames(df()) == df_country()]),]
            } else {
                selectedData2 <- df() %>% filter(Year == df_year())
                selectedData <- selectedData2[!is.na(selectedData2[, colnames(selectedData2) == df_country()]),]
            }
        }
        
        WN_journal <- selectedData %>% select(`Source title`) %>% 
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
        if (df_country() == "All"){
            if (df_year() == "All") {
                selectedData <- df()
            } else {
                selectedData <- df() %>% filter(Year == df_year())
            }
        } else if(df_country() == "Partner countries"){
            if (df_year() == "All") {
                m <-df()[, which(colnames(df()) %in% c("Benin", "Burkina Faso", "Burundi", "DR Congo", "Guinea", "Mali", "Morocco", 
                                                       "Mozambique", "Niger", "Uganda", "Palestine", "Rwanda", "Senegal", "Tanzania"))]
                selectedData <- df()[rowSums(is.na(m)) != ncol(m), ]
                selectedData <- selectedData[complete.cases(selectedData[ ,1:3]),]
            } else {
                selectedData3 <- df() %>% filter(Year == df_year())
                m <-selectedData3[, which(colnames(selectedData3) %in% c("Benin", "Burkina Faso", "Burundi", "DR Congo", "Guinea", "Mali", "Morocco", 
                                                       "Mozambique", "Niger", "Uganda", "Palestine", "Rwanda", "Senegal", "Tanzania"))]
                selectedData <- selectedData3[rowSums(is.na(m)) != ncol(m), ]
                selectedData <- selectedData[complete.cases(selectedData[ ,1:3]),]
            }
        } else {
            if (df_year() == "All") {
                selectedData <- df()[!is.na(df()[, colnames(df()) == df_country()]),]
            } else {
                selectedData2 <- df() %>% filter(Year == df_year())
                selectedData <- selectedData2[!is.na(selectedData2[, colnames(selectedData2) == df_country()]),]
            }
        }

        WN_OA <- selectedData %>% select(`Access Type`, Year) %>% 
            dplyr::group_by(`Access Type`,Year) %>% 
            dplyr::summarise(n=n()) 
        WN_OA2 <- WN_OA[1:11,]
        WN_OA2$`Open Access` <- round(WN_OA2$n*100/(WN_OA[12:22,]$n+WN_OA2$n), 2)
        
        ggplotly(ggplot(WN_OA2, aes(x = Year,  y = `Open Access`, color = 'tomato')) +
                     geom_point(size = 2, color = 'tomato')+
                     geom_line(size = 1.1125, color = 'tomato')+
                     theme_bw() +
                     xlab("Year") +
                     ylab("Open Acess (%)") +
                     scale_x_continuous(name = "Year", limits = c(2009,2019), breaks = c(2009:2019)) +
                     theme(text=element_text(family = "Arial")) +
                     theme(legend.title = element_blank()) +
                     theme(legend.text =  element_blank())
        )
    })
    # Output publication year in Research tab ####
    output$Pubyear <- renderPlotly({
        if (df_country() == "All"){
            if (df_year() == "All") {
                selectedData <- df()
            } else {
                selectedData <- df() %>% filter(Year == df_year())
            }
        } else if(df_country() == "Partner countries"){
            if (df_year() == "All") {
                m <-df()[, which(colnames(df()) %in% c("Benin", "Burkina Faso", "Burundi", "DR Congo", "Guinea", "Mali", "Morocco", 
                                                       "Mozambique", "Niger", "Uganda", "Palestine", "Rwanda", "Senegal", "Tanzania"))]
                selectedData <- df()[rowSums(is.na(m)) != ncol(m), ]
                selectedData <- selectedData[complete.cases(selectedData[ ,1:3]),]
            } else {
                selectedData3 <- df() %>% filter(Year == df_year())
                m <-selectedData3[, which(colnames(selectedData3) %in% c("Benin", "Burkina Faso", "Burundi", "DR Congo", "Guinea", "Mali", "Morocco", 
                                                       "Mozambique", "Niger", "Uganda", "Palestine", "Rwanda", "Senegal", "Tanzania"))]
                selectedData <- selectedData3[rowSums(is.na(m)) != ncol(m), ]
                selectedData <- selectedData[complete.cases(selectedData[ ,1:3]),]
            }
        } else {
            if (df_year() == "All") {
                selectedData <- df()[!is.na(df()[, colnames(df()) == df_country()]),]
            } else {
                selectedData2 <- df() %>% filter(Year == df_year())
                selectedData <- selectedData2[!is.na(selectedData2[, colnames(selectedData2) == df_country()]),]
            }
        }
        
        WN_PU_year <- selectedData %>% select(Year,`Document Type`) %>% 
            dplyr::group_by(Year, `Document Type`) %>% 
            dplyr::summarise(`Number of publication`=n()) %>% 
            dplyr::arrange(Year)
        
        ggplotly(ggplot(WN_PU_year, aes(x=Year, y=`Number of publication`, color = `Document Type`, group = `Document Type`))+
                     geom_point(size = 2)+
                     geom_line(size = 1.1125)+
                     theme_bw() +
                     xlab("Year") +
                     ylab("Number of Publication") +
                     theme(text=element_text(family = "Arial")) +
                     scale_x_continuous(name = "Year", limits = c(2009,2019), breaks = c(2009:2019)) +
                     theme(legend.title = element_blank())
        )
        
    })
    # Output top keywords in Research tab ####
    output$Topkw <- renderPlot({
        if (df_country() == "All"){
            if (df_year() == "All") {
                selectedData <- df()
            } else {
                selectedData <- df() %>% filter(Year == df_year())
            }
        } else if(df_country() == "Partner countries"){
            if (df_year() == "All") {
                m <-df()[, which(colnames(df()) %in% c("Benin", "Burkina Faso", "Burundi", "DR Congo", "Guinea", "Mali", "Morocco", 
                                                       "Mozambique", "Niger", "Uganda", "Palestine", "Rwanda", "Senegal", "Tanzania"))]
                selectedData <- df()[rowSums(is.na(m)) != ncol(m), ]
                selectedData <- selectedData[complete.cases(selectedData[ ,1:3]),]
            } else {
                selectedData3 <- df() %>% filter(Year == df_year())
                m <-selectedData3[, which(colnames(selectedData3) %in% c("Benin", "Burkina Faso", "Burundi", "DR Congo", "Guinea", "Mali", "Morocco", 
                                                       "Mozambique", "Niger", "Uganda", "Palestine", "Rwanda", "Senegal", "Tanzania"))]
                selectedData <- selectedData3[rowSums(is.na(m)) != ncol(m), ]
                selectedData <- selectedData[complete.cases(selectedData[ ,1:3]),]
            }
        } else {
            if (df_year() == "All") {
                selectedData <- df()[!is.na(df()[, colnames(df()) == df_country()]),]
            } else {
                selectedData2 <- df() %>% filter(Year == df_year())
                selectedData <- selectedData2[!is.na(selectedData2[, colnames(selectedData2) == df_country()]),]
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
        WN_TopKW <- KW(selectedData$`Author Keywords`)
        ggplot(WN_TopKW[1:50,], aes(label = keyword, size =n,color = rainbow_hcl(50))) +
            geom_text_wordcloud_area(shape = "circle") +
            scale_size_area(max_size = 20) +
            theme_minimal()
    })
}

#### Run the application ####
shinyApp(ui = ui, server = server)
