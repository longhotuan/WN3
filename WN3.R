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

load("WN3_v2.RData")

# 3 pages: general info: table + map, journal info: Top 20 journal + open-access , research info: Citation year, top keywords
# + 6 value boxes: Total of number publication, total countries, total document types, total cites, total authors, percentage of open-access

