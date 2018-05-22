library(tidyverse)
library(dplyr)
library(plotly)
library(readr)
library(ggplot2)
library(ggpubr)

#Import the necessary data set downloaded from the WTO website
USA <- read_csv("~/Documents/UChicago/2017 Autumn/R/hw10/WorldTrade/United_WTO.csv")
China <- read_csv("~/Documents/UChicago/2017 Autumn/R/hw10/WorldTrade/China.csv")

USA_X <- filter(USA,
                Partner_code=="WL" &
                  Flow_Description=="Exports" &
                  Indicator_description=="Total merchandise") %>%
  #Select the variables
  select(9,11)

China_X <- filter(China,
                  Partner_code=="WL" & 
                    Flow_Description=="Exports" &
                    Indicator_description=="Total merchandise")%>%
  #Select the variables
  select(9,11)

China_M <- filter(China,
                  Partner_code=="WL" & 
                    Flow_Description=="Imports" &
                    Indicator_description=="Total merchandise")%>%
  #Select the variables
  select(9,11)

USA_M <- filter(USA,
                Partner_code=="WL" &
                  Flow_Description=="Imports" &
                  Indicator_description=="Total merchandise") %>%
  select(9,11)

all_X <- 
  USA_X %>% 
  left_join(China_X,by=c("Year")) %>%
  #Rename the variables
  select(Year = Year,USA_X = Value.x,China_X = Value.y) %>%
  filter(Year>1977)

all_M <- 
  USA_M %>% 
  left_join(China_M,by=c("Year")) %>%
  #Rename the variables
  select(Year = Year,USA = Value.x,China = Value.y) %>%
  filter(Year>1977)
total_M <- all_M %>%
  gather('China','USA',key="Country",value="Value")

combine <- all_X %>%
  left_join(all_M, by=c("Year"))

combine$Year=as.character(combine$Year)

X <- plot_ly(combine, x= ~Year) %>%
  add_lines(y = ~China_M, name = "China Imports", 
            line = list(color = 'rgb(205, 12, 24)')) %>%
  add_lines(y = ~China_X, name = "China Exports",
            line = list(color = 'rgb(22, 96, 167)')) %>%
  add_lines(y = ~USA_M, name = "USA Imports",
            line = list(color = 'rgb(205, 12, 24)', dash = 'dash')) %>%
  add_lines(y = ~USA_X, name = "USA Exports",
            line = list(color = 'rgb(22, 96, 167)', dash = 'dash')) %>%
  layout(
    title = "USA vs. China Exports",
    xaxis = list(rangeslider = list(type = "date")),
    
    yaxis = list(title = "Value (million $)"))

server <- function(input, output){

  
  output$coolplot <- renderPlotly({X})

  }
