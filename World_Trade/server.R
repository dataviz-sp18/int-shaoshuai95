library(tidyverse)
library(dplyr)
library(plotly)
library(readr)
library(ggplot2)
library(ggpubr)
library(quantmod)

#Import the dataset downloaded from the WTO website
USA <- read_csv("United_WTO.csv")
China <- read_csv("China.csv")

#USA and China Exports
USA_X <- filter(USA,Partner_code=="WL" &
                  Flow_Description=="Exports" &
                  Indicator_description=="Total merchandise") %>%
  select(9,11)

China_X <- filter(China,Partner_code=="WL" &
                    Flow_Description=="Exports" &
                    Indicator_description=="Total merchandise") %>%
  select(9,11)

#USA and China Imports
China_M <- filter(China,Partner_code=="WL" &
                    Flow_Description=="Imports" &
                    Indicator_description=="Total merchandise") %>%
  select(9,11)

USA_M <- filter(USA,Partner_code=="WL" &
                  Flow_Description=="Imports" &
                  Indicator_description=="Total merchandise") %>%
  select(9,11)

#Combine the Imports and Exports data
all_X <- 
  USA_X %>% 
  left_join(China_X,by=c("Year")) %>%
  select(Year = Year,USA_X = Value.x,China_X = Value.y) %>%
  filter(Year>1977)

all_M <- 
  USA_M %>% 
  left_join(China_M,by=c("Year")) %>%
  select(Year = Year,USA_M = Value.x,China_M = Value.y) %>%
  filter(Year>1977)

total_M <- 
  all_M %>% 
  select(USA=USA_M,China=China_M,Year=Year) %>%
  gather('China','USA',key="Country",value="Value") 

total_X <- 
  all_X %>% 
  select(USA=USA_X,China=China_X,Year=Year) %>%
  gather('China','USA',key="Country",value="Value") 

combine <- all_M %>%
  left_join(all_X, by=c("Year"))





p <- plot_ly(combine, x= ~Year) %>%
  add_lines(y = ~China_M, name = "China Imports", 
            line = list(color = 'rgb(205, 12, 24)')) %>%
  add_lines(y = ~China_X, name = "China Exports",
            line = list(color = 'rgb(22, 96, 167)')) %>%
  add_lines(y = ~USA_M, name = "USA Imports",
            line = list(color = 'rgb(205, 12, 24)', dash = 'dash')) %>%
  add_lines(y = ~USA_X, name = "USA Exports",
            line = list(color = 'rgb(22, 96, 167)', dash = 'dash')) %>%
  layout(
    xaxis = list(rangeslider = list(type = "date")),
    yaxis = list(title = "Value (Million USD$)"))


accumulate_by <- function(dat, var) {
  var <- lazyeval::f_eval(var, dat)
  lvls <- plotly:::getLevels(var)
  dats <- lapply(seq_along(lvls), function(x) {
    cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
  })
  dplyr::bind_rows(dats)
}

total_M$Year=as.integer(total_M$Year)
m <- total_M %>%
  filter(Country %in% c("USA", "China")) %>%
  accumulate_by(~Year)

total_X$Year=as.integer(total_X$Year)
x <- total_X %>%
  filter(Country %in% c("USA", "China")) %>%
  accumulate_by(~Year)

M <- m %>%
  plot_ly(
    x = ~Year, 
    y = ~Value,
    split = ~Country,
    type = 'scatter',
    mode = 'lines', 
    frame = ~frame,
    line = list(simplyfy = F)
  ) %>% 
  layout(
    xaxis = list(
      title = "Year",
      zeroline = F
    ),
    yaxis = list(
      title = "Value (Million USD$)",
      zeroline = F
    )) %>% 
  animation_opts(
    frame = 100, 
    transition = 0, 
    redraw = FALSE
  ) %>%
  animation_slider(
    hide = T
  ) %>%
  animation_button(x = 1, xanchor = "right", y = 0, yanchor = "bottom"
  )

X <- x %>%
  plot_ly(
    x = ~Year, 
    y = ~Value,
    split = ~Country,
    type = 'scatter',
    mode = 'lines', 
    frame = ~frame,
    line = list(simplyfy = F)
  ) %>% 
  layout(
    xaxis = list(
      title = "Year",
      zeroline = F
    ),
    yaxis = list(
      title = "Value (Million USD$)",
      zeroline = F
    )) %>% 
  animation_opts(
    frame = 100, 
    transition = 0, 
    redraw = FALSE
  ) %>%
  animation_slider(
    hide = T
  ) %>%
  animation_button(x = 1, xanchor = "right", y = 0, yanchor = "bottom"
  )





server <- function(input, output){
  output$coolplot <- renderPlotly({p})
  output$Xplot <- renderPlotly({X})
  output$Mplot <- renderPlotly({M})
  }
