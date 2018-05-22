library(shiny)
library(shinythemes)
library(plotly)
library(DT)

ui <- fluidPage(
  theme = shinytheme("sandstone"),
  titlePanel("USA&China World Trade (1950-2016)"),
  mainPanel(tabsetPanel(type = "tabs",
            tabPanel("Plot", plotlyOutput("coolplot"))
      )
    )
  )

