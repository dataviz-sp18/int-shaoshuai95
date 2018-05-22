library(shiny)
library(shinythemes)
library(plotly)
library(DT)

ui <- fluidPage(
  theme = shinytheme("sandstone"),
  titlePanel("USA & China World Trade (1978-2016)"),
  mainPanel(tabsetPanel(type = "tabs",
            tabPanel("General", plotlyOutput("coolplot")),
            tabPanel("Exports", plotlyOutput("Xplot")),
            tabPanel("Imports", plotlyOutput("Mplot"))
      )
    )
  )

