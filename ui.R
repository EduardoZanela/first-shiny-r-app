#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(DT)
library(plotly)
library(shinycssloaders)
library(formattable)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  fluidRow(
    column(12,
      titlePanel("Analise estatística do dataset FIFA 19")
    )
  ),
  br(),
  fluidRow(
    column(12,
      tabsetPanel(
        tabPanel("Dados Totais", 
                 br(),
                 withSpinner(dataTableOutput("table"))),
        tabPanel("Posicao por clube",
                 br(),
                 fluidRow(
                   column(4,
                          withSpinner(uiOutput('fifa'))
                   ),
                   column(8,
                          hr(), 
                          withSpinner(plotlyOutput("distPlot")))
                   )
                 ),
        tabPanel("Mapa", withSpinner(leafletOutput("map", width = "100%",height = "700px")))
      )
    )
  )
  
))
