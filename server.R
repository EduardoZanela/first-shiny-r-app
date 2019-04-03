#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(DT)
library(plotly)
library(dplyr)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  posicoes <- read.csv(file="posicoes.csv", header=TRUE, sep=",")
  fifa <- read.csv(file="fifa.csv", header=TRUE, sep=",")
  countries <- read.csv(file="countries.csv", header=TRUE, sep=",")
  fifa_names <- data.frame(
    Jogador = paste0('<img src="',fifa$Photo,'" height="48" width="48"></img> <span>',fifa$Name,'</span>'),
    Idade = fifa$Age,
    Clube = paste0('<img src="',fifa$Club.Logo,'" height="24" width="24"></img> <span>',fifa$Club,'</span>'),
    Nacionalidade = paste0('<img src="',fifa$Flag,'" height="17" width="23"></img> <span>',fifa$Nationality,'</span>')
    )
  
  derive_beta_angles <- reactive({
    filtrado <- subset(fifa, fifa$Club == input$club)
    filtrado2 <- data.frame(ab = filtrado$Position)
    posicoes <- posicoes[order(posicoes$ab),]
    filtrado2 <- left_join(filtrado2, posicoes)
    a <-as.data.frame(table(filtrado2))
    subset(a, a$Freq > 0)
  })
  
  output$fifa <- renderUI({
    selectInput('club', 'Selecione o time', fifa$Club, selectize=TRUE)
  })
  output$distPlot <- renderPlotly({
    filter <- derive_beta_angles()
    plot_ly(filter, labels = ~nome, values = ~Freq, type = 'pie') %>%
      layout(title = 'Grafico de posicoes de jogadores por clube',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  
  output$table <- DT::renderDataTable({
    DT::datatable(fifa_names, escape = FALSE)
  }, options = list(pageLength = 5, stateSave = TRUE))
  
  #output$map <- renderLeaflet({
  #  leaflet(countries) %>% addTiles() %>% addMarkers(~longitude, ~latitude, popup = ~as.character(name), label = ~as.character(country))
  #})
  
})
