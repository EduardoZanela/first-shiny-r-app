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
library(formattable)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  # importacao dos dados
  posicoes <- read.csv(file="./data/posicoes.csv", header=TRUE, sep=",", encoding = "UTF-8")
  fifa <- read.csv(file="./data/fifa.csv", header=TRUE, sep=",", encoding = "UTF-8")
  countries <- read.csv(file="./data/countries.csv", header=TRUE, sep=",", encoding = "UTF-8")
  
  # cria um data frama com apes alguns campos e fotos em html para ser mostrado na tabela
  fifa_names <- data.frame(
    Jogador = paste0('<img src="',fifa$Photo,'" height="48" width="48"></img> <span>',fifa$Name,'</span>'),
    Idade = fifa$Age,
    Clube = paste0('<img src="',fifa$Club.Logo,'" height="24" width="24"></img> <span>',fifa$Club,'</span>'),
    Nacionalidade = paste0('<img src="',fifa$Flag,'" height="17" width="23"></img> <span>',fifa$Nationality,'</span>')
  )
  
  # funcao gera dados para o grafico
  chart_player_position_club <- reactive({
    #filtra pelo nome do clube que vem do UI
    club_filtered_list <- subset(fifa, fifa$Club == input$club)
    #Cria vetor somente com Position e coluna chamada abreviation
    club_filtered_list <- data.frame(abreviation = club_filtered_list$Position)
    #Left join inserir nome de posicoes na tabela
    chart_list <- left_join(club_filtered_list, posicoes, by="abreviation")
    # faz o calculo de jogadores por posicoes
    freq_position <- as.data.frame(table(chart_list))
    # cria um sub vetor com maiores q zero
    subset(freq_position, freq_position$Freq > 0)
  })
  
  # funcao que converte valor de moeda para numero ex: $1M -> 1000
  toNumber <- function(X){
    A <- gsub("%","*1e-2",gsub("K","*1e+3",gsub("M","*1e+6",gsub("\\€|,","",as.character(X)),fixed=TRUE),fixed=TRUE),fixed=TRUE)
    B <- try(sapply(A,function(a){eval(parse(text=a))}),silent=TRUE)
    if (is.numeric(B)) return (as.numeric(B)) else return(X)
  }
  
  # saida para ui renderiza a o input para selecao de times
  output$fifa <- renderUI({
    selectInput('club', 'Selecione o time', fifa$Club, selectize=TRUE)
  })
  
  #  plota o grafico 
  output$distPlot <- renderPlotly({
    # cria o filtro por posicao
    filter <- chart_player_position_club()
    # plota grafico
    plot_ly(filter, labels = ~nome, values = ~Freq, type = 'pie') %>%
      layout(title = 'Grafico de posicoes de jogadores por clube',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  
  # saida para ui da tabela de jogadores
  output$table <- DT::renderDataTable({
    DT::datatable(fifa_names, escape = FALSE)
  }, options = list(pageLength = 5, stateSave = TRUE))
  
  # saida ui mapa com dados
  output$map <- renderLeaflet({
    # agrupa os dados por pais dos jogadores sumariza os dados nas colunas abaixo e faz uma juncao com a tabela countries
    fifaSummary <- group_by(fifa, Nationality) %>% 
                    summarise(players =n(), 
                               media_valor = currency(mean(toNumber(Value)), digits = 0L,symbol = "€"),
                               maior_valor = currency(max(toNumber(Value)), digits = 0L, symbol = "€"),
                               nome = Name[which.max(toNumber(Value))]) %>% 
                  left_join(countries, by="Nationality")
    # monta mapa leaflet
    leaflet(fifaSummary) %>% 
      addTiles() %>% 
      addMarkers(
        ~longitude, 
        ~latitude, 
        popup =  ~paste0('<span>',Nationality,
                         '</span><br><span> Quantidade de Jogadores: ',players,
                         '</span><br><span> Media de valor de mercado dos jogadores: ', media_valor,
                         '</span><br><span> Valor Jogador mais caro: ', maior_valor,
                         '</span><br><span> Jogador mais caro: ', nome,
                         '</span>'), 
        label = ~as.character(country))
    })
})
