library(plotly)
library(plyr)
#USPersonalExpenditure <- data.frame("Categorie"=rownames(USPersonalExpenditure), USPersonalExpenditure)
#data <- USPersonalExpenditure[,c('Categorie', 'X1955')]
Sys.setlocale(category = "LC_ALL", locale = "Portuguese_Portugal.1252")

fifa <-read.csv(file="D:/Downloads/shiny_app/Shiny_FIFA/fifa.csv", header=TRUE, sep=",", stringsAsFactors = TRUE, encoding = "UTF-8")
countries <- read.csv(file="D:/Downloads/shiny_app/Shiny_FIFA/countries.csv", header=TRUE, sep=",", encoding = "UTF-8")

fifa_group <- group_by(fifa, Nationality) 

ToNumber <- function(X)
{
  A <- gsub("%","*1e-2",gsub("K","*1e+3",gsub("M","*1e+6",gsub("\\€|,","",as.character(X)),fixed=TRUE),fixed=TRUE),fixed=TRUE)
  B <- try(sapply(A,function(a){eval(parse(text=a))}),silent=TRUE)
  if (is.numeric(B)) return (as.numeric(B)) else return(X)
}

ToNumber("€77M")

fifaSummary <- group_by(fifa, Nationality) %>% summarise(players =n(), 
                         media_valor = 
                           currency(mean(ToNumber(Value)), digits = 0L,symbol = "€"),
                         maior_valor = currency(max(ToNumber(Value)), digits = 0L, symbol = "€"),
                         nome = Name[which.max(ToNumber(Value))]
) %>% left_join(countries, by="Nationality")

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

c <- filter(fifa, Name == "Cristiano Ronaldo")

numX <- as.data.frame(lapply(as.list(c),ToNumber))

fifa$Name[which.max(max(as.numeric(fifa$Value)))]
fifaSummary <- left_join(fifaSummary, countries, by="Nationality")

countries <- read.csv(file="D:/Downloads/shiny_app/Shiny_FIFA/countries.csv", header=TRUE, sep=",")
leaflet(fifaSummary) %>% 
  addTiles() %>% 
  addMarkers(
    ~longitude, 
    ~latitude, 
    popup =  ~paste0('<span> Quantidade de Jogadores: ',players,'</span><br><span> Media de valor de mercado dos jogadores:', media_valor ,'</span>'), 
    label = ~as.character(country))


filtrado <- subset(fifa, fifa$Club == "FC Barcelona")
filtrado2 <- data.frame(filtrado$Position)
pie <- as.data.frame(table(filtrado2))


plot_ly(pie, labels = ~filtrado2, values = ~Freq, type = 'pie') %>%
  layout(title = 'United States Personal Expenditures by Categories in 1960',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

posicoes <- read.csv(file="C:/Users/145063/Downloads/fifa19/posicoes.csv", header=TRUE, sep=",")
filtrado <- subset(fifa, fifa$Club == "Juventus")
filtrado2 <- data.frame(position=filtrado$Position)
posicoes <- posicoes[order(posicoes$ab),]  

posicoes <- data.frame(posicoes)
ind <- posicoes[order(posicoes$ab),]
require(reshape2)
fifa$id <- rownames(fifa) 
melt(fifa)

fifa_2 <- data.frame(name = fifa$Name, country = fifa$Nationality)
median(as.numeric(fifa$Value))
a <-aggregate(as.numeric(fifa$Value), list(fifa$Nationality), mean)

require()
aggregate(as.numeric(fifa$Value), list(fifa$Nationality), mean)
us <- mean(as.numeric(fifa$Value)) %>% group_by(fifa$Nationality)

