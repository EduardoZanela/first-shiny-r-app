library(plotly)
library(plyr)
USPersonalExpenditure <- data.frame("Categorie"=rownames(USPersonalExpenditure), USPersonalExpenditure)
data <- USPersonalExpenditure[,c('Categorie', 'X1955')]
fifa <- read.csv(file="C:/Users/145063/Downloads/fifa19/data.csv", 
                      header=TRUE, 
                      sep=",")


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
