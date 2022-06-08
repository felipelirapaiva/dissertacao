# Calculando o NEPP das Assembleias Legislativas estaduais

library(tidyverse)
library(haven)
library(readr)
library(tidyr)


link <- "https://github.com/felipelirapaiva/dissertacao/blob/main/rawdata/deputados.csv?raw=true"
download.file(link, "deputados.csv")
deputados <- read.csv("deputados.csv", dec = ",", encoding = "UTF-8", check.names = FALSE)


# deputados_estaduais <- read.csv("~/Dissertacao R/Diss/deputados_estaduais.csv")


seats <- deputados %>%
  group_by (ANO_ELEICAO, UF)%>%
  count()%>%
  rename(cadeiras = n)


parlamentares <- deputados %>%
  group_by(ANO_ELEICAO, UF, SIGLA_PARTIDO)%>%
  count()%>%
  rename (eleitos = n)


library(scales)
NEPP <- left_join(parlamentares, seats,
                  by = c("UF" = "UF",
                         "ANO_ELEICAO" = "ANO_ELEICAO"))


  NEPP$divisao <- NEPP$eleitos / NEPP$cadeiras
  NEPP$quadrado <- NEPP$divisao * NEPP$divisao
  
  
  NEPP <- NEPP %>%
    group_by(ANO_ELEICAO, UF)%>%
    summarise(soma = sum(quadrado))
  
  
  NEPP$final <- 1 / NEPP$soma
  
  
# Construindo um boxplot
  library(ggpubr)
  
  NEPP %>%
    ggplot( aes(x=UF, y=final)) +
    geom_boxplot() +
    theme_pubr()+
    xlab("") + ylab("NEPP")+
    theme(axis.text.x = element_text( 
      angle = 30))+
    theme(axis.text = element_text(
      size = 10.5))