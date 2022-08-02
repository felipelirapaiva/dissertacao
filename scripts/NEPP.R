# Calculando o NEPP das Assembleias Legislativas estaduais

library(tidyverse)
library(haven)
library(readr)
library(tidyr)

link <- "https://github.com/felipelirapaiva/dissertacao/blob/main/rawdata/deputados.csv?raw=true"
download.file(link, "deputados.csv")
deputados <- read.csv("deputados.csv", dec = ",", encoding = "UTF-8", check.names = FALSE)


# Contando a magnitude distrital
seats <- deputados %>%
  group_by (ANO_ELEICAO, UF)%>%
  count()%>%
  rename(magnitude = n)

# Contando número de parlamentares eleitos por partido por mandato por UF
parlamentares <- deputados %>%
  group_by(ANO_ELEICAO, UF, SIGLA_PARTIDO)%>%
  count()%>%
  rename (eleitos = n)

# Mergindo os bancos
library(scales)
NEPP <- left_join(parlamentares, seats,
                  by = c("UF" = "UF",
                         "ANO_ELEICAO" = "ANO_ELEICAO"))


# Operações matemáticas para chegar ao NEPP
  NEPP$divisao <- NEPP$eleitos / NEPP$magnitude
  NEPP$quadrado <- NEPP$divisao * NEPP$divisao
  
  NEPP <- NEPP %>%
    group_by(ANO_ELEICAO, UF)%>%
    summarise(soma = sum(quadrado))
  
  NEPP$final <- 1 / NEPP$soma
  
# Juntando a magnitude
  valores_NEPP <- left_join(NEPP, seats,
                    by = c("UF" = "UF",
                           "ANO_ELEICAO" = "ANO_ELEICAO"))%>%
    select(ANO_ELEICAO, UF, final, magnitude)%>%
    rename(NEPP = final)
  
    
# Construindo um boxplot
  library(ggpubr)
  
  valores_NEPP %>%
    ggplot( aes(x=UF, y=NEPP)) +
    geom_boxplot() +
    theme_pubr()+
    xlab("") + ylab("NEPP")+
    theme(axis.text.x = element_text( 
      angle = 30))+
    theme(axis.text = element_text(
      size = 10.5))

# Download dos dados  
  library(writexl)
  
  write_csv(valores_NEPP,"C:\\Users\\Felipe\\Documents\\Dissertacao R\\Diss\\valores_NEPP.csv")