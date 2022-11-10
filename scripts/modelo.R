##### Capítulo 4
library(tidyverse)
library(readr)

##### Dados de PIB #####
library(deflateBR)

# Fazendo download da base de dados
  link <- "https://github.com/felipelirapaiva/dissertacao/blob/main/rawdata/PIB_nominal.csv?raw=true"
  download.file(link, "PIB_nominal.csv")
  dados_pib <- read.csv("PIB_nominal.csv", dec = ",", encoding = "UTF-8", check.names = FALSE)

  dados_pib <- dados_pib %>%
    mutate(ANO=as.Date(ANO))

  dados_pib$PIB_nominal <- dados_pib$PIB_nominal * 1000000
  
# Usando o deflator IPCA (IBGE)
  dados_pib <- dados_pib %>%
    mutate(pib_deflacionado = deflate(PIB_nominal, ANO, "12/2010", "ipca"))

# Extraindo o ano  
  dados_pib$ANO_PIB <- format(dados_pib$ANO, format="%Y")

# Selecionando apenas 3 colunas    
  dados_pib <- dados_pib %>%
    select(UF, pib_deflacionado, ANO_PIB)
  
# Adicionando população
  link <- "https://github.com/felipelirapaiva/dissertacao/blob/main/rawdata/populacao.csv?raw=true"
  download.file(link, "populacao.csv")
  populacao <- read.csv("populacao.csv", dec = ",", encoding = "UTF-8", check.names = FALSE)
  
# Mergindo PIB e populacao
  dados_pib <- dados_pib %>%
    mutate(ANO_PIB=as.numeric(ANO_PIB))
  
  populacao <- populacao %>%
    mutate(ano = as.numeric(ano))
  
library(scales)

  dados_pibpercapita <- left_join(dados_pib, populacao,
                                  by = c("ANO_PIB" = "ano",
                                         "UF" = "sigla_uf"))

# PIB per capita
  dados_pibpercapita$pibpercapita <- dados_pibpercapita$pib_deflacionado / dados_pibpercapita$populacao
  

##### Abrindo a base de dados dos gabinetes #####
  link <- "https://github.com/felipelirapaiva/dissertacao/blob/main/mydata/gabinetes_final.csv?raw=true"
  download.file(link, "gabinetes_final.csv")
  gabinetes_final <- read.csv("gabinetes_final.csv", dec = ",", encoding = "UTF-8", check.names = FALSE)

##### Organizando as variáveis ######
#  glimpse(gabinetes_final)

# Transformando as variáveis em numéricas
  gabinetes_final <- gabinetes_final %>%
    mutate(coalescencia=as.numeric(coalescencia),
           id_distancia_gabinete =as.numeric(id_distancia_gabinete),
           id_distancia_coalizao =as.numeric(id_distancia_coalizao),
           NEPP=as.numeric(NEPP),
           magnitude=as.numeric(magnitude),
           napart_percent=as.numeric(napart_percent),
           PERCENTUAL=as.numeric(PERCENTUAL),
           INICIO_COALIZAO=as.Date(INICIO_COALIZAO),
           FINAL_COALIZAO=as.Date(FINAL_COALIZAO))%>%
    rename(naopart_percent = napart_percent)%>%
    rename(percentual_vitoria = PERCENTUAL)

# Extraindo o ano para fazer o merge
  gabinetes_final$ANO_COMECO <- format(gabinetes_final$INICIO_COALIZAO, format="%Y")
  
# Mergindo os gabinetes e o PIB

  gabinetes_final <- gabinetes_final %>%
    mutate(ANO_COMECO=as.numeric(ANO_COMECO))
  
  library(scales)
  gabinetes_final <- left_join(gabinetes_final, dados_pibpercapita,
                    by = c("SIGLA_UE" = "UF",
                           "ANO_COMECO" = "ANO_PIB"))

# Recodificando: variável de ano eleitoral
  gabinetes_final <- gabinetes_final %>%
    mutate(ano_eleitoral = case_when (ANO_CICLO_INIC_COAL == 1 ~ 0,
                                      ANO_CICLO_INIC_COAL == 3 ~ 0,
                                      ANO_CICLO_INIC_COAL == 2 ~ 1,
                                      ANO_CICLO_INIC_COAL == 4 ~ 1))
 
# Controle de cadeiras (%)
  # Percentual de cadeiras controladas pelo partido do governador
  gabinetes_final$cadeiras_gov_percent = gabinetes_final$CADEIRAS_GOV / gabinetes_final$magnitude
  
  # Percentual de cadeiras controladas pelo partido da chapa
  gabinetes_final$cadeiras_chapa_percent = gabinetes_final$CADEIRAS_CHAPA / gabinetes_final$magnitude


# Coligação e coalizão  
  gabinetes_final$COALIZAO_COLI_GOVERN_percent <- gabinetes_final$COALIZAO_COLI_GOVERN / gabinetes_final$NUM_PARTIDOS_COALIZAO

# Coligação e gabinete
  gabinetes_final$GABINETE_COLI_GOVERN_percent <- gabinetes_final$GABINETE_COLI_GOVERN / gabinetes_final$NUM_PARTIDOS_GABINETE

# Adicionando a duração  
  library(lubridate)
  
  gabinetes_final$diff_in_days = as.numeric (difftime(gabinetes_final$FINAL_COALIZAO, 
                     gabinetes_final$INICIO_COALIZAO, units = "days"))

  
    
##### Regressão das coalizões #####
  library(plm)
  library (fixest)
  
# Filtrando gabinetes com menos de 1 mês e retirando os governos que não foram coalizão
  testes_coalizoes <- gabinetes_final %>%
    filter(diff_in_days >= 30)%>% # 10 casos retirados
    filter(NUM_PARTIDOS_COALIZAO >= 2) # 52 casos retirados

# VD: Número de partidos na coalizão, sem controles
 regressao <- fixest::feols(NUM_PARTIDOS_COALIZAO ~ NEPP +
                      as.factor(SIGLA_UE), data=testes_coalizoes)

 summary(regressao)
 etable(regressao)
 
# VD: Número de partidos na coalizão, com controles
 regressao <- fixest::feols(NUM_PARTIDOS_COALIZAO ~
                              NEPP + pibpercapita + magnitude +
                              percentual_vitoria +
                              cadeiras_gov_percent + ano_eleitoral +
                              COALIZAO_COLI_GOVERN_percent +
                              as.factor(SIGLA_UE), data=testes_coalizoes)
 
 summary(regressao)
 etable(regressao)
 
 # VD: Número de partidos na coalizão, com controles e interações
 regressao <- fixest::feols(NUM_PARTIDOS_COALIZAO ~
                              NEPP + pibpercapita + magnitude +
                              percentual_vitoria +
                              cadeiras_gov_percent + ano_eleitoral +
                              COALIZAO_COLI_GOVERN_percent + 
                              NEPP * pibpercapita +
                              NEPP * magnitude +
                              NEPP * percentual_vitoria +
                              NEPP * cadeiras_gov_percent +
                              NEPP * ano_eleitoral +
                              NEPP * COALIZAO_COLI_GOVERN_percent +
                              as.factor(SIGLA_UE), data=testes_coalizoes)
 
 summary(regressao)
 etable(regressao) 

 
# VD: Ideologia, sem controles
 regressao <- fixest::feols(id_distancia_coalizao ~ NEPP + as.factor(SIGLA_UE),
                            data=testes_coalizoes)
 
 summary(regressao)
 etable(regressao)
 
# VD: Ideologia, com os controles, sem o controle de secretários técnicos
 regressao <- fixest::feols(id_distancia_coalizao ~
                              NEPP + pibpercapita + magnitude +
                              percentual_vitoria +
                              cadeiras_gov_percent + ano_eleitoral +
                              COALIZAO_COLI_GOVERN_percent +
                              as.factor(SIGLA_UE), data=testes_coalizoes)
 
 summary(regressao)
 etable(regressao)
 
# VD: Ideologia, controles e interações
 regressao <- fixest::feols(id_distancia_coalizao ~
                              NEPP + pibpercapita + magnitude +
                              percentual_vitoria +
                              cadeiras_gov_percent + ano_eleitoral +
                              COALIZAO_COLI_GOVERN_percent +
                              NEPP * pibpercapita +
                              NEPP * magnitude +
                              NEPP * percentual_vitoria +
                              NEPP * cadeiras_gov_percent +
                              NEPP * ano_eleitoral +
                              NEPP * COALIZAO_COLI_GOVERN_percent +
                              as.factor(SIGLA_UE), data=testes_coalizoes)
 
 summary(regressao)
 etable(regressao)  

##### Regressão dos gabinetes  #####
  testes_gabinetes <- gabinetes_final %>%
    filter(diff_in_days >= 30) %>% # 10 casos retirados
    filter(NUM_PARTIDOS_COALIZAO >= 2) # 52 casos retirados
 
# VD: Número de partidos no gabinete, sem controles
 regressao <- fixest::feols(NUM_PARTIDOS_GABINETE ~ NEPP + 
                 as.factor(SIGLA_UE), data=testes_gabinetes)
 
 summary(regressao)
 etable(regressao) 
 
# VD: Número de partidos no gabinete com controles
 regressao <- fixest::feols(NUM_PARTIDOS_GABINETE ~
                 NEPP + pibpercapita + magnitude +
                 percentual_vitoria +
                 cadeiras_gov_percent + ano_eleitoral +
                 GABINETE_COLI_GOVERN_percent +
                 as.factor(SIGLA_UE), data=testes_gabinetes)
 
 summary(regressao)
 etable(regressao) 

# VD: Número de partidos no gabinete com controles e interações
 regressao <- fixest::feols(NUM_PARTIDOS_GABINETE ~
                              NEPP + pibpercapita + magnitude +
                              percentual_vitoria +
                              cadeiras_gov_percent + ano_eleitoral +
                              GABINETE_COLI_GOVERN_percent +
                              NEPP * pibpercapita +
                              NEPP * magnitude +
                              NEPP * percentual_vitoria +
                              NEPP * cadeiras_gov_percent +
                              NEPP * ano_eleitoral +
                              NEPP * GABINETE_COLI_GOVERN_percent +
                              as.factor(SIGLA_UE), data=testes_gabinetes)
 
 summary(regressao)
 etable(regressao)  
 
# VD: Distância ideológica do gabinete sem controles
 regressao <-  fixest::feols(id_distancia_gabinete ~ NEPP +
                  as.factor(SIGLA_UE), data=testes_gabinetes)
  
 summary(regressao)
 etable(regressao)
 
  
 # VD: Distância ideológica do gabinete com controles
 regressao <-  fixest::feols(id_distancia_gabinete ~
                               NEPP + pibpercapita + magnitude +
                               percentual_vitoria +
                               cadeiras_gov_percent + ano_eleitoral +
                               GABINETE_COLI_GOVERN_percent +
                               as.factor(SIGLA_UE), data=testes_gabinetes)
 
 summary(regressao)
 etable(regressao)
 
# VD: Distância ideológica do gabinete com controles e interações
 regressao <-  fixest::feols(id_distancia_gabinete ~
                               NEPP + pibpercapita + magnitude +
                               percentual_vitoria +
                               cadeiras_gov_percent + ano_eleitoral +
                               GABINETE_COLI_GOVERN_percent +
                               NEPP * pibpercapita +
                               NEPP * magnitude +
                               NEPP * percentual_vitoria +
                               NEPP * cadeiras_gov_percent +
                               NEPP * ano_eleitoral +
                               NEPP * GABINETE_COLI_GOVERN_percent +
                               as.factor(SIGLA_UE), data=testes_gabinetes)
 
 summary(regressao)
 etable(regressao) 
 

 
# VD: Coalescência, sem controles
 regressao <- fixest::feols(coalescencia ~ NEPP +
                  as.factor(SIGLA_UE), data=testes_gabinetes)
  
 summary(regressao)
 etable(regressao)  

# VD: Coalescência, com controles
 regressao <- fixest::feols(coalescencia ~
                              NEPP + pibpercapita + magnitude +
                              percentual_vitoria +
                              cadeiras_gov_percent + ano_eleitoral +
                              GABINETE_COLI_GOVERN_percent +
                              as.factor(SIGLA_UE), data=testes_gabinetes)
 
 summary(regressao)
 etable(regressao)  

# VD: Coalescência, com controles e interações
 testes_gabinetes_renomeado <- testes_gabinetes %>%
   rename(cadeiras_governador = cadeiras_gov_percent) %>%
   rename(gabinete_coligação = GABINETE_COLI_GOVERN_percent)
   
   
 regressaoplot1 <- fixest::feols(coalescencia ~
                              NEPP + pibpercapita + magnitude +
                              percentual_vitoria +
                              cadeiras_governador + ano_eleitoral +
                              gabinete_coligação +
                              NEPP * pibpercapita +
                              NEPP * magnitude +
                              NEPP * percentual_vitoria +
                              NEPP * cadeiras_governador +
                              NEPP * ano_eleitoral +
                              NEPP * gabinete_coligação +
                              as.factor(SIGLA_UE), data=testes_gabinetes_renomeado)
 
 summary(regressaoplot1)
 etable(regressaoplot1)   
 
 coefplot(regressaoplot1,
          horiz = TRUE,
          drop = "SIGLA_UE")
 
 
 # VD: Partidarização dos secretários, sem controles
 regressao <- fixest::feols(naopart_percent ~ NEPP +
                              as.factor(SIGLA_UE), data=testes_gabinetes)
 
 summary(regressao)
 etable(regressao)  
 
 # VD: Secretários técnicos, com controles
 regressao <- fixest::feols(naopart_percent ~
                              NEPP + pibpercapita + magnitude +
                              percentual_vitoria +
                              cadeiras_gov_percent + ano_eleitoral +
                              GABINETE_COLI_GOVERN_percent +
                              as.factor(SIGLA_UE), data=testes_gabinetes)
 
 summary(regressao)
 etable(regressao)  
 
 
 # VD: Secretários técnicos, com controles e interações
 regressaoplot2 <- fixest::feols(naopart_percent ~
                              NEPP + pibpercapita + magnitude +
                              percentual_vitoria +
                              cadeiras_governador + ano_eleitoral +
                              gabinete_coligação +
                              NEPP * pibpercapita +
                              NEPP * magnitude +
                              NEPP * percentual_vitoria +
                              NEPP * cadeiras_governador +
                              NEPP * ano_eleitoral +
                              NEPP * gabinete_coligação +
                              as.factor(SIGLA_UE), data=testes_gabinetes_renomeado)
 
 summary(regressaoplot2)
 etable(regressaoplot2)   
 
 coefplot(regressaoplot2,
          horiz = TRUE,
          drop = "SIGLA_UE")