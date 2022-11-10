# Análises descritivas
  library(tidyverse)
  library(haven)
  library(readr)
  library(tidyr)
  library(ggpubr)
  library(ggplot2)

# OBS.: a numeração das figuras obedece à ordem em que elas
# apareceram no texto (31 = 3.1; 32 = 3.2 e assim por diante)
  
####### * Abrindo a base de dados (fazer download) ####
  link <- "https://github.com/felipelirapaiva/dissertacao/blob/main/mydata/gabinetes_final.csv?raw=true"
  download.file(link, "gabinetes_final.csv")
  gabinetes_final <- read.csv("gabinetes_final.csv", dec = ",", encoding = "UTF-8", check.names = FALSE)


#### Fig 1 - Número de gabinetes ####
# Contando o número de partidos no governo
  figura31 <- gabinetes_final %>%
  group_by(NUM_PARTIDOS_COALIZAO)%>%
  count()

  ggplot(figura31, aes(y=n, x=NUM_PARTIDOS_COALIZAO))+
  geom_bar(stat="identity")+
  theme_pubr()+
  scale_x_continuous(breaks=seq(1,6,1))+
  xlab("Número de partidos no governo") + ylab("Frequência")+
  theme(axis.text = element_text(size = 10.5))+
  geom_text(aes(label = n), nudge_y = 4)

####### * Filtro para analisar somente as coalizões daqui em diante ####  
  gabinetes_final <- gabinetes_final %>%
  filter(NUM_PARTIDOS_COALIZAO > 1)
    
#### Fig 2 e 3 - Contando o número de coalizões por UF ####
  figura32 <- gabinetes_final %>%
    filter(NUM_PARTIDOS_COALIZAO > 1)%>%
    group_by(SIGLA_UE)%>%
    count()
  
  figura32 <- figura32 %>%
    rename(casos = n)
  
    ggplot(figura32, aes(fct_reorder(SIGLA_UE, desc(casos)), casos ))+
    geom_bar(stat="identity")+
    theme_pubr()+
    scale_y_continuous(breaks=seq(0,35,5))+
    xlab("") + ylab("Número de coalizões")+
    theme(axis.text.x = element_text(angle = 30))+
    theme(axis.text = element_text(size = 10.5))+
    geom_text(aes(label = casos), nudge_y = 1)
    
# Descritivos
  median(figura32$casos)
  mean(figura32$casos)
  max(figura32$casos)
  min(figura32$casos)
  
# Contando o número de coalizões por UF por mandato
  figura33 <- gabinetes_final %>%
    filter(NUM_PARTIDOS_COALIZAO > 1)%>%
    group_by(SIGLA_UE, ANO_REFERENCIA)%>%
    count()

  ggplot(figura33, aes(y=n, x=SIGLA_UE))+
    geom_bar(stat="identity")+
    theme_pubr()+
    facet_wrap(~ANO_REFERENCIA, ncol = 2)+
    scale_y_continuous(breaks=c(1, 5, 10, 15))+
    xlab("") + ylab("Número de coalizões")+
    theme(axis.text.x = element_text( 
      angle = 90))+
    theme(axis.text = element_text(
      size = 10.5))+
    geom_hline(yintercept = 2, lwd=0.5, linetype=2)+
    geom_hline(yintercept = 4, lwd=0.5, linetype=2)+
    geom_hline(yintercept = 6, lwd=0.5, linetype=2)+
    geom_hline(yintercept = 8, lwd=0.5, linetype=2)

  figura33B <- gabinetes_final %>%
    filter(NUM_PARTIDOS_COALIZAO > 1)%>%
    group_by(ANO_REFERENCIA)%>%
    count() # Agrupando por mandato.
  

#### Fig 4-6 - Duração e formação dos gabinetes ####
library(lubridate)

  figura34 <- gabinetes_final%>%
    mutate(INICIO_COALIZAO=as.Date(INICIO_COALIZAO),
           FINAL_COALIZAO=as.Date(FINAL_COALIZAO)) # Transformando em datas
  
  
  figura34$diff_in_days = as.numeric(difftime(figura34$FINAL_COALIZAO, 
           figura34$INICIO_COALIZAO, units = "days")) # Diferença em dias

  
  ggplot(figura34, aes(x=diff_in_days)) + 
    geom_histogram(binwidth=30) +
    geom_vline(xintercept = 182, lwd=0.5, linetype=2)+
    geom_vline(xintercept = 365, lwd=0.5, linetype=2)+
    geom_vline(xintercept = 730, lwd=0.5, linetype=2)+
    geom_vline(xintercept = 1095, lwd=0.5, linetype=2)+
    geom_vline(xintercept = 1461, lwd=0.5, linetype=2)+
    theme_pubr()+
    scale_y_continuous(breaks=c(1, 5, 10, 15, 20, 25, 30, 35, 40))+
    scale_x_continuous(breaks=c(0, 182, 365, 730, 1095, 1461))+
    xlab("Dias") + ylab("Frequência")+
    theme(axis.text = element_text(size = 10.5))
    
#  figura34B <- figura34 %>%
 #  filter(diff_in_days > 1460)
  #  group_by(diff_in_days)%>%
  #  count()
  median(figura34$diff_in_days)
  mean(figura34$diff_in_days)
  max(figura34$diff_in_days)
  min(figura34$diff_in_days)

## Formação dos gabinetes

# Ano e mês com mais formação de gabinetes
  testes <- gabinetes_final %>%
    group_by(ANO_CICLO_INIC_COAL)%>%
    count()
  
  testes <- gabinetes_final %>%
    group_by(ANO_CICLO_INIC_COAL, MES_CICLO_INIC_COAL)%>%
    count()

  
  figura35 <- gabinetes_final # %>%
#  filter(COALIZAO == "Iberê I")
  
  ggplot(figura35, aes(x=MES_CICLO_INIC_COAL)) + 
    geom_histogram(binwidth=1) +
    theme_pubr()+
    facet_wrap(~ANO_CICLO_INIC_COAL, ncol = 2)+
    scale_x_continuous(breaks=c(1, 3, 6, 9, 12))+
    xlab("Mês") + ylab("Frequência")+
    theme(axis.text = element_text(
      size = 10.5))

# Motivos do início dos governos
  Figura36 <- gabinetes_final 
  Figura36 <- Figura36  %>% filter(grepl('Eleição', EVENTO_INICIO_GABINETE))
  
  Figura36 <- gabinetes_final 
  Figura36 <- Figura36  %>% filter(grepl('Saída', EVENTO_INICIO_GABINETE))
  
  Figura36 <- gabinetes_final 
  Figura36 <- Figura36  %>% filter(grepl('Entrada', EVENTO_INICIO_GABINETE))
  
  Figura36 <- gabinetes_final 
  Figura36 <- Figura36  %>% filter(grepl('Mudança', EVENTO_INICIO_GABINETE))
  # Aqui, há 1 a menos para não repetir o Vice.
  
  Figura36 <- gabinetes_final 
  Figura36 <- Figura36  %>% filter(grepl('Cassação', EVENTO_INICIO_GABINETE))
  
  Figura36 <- gabinetes_final 
  Figura36 <- Figura36  %>% filter(grepl('Vice', EVENTO_INICIO_GABINETE))
  
  Figura36 <- gabinetes_final 
  Figura36 <- Figura36  %>% filter(grepl('Licença', EVENTO_INICIO_GABINETE))
  
  Figura36 <- gabinetes_final 
  Figura36 <- Figura36  %>% filter(grepl('Renúncia', EVENTO_INICIO_GABINETE))
  
  Figura36 <- gabinetes_final 
  Figura36 <- Figura36  %>% filter(grepl('Segundo colocado', EVENTO_INICIO_GABINETE))

  
  eventos_inicio <- data.frame (evento  = c("Saída de partido do gabinete", 
                    "Entrada de partido no gabinete", "Eleição", "Renúncia",
                    "Mudança de partido (Gov)", "Cassação", "Mudança de partido (Vice)",
                    "Licença", "Segundo colocado" ),
                    casos = c("136", "120", "71", "12", "9", "3", "1", "1", "1"))
  
  eventos_inicio <- eventos_inicio %>%
    mutate(casos=as.numeric(casos))

  
  eventos_inicio %>%
    mutate(evento = fct_reorder(evento, desc(casos))) %>%
    ggplot( aes(y=casos, x=evento))+
    geom_bar(stat="identity")+
    theme_pubr()+
    xlab("") + ylab("Frequência")+
    scale_y_continuous(breaks=c(1, 25, 50, 75, 100, 125))+
    theme(axis.text = element_text(size = 10.5))+
    coord_flip()+
    geom_text(aes(label = casos), nudge_y = 5)

#### Fig 7 - Quem assumiu a governadoria ####
  figura37 <- gabinetes_final
  figura37 <- figura37 %>%
    group_by(ELEICOES_CABECA)%>%
    count()

  figura37B <- gabinetes_final

  figura37B <- figura37B %>%
    group_by(ELEICOES_CABECA_0)%>%
    count()
  
  figura37B <- figura37B %>% 
    mutate(ELEICOES_CABECA_0 =  na_if(ELEICOES_CABECA_0,""))%>%
    filter(ELEICOES_CABECA_0 != "NA")

    ggplot(figura37B, aes((fct_reorder(ELEICOES_CABECA_0, n, .fun='sum')), y=n)) + 
    geom_bar(stat="identity")+
    theme_pubr()+
    xlab("") + ylab("Frequência")+
    scale_y_continuous(breaks=c(1, 5, 10, 15, 20, 25, 30))+
    theme(axis.text = element_text(size = 10.5))+
    geom_text(aes(label = n), nudge_y = 1)
  
#### Fig 8 Partidos e coalizões ####
  figura38 <- gabinetes_final %>%
    group_by(NUM_PARTIDOS_COALIZAO)%>%
    count()
  
  figura38 <- gabinetes_final %>%
    group_by(NUM_PARTIDOS_GABINETE)%>%
    count()
  
  ggplot(figura38, aes(y=n, x=NUM_PARTIDOS_GABINETE))+
    geom_bar(stat="identity")+
    theme_pubr()+
    scale_x_continuous(breaks=c(0, 1, 2, 3, 4, 5, 6))+
    scale_y_continuous(breaks=c(10, 30, 50, 70, 90))+
    xlab("Número de partidos no gabinete") + ylab("Frequência")+
    theme(axis.text = element_text(size = 10.5))+
    geom_text(aes(label = n), nudge_y = 4)

#### Fig 9 -  Frequência de secretários do partido do governador e do vice #####
  figura39A <- gabinetes_final %>%
    group_by(PARTIDO_GOV_SEC)%>%
    count()%>%
    filter(PARTIDO_GOV_SEC != "NA")%>%
    mutate(PARTIDO_GOV_SEC_BI = case_when(PARTIDO_GOV_SEC == 1 ~ "Sim",
                                          PARTIDO_GOV_SEC == 0 ~ "Não"))
  
  fig39A <- ggplot(figura39A, aes(y=n, x=PARTIDO_GOV_SEC_BI))+
    geom_bar(stat="identity")+
    ylim(0,250)+
    theme_pubr()+
    xlab("Partido do gov. com secretaria?") + ylab("Frequência")+
    theme(axis.text = element_text(size = 10.5))+
    geom_text(aes(label = n), nudge_y = 13)
  
  # Figura B
  figura39B <- gabinetes_final %>%
    group_by(PARTIDO_VICE_SEC)%>%
    count()%>%
    filter(PARTIDO_VICE_SEC != "NA")%>%
    mutate(PARTIDO_VICE_SEC_BI = case_when(PARTIDO_VICE_SEC == 1 ~ "Sim",
                                          PARTIDO_VICE_SEC == 0 ~ "Não"))
    
  fig39B <- ggplot(figura39B, aes(y=n, x=PARTIDO_VICE_SEC_BI))+
    geom_bar(stat="identity")+
    ylim(0,250)+
    theme_pubr()+
    xlab("Partido do vice com secretaria?") + ylab("")+
    theme(axis.text = element_text(size = 10.5))+
    geom_text(aes(label = n), nudge_y = 13)

# Continuação: filtrando 
# Descobrindo se governadores e vices possuem o mesmo partido  
  figura39D <- gabinetes_final %>%
    filter(PARTIDO_VICE_SEC != "NA")%>% # Retirando casos ausentes
    group_by(PARTIDO_GOV_GOVVICE)%>%
    count()%>%
    filter(PARTIDO_GOV_GOVVICE != "NA")%>%
    mutate(PARTIDO_GOV_GOVVICE_BI = case_when(PARTIDO_GOV_GOVVICE == 1 ~ "Sim",
                                              PARTIDO_GOV_GOVVICE == 0 ~ "Não"))
  
  fig39D  <-  ggplot(figura39D, aes(y=n, x=PARTIDO_GOV_GOVVICE_BI))+
    geom_bar(stat="identity")+
    # ylim(0,240)+
    theme_pubr()+
    xlab("Gov. e vice possuem o mesmo partido?")+
    ylab("Frequência")+
    theme(axis.text = element_text(size = 10.5))+
    geom_text(aes(label = n), nudge_y = 6)


# Os últimos dois gráficos da imagem
  figura39C <- gabinetes_final %>%
    filter(PARTIDO_GOV_GOVVICE == 0) %>% # governadores e vices c/ partidos diferentes
    group_by(PARTIDO_VICE_SEC)%>%
    count()%>%
    filter(PARTIDO_VICE_SEC != "NA")%>%
    mutate(PARTIDO_VICE_SEC_BI = case_when(PARTIDO_VICE_SEC == 1 ~ "Sim",
                                           PARTIDO_VICE_SEC == 0 ~ "Não"))
  
  fig39C  <-  ggplot(figura39C, aes(y=n, x=PARTIDO_VICE_SEC_BI))+
    geom_bar(stat="identity")+
    theme_pubr()+
    xlab("Partido do vice (diferente do gov.) com secretaria?")+
    ylab("")+
    ylim(0,120)+
    theme(axis.text = element_text(size = 10.5))+
    geom_text(aes(label = n), nudge_y = 7)
  
  figura39E <- gabinetes_final %>%
    filter(PARTIDO_GOV_GOVVICE == 1) %>% # governadores e vices c/ mesmo partido
    group_by(PARTIDO_VICE_SEC)%>%
    count()%>%
    filter(PARTIDO_VICE_SEC != "NA")%>%
    mutate(PARTIDO_VICE_SEC_BI = case_when(PARTIDO_VICE_SEC == 1 ~ "Sim",
                                           PARTIDO_VICE_SEC == 0 ~ "Não"))
  
  fig39E  <-  ggplot(figura39E, aes(y=n, x=PARTIDO_VICE_SEC_BI))+
    geom_bar(stat="identity")+
    theme_pubr()+
    xlab("Partido do vice (igual ao gov.) com secretaria?")+
    ylab("Frequência")+
    ylim(0,120)+
    theme(axis.text = element_text(size = 10.5))+
    geom_text(aes(label = n), nudge_y = 7)
  
  figure <- ggarrange(fig39A, fig39B, fig39E, fig39C,  ncol=2, nrow=2)
  figure
  
##### Fig 10 - Secretários do partido do vice governador por controle de cadeiras ######
  
# Número de cadeiras controladas pelos vices (quando de partido diferente do gov)
  figura310 <- gabinetes_final %>%
    filter(PARTIDO_GOV_GOVVICE == "0") %>% # Filtro de vices diferentes de govs
    filter(PARTIDO_VICE_SEC != "NA")  # Filtro dos gabinetes com 1 ou mais partidos
  
  figura310 <- figura310
    figura310$CADEIRAS_VICE = figura310$CADEIRAS_CHAPA - figura310$CADEIRAS_GOV

  figura310 <- figura310
    figura310$CADEIRAS_VICE_PERC = (figura310$CADEIRAS_VICE / figura310$CADEIRAS_CHAPA) * 100
 
  figura310 <- figura310 %>%
    group_by(CADEIRAS_VICE_PERC,PARTIDO_VICE_SEC )%>%
    count()
    
  figura310 <- figura310 %>%
    mutate(CADEIRAS_VICE_BI = case_when(CADEIRAS_VICE_PERC < 50 ~ "Cadeiras do vice < 50% da chapa",
                                        CADEIRAS_VICE_PERC == 50 ~ "Cadeiras do vice = 50% da chapa",
                                        CADEIRAS_VICE_PERC > 50 ~ "Cadeiras do vice > 50% da chapa"))%>%
    mutate(PARTIDO_VICE_SEC_SIMNAO = case_when(PARTIDO_VICE_SEC == 1 ~ "Sim",
                                               PARTIDO_VICE_SEC == 0 ~ "Não"))
    
  ggplot(figura310, aes(y=n, x=PARTIDO_VICE_SEC_SIMNAO))+
    geom_bar(stat="identity")+
    facet_wrap(~CADEIRAS_VICE_BI, ncol = 2)+
    theme_pubr()+
    xlab("O partido do vice-governador tem acesso ao gabinete? ")+
    ylab("Frequência")+
    ylim(0, 85)+
    theme(axis.text = element_text(size = 10.5))

####### * Retirando o filtro para as próximas análises #######
 link <- "https://github.com/felipelirapaiva/dissertacao/blob/main/mydata/gabinetes_final.csv?raw=true"
 download.file(link, "gabinetes_final.csv")
 gabinetes_final <- read.csv("gabinetes_final.csv", dec = ",", encoding = "UTF-8", check.names = FALSE)
    
##### Fig 11, 12, 13 - Secretarias #####
# Figura 11 - Número de secretarias por UF (1995-2010)
  figura311 <- gabinetes_final
  
  ggplot(figura311, aes((fct_reorder(SIGLA_UE, NUM_SEC, .fun='median')), y=NUM_SEC)) + 
    geom_boxplot()+
    theme_pubr()+
    scale_y_continuous(breaks=c(1,5,10,15,20,25,30))+
    xlab("") + ylab("Número de secretarias")+
    theme(axis.text = element_text(size = 10.5))

  mean(figura311$NUM_SEC)
  max(figura311$NUM_SEC)
  min(figura311$NUM_SEC)
  
  dados311 <- figura311 %>%
    filter(ANO_REFERENCIA == 2006) # == 1998 / == 2002 / == 2006 p/ outros anos.
    mean(dados311$NUM_SEC)
    max(dados311$NUM_SEC)
    min(dados311$NUM_SEC)

# Figura 12
    ggplot(figura311, aes(x=ANO_REFERENCIA, y=NUM_SEC)) + 
      geom_line()+
      geom_point()+
      facet_wrap(~ SIGLA_UE, ncol = 4)+
      theme_pubr()+
      xlab("") + ylab("Número de secretarias")+
      theme(axis.text = element_text(size = 10.5))+
      scale_x_continuous(breaks=c(1994, 1998, 2002, 2006))+
      scale_y_continuous(breaks=c(1, 10, 20, 30))+
      theme(axis.text.x = element_text(angle = 90))
    
    
# Figura 13 - Percentual de secretários partidários por UF (1995-2010)
  figura312 <- gabinetes_final 
  figura312$secpartidarios_percent = (figura312$tot_partidarios / figura312$NUM_SEC)
    
  ggplot(figura312, aes(fct_reorder(SIGLA_UE, secpartidarios_percent, .fun='median'), y=secpartidarios_percent)) + 
    geom_boxplot()+
    theme_pubr()+
    xlab("") + ylab("Percentual de secretários filiados a partidos")+
    theme(axis.text = element_text(size = 10.5))

# Descritivos  
  mean(figura312$secpartidarios_percent)
  max(figura312$secpartidarios_percent)
  min(figura312$secpartidarios_percent)
  
  texto <- figura312 %>%
    filter(secpartidarios_percent >= 0.7)%>% # Filtrando para fazer análises
    group_by(SIGLA_UE)%>%
    count()
  
  texto <- figura312 %>%
    filter(ANO_REFERENCIA == 2006) # == 1998 / == 2002 / == 2006 p/ outros anos.
    mean(texto$secpartidarios_percent)
    max(texto$secpartidarios_percent)
    min(texto$secpartidarios_percent)

# Figura 13 - Percentual de secretários partidários por UF por mandato
  ggplot(figura312, aes(x = SIGLA_UE, y=secpartidarios_percent)) + 
    geom_boxplot()+
    facet_wrap(~ ANO_REFERENCIA, scales = "free_x")+
    theme_pubr()+
    xlab("") + ylab("Percentual de secretários filiados a partidos")+
    theme(axis.text = element_text(size = 10.5))+
    theme(axis.text.x = element_text(angle = 90))

# Descritivos
  texto <- figura312 %>%
    filter(SIGLA_UE == "MG") #%>% # Troque o UF
 #   filter(ANO_REFERENCIA == 1998)
  mean(texto$secpartidarios_percent)
  max(texto$secpartidarios_percent)
  min(texto$secpartidarios_percent)
  
#### Fig 14 - Partidos 1 ####
  figura314 <- gabinetes_final #%>%
#  filter(ANO_REFERENCIA == 1994) # 1994, 1998, 2002 e 2006
    
# Criando um novo dataframe
  partidos <- data.frame (sigla_partido  = c("PSDB", "PP", "PMDB", "DEM", "PPS",
                                             "PV", "PMN", "PT", "PSB", "PCdoB",
                                             "PSD", "PTB", "PL", "PSC", "PSL",
                                             "PDT", "PRONA", "PSDC", "PST", "PRTB"),
    casos = c(sum(figura314$PSDB_SEC), sum(figura314$PPR_PPB_PP_SEC),
              sum(figura314$PMDB_SEC), sum(figura314$PFL_DEM_SEC),
              sum(figura314$PPS_SEC), sum(figura314$PV_SEC),
              sum(figura314$PMN_SEC), sum(figura314$PT_SEC),
              sum(figura314$PSB_SEC), sum(figura314$PCdoB_SEC),
              sum(figura314$PSD_SEC), sum(figura314$PTB_SEC),
              sum(figura314$PL_PR_SEC), sum(figura314$PSC_SEC),
              sum(figura314$PSL_SEC), sum(figura314$PDT_SEC),
              sum(figura314$PRONA_SEC), sum(figura314$PSDC_SEC),
              sum(figura314$PST_SEC), sum(figura314$PRTB_SEC)))
                                          
# Figura 14   
  ggplot(partidos, aes(fct_reorder(sigla_partido, desc(casos)), casos))+
    geom_bar(stat="identity")+
    theme_pubr()+
    xlab("") +
    ylab("Frequência")+
    theme(axis.text = element_text(size = 10.5))+
    theme(axis.text.x = element_text(angle = 30))+
    geom_text(aes(label = casos), nudge_y = 15)
  
  
# Figura 15  
  partidos <- partidos %>%
    filter(casos != "0")
  
  fig315a <-  ggplot(partidos, aes(fct_reorder(sigla_partido, desc(casos)), casos))+
    geom_bar(stat="identity")+
    theme_pubr()+
    ylim(0,155)+
    xlab("1995-1998") +
    ylab("Frequência")+
    theme(axis.text = element_text(size = 10.5))+
    theme(axis.text.x = element_text(angle = 90))
  
  fig315b <-  ggplot(partidos, aes(fct_reorder(sigla_partido, desc(casos)), casos))+
    geom_bar(stat="identity")+
    theme_pubr()+
    ylim(0,155)+
    xlab("1999-2002") +
    ylab("")+
    theme(axis.text = element_text(size = 10.5))+
    theme(axis.text.x = element_text(angle = 90))
  
  fig315c <-  ggplot(partidos, aes(fct_reorder(sigla_partido, desc(casos)), casos))+
    geom_bar(stat="identity")+
    theme_pubr()+
    ylim(0,155)+
    xlab("2003-2006") +
    ylab("Frequência")+
    theme(axis.text = element_text(size = 10.5))+
    theme(axis.text.x = element_text(angle = 90))
  
  fig315d <-  ggplot(partidos, aes(fct_reorder(sigla_partido, desc(casos)), casos))+
    geom_bar(stat="identity")+
    theme_pubr()+
    ylim(0,155)+
    xlab("2007-2010") +
    ylab("")+
    theme(axis.text = element_text(size = 10.5))+
    theme(axis.text.x = element_text(angle = 90))
  
  figure15 <- ggarrange(fig315a, fig315b, fig315c, fig315d,  ncol=2, nrow=2)
  figure15
  
  
#### Fig 14 - Partidos ####
  figura314 <- gabinetes_final %>%
  
figura314 <-  figura314 %>%
    group_by(SIGLA_UE) %>% # Fazer sem; fazer por mandato; e por UF e mandato
    summarise(PSDB = sum(PSDB_SEC), PP = sum(PPR_PPB_PP_SEC),
              PMDB = sum(PMDB_SEC), DEM = sum (PFL_DEM_SEC),
              PPS = sum(PPS_SEC), PV = sum(PV_SEC),
              PMN = sum(PMN_SEC), PT = sum(PT_SEC),
              PSB = sum(PSB_SEC), PCdoB = sum(PCdoB_SEC),
              PSD = sum(PSD_SEC), PTB = sum(PTB_SEC),
              PL = sum(PL_PR_SEC), PSC = sum(PSC_SEC),
              PSL = sum(PSL_SEC), PDT = sum(PDT_SEC),
              PRONA = sum(PRONA_SEC), PSDC = sum(PSDC_SEC),
              PST = sum(PST_SEC), PRTB = sum(PRTB_SEC))
  
  figura314A <- figura314 %>%
   pivot_longer(cols = c("PSDB", "PP", "PMDB", "DEM", "PPS",
                "PV", "PMN", "PT", "PSB", "PCdoB",
                "PSD", "PTB", "PL", "PSC", "PSL",
                "PDT", "PRONA", "PSDC", "PST", "PRTB"),
                names_to = "partido",
                values_to = "casos")
  
  ggplot(figura314A, aes(fct_reorder(partido, desc(casos)), casos))+
    geom_bar(stat="identity")+
    theme_pubr()+
    xlab("") +
    ylab("Frequência")+
    theme(axis.text = element_text(size = 10.5))+
    theme(axis.text.x = element_text(angle = 30))+
    geom_text(aes(label = casos), nudge_y = 15)

##### Fig Apêndice A - Filiação partidária dos secretários estaduais por mandato #####
  figura315 <- gabinetes_final
    
    figura315 <-  figura315 %>%
    group_by(SIGLA_UE) %>% # Fazer sem; fazer por mandato; e por UF e mandato
    summarise(PSDB = sum(PSDB_SEC), PP = sum(PPR_PPB_PP_SEC),
              PMDB = sum(PMDB_SEC), DEM = sum (PFL_DEM_SEC),
              PPS = sum(PPS_SEC), PV = sum(PV_SEC),
              PMN = sum(PMN_SEC), PT = sum(PT_SEC),
              PSB = sum(PSB_SEC), PCdoB = sum(PCdoB_SEC),
              PSD = sum(PSD_SEC), PTB = sum(PTB_SEC),
              PL = sum(PL_PR_SEC), PSC = sum(PSC_SEC),
              PSL = sum(PSL_SEC), PDT = sum(PDT_SEC),
              PRONA = sum(PRONA_SEC), PSDC = sum(PSDC_SEC),
              PST = sum(PST_SEC), PRTB = sum(PRTB_SEC))
  
    figura315A <- figura315 %>%
    pivot_longer(cols = c("PSDB", "PP", "PMDB", "DEM", "PPS",
                          "PV", "PMN", "PT", "PSB", "PCdoB",
                          "PSD", "PTB", "PL", "PSC", "PSL",
                          "PDT", "PRONA", "PSDC", "PST", "PRTB"),
                 names_to = "partido",
                 values_to = "casos")
  
    sum(figura315A$casos)  
    
    figura315A[figura315A == 0] <- NA
    
   figura315A <- figura315A %>%
      filter(casos != "NA")
    
    ggplot(figura315A, aes(partido, casos))+
    geom_bar(stat="identity")+
    facet_wrap(~SIGLA_UE, ncol = 4, scales = "free_x")+
    theme_pubr()+
    xlab("") +
    ylab("Frequência")+
    theme(axis.text = element_text(size = 10.5))+
    theme(axis.text.x = element_text(angle = 90))+
    geom_text(aes(label = casos), nudge_y = 15)
  
    texto <- figura315A %>%
      group_by(partido)%>%
      count()
      filter(casos != "NA")

##### Fig 16 - Distancia ideológica #####
  link <- "https://github.com/felipelirapaiva/dissertacao/blob/main/mydata/gabinetes_final.csv?raw=true"
  download.file(link, "gabinetes_final.csv")
  gabinetes_final <- read.csv("gabinetes_final.csv", dec = ",", encoding = "UTF-8", check.names = FALSE)
      
  gabinetes_final <- gabinetes_final %>%
    filter(NUM_PARTIDOS_COALIZAO > 1)
    
  figura316 <- gabinetes_final %>%
    mutate(id_distancia_gabinete=as.numeric(id_distancia_gabinete))%>%
    filter(id_distancia_gabinete != "Inf")

 # texto <-  figura316 %>%
  #  filter(ANO_REFERENCIA == "2006")
   # group_by(distancia)%>%
  #  count()
  
   # mean(texto$distancia)
    
# Histograma    
  ggplot(figura316, aes(x=id_distancia_gabinete)) + 
    geom_histogram(binwidth=0.5) +
    theme_pubr()+
    scale_x_continuous(breaks=c(0, 1, 2, 3, 4, 5, 6, 7))+
    scale_y_continuous(breaks=c(1, 10, 20, 30, 40, 50, 60))+
    xlab("Distância ideológica") + ylab("Frequência")+
    theme(axis.text = element_text(size = 10.5))

# Boxplot  
  ggplot(figura316, aes(fct_reorder(SIGLA_UE, id_distancia_gabinete, .fun='mean'), y=id_distancia_gabinete)) + 
    geom_boxplot()+
    theme_pubr()+
    xlab("") + ylab("Distância ideológica")+
    scale_y_continuous(breaks=c(1,2,3,4,5,6))+
    theme(axis.text = element_text(size = 10.5))

# Boxplot por mandato    
  ggplot(figura316, aes(x=SIGLA_UE, y=id_distancia_gabinete)) + 
    geom_boxplot()+
    facet_wrap(~ ANO_REFERENCIA, scales = "free_x")+
    theme_pubr()+
    xlab("") + ylab("Distância ideológica")+
    theme(axis.text = element_text(size = 10.5))+
    theme(axis.text.x = element_text(angle = 90))
  

##### Fig 17 e 18 - Coalescência  #####
  link <- "https://github.com/felipelirapaiva/dissertacao/blob/main/mydata/gabinetes_final.csv?raw=true"
  download.file(link, "gabinetes_final.csv")
  gabinetes_final <- read.csv("gabinetes_final.csv", dec = ",", encoding = "UTF-8", check.names = FALSE)
      
  figura317 <- gabinetes_final %>%
    filter(coalescencia != "NA")%>%
    mutate(coalescencia=as.numeric(coalescencia))
      
  figura317$coales_r <- round(figura317$coalescencia, digit = 3)

# Figura 17 - Coalescência por UF        
  ggplot(figura317, aes(fct_reorder(SIGLA_UE, coales_r, .fun='mean'), y=coales_r)) + 
    geom_boxplot()+
    theme_pubr()+
    xlab("") + ylab("Coalescência")+
    scale_y_continuous(breaks=c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7))+
    theme(axis.text = element_text(size = 10.5))+
    geom_hline(yintercept = 0.2, lwd=0.5, linetype=2)+
    geom_hline(yintercept = 0.4, lwd=0.5, linetype=2)+
    geom_hline(yintercept = 0.6, lwd=0.5, linetype=2)

# Figura 18 - Coalescência por UF por mandato  
  ggplot(figura317, aes(x = SIGLA_UE, y=coales_r)) + 
    geom_boxplot()+
    facet_wrap(~ ANO_REFERENCIA, scales = "free_x")+
    theme_pubr()+
    xlab("") + ylab("Coalescência")+
    theme(axis.text = element_text(size = 10.5))+
    theme(axis.text.x = element_text(angle = 90))+
    geom_hline(yintercept = 0.2, lwd=0.5, linetype=2)+
    geom_hline(yintercept = 0.4, lwd=0.5, linetype=2)+
    geom_hline(yintercept = 0.6, lwd=0.5, linetype=2)

  
  texto <- figura317 %>%
# filter(SIGLA_UE == "AC")
  filter(ANO_REFERENCIA == 2006)   
  mean(texto$coales_r)
  min(texto$coales_r)
  max(texto$coales_r)

##### Figura correspondência #####
  figura321 <- gabinetes_final

  figura321$perc_coalizao_coli_gov <- (figura321$COALIZAO_COLI_GOVERN / figura321$NUM_PARTIDOS_COALIZAO) * 100
  # Percentual de partidos da coalizão que estavam na coligação do governador eleito.
  
  figura321A <- figura321 %>%
    group_by(perc_coalizao_coli_gov)%>%
    count()%>%
    filter(perc_coalizao_coli_gov != "NA")
  
  figura321A$perc_coalizao_coli_gov <- round(figura321A$perc_coalizao_coli_gov, digit = 2)
  
  figura321A <- figura321A %>%
    mutate(perc_coalizao_coli_gov=as.character(perc_coalizao_coli_gov))
  
  ggplot(figura321A, aes (fct_relevel(perc_coalizao_coli_gov,
                          "16.67", "20", "25", "33.33", "40", "50", "60",
                          "66.67", "75", "80", "83.33", "100"), y=n))+
    geom_bar(stat="identity")+
    theme_pubr()+
    scale_y_continuous(breaks=c(1,20,40,60,80,100,120))+
    xlab("") + ylab("Frequência")+
    theme(axis.text = element_text(size = 10.5))+
    geom_text(aes(label = n), nudge_y = 4)
    

  figura321$perc_gabinete_coli_gov <- (figura321$GABINETE_COLI_GOVERN / figura321$NUM_PARTIDOS_GABINETE) * 100
  # Percentual de partidos dO gabinete que estavam na coligação do governador eleito.
  
  figura321B <- figura321 %>%
    group_by(perc_gabinete_coli_gov)%>%
    count()%>%
    filter(perc_gabinete_coli_gov != "NA")
  
  figura321B$perc_gabinete_coli_gov <- round(figura321B$perc_gabinete_coli_gov, digit = 2)
  
  figura321B <- figura321B %>%
    mutate(perc_gabinete_coli_gov=as.character(perc_gabinete_coli_gov))
  
  ggplot(figura321B, aes(fct_relevel(perc_gabinete_coli_gov,
                                     "0", "16.67", "20", "25", "33.33", "40",
                                     "50", "60", "66.67", "75", "80", "100"), y=n))+
    geom_bar(stat="identity")+
    theme_pubr()+
    scale_y_continuous(breaks=c(1,20,40,60,80,100,120))+
    xlab("") + ylab("Frequência")+
    theme(axis.text = element_text(size = 10.5))+
    geom_text(aes(label = n), nudge_y = 4)
  
##### Fig 19 e 20 - NEPP  #####
  link <- "https://github.com/felipelirapaiva/dissertacao/blob/main/mydata/gabinetes_final.csv?raw=true"
  download.file(link, "gabinetes_final.csv")
  gabinetes_final <- read.csv("gabinetes_final.csv", dec = ",", encoding = "UTF-8", check.names = FALSE)
  
  figura319 <- gabinetes_final %>%
    mutate(NEPP=as.numeric(NEPP))
  
  figura319 <- figura319 %>%
    group_by(SIGLA_UE, ANO_REFERENCIA, NEPP)%>%
    count()

# Figura 3.19  
  ggplot(figura319, aes(fct_reorder(SIGLA_UE, NEPP, .fun='mean'), y=NEPP)) + 
    geom_boxplot()+
    theme_pubr()+
    xlab("") + ylab("NEPP")+
    theme(axis.text = element_text(size = 10.5))

# Figura 3.20
  ggplot(figura319, aes(x=ANO_REFERENCIA, y=NEPP)) + 
    geom_line()+
    geom_point()+
    facet_wrap(~ SIGLA_UE)+
    theme_pubr()+
    xlab("") + ylab("NEPP")+
    theme(axis.text = element_text(size = 10.5))+
    scale_x_continuous(breaks=c(1994, 1998, 2002, 2006))+
    scale_y_continuous(breaks=c(2.5, 7.5, 12.5))+
    theme(axis.text.x = element_text(angle = 90))
  
 