# Ideologia

library(tidyverse)
library(haven)
library(readr)
library(tidyr)

# Criando o DF
ideologia <- data.frame(sigla_partido = c(
            "PSTU", "PCO", "PCB", "PSOL", "PCdoB", "PT", "PDT",
            "PSB", "REDE", "PPS", "PV", "PTB", "AVANTE",
            "SDD", "PMN", "PMB", "PHS", "MDB",
            "PSD", "PSDB", "PODE", "PPL", "PRTB",
            "PROS", "PRP", "PRB", "PR", "PTC", "DC",
            "PSL", "NOVO", "PP", "PSC", "PATRIOTA", "DEM"),
                        ideologia_media = c(
            "0.51", "0.61", "0.91", "1.28", "1.92", "2.97", "3.92",
            "4.05", "4.77", "4.92", "5.29", "6.1", "6.32", "6.5",
            "6.88", "6.9", "6.96", "7.01", "7.09", "7.11", "7.24",
            "7.27", "7.45", "7.47", "7.59", "7.78", "7.78", "7.86",
            "8.11", "8.11", "8.13", "8.20", "8.33", "8.55", "8.57"))

# Variável numérica; seleção; banco wider
ideologia <- ideologia %>%
  mutate(ideo_media=as.numeric(ideologia_media))%>%
  select(sigla_partido, ideo_media)
  
ideologia2 <- ideologia %>%
  pivot_wider(
    names_from = "sigla_partido", values_from = "ideo_media")


# Abrindo a base de dados de coalizões
link <- "https://github.com/felipelirapaiva/dissertacao/blob/main/mydata/gabinetes_semideologia.csv?raw=true"
download.file(link, "gabinetes_semideologia.csv")
gabinetes_semideologia <- read.csv("gabinetes_semideologia.csv", dec = ",", encoding = "UTF-8", check.names = FALSE)

# Juntando
gabinetes_comideologia <- cbind(gabinetes_semideologia, ideologia2)


gabinetes_comideologia <-   gabinetes_comideologia %>%
  mutate(
    id_DEM= ifelse(PFL_DEM_SEC>0,DEM,NA),
    id_PCDOB= ifelse(PCdoB_SEC>0,PCdoB,NA),
    id_PDT= ifelse(PDT_SEC>0,PDT,NA),
    id_PL= ifelse(PL_PR_SEC>0,PR,NA),
    id_PMDB= ifelse(PMDB_SEC>0,MDB,NA),
    id_PP= ifelse(PPR_PPB_PP_SEC>0,PP,NA),
    id_PSB= ifelse(PSB_SEC>0,PSB,NA),
    id_PSDB= ifelse(PSDB_SEC>0,PSDB,NA),
    id_PT= ifelse(PT_SEC>0,PT,NA),
    id_PTB= ifelse(PTB_SEC>0,PTB,NA),
    id_PV= ifelse(PV_SEC>0,PV,NA),
    id_PRONA= ifelse(PRONA_SEC>0,PR,NA), #ANOTAR
    id_PST= ifelse(PST_SEC>0,PR,NA), #ANOTAR
    id_PRTB= ifelse(PRTB_SEC>0,PRTB,NA),
    id_PMN= ifelse(PMN_SEC>0,PMN,NA), 
    id_PSDC= ifelse(PSDC_SEC>0,DC,NA), #ANOTAR
    id_PSC= ifelse(PSC_SEC>0,PSC,NA),
    id_PSL= ifelse(PSL_SEC>0,PSL,NA),
    id_PSD= ifelse(PSD_SEC>0,PSD,NA),
    id_PPS= ifelse(PPS_SEC>0,PPS,NA)) %>% 
  rowwise() %>%
  mutate(id_max = max(id_DEM,
                      id_PCDOB,
                      id_PDT,
                      id_PL,
                      id_PMDB,
                      id_PP,
                      id_PSB,
                      id_PSDB,
                      id_PT,
                      id_PTB,
                      id_PV,
                      id_PRONA,
                      id_PST,
                      id_PRTB,
                      id_PMN,
                      id_PSDC,
                      id_PSC,
                      id_PSL,
                      id_PSD,
                      id_PPS,
                      na.rm=TRUE),
         id_min = min(id_DEM,
                      id_PCDOB,
                      id_PDT,
                      id_PL,
                      id_PMDB,
                      id_PP,
                      id_PSB,
                      id_PSDB,
                      id_PT,
                      id_PTB,
                      id_PV,
                      id_PRONA,
                      id_PST,
                      id_PRTB,
                      id_PMN,
                      id_PSDC,
                      id_PSC,
                      id_PSL,
                      id_PSD,
                      id_PPS,
                      na.rm = TRUE),
         id_distancia_gabinete = abs(id_max-id_min))


# Ideologia das coalizões
  siglas <- gabinetes_comideologia %>%
    group_by(SIGLA_PARTIDO_GOV)%>%
    count()
  siglas$SIGLA_PARTIDO_GOV

  gabinetes_comideologia <- gabinetes_comideologia %>%
  mutate(
    id_gov = NA,
    id_gov = ifelse(SIGLA_PARTIDO_GOV == "DEM", DEM, id_gov),
    id_gov = ifelse(SIGLA_PARTIDO_GOV == "PDT", PDT,  id_gov),
    id_gov = ifelse(SIGLA_PARTIDO_GOV == "PL", PR,  id_gov),
    id_gov = ifelse(SIGLA_PARTIDO_GOV == "PMDB", MDB,  id_gov),
    id_gov = ifelse(SIGLA_PARTIDO_GOV == "PMN", PMN,  id_gov),
    id_gov = ifelse(SIGLA_PARTIDO_GOV == "PP", PP,  id_gov),
    id_gov = ifelse(SIGLA_PARTIDO_GOV == "PPS", PPS,  id_gov),
    id_gov = ifelse(SIGLA_PARTIDO_GOV == "PSB", PSB,  id_gov),
    id_gov = ifelse(SIGLA_PARTIDO_GOV == "PSDB", PSDB,  id_gov),
    id_gov = ifelse(SIGLA_PARTIDO_GOV == "PSL", PSL,  id_gov),
    id_gov = ifelse(SIGLA_PARTIDO_GOV == "PT", PT,  id_gov),
    id_gov = ifelse(SIGLA_PARTIDO_GOV == "PTB", PTB,  id_gov),
    id_gov = ifelse(SIGLA_PARTIDO_GOV == "PTN", PODE,  id_gov))

  
  siglas <- gabinetes_comideologia %>%
    group_by(SIGLA_PARTIDO_VICEGOVERNADOR)%>%
    count()
   siglas$SIGLA_PARTIDO_VICEGOVERNADOR  
  
   gabinetes_comideologia <- gabinetes_comideologia %>%
     mutate(
       id_vice = NA,
       id_vice = ifelse(SIGLA_PARTIDO_VICEGOVERNADOR == "DEM", DEM, id_vice),
       id_vice = ifelse(SIGLA_PARTIDO_VICEGOVERNADOR == "PCdoB", PCdoB, id_vice),
       id_vice = ifelse(SIGLA_PARTIDO_VICEGOVERNADOR == "PDT", PDT,  id_vice),
       id_vice = ifelse(SIGLA_PARTIDO_VICEGOVERNADOR == "PL", PR,  id_vice),
       id_vice = ifelse(SIGLA_PARTIDO_VICEGOVERNADOR == "PMDB", MDB,  id_vice),
       id_vice = ifelse(SIGLA_PARTIDO_VICEGOVERNADOR == "PMN", PMN,  id_vice),
       id_vice = ifelse(SIGLA_PARTIDO_VICEGOVERNADOR == "PP", PP,  id_vice),
       id_vice = ifelse(SIGLA_PARTIDO_VICEGOVERNADOR == "PPS", PPS,  id_vice),
       id_vice = ifelse(SIGLA_PARTIDO_VICEGOVERNADOR == "PSB", PSB,  id_vice),
       id_vice = ifelse(SIGLA_PARTIDO_VICEGOVERNADOR == "PSDB", PSDB,  id_vice),
       id_vice = ifelse(SIGLA_PARTIDO_VICEGOVERNADOR == "PSL", PSL,  id_vice),
       id_vice = ifelse(SIGLA_PARTIDO_VICEGOVERNADOR == "PT", PT,  id_vice),
       id_vice = ifelse(SIGLA_PARTIDO_VICEGOVERNADOR == "PTB", PTB,  id_vice))

   
   
gabinetes_comideologia <-   gabinetes_comideologia %>%
  rowwise() %>%
  mutate(id_max = max(id_DEM,
                      id_PCDOB,
                      id_PDT,
                      id_PL,
                      id_PMDB,
                      id_PP,
                      id_PSB,
                      id_PSDB,
                      id_PT,
                      id_PTB,
                      id_PV,
                      id_PRONA,
                      id_PST,
                      id_PRTB,
                      id_PMN,
                      id_PSDC,
                      id_PSC,
                      id_PSL,
                      id_PSD,
                      id_PPS,
                      id_gov,
                      id_vice,
                      na.rm=TRUE),
         id_min = min(id_DEM,
                      id_PCDOB,
                      id_PDT,
                      id_PL,
                      id_PMDB,
                      id_PP,
                      id_PSB,
                      id_PSDB,
                      id_PT,
                      id_PTB,
                      id_PV,
                      id_PRONA,
                      id_PST,
                      id_PRTB,
                      id_PMN,
                      id_PSDC,
                      id_PSC,
                      id_PSL,
                      id_PSD,
                      id_PPS,
                      id_gov,
                      id_vice,
                      na.rm = TRUE),
         id_distancia_coalizao = abs(id_max-id_min))


write_csv(gabinetes_comideologia,"C:\\Users\\Felipe\\Documents\\Dissertacao R\\Diss\\gabinetes_comideologia.csv")
