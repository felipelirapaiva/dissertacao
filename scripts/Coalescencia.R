# Cálculo da coalescência

library(tidyverse)
library(haven)
library(readr)
library(tidyr)

##### Download #####

link <- "https://github.com/felipelirapaiva/dissertacao/blob/main/temporarios/merged.csv?raw=true"
download.file(link, "merged.csv")
coalizoes <- read.csv("merged.csv", dec = ",", encoding = "UTF-8", check.names = FALSE)


##### Composição do partidária do secretariado #####

# Somando os secretários partidários
  coalizoes$tot_partidarios <- coalizoes$PSDB_SEC + coalizoes$PPR_PPB_PP_SEC + coalizoes$PMDB_SEC	+ 
  coalizoes$PFL_DEM_SEC	+ coalizoes$PPS_SEC +	coalizoes$PV_SEC	+	coalizoes$PMN_SEC +
  coalizoes$PT_SEC	+	coalizoes$PSB_SEC	+ coalizoes$PCdoB_SEC	+	coalizoes$PSD_SEC	+	
  coalizoes$PTB_SEC	+	coalizoes$PL_PR_SEC	+	coalizoes$PSC_SEC	+ coalizoes$PSL_SEC	+
  coalizoes$PDT_SEC	+	coalizoes$PRONA_SEC	+	coalizoes$PSDC_SEC	+	coalizoes$PST_SEC	+
  coalizoes$PRTB_SEC

# Criando os não partidários
  coalizoes$tot_naopartidarios <- coalizoes$NUM_SEC - coalizoes$tot_partidarios


# Percentual dos secretários partidários (cargos)
coalizoes$PSDB_percent <- (coalizoes$PSDB_SEC / coalizoes$NUM_SEC)
coalizoes$PPR_PPB_PP_percent <- (coalizoes$PPR_PPB_PP_SEC / coalizoes$NUM_SEC)
coalizoes$PMDB_percent <- (coalizoes$PMDB_SEC / coalizoes$NUM_SEC)
coalizoes$PFL_DEM_percent <- (coalizoes$PFL_DEM_SEC / coalizoes$NUM_SEC)
coalizoes$PPS_percent <- (coalizoes$PPS_SEC / coalizoes$NUM_SEC)
coalizoes$PV_percent <- (coalizoes$PV_SEC / coalizoes$NUM_SEC)
coalizoes$PMN_percent <- (coalizoes$PMN_SEC / coalizoes$NUM_SEC)
coalizoes$PT_percent <- (coalizoes$PT_SEC / coalizoes$NUM_SEC)
coalizoes$PSB_percent <- (coalizoes$PSB_SEC / coalizoes$NUM_SEC)
coalizoes$PCdoB_percent <- (coalizoes$PCdoB_SEC / coalizoes$NUM_SEC) 
coalizoes$PSD_percent <- (coalizoes$PSD_SEC / coalizoes$NUM_SEC) 
coalizoes$PTB_percent <- (coalizoes$PTB_SEC / coalizoes$NUM_SEC)
coalizoes$PL_PR_percent <- (coalizoes$PL_PR_SEC / coalizoes$NUM_SEC)
coalizoes$PSC_percent <- (coalizoes$PSC_SEC / coalizoes$NUM_SEC) 
coalizoes$PSL_percent <- (coalizoes$PSL_SEC / coalizoes$NUM_SEC)
coalizoes$PDT_percent <- (coalizoes$PDT_SEC / coalizoes$NUM_SEC) 
coalizoes$PRONA_percent <- (coalizoes$PRONA_SEC / coalizoes$NUM_SEC) 
coalizoes$PSDC_percent <- (coalizoes$PSDC_SEC / coalizoes$NUM_SEC)
coalizoes$PST_percent <- (coalizoes$PST_SEC / coalizoes$NUM_SEC)
coalizoes$PRTB_percent <- (coalizoes$PRTB_SEC / coalizoes$NUM_SEC) 
coalizoes$napart_percent <- (coalizoes$tot_naopartidarios / coalizoes$NUM_SEC)



##### Composição do parlamentar do secretariado #####

# Somando as cadeiras que a coalizão controla
  coalizoes$tot_cadeiras <- coalizoes$PSDB_CD + coalizoes$PPR_PPB_PP_CD + coalizoes$PMDB_CD	+ 
  coalizoes$PFL_DEM_CD	+ coalizoes$PPS_CD +	coalizoes$PV_CD	+	coalizoes$PMN_CD +
  coalizoes$PT_CD	+	coalizoes$PSB_CD	+ coalizoes$PCdoB_CD	+	coalizoes$PSD_CD	+	
  coalizoes$PTB_CD	+	coalizoes$PL_PR_CD	+	coalizoes$PSC_CD	+ coalizoes$PSL_CD	+
  coalizoes$PDT_CD	+	coalizoes$PRONA_CD	+	coalizoes$PSDC_CD	+	coalizoes$PST_CD	+
  coalizoes$PRTB_CD

# Percentual de cadeiras controladas pela coalizão
coalizoes$PSDB_percent_CD <- (coalizoes$PSDB_CD / coalizoes$tot_cadeiras)
coalizoes$PPR_PPB_PP_percent_CD <- (coalizoes$PPR_PPB_PP_CD / coalizoes$tot_cadeiras)
coalizoes$PMDB_percent_CD <- (coalizoes$PMDB_CD / coalizoes$tot_cadeiras)
coalizoes$PFL_DEM_percent_CD <- (coalizoes$PFL_DEM_CD / coalizoes$tot_cadeiras)
coalizoes$PPS_percent_CD <- (coalizoes$PPS_CD / coalizoes$tot_cadeiras) 
coalizoes$PV_percent_CD <- (coalizoes$PV_CD / coalizoes$tot_cadeiras)
coalizoes$PMN_percent_CD <- (coalizoes$PMN_CD / coalizoes$tot_cadeiras)
coalizoes$PT_percent_CD <- (coalizoes$PT_CD / coalizoes$tot_cadeiras)
coalizoes$PSB_percent_CD <- (coalizoes$PSB_CD / coalizoes$tot_cadeiras)
coalizoes$PCdoB_percent_CD <- (coalizoes$PCdoB_CD / coalizoes$tot_cadeiras)
coalizoes$PSD_percent_CD <- (coalizoes$PSD_CD / coalizoes$tot_cadeiras) 
coalizoes$PTB_percent_CD <- (coalizoes$PTB_CD / coalizoes$tot_cadeiras) 
coalizoes$PL_PR_percent_CD <- (coalizoes$PL_PR_CD / coalizoes$tot_cadeiras) 
coalizoes$PSC_percent_CD <- (coalizoes$PSC_CD / coalizoes$tot_cadeiras)
coalizoes$PSL_percent_CD <- (coalizoes$PSL_CD / coalizoes$tot_cadeiras) 
coalizoes$PDT_percent_CD <- (coalizoes$PDT_CD / coalizoes$tot_cadeiras)
coalizoes$PRONA_percent_CD <- (coalizoes$PRONA_CD / coalizoes$tot_cadeiras)
coalizoes$PSDC_percent_CD <- (coalizoes$PSDC_CD / coalizoes$tot_cadeiras) 
coalizoes$PST_percent_CD <- (coalizoes$PST_CD / coalizoes$tot_cadeiras)
coalizoes$PRTB_percent_CD <- (coalizoes$PRTB_CD / coalizoes$tot_cadeiras)


#### Coalescência ####

# Calculando a diferença
coalizoes$PSDB_dif <- abs(coalizoes$PSDB_percent - coalizoes$PSDB_percent_CD)
coalizoes$PPR_PPB_PP_dif <- abs(coalizoes$PPR_PPB_PP_percent - coalizoes$PPR_PPB_PP_percent_CD)
coalizoes$PMDB_dif <- abs(coalizoes$PMDB_percent -  coalizoes$PMDB_percent_CD)
coalizoes$PFL_DEM_dif <-  abs(coalizoes$PFL_DEM_percent - coalizoes$PFL_DEM_percent_CD)
coalizoes$PPS_dif <- abs(coalizoes$PPS_percent -  coalizoes$PPS_percent_CD)
coalizoes$PV_dif <- abs(coalizoes$PV_percent - coalizoes$PV_percent_CD)
coalizoes$PMN_dif <- abs(coalizoes$PMN_percent -  coalizoes$PMN_percent_CD) 
coalizoes$PT_dif <-  abs(coalizoes$PT_percent - coalizoes$PT_percent_CD)
coalizoes$PSB_dif <- abs(coalizoes$PSB_percent -  coalizoes$PSB_percent_CD)
coalizoes$PCdoB_dif <- abs(coalizoes$PCdoB_percent - coalizoes$PCdoB_percent_CD)
coalizoes$PSD_dif <- abs(coalizoes$PSD_percent - coalizoes$PSD_percent_CD)
coalizoes$PTB_dif <- abs(coalizoes$PTB_percent - coalizoes$PTB_percent_CD)
coalizoes$PL_PR_dif <- abs(coalizoes$PL_PR_percent - coalizoes$PL_PR_percent_CD)
coalizoes$PSC_dif <-  abs(coalizoes$PSC_percent - coalizoes$PSC_percent_CD)
coalizoes$PSL_dif <- abs(coalizoes$PSL_percent - coalizoes$PSL_percent_CD)
coalizoes$PDT_dif <-  abs(coalizoes$PDT_percent - coalizoes$PDT_percent_CD)
coalizoes$PRONA_dif <- abs(coalizoes$PRONA_percent - coalizoes$PRONA_percent_CD)
coalizoes$PSDC_dif <- abs(coalizoes$PSDC_percent - coalizoes$PSDC_percent_CD)
coalizoes$PST_dif <- abs(coalizoes$PST_percent - coalizoes$PST_percent_CD)
coalizoes$PRTB_dif <-  abs(coalizoes$PRTB_percent - coalizoes$PRTB_percent_CD)


# Soma
coalizoes$somadifs <- coalizoes$PSDB_dif + coalizoes$PPR_PPB_PP_dif + coalizoes$PMDB_dif + 
  coalizoes$PFL_DEM_dif +
  coalizoes$PPS_dif + coalizoes$PV_dif + coalizoes$PMN_dif + coalizoes$PT_dif +
  coalizoes$PSB_dif + coalizoes$PCdoB_dif + coalizoes$PSD_dif + coalizoes$PTB_dif + 
  coalizoes$PL_PR_dif + coalizoes$PSC_dif + coalizoes$PSL_dif + coalizoes$PDT_dif +
  coalizoes$PRONA_dif + coalizoes$PSDC_dif + coalizoes$PST_dif + coalizoes$PRTB_dif +
  coalizoes$napart_percent

# Coalescência
coalizoes$coalescencia <- (1 - ((1/2) * coalizoes$somadifs))