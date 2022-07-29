# Ideologia

library(tidyverse)
library(haven)
library(readr)
library(tidyr)


# Abrir o BLS8
link <- "https://github.com/felipelirapaiva/dissertacao/blob/main/temporarios/bls8_estimates_parties_long.csv?raw=true"
download.file(link, "bls8_estimates_parties_long.csv")
bls8_estimates_parties_long <- read.csv("bls8_estimates_parties_long.csv", dec = ",", encoding = "UTF-8", check.names = FALSE)


# Renomear os anos; filtrar os casos;
  ideologia <- bls8_estimates_parties_long %>%
    mutate(year = case_when(year == "1997" ~ "1994",
                            year == "2001" ~ "1998",
                            year == "2005" ~ "2002",
                            year == "2009" ~ "2006"))

  ideologia <- ideologia %>%
    filter(year != "NA") %>%
    select(year, ideo, party)

# Transformando em um banco longo
  ideologia2 <- ideologia %>%
    pivot_wider(
      names_from = "party", values_from = "ideo", values_fill = 0)
  
  
# Abrindo a base de dados de coalizões
  link <- "https://github.com/felipelirapaiva/dissertacao/blob/main/temporarios/merged.csv?raw=true"
  download.file(link, "merged.csv")
  coalizoes <- read.csv("merged.csv", dec = ",", encoding = "UTF-8", check.names = FALSE)

# Merge
  library(scales)
   coalizoes <- coalizoes %>%
    mutate(ANO_REFERENCIA = case_when(ANO_REFERENCIA == "1994" ~ "1994",
                            ANO_REFERENCIA == "1998" ~ "1998",
                            ANO_REFERENCIA == "2002" ~ "2002",
                            ANO_REFERENCIA == "2006" ~ "2006"))
   
   tudo <- left_join(coalizoes, ideologia2,
                    by = c("ANO_REFERENCIA" = "year"))

  
