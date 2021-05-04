rm(list = ls())

rm(list = grep("^sivep", ls(), value = TRUE, invert = TRUE))

library(tidyverse)
library(lubridate)
library(zoo)
library(readr)
library(ggplot2)
library(tidylog)

# Carregar as funções

sf <- stamp("01/20")

sf(ymd("2020-12-05"))

end.of.epiweek <- function(x, end = 6) {
  offset <- (end - 4) %% 7
  num.x <- as.numeric(x)
  return(x - (num.x %% 7) + offset + ifelse(num.x %% 7 > offset, 7, 0))
}


# Carregar as bases com as colunas pré-selecionadas

sivep2020 <- read.csv2('C:/Users/cmoreno/Documents/TV/Códigos/Sivep/data-raw/INFLUD-19-04-2021.csv', encoding="Windows-1252") %>%
  select(DT_NOTIFIC, DT_SIN_PRI, SEM_PRI, SG_UF_NOT, CO_MUN_NOT, ID_MUNICIP, CS_SEXO, NU_IDADE_N, SG_UF, CO_MUN_RES, ID_MN_RESI, ID_UNIDADE, HOSPITAL, DT_INTERNA, SG_UF_INTE
         , CO_MU_INTE, ID_MN_INTE, UTI, DT_ENTUTI, DT_SAIDUTI, SUPORT_VEN, PCR_SARS2, CLASSI_FIN, CRITERIO, EVOLUCAO, DT_EVOLUCA, DT_ENCERRA, CS_GESTANT, PUERPERA)

sivep2021 <- read.csv2('C:/Users/cmoreno/Documents/TV/Códigos/Sivep/data-raw/INFLUD21-19-04-2021.csv', encoding="Windows-1252") %>%
  select(DT_NOTIFIC, DT_SIN_PRI, SEM_PRI, SG_UF_NOT, CO_MUN_NOT, ID_MUNICIP, CS_SEXO, NU_IDADE_N, SG_UF, CO_MUN_RES, ID_MN_RESI, ID_UNIDADE, HOSPITAL, DT_INTERNA, SG_UF_INTE
         , CO_MU_INTE, ID_MN_INTE, UTI, DT_ENTUTI, DT_SAIDUTI, SUPORT_VEN, PCR_SARS2, CLASSI_FIN, CRITERIO, EVOLUCAO, DT_EVOLUCA, DT_ENCERRA, CS_GESTANT, PUERPERA)

# Concatenar as bases e criar as colunas agrupando por faixa etária, semana, mês diagnóstico e desfecho

sivep <- bind_rows(
  sivep2020,
  sivep2021
) %>%
  mutate(
    DT_NOTIFIC = lubridate::dmy(DT_NOTIFIC),
    sem_notific = end.of.epiweek(DT_NOTIFIC),
    DT_SIN_PRI = lubridate::dmy(DT_SIN_PRI),
    sem_sintoma = end.of.epiweek(DT_SIN_PRI),
    DT_INTERNA = lubridate::dmy(DT_INTERNA),
    sem_interna = end.of.epiweek(DT_INTERNA),
    DT_ENTUTI = lubridate::dmy(DT_ENTUTI),
    sem_uti = end.of.epiweek(DT_ENTUTI),
    DT_SAIDUTI = lubridate::dmy(DT_SAIDUTI),
    sem_saida_uti = end.of.epiweek(DT_SAIDUTI),
    DT_EVOLUCA = lubridate::dmy(DT_EVOLUCA),
    sem_evolucao = end.of.epiweek(DT_EVOLUCA),
    faixa_etaria_20 = case_when(
      NU_IDADE_N < 20 ~ "0 a 19 anos",
      NU_IDADE_N > 19 & NU_IDADE_N < 40 ~ "20 a 39 anos",
      NU_IDADE_N > 39 & NU_IDADE_N < 60 ~ "40 a 59 anos",
      NU_IDADE_N > 59 & NU_IDADE_N < 80 ~ "60 a 79 anos",
      NU_IDADE_N > 79 ~ "80 anos ou mais",
      T ~ "Idade não informada"),
    faixa_etaria = case_when(
      NU_IDADE_N < 1 ~ "0 a menos de 01 ano",
      NU_IDADE_N > 0 & NU_IDADE_N < 5 ~ "01 a 04 anos",
      NU_IDADE_N > 4 & NU_IDADE_N < 10 ~ "05 a 09 anos",
      NU_IDADE_N > 9 & NU_IDADE_N < 15 ~ "10 a 14 anos",
      NU_IDADE_N > 14 & NU_IDADE_N < 20 ~ "15 a 19 anos",
      NU_IDADE_N > 19 & NU_IDADE_N < 30 ~ "20 a 29 anos",
      NU_IDADE_N > 29 & NU_IDADE_N < 40 ~ "30 a 39 anos",
      NU_IDADE_N > 39 & NU_IDADE_N < 50 ~ "40 a 49 anos",
      NU_IDADE_N > 49 & NU_IDADE_N < 60 ~ "50 a 59 anos",
      NU_IDADE_N > 59 & NU_IDADE_N < 70 ~ "60 a 69 anos",
      NU_IDADE_N > 69 & NU_IDADE_N < 80 ~ "70 a 79 anos",
      NU_IDADE_N > 79 & NU_IDADE_N < 90 ~ "80 a 89 anos",
      NU_IDADE_N > 89 ~ "90 anos ou mais",
      T ~ "Idade não informada"),
    faixa_etaria_10 = case_when(
      NU_IDADE_N < 10 ~ "0 a 9 anos",
      NU_IDADE_N > 10 & NU_IDADE_N < 20 ~ "10 a 19 anos",
      NU_IDADE_N > 19 & NU_IDADE_N < 30 ~ "20 a 29 anos",
      NU_IDADE_N > 29 & NU_IDADE_N < 40 ~ "30 a 39 anos",
      NU_IDADE_N > 39 & NU_IDADE_N < 50 ~ "40 a 49 anos",
      NU_IDADE_N > 49 & NU_IDADE_N < 60 ~ "50 a 59 anos",
      NU_IDADE_N > 59 & NU_IDADE_N < 70 ~ "60 a 69 anos",
      NU_IDADE_N > 69 & NU_IDADE_N < 80 ~ "70 a 79 anos",
      NU_IDADE_N > 79 & NU_IDADE_N < 90 ~ "80 a 89 anos",
      NU_IDADE_N > 89 & NU_IDADE_N < 120 ~ "90 anos ou mais",
      T ~ "Idade não informada"),
    faixa_etaria_5 = case_when(
      NU_IDADE_N < 5 ~ "00 a 04 anos",
      NU_IDADE_N > 4 & NU_IDADE_N < 10 ~ "05 a 09 anos",
      NU_IDADE_N > 9 & NU_IDADE_N < 15 ~ "10 a 14 anos",
      NU_IDADE_N > 14 & NU_IDADE_N < 20 ~ "15 a 19 anos",
      NU_IDADE_N > 19 & NU_IDADE_N < 25 ~ "20 a 24 anos",
      NU_IDADE_N > 24 & NU_IDADE_N < 30 ~ "25 a 29 anos",
      NU_IDADE_N > 29 & NU_IDADE_N < 35 ~ "30 a 34 anos",
      NU_IDADE_N > 34 & NU_IDADE_N < 40 ~ "35 a 39 anos",
      NU_IDADE_N > 39 & NU_IDADE_N < 45 ~ "40 a 44 anos",
      NU_IDADE_N > 44 & NU_IDADE_N < 50 ~ "45 a 49 anos",
      NU_IDADE_N > 49 & NU_IDADE_N < 55 ~ "50 a 54 anos",
      NU_IDADE_N > 54 & NU_IDADE_N < 60 ~ "55 a 59 anos",
      NU_IDADE_N > 59 & NU_IDADE_N < 65 ~ "60 a 64 anos",
      NU_IDADE_N > 64 & NU_IDADE_N < 70 ~ "65 a 69 anos",
      NU_IDADE_N > 69 & NU_IDADE_N < 75 ~ "70 a 74 anos",
      NU_IDADE_N > 74 ~ "75 anos ou mais",
      T ~ "Idade não informada"),
    faixa_etaria_5_80m = case_when(
      NU_IDADE_N < 5 ~ "00 a 04 anos",
      NU_IDADE_N > 4 & NU_IDADE_N < 10 ~ "05 a 09 anos",
      NU_IDADE_N > 9 & NU_IDADE_N < 15 ~ "10 a 14 anos",
      NU_IDADE_N > 14 & NU_IDADE_N < 20 ~ "15 a 19 anos",
      NU_IDADE_N > 19 & NU_IDADE_N < 25 ~ "20 a 24 anos",
      NU_IDADE_N > 24 & NU_IDADE_N < 30 ~ "25 a 29 anos",
      NU_IDADE_N > 29 & NU_IDADE_N < 35 ~ "30 a 34 anos",
      NU_IDADE_N > 34 & NU_IDADE_N < 40 ~ "35 a 39 anos",
      NU_IDADE_N > 39 & NU_IDADE_N < 45 ~ "40 a 44 anos",
      NU_IDADE_N > 44 & NU_IDADE_N < 50 ~ "45 a 49 anos",
      NU_IDADE_N > 49 & NU_IDADE_N < 55 ~ "50 a 54 anos",
      NU_IDADE_N > 54 & NU_IDADE_N < 60 ~ "55 a 59 anos",
      NU_IDADE_N > 59 & NU_IDADE_N < 65 ~ "60 a 64 anos",
      NU_IDADE_N > 64 & NU_IDADE_N < 70 ~ "65 a 69 anos",
      NU_IDADE_N > 69 & NU_IDADE_N < 75 ~ "70 a 74 anos",
      NU_IDADE_N > 74 & NU_IDADE_N < 80 ~ "75 a 79 anos",
      NU_IDADE_N > 79 ~ "80 anos ou mais",
      T ~ "Idade não informada"),
    mes = lubridate::month(DT_INTERNA),
    ano = lubridate::year(DT_INTERNA),
    mes_ano_internacao1 = case_when(
      mes == 1 & ano == 2020 ~ "01/2020",
      mes == 2 & ano == 2020 ~ "02/2020",
      mes == 3 & ano == 2020 ~ "03/2020",
      mes == 4 & ano == 2020 ~ "04/2020",
      mes == 5 & ano == 2020 ~ "05/2020",
      mes == 6 & ano == 2020 ~ "06/2020",
      mes == 7 & ano == 2020 ~ "07/2020",
      mes == 8 & ano == 2020 ~ "08/2020",
      mes == 9 & ano == 2020 ~ "09/2020",
      mes == 10 & ano == 2020 ~ "10/2020",
      mes == 11 & ano == 2020 ~ "11/2020",
      mes == 12 & ano == 2020 ~ "12/2020",
      mes == 1 & ano == 2021 ~ "01/2021",
      mes == 2 & ano == 2021 ~ "02/2021",
      mes == 3 & ano == 2021 ~ "03/2021",
      mes == 4 & ano == 2021 ~ "04/2021",
      T  ~ "Data não informada/erro de digitação"
    ),
    mes_ano_internacao2 = sf(ymd(DT_INTERNA)),
    mes_ano_entrada_uti = sf(ymd(DT_ENTUTI)),
    mes_uti = lubridate::month(DT_ENTUTI),
    ano_uti = lubridate::year(DT_ENTUTI),
    mes_ano_UTI = case_when(
      mes_uti == 1 & ano_uti == 2020 ~ "01/2020",
      mes_uti == 2 & ano_uti == 2020 ~ "02/2020",
      mes_uti == 3 & ano_uti == 2020 ~ "03/2020",
      mes_uti == 4 & ano_uti == 2020 ~ "04/2020",
      mes_uti == 5 & ano_uti == 2020 ~ "05/2020",
      mes_uti == 6 & ano_uti == 2020 ~ "06/2020",
      mes_uti == 7 & ano_uti == 2020 ~ "07/2020",
      mes_uti == 8 & ano_uti == 2020 ~ "08/2020",
      mes_uti == 9 & ano_uti == 2020 ~ "09/2020",
      mes_uti == 10 & ano_uti == 2020 ~ "10/2020",
      mes_uti == 11 & ano_uti == 2020 ~ "11/2020",
      mes_uti == 12 & ano_uti == 2020 ~ "12/2020",
      mes_uti == 1 & ano_uti == 2021 ~ "01/2021",
      mes_uti == 2 & ano_uti == 2021 ~ "02/2021",
      mes_uti == 3 & ano_uti == 2021 ~ "03/2021",
      mes_uti == 4 & ano_uti == 2021 ~ "04/2021",
      T | is.na(DT_ENTUTI) ~ "Data não informada/erro de digitação"
    ),
    mes_obito = lubridate::month(DT_EVOLUCA),
    ano_obito = lubridate::year(DT_EVOLUCA),
    mes_ano_obito = case_when(
      mes_obito == 1 & ano_obito == 2020 ~ "01/2020",
      mes_obito == 2 & ano_obito == 2020 ~ "02/2020",
      mes_obito == 3 & ano_obito == 2020 ~ "03/2020",
      mes_obito == 4 & ano_obito == 2020 ~ "04/2020",
      mes_obito == 5 & ano_obito == 2020 ~ "05/2020",
      mes_obito == 6 & ano_obito == 2020 ~ "06/2020",
      mes_obito == 7 & ano_obito == 2020 ~ "07/2020",
      mes_obito == 8 & ano_obito == 2020 ~ "08/2020",
      mes_obito == 9 & ano_obito == 2020 ~ "09/2020",
      mes_obito == 10 & ano_obito == 2020 ~ "10/2020",
      mes_obito == 11 & ano_obito == 2020 ~ "11/2020",
      mes_obito == 12 & ano_obito == 2020 ~ "12/2020",
      mes_obito == 1 & ano_obito == 2021 ~ "01/2021",
      mes_obito == 2 & ano_obito == 2021 ~ "02/2021",
      mes_obito == 3 & ano_obito == 2021 ~ "03/2021",
      mes_obito == 4 & ano_obito == 2021 ~ "04/2021",
      T | is.na(DT_EVOLUCA) ~ "Data não informada/erro de digitação"
    ),
    contagem = case_when(
      is.na(DT_NOTIFIC) ~ 1,
      T ~ 1
    ),
    diagnostico = case_when(
      CLASSI_FIN == 1 ~ "Influenza",
      CLASSI_FIN == 2 ~ "Outro vírus respiratório",
      CLASSI_FIN == 3 ~ "Outro agente etiológico",
      CLASSI_FIN == 4 ~ "SRAG não especificada",
      CLASSI_FIN == 5 | PCR_SARS2 == 1 ~ "Covid-19"
    ),
    desfecho = case_when(
      EVOLUCAO == 1 ~ "Alta",
      EVOLUCAO == 2 & diagnostico == "Covid-19"~ "Óbito confirmado por Covid-19",
      EVOLUCAO == 2 & diagnostico != "Covid-19"~ "Óbito por SRAG, mas não Covid-19",
      EVOLUCAO == 3 ~ "Óbito por outras causas que não SRAG nem Covid-19",
      EVOLUCAO == 9 | is.na(EVOLUCAO) ~ "Sem desfecho ainda"
    ))

# Filtras só os casos de residentes do Estado de São Paulo

sivep_sp <- sivep %>%
  filter(SG_UF == "SP")




# ANÁLISES A PARTIR DA SIVEP JÁ CONCATENADA

# ÍNDICE:

# ARQUIVO sivep_analises_SP.R:
# CASOS DE COVID-19 CONFIRMADA NO ESTADO DE SÃO PAULO (ESP)
# 1.1- Total de internações e mortes por Covid por dia no ESP
# 1.2- Média móvel de internações e mortes por Covid por dia no ESP
# 1.3- Total de internações por Covid por semana epidemiológica no ESP
# 1.4- Total de internações por Covid por mês no ESP
# 1.5 - Total de internações por Covid por mês para cada faixa etária no ESP
# 1.6 - Total de internações em UTI por Covid por mês para cada faixa etária no ESP
# 1.7 - Total de óbitos por Covid por mês para cada faixa etária no ESP

# ARQUIVO sivep_analises_BR.R:
# CASOS DE SRAG NO ESTADO DE SÃO PAULO (INCLUI COVID E DEMAIS DIAGNÓSTICOS)
# 2.1- Total de internações e mortes por SRAG por dia no ESP
# 2.2- Média móvel de internações e mortes por SRAG por dia no ESP
# 2.3- Total de internações por SRAG por semana epidemiológica no ESP
# 2.4- Total de internações por SRAG por mês no ESP
# 2.5 - Total de internações por SRAG por mês para cada faixa etária no ESP
# 2.6 - Total de internações em UTI por SRAG por mês para cada faixa etária no ESP
# 2.7 - Total de óbitos por SRAG por mês para cada faixa etária no ESP

# CAPÍTULO 3: CASOS DE COVID-19 CONFIRMADA NO BRASIL
# 3.1- Elefante (total de mortes no Brasil confirmadas por Covid-19 pela data de ocorrência)
# 3.2- Total de internações e mortes por Covid por dia no Brasil
# 3.3- Total de internações por Covid por semana epidemiológica no Brasil
# 3.4- Total de internações por Covid por mês no Brasil
# 3.5 - Total de internações por Covid por mês para cada faixa etária no Brasil
# 3.6 - Total de internações em UTI por Covid por mês para cada faixa etária no Brasil
# 3.7 - Total de óbitos por Covid por mês para cada faixa etária no Brasil

# 3- CASOS DE SRAG (INCLUI COVID E OUTROS DIAGNÓSTICOS)
# 3.1- Total de internações e mortes por SRAG por dia no Brasil
# 3.2- Média móvel de internações e mortes por SRAG por dia no Brasil
# 3.3- Total de internações por SRAG por semana epidemiológica no Brasil
# 3.4- Total de internações por SRAG por mês no Brasil
# 3.5 - Total de internações por SRAG por mês para cada faixa etária no Brasil
# 3.6 - Total de internações em UTI por SRAG por mês para cada faixa etária no Brasil
# 3.7 - Total de óbitos por SRAG por mês para cada faixa etária no Brasil


