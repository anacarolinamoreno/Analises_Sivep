
library(tidyverse)
library(lubridate)
library(zoo)
library(readr)
library(ggplot2)
library(tidylog)
library(ggthemes)
library(lme4)
library(plotly)
library(hrbrthemes)
library(svglite)




# IMPORTANTE: As análises abaixo dependem do código no script sivep_preparar_base.R

# ÍNDICE DE ANÁLISES: CASOS DE COVID-19 CONFIRMADA NO ESTADO DE SÃO PAULO (ESP)
# 1.1- Total de internações e mortes por Covid por dia no ESP
# 1.2- Média móvel de internações e mortes por Covid por dia no ESP
# 1.3 - Total de óbitos por Covid por mês para cada faixa etária no ESP


# Criar condições para delimitar o período dos casos entre o início de fevereiro e a segunda-feira da extração da base

if(wday(today()) == 1) {
  ultima_segunda <- today() - 6
} else {
  ultima_segunda <- today() - wday(today()) - 2
}

inicio_pandemia <- ymd('2020-02-01')


# Melhorando a imagem dos gráficos pixelados do Windows

trace(grDevices::png, quote({
  if (missing(type) && missing(antialias)) {
    type <- "cairo-png"
    antialias <- "subpixel"
  }
}), print = FALSE)



# 1.1- Total de internações e mortes por Covid por dia no ESP

internacoes_sp_dia <- sivep_sp %>%
  filter(
    DT_INTERNA > inicio_pandemia
    & DT_INTERNA < ultima_segunda
    & SG_UF == "SP"
    & diagnostico == "Covid-19"
    & !is.na(DT_INTERNA)
    & DT_INTERNA > "2020-01-31") %>%
  group_by(diagnostico, DT_INTERNA) %>%
  summarise(
    contagem = sum(contagem)
  ) %>%
  rename(
    internacoes = contagem,
    data = DT_INTERNA
  ) %>%
  select(diagnostico, data, internacoes)

obitos_sp_dia <- sivep_sp %>%
  filter(SG_UF == "SP"
         & diagnostico == "Covid-19"
         & !is.na(DT_EVOLUCA)
         & EVOLUCAO == 2
         & DT_EVOLUCA <= ultima_segunda) %>%
  group_by(diagnostico, DT_EVOLUCA) %>%
  summarise(
    contagem = sum(contagem)
  ) %>%
  rename(
    mortes = contagem,
    data = DT_EVOLUCA
  ) %>%
  select(diagnostico, data, mortes)

covid_sp_dia <- left_join(x = internacoes_sp_dia, y = obitos_sp_dia, by = 'data') %>%
  select(data, internacoes, mortes) %>%
  replace(is.na(.), 0) %>%
  pivot_longer(cols = c(internacoes, mortes), names_to = "categoria", values_to = "valor") %>%
  write_rds("data/analises_semanais_sp/covid_sp_dia.rds")





# 1.2- Média móvel de internações e mortes por Covid por dia no ESP

internacoes_sp_dia_media <- sivep_sp %>%
  filter(
    DT_INTERNA > inicio_pandemia
    & DT_INTERNA < ultima_segunda
    & SG_UF == "SP"
    & diagnostico == "Covid-19"
    & !is.na(DT_INTERNA)
    & DT_INTERNA > "2020-01-31") %>%
  group_by(diagnostico, DT_INTERNA) %>%
  summarise(
    contagem = sum(contagem)
  ) %>%
  mutate(
    media_movel = round(rollapply(contagem, 7, mean, align = 'right', fill = NA), 0)
  ) %>%
  rename(
    internacoes_mm7d = media_movel,
    data = DT_INTERNA
  ) %>%
  replace(is.na(.), 0) %>%
  select(diagnostico, data, internacoes_mm7d)

obitos_sp_dia_media <- sivep_sp %>%
  filter(SG_UF == "SP"
         & diagnostico == "Covid-19"
         & !is.na(DT_EVOLUCA)
         & EVOLUCAO == 2
         & DT_EVOLUCA <= ultima_segunda) %>%
  group_by(diagnostico, DT_EVOLUCA) %>%
  summarise(
    contagem = sum(contagem)
  ) %>%
  mutate(
    media_movel = round(rollapply(contagem, 7, mean, align = 'right', fill = NA), 0)
  ) %>%
  rename(
    mortes_mm7d = media_movel,
    data = DT_EVOLUCA
  ) %>%
  replace(is.na(.), 0) %>%
  select(diagnostico, data, mortes_mm7d)

covid_sp_dia_media <- left_join(x = internacoes_sp_dia_media, y = obitos_sp_dia_media, by = 'data') %>%
  filter(!is.na(data)) %>%
  select(data, internacoes_mm7d, mortes_mm7d) %>%
  replace(is.na(.), 0) %>%
  pivot_longer(cols = c(internacoes_mm7d, mortes_mm7d), names_to = "categoria", values_to = "valor") %>%
  write_rds("data/analises_semanais_sp/covid_sp_dia_media.rds")




# 1.3 - Total de óbitos por Covid por mês para cada faixa etária no ESP (20 em 20 anos)


obitos_covid_sp_idade_20 <- sivep_sp %>%
  filter(
    SG_UF == "SP"
    & diagnostico == "Covid-19"
    & EVOLUCAO == 2
  ) %>%
  group_by(mes_ano_obito, faixa_etaria_20) %>%
  summarise(
    contagem = sum(contagem)
  ) %>%
  rename(
    mortes = contagem,
    mes = mes_ano_obito,
    faixa_etaria = faixa_etaria_20
  ) %>%
  mutate(
    mes = as.yearmon(mes, "%m/%Y")
  ) %>%
  select(mes, mortes, faixa_etaria) %>%
  write_rds("data/analises_semanais_sp/obitos_covid_sp_idade_20.rds")









