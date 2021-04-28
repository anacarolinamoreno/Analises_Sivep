
library(tidyverse)
library(lubridate)
library(zoo)
library(readr)
library(ggplot2)
library(tidylog)
library(ggthemes)
library(lme4)

# IMPORTANTE: As análises abaixo dependem do código no script sivep_preparar_base.R

# ÍNDICE: CASOS DE COVID-19 CONFIRMADA NO ESTADO DE SÃO PAULO (ESP)
# 1.1- Total de internações e mortes por Covid por dia no ESP
# 1.2- Média móvel de internações e mortes por Covid por dia no ESP
# 1.3- Total de internações por Covid por semana epidemiológica no ESP
# 1.4- Total de internações por Covid por mês no ESP
# 1.5 - Total de internações por Covid por mês para cada faixa etária no ESP
# 1.6 - Total de internações em UTI por Covid por mês para cada faixa etária no ESP
# 1.7 - Total de óbitos por Covid por mês para cada faixa etária no ESP


# Criar condições para delimitar o período dos casos entre o início de fevereiro e a segunda-feira da extração da base

if(wday(today()) == 1) {
  ultima_segunda <- today() - 6
} else {
  ultima_segunda <- today() - wday(today()) - 2
}

inicio_pandemia <- ymd('2020-02-01')



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
  write_rds("data/covid_sp_dia.rds")

ggplot_covid_sp_dia <- read_rds("data/covid_sp_dia.rds") %>%
  ggplot(aes(x=data, y=valor, group=categoria, colour=categoria)) +
  geom_line()+
  labs(y = 'Total de internações e mortes por dia',
       x = 'Fonte: Censo Covid/SES-SP')+
  ggtitle("Novas internações e novas mortes confirmadas por Covid-19 (Estado de SP)")+
  theme(
    plot.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white", colour="grey"),
    panel.grid.major = element_line(colour = "grey", linetype = "dotted"),
    panel.grid.minor = element_line(colour = "grey", linetype = "dotted"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
  )+
  geom_vline(xintercept=ymd("2021-03-24"), color="blue", size=0.2)


ggplot_covid_sp_dia



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
  write_rds("data/covid_sp_dia_media.rds")

ggplot_covid_sp_dia_media <- read_rds("data/covid_sp_dia_media.rds") %>%
  ggplot(aes(x=data, y=valor, group=categoria, colour=categoria)) +
  geom_line()+
  labs(y = 'Média móvel de internações e mortes por dia',
       x = 'Fonte: Censo Covid/SES-SP')+
  ggtitle("Novas internações e novas mortes confirmadas por Covid-19 (Estado de SP)")+
  theme(
    plot.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white", colour="grey"),
    panel.grid.major = element_line(colour = "grey", linetype = "dotted"),
    panel.grid.minor = element_line(colour = "grey", linetype = "dotted"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
  )+
  geom_vline(xintercept=ymd("2021-03-24"), color="blue", size=0.2)


ggplot_covid_sp_dia_media


# 1.3- Total de internações por Covid por semana epidemiológica no ESP

internacoes_sp_semana <- sivep_sp %>%
  filter(
    DT_INTERNA > inicio_pandemia
    & DT_INTERNA < ultima_segunda
    & SG_UF == "SP"
    & diagnostico == "Covid-19"
    & !is.na(DT_INTERNA)
    & DT_INTERNA > "2020-01-31") %>%
  group_by(diagnostico, sem_interna) %>%
  summarise(
    contagem = sum(contagem)
  ) %>%
  rename(
    internacoes = contagem,
    semana = sem_interna
  ) %>%
  select(diagnostico, semana, internacoes) %>%
  write_rds("data/covid_sp_semana.rds")

ggplot_covid_sp_semana <- read_rds("data/covid_sp_semana.rds") %>%
  ggplot(aes(x=semana, y=internacoes)) +
  geom_line()+
  labs(y = 'Total de internações por semana',
       x = 'Fonte: Censo Covid/SES-SP')+
  ggtitle("Internações confirmadas por Covid-19 (Estado de SP)")+
  geom_vline(xintercept=ymd("2021-03-24"), color="blue", size=0.2)


ggplot_covid_sp_semana


# 1.4- Total de internações por Covid por mês no ESP

internacoes_sp_mes <- sivep_sp %>%
  filter(
    DT_INTERNA > inicio_pandemia
    & DT_INTERNA < ultima_segunda
    & SG_UF == "SP"
    & diagnostico == "Covid-19"
    & !is.na(DT_INTERNA)
    & DT_INTERNA > "2020-02-29") %>%
  group_by(diagnostico, mes_ano_internacao1) %>%
  summarise(
    contagem = sum(contagem)
  ) %>%
  rename(
    internacoes = contagem,
    mes = mes_ano_internacao1
  ) %>%
  mutate(
    mes = as.yearmon(mes, "%m/%Y")
  ) %>%
  select(diagnostico, mes, internacoes) %>%
  arrange(mes) %>%
  write_rds("data/covid_sp_mes.rds")

ggplot_covid_sp_mes <- read_rds("data/covid_sp_mes.rds") %>%
  ggplot(aes(x=mes, y=internacoes, colour="blue")) +
  geom_bar(stat = "identity")+
  geom_text(aes(x = mes,
                y = internacoes,
                vjust = -0.1,
                size = 5,
                label = internacoes,
                colour = "red"))+
  labs(y = 'Total de internações por mês',
       x = 'Fonte: Censo Covid/SES-SP')+
  ggtitle("Internações confirmadas por Covid-19 (Estado de SP)")

ggplot_covid_sp_mes


# 1.5 - Total de internações por Covid por mês para cada faixa etária no ESP



# 1.6 - Total de internações em UTI por Covid por mês para cada faixa etária no ESP
# 1.7 - Total de óbitos por Covid por mês para cada faixa etária no ESP
