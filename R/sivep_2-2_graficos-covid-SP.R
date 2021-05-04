

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


# IMPORTANTE: As análises abaixo dependem do código no script sivep_1_preparar_base.R e dos arquivos RDS gerados pelo script sivep_2-1_analises-covid-SP.R

# ÍNDICE DE GRÁFICOS: CASOS DE COVID-19 CONFIRMADA NO ESTADO DE SÃO PAULO (ESP)
# 1.1- Total de internações e mortes por Covid por dia no ESP
# 1.2- Média móvel de internações e mortes por Covid por dia no ESP
# 1.3 - Total de óbitos por Covid por mês para cada faixa etária no ESP


# 1.1- Total de internações e mortes por Covid por dia no ESP

ggplot_covid_sp_dia <- read_rds("data/analises_semanais_sp/covid_sp_dia.rds") %>%
  ggplot(aes(x=data, y=valor, group=categoria, colour=categoria))+
  geom_line(size = 1)+
  labs(
    title = "Internações e mortes confirmadas por Covid-19 (Estado de SP)",
    subtitle = "Total por dia (VALOR ABSOLUTO)",
    caption = "Fonte: Sivep-Gripe (Ministério da Saúde)",
    y = "total por dia",
    x = "data"
  )+
  scale_color_manual(
    values=c("#3D748F", "#a80000"),
    labels = c("Novas internações", "Novas mortes")
  )+
  theme(
    plot.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white", colour="grey"),
    panel.grid.major = element_line(colour = "grey", linetype = "dotted"),
    panel.grid.minor = element_line(colour = "grey", linetype = "dotted"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.caption = element_text(face = "italic"),
    legend.position="top",
    legend.title = element_blank()
  )+
  # Destaques para mostrar a data e o valor do pico de INTERNAÇÕES
  annotate(
    geom="text",
    x=as.Date("2021-03-01"),
    y=2248,
    label="23/03",
    size = 3) +
  annotate(
    geom="text",
    x=as.Date("2021-03-01"),
    y=2100,
    label="2.248",
    size = 3) +
  annotate(
    geom="point",
    x=as.Date("2021-03-23"),
    y=2248,
    size=3,
    shape=20,
    fill="black")+
  # Destaques para mostrar a data e o valor do pico de MORTES
  annotate(
    geom="text",
    x=as.Date("2021-03-10"),
    y=813,
    label="27/03",
    size = 3) +
  annotate(
    geom="text",
    x=as.Date("2021-03-10"),
    y=690,
    label="816",
    size = 3) +
  annotate(
    geom="point",
    x=as.Date("2021-03-27"),
    y=816,
    size=3,
    shape=20,
    fill="black")+
  # linhas vertical e horizontal
  geom_vline(xintercept=ymd("2021-04-01"), color="grey", size=0.2)+
  geom_hline(yintercept=1000, color="#BEBEBE", size=.5) +
  # retângulo de fundo cinza indicando o atraso de notificação
  annotate(
    "rect",
    xmin=as.Date("2021-04-01"),
    xmax=as.Date("2021-05-10"),
    ymin=-20,
    ymax=2200,
    alpha=0.2,
    color="transparent",
    fill="grey")+
  # destaque indicanddo o atraso de notificação
  annotate(
    "text",
    x = as.Date("2021-05-06"),
    y = 1600,
    label = "Atraso de notificação",
    color = "#666666",
    size = 3.5,
    angle = -90,
    fontface = "bold"
  )


ggplot_covid_sp_dia

# 1.2- Média móvel de internações e mortes por Covid por dia no ESP

ggplot_covid_sp_dia_media <- read_rds("data/analises_semanais_sp/covid_sp_dia_media.rds") %>%
  ggplot(aes(x=data, y=valor, group=categoria, colour=categoria)) +
  # Definindo a forma do gráfico (nesse caso, gráfico de linhas)
  geom_line(size = 1.5)+
  # Definindo os textos ao redor do gráfico
  labs(
    title = "Internações e mortes confirmadas por Covid-19 (Estado de SP)",
    subtitle = "Total por dia (MÉDIA MÓVEL)",
    caption = "Fonte: Sivep-Gripe (Ministério da Saúde)",
    y = "total por dia",
    x = "data"
  )+
  # Definindo as cores das linhas e os detalhes da legenda
  scale_color_manual(
    values=c("#3D748F", "#a80000"),
    labels = c("Novas internações", "Novas mortes")
  )+
  # Definindo cores e formas dos elementos de fundo do gráfico
  theme(
    plot.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white", colour="grey"),
    panel.grid.major = element_line(colour = "grey", linetype = "dotted"),
    panel.grid.minor = element_line(colour = "grey", linetype = "dotted"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.caption = element_text(face = "italic"),
    legend.position="top",
    legend.title = element_blank()
  )+
  # Destaques para mostrar a data e o valor do pico de INTERNAÇÕES
  annotate(
    geom="text",
    x=as.Date("2021-03-05"),
    y=2156,
    label="23/03",
    size = 3) +
  annotate(
    geom="text",
    x=as.Date("2021-03-05"),
    y=2050,
    label="2.096",
    size = 3) +
  annotate(
    geom="point",
    x=as.Date("2021-03-23"),
    y=2096,
    size=3,
    shape=20,
    fill="black")+
  # Destaques para mostrar a data e o valor do pico de MORTES
  annotate(
    geom="text",
    x=as.Date("2021-03-13"),
    y=794,
    label="28/03",
    size = 3) +
  annotate(
    geom="text",
    x=as.Date("2021-03-13"),
    y=690,
    label="794",
    size = 3) +
  annotate(
    geom="point",
    x=as.Date("2021-03-28"),
    y=794,
    size=3,
    shape=20,
    fill="black")+
  # linhas vertical e horizontal
  geom_vline(xintercept=ymd("2021-04-01"), color="grey", size=0.2)+
  geom_hline(yintercept=1000, color="#BEBEBE", size=.5) +
  # retângulo de fundo cinza indicando o atraso de notificação
  annotate(
    "rect",
    xmin=as.Date("2021-04-01"),
    xmax=as.Date("2021-05-10"),
    ymin=0,
    ymax=2200,
    alpha=0.2,
    color="transparent",
    fill="grey")+
  # destaque indicanddo o atraso de notificação
  annotate(
    "text",
    x = as.Date("2021-05-06"),
    y = 1600,
    label = "Atraso de notificação",
    color = "#666666",
    size = 3.5,
    angle = -90,
    fontface = "bold"
  )+
  # salvando uma versão do gráfico em formato .SVG para ser facilmente replicado no Illustrator
  ggsave('docs/grafico_covid_sp_dia_mediamovel.svg', dpi = 'retina',
         height = 8, width = 12, limitsize = F)


ggplot_covid_sp_dia_media


# 1.3 - Total de óbitos por Covid por mês para cada faixa etária no ESP

ggplot_obitos_covid_sp_idade_20 <- read_rds("data/analises_semanais_sp/obitos_covid_sp_idade_20.rds") %>%
  filter(mes != "abr 2021") %>%
  ggplot(aes(x=mes, y=mortes, group=faixa_etaria, color=faixa_etaria)) +
  # Definindo a forma do gráfico (nesse caso, gráfico de linhas)
  geom_line(size = 1)+
  # Definindo os textos ao redor do gráfico
  labs(
    title = "Mortes confirmadas por Covid-19 (Estado de SP)",
    subtitle = "Total por FAIXA ETÁRIA e por MÊS DO ÓBITO",
    caption = "Fonte: Sivep-Gripe (Ministério da Saúde)",
    y = "total de mortes",
    x = "mês/ano"
  )+
  # Definindo as cores das linhas e os detalhes da legenda
  scale_color_manual(
    values=c("#9398B8", "#8BACBC", "#3D748F", "#a80000", "#7E0F08")
  )+
  geom_point()+
  geom_label(
    aes(label = mortes),
    nudge_y = 0.25,
    size = 2)+
  # Definindo cores e formas dos elementos de fundo do gráfico
  theme(
    plot.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white", colour="grey"),
    panel.grid.major = element_line(colour = "grey", linetype = "dotted"),
    panel.grid.minor = element_line(colour = "grey", linetype = "dotted"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.caption = element_text(face = "italic"),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 8, face = "bold"),
    legend.background = element_rect(fill="#EEEEEE",
                                     size=0.5, linetype="solid")
  )



ggplot_obitos_covid_sp_idade_20
