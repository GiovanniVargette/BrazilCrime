## ----setup, include=FALSE-----------------------------------------------------
library(BrazilCrime)
library(ggplot2)
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")


## ----exemplo1-----------------------------------------------------------------
dados <- get_sinesp_vde_data()
head(dados)


## ----exemplo2-----------------------------------------------------------------
recife <- get_sinesp_vde_data(
  state = "PE",
  city = "Recife",
  typology = "Homicídio doloso",
  year = 2020:2022
)

head(recife)


## ----exemplo3-----------------------------------------------------------------
sp_anuais <- get_sinesp_vde_data(
  state = "SP",
  category = "ocorrencias",
  granularity = "year"
)

head(sp_anuais)


## ----exemplo4-----------------------------------------------------------------
library(ggplot2)

ggplot(sp_anuais, aes(x = ano, y = total)) +
  geom_line() +
  facet_wrap(~evento) +
  labs(title = "Evolução anual de ocorrências no Estado de SP",
       y = "Total de ocorrências")


## ----exemplo5-----------------------------------------------------------------
library(ggplot2)

sp_anuais_vit <- get_sinesp_vde_data(
  state = "SP",
  city = "SÃO PAULO",
  category = "vitimas",
  granularity = "year"
)

ggplot(sp_anuais_vit, aes(x = ano, y = total_vitimas)) +
  geom_line() +
  facet_wrap(~evento) +
  labs(title = "Evolução anual de vítimas na cidade de SP",
       y = "Total de vítimas")

