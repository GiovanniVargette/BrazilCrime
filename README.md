# BrazilCrime
[![NPM](https://img.shields.io/npm/l/react)](https://github.com/GiovanniVargette/BrazilCrime/blob/master/LICENSE) 

## Em andamento

- Adição de funções de filtragem por geolocalização, temporalidade e tipologia criminal;
- Adição da função que coleta os dados a partir de 2023 - esses dados possuem meio de divulgação diferente dos já coletados;
- Documentação das funções existentes. 

## Sobre o projeto

A principal função do pacote BrazilCrime é disponibilizar de maneira acessível os dados sobre criminalidade e violência do Brasil, através da linguagem R e do software RStudio.
Para isso coleta-se as informações divulgadas pelo Sinesp, órgao do Ministério da Justiça e Segurança Pública, esses dados são organizados em um data frame e disponibilizados ao usuário.

Nessa primeira versão, temos disponível dados a partir de janeiro de 2015 até dezembro de 2022, em estratificação por unidade federativa, para as seguintes tipologias criminais: Estupro, Furto de Veículo, Homicídio Doloso,
Lesão Corporal Seguida de Morte, Roubo a Insituição Financeira, Roubo de Carga, Roubo de Veículo, Roubo Seguido de Morte (Latrocínio) e Tentativa de Homicídio.


# Instalação do Pacote

```bash
install.packages("devtools")
library(devtools)
devtools::install_github("GiovanniVargette/BrazilCrime")
library('BrazilCrime')
```
