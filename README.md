# BrazilCrime
[![NPM](https://img.shields.io/npm/l/react)](https://github.com/GiovanniVargette/BrazilCrime/blob/master/LICENSE) 

## Em andamento

- Adição da função que coleta os dados divulgados na plataforma gov.br - esses dados possuem meio e formato de divulgação diferente dos já coletados;
- Adição de dados sobre população, para permitir cálculos de taxas.

## Sobre o projeto

A principal função do pacote BrazilCrime é disponibilizar de maneira acessível os dados sobre criminalidade e violência do Brasil, através da linguagem R e do software RStudio.
Para isso coleta-se as informações divulgadas pelo Sinesp, órgao do Ministério da Justiça e Segurança Pública, esses dados são organizados em um data frame e disponibilizados ao usuário.

Nessa primeira versão, temos disponível dados a partir de janeiro de 2015 até dezembro de 2022, em estratificação por unidade federativa, para as seguintes tipologias criminais: Estupro, Furto de Veículo, Homicídio Doloso,
Lesão Corporal Seguida de Morte, Roubo a Insituição Financeira, Roubo de Carga, Roubo de Veículo, Roubo Seguido de Morte (Latrocínio) e Tentativa de Homicídio.


# Instalação do Pacote

```bash
install.packages("devtools")
devtools::install_github("GiovanniVargette/BrazilCrime")
library('BrazilCrime')
```
# Exemplo de Uso das Funções
### Função getData

```bash
dados <- getData #Realiza a coleta dos dados diretamente do Sinesp
```

### Função UFData

```bash
SP = UFData("São Paulo") #Retorna todos os dados para o Estado de São Paulo

Sudeste <- UFData(c("São Paulo","Minas Gerais","Rio de Janeiro","Espírito Santo")) #Retorna todos os dados para os Estados da Região Sudeste
```

### Função yearData

```bash
y2016 = yearData(2016) #Retorna todos os dados para o ano de 2016

y2016_18 <- yearData(2016:2018) #Retorna todos os dados para os anos de 2016, 2017 e 2018
```

### Função crimeType

```bash
estupro <- crimeType("Estupro") #Retorna os dados de estupros de todos os Estados, para todos os anos disponívies

estu_e_homic <- crimeType(c("Estupro","Homicídio doloso")) #Retorna os dados de estupros e homicídios dolosos para todos os Estados, em todos os anos disponíveis
```

### Função allFilters

```bash
acre_15_homc <- allFilters("Acre",2015,"Homicídio doloso") #Retorna os dados de homicídio doloso, no ano de 2015 no Estado do Acre

acre_sp_2016_18_estp_homic <- allFilters(c("Acre","São Paulo"),2016:2018,c("Estupro","Homicídio doloso")) #Retorna os dados de homicídio doloso e estupro, ocorridos durante 2016 e 2018, nos Estados do Acre e São Paulo
```
# Citação

Para citar em trabalhos utilize

```bash
citation("BrazilCrime)

#To cite package ‘BrazilCrime’ in publications use:

#  Vargette G, Justus M (2024). _BrazilCrime: Crime data from Brazil_. R
#  package version 0.0.2, <https://github.com/GiovanniVargette/BrazilCrime>.

#A BibTeX entry for LaTeX users is

#  @Manual{,
#    title = {BrazilCrime: Crime data from Brazil},
#    author = {Giovanni Vargette and Marcelo Justus},
#    year = {2024},
#    note = {R package version 0.0.2},
#    url = {https://github.com/GiovanniVargette/BrazilCrime},
#  }


```
