## code to prepare `DATASET` dataset goes here

link19 <- "https://www.gov.br/mj/pt-br/assuntos/sua-seguranca/seguranca-publica/estatistica/download/dnsp-base-de-dados/banco_vde_2019.xlsx/"

link20 <- "https://www.gov.br/mj/pt-br/assuntos/sua-seguranca/seguranca-publica/estatistica/download/dnsp-base-de-dados/banco_vde_2020.xlsx/"

link21 <- "https://www.gov.br/mj/pt-br/assuntos/sua-seguranca/seguranca-publica/estatistica/download/dnsp-base-de-dados/banco_vde_2021.xlsx/"

link22 <- "https://www.gov.br/mj/pt-br/assuntos/sua-seguranca/seguranca-publica/estatistica/download/dnsp-base-de-dados/banco_vde_2022.xlsx/"

link23 <- "https://www.gov.br/mj/pt-br/assuntos/sua-seguranca/seguranca-publica/estatistica/download/dnsp-base-de-dados/banco_vde_2023.xlsx/"

link24 <- "https://www.gov.br/mj/pt-br/assuntos/sua-seguranca/seguranca-publica/estatistica/download/dnsp-base-de-dados/banco_vde_2024.xlsx/"


df19 <- openxlsx::read.xlsx(link19) |>
  dplyr::mutate(mes = dplyr::case_when(
    data_referencia == 43466 ~ 1,
    data_referencia == 43497 ~ 2,
    data_referencia == 43525 ~ 3,
    data_referencia == 43556 ~ 4,
    data_referencia == 43586 ~ 5,
    data_referencia == 43617 ~ 6,
    data_referencia == 43647 ~ 7,
    data_referencia == 43678 ~ 8,
    data_referencia == 43709 ~ 9,
    data_referencia == 43739 ~ 10,
    data_referencia == 43770 ~ 11,
    data_referencia == 43800 ~ 12))|>
  dplyr::mutate(ano=2019)

df20 <- openxlsx::read.xlsx(link20) |>
  dplyr::mutate(mes = dplyr::case_when(
    data_referencia == 43831 ~ 1,
    data_referencia == 43862 ~ 2,
    data_referencia == 43891 ~ 3,
    data_referencia == 43922 ~ 4,
    data_referencia == 43952 ~ 5,
    data_referencia == 43983 ~ 6,
    data_referencia == 44013 ~ 7,
    data_referencia == 44044 ~ 8,
    data_referencia == 44075 ~ 9,
    data_referencia == 44105 ~ 10,
    data_referencia == 44136 ~ 11,
    data_referencia == 44166 ~ 12))|>
  dplyr::mutate(ano=2020)

df21 <- openxlsx::read.xlsx(link21) |>
  dplyr::mutate(mes = dplyr::case_when(
    data_referencia == 44197 ~ 1,
    data_referencia == 44228 ~ 2,
    data_referencia == 44256 ~ 3,
    data_referencia == 44287 ~ 4,
    data_referencia == 44317 ~ 5,
    data_referencia == 44348 ~ 6,
    data_referencia == 44378 ~ 7,
    data_referencia == 44409 ~ 8,
    data_referencia == 44440 ~ 9,
    data_referencia == 44470 ~ 10,
    data_referencia == 44501 ~ 11,
    data_referencia == 44531 ~ 12))|>
  dplyr::mutate(ano=2021)

df22 <- openxlsx::read.xlsx(link22)|>
  dplyr::mutate(mes = dplyr::case_when(
    data_referencia == 44562 ~ 1,
    data_referencia == 44593 ~ 2,
    data_referencia == 44621 ~ 3,
    data_referencia == 44652 ~ 4,
    data_referencia == 44682 ~ 5,
    data_referencia == 44713 ~ 6,
    data_referencia == 44743 ~ 7,
    data_referencia == 44774 ~ 8,
    data_referencia == 44805 ~ 9,
    data_referencia == 44835 ~ 10,
    data_referencia == 44866 ~ 11,
    data_referencia == 44896 ~ 12))|>
  dplyr::mutate(ano=2022)

df23 <- openxlsx::read.xlsx(link23)|>
  dplyr::mutate(mes = dplyr::case_when(
    data_referencia == 44927 ~ 1,
    data_referencia == 44958 ~ 2,
    data_referencia == 44986 ~ 3,
    data_referencia == 45017 ~ 4,
    data_referencia == 45047 ~ 5,
    data_referencia == 45078 ~ 6,
    data_referencia == 45108 ~ 7,
    data_referencia == 45139 ~ 8,
    data_referencia == 45170 ~ 9,
    data_referencia == 45200 ~ 10,
    data_referencia == 45231 ~ 11,
    data_referencia == 45261 ~ 12))|>
  dplyr::mutate(ano=2023)

df24 <- openxlsx::read.xlsx(link24)|>
  dplyr::mutate(mes = dplyr::case_when(
    data_referencia == 45292 ~ 1,
    data_referencia == 45323 ~ 2))|>
  dplyr::mutate(ano=2024)

df <- dplyr::bind_rows(df19,df20,df21,df22,df23,df24) |>
  dplyr::mutate(categoria = dplyr::case_when(
    evento == "Apreensão de Cocaína" ~ "drogas",
    evento == "Apreensão de Maconha" ~ "drogas",
    evento == "Tráfico de drogas" ~ "drogas",
    evento == "Arma de Fogo Apreendida" ~ "arma de fogo",
    evento == "Feminicídio" ~ "vitimas",
    evento == "Homicídio doloso" ~ "vitimas",
    evento == "Lesão corporal seguida de morte" ~ "vitimas",
    evento == "Morte no trânsito ou em decorrência dele (exceto homicídio doloso)" ~ "vitimas",
    evento == "Mortes a esclarecer (sem indício de crime)" ~ "vitimas",
    evento == "Roubo seguido de morte (latrocínio)" ~ "vitimas",
    evento == "Suicídio" ~ "vitimas",
    evento == "Tentativa de homicídio" ~ "vitimas",
    evento == "Estupro" ~ "vitimas",
    evento == "Morte por intervenção de Agente do Estado" ~ "vitimas",
    evento == "Furto de veículo" ~ "ocorrencias",
    evento == "Roubo a instituição financeira" ~ "ocorrencias",
    evento == "Roubo de carga" ~ "ocorrencias",
    evento == "Roubo de veículo" ~ "ocorrencias",
    evento == "Pessoa Desaparecida" ~ "desaparecidos/localizados",
    evento == "Pessoa Localizada" ~ "desaparecidos/localizados",
    evento == "Mandado de prisão cumprido" ~ "mandado de prisao cumprido",
    evento == "Morte de Agente do Estado" ~ "profissionais de seguranca",
    evento == "Suicídio de Agente do Estado" ~ "profissionais de seguranca",
    evento == "Atendimento pré-hospitalar" ~ "bombeiros",
    evento == "Busca e salvamento" ~ "bombeiros",
    evento == "Combate a incêndios" ~ "bombeiros",
    evento == "Emissão de Alvarás de licença" ~ "bombeiros",
    evento == "Realização de vistorias" ~ "bombeiros")) |>
  dplyr::select(uf,municipio,ano,mes,categoria,evento,agente,arma,faixa_etaria,feminino,
                masculino,nao_informado,total,total_peso,total_vitimas) |>
  dplyr::arrange(uf,municipio,ano,mes,categoria,evento)

usethis::use_data(df,compress = "xz")
