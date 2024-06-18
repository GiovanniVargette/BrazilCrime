## code to prepare `sinesp_vde` dataset goes here

library(openxlsx)
library(dplyr)
options(timeout = 300)

current_year <- as.numeric(format(Sys.Date(), "%Y"))
anos <- 2015:current_year

base_url <- "https://www.gov.br/mj/pt-br/assuntos/sua-seguranca/seguranca-publica/estatistica/download/dnsp-base-de-dados/bancovde-"

# Funcion to download the data
baixar_arquivo <- function(ano) {
  link <- paste0(base_url, ano, ".xlsx")
  destfile <- paste0("data-raw/raw-sinesp-vde-data/bancovde-", ano, ".xlsx")
  tryCatch({
    download.file(link, destfile, mode = "wb")
    return(destfile)
  }, error = function(e) {
    cat("Erro ao baixar o arquivo para o ano", ano, ": ", e$message, "\n")
    return(NULL)
  })
}

# Function to read and treat the data
processar_dados <- function(destfile, ano) {
  df <- openxlsx::read.xlsx(destfile) %>%
    dplyr::mutate(data = as.Date(data_referencia, origin = "1899-12-30"),
                  mes = format(as.Date(data_referencia, origin = "1899-12-30"), "%m"),
                  ano = ano)
  return(df)
}


lista_dfs <- lapply(anos, function(ano) {
  destfile <- baixar_arquivo(ano)
  if (!is.null(destfile)) {
    return(processar_dados(destfile, ano))
  } else {
    return(NULL)
  }
})

# Remover os data frames que não foram baixados com sucesso
lista_dfs <- Filter(Negate(is.null), lista_dfs)

# Combinar todos os data frames em um único
if (length(lista_dfs) > 0) {
  dados_completos <- bind_rows(lista_dfs)
  # Exibir os primeiros registros
  print(head(dados_completos))
} else {
  cat("Nenhum dado foi baixado com sucesso.")
}


sinesp_vde_data <- dados_completos |>
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


# Dados Populacionais

# População mensal
pop_mensal <- readxl::read_excel('data-raw/pop_projetada_mensal_dia_15.xlsx') |>
  dplyr::mutate(data = as.Date(data, origin = "1899-12-30"))

col_names <- names(pop_mensal)[2:29]

pop_mensal <- pop_mensal |>
  tidyr::pivot_longer(
    cols = dplyr::all_of(col_names),
    names_to = "uf",
    values_to = "populacao"
  ) |>
  dplyr::select(uf, data, populacao)


# População anual
pop_anual <- readxl::read_excel('data-raw/pop_projetada_anual.xlsx')


usethis::use_data(sinesp_vde_data, pop_mensal, pop_anual, compress = "xz",
                  internal = TRUE, overwrite = TRUE)
