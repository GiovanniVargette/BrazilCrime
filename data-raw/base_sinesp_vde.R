library(openxlsx)
library(dplyr)
library(readxl)
options(timeout = 300)

devtools::load_all()

# Descobrir a quantidade de linhas na tabela anterior
tabela_anterior <- get_sinesp_vde_data()
quantidade_linhas_anterior <- nrow(tabela_anterior)

current_year <- as.numeric(format(Sys.Date(), "%Y"))
anos <- 2015:current_year

base_url <- "https://www.gov.br/mj/pt-br/assuntos/sua-seguranca/seguranca-publica/estatistica/download/dnsp-base-de-dados/bancovde-"

# Function to download the data
baixar_arquivo <- function(ano) {
  link <- paste0(base_url, ano, ".xlsx/@@download/file")
  destfile <- paste0("data-raw/raw-sinesp-vde-data/bancovde-", ano, ".xlsx")

  #Cria diretorio se nao existir
  dir.create(dirname(destfile), recursive = TRUE, showWarnings = FALSE)

  tentativas <- 0
  max_tentativas <- 3
  sucesso <- FALSE

  repeat {
    tentativas <- tentativas + 1
    result <- tryCatch({
      download.file(link, destfile, mode = "wb")
      sucesso <- TRUE
      return(destfile)
    }, error = function(e) {
      cat("Erro ao baixar o arquivo para o ano", ano, ": ", e$message, "\n")
      return(NULL)
    })

    if (!is.null(result) || tentativas >= max_tentativas) {
      break
    }

    cat("Tentativa", tentativas, "falhou. Retentando em 5 segundos...\n")
    Sys.sleep(5)
  }

  if (!sucesso) {
    writeLines("fail", "data-raw/log_status.txt")
    stop("Não foi possível atualizar os dados devido à instabilidades do site do SINESP")
  }

  return(result)
}

# Function to read and treat the data
processar_dados <- function(destfile, ano) {
  df <- readxl::read_xlsx(destfile) %>%
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

# Diretório onde os arquivos estão salvos
dir_path <- "data-raw/raw-sinesp-vde-data"

# Listar todos os arquivos .xlsx no diretório
arquivos <- list.files(dir_path, pattern = "\\.xlsx$", full.names = TRUE)

# Função para ler e processar cada arquivo
ler_e_processar <- function(filepath) {
  ano <- as.numeric(gsub(".*bancovde-([0-9]{4})\\.xlsx$", "\\1", filepath)) # Extrai o ano do nome do arquivo

  df <- readxl::read_xlsx(filepath) %>%
    dplyr::mutate(data = as.Date(data_referencia, origin = "1899-12-30"),
                  mes = format(as.Date(data_referencia, origin = "1899-12-30"), "%m"),
                  ano = ano) # Adiciona a coluna do ano

  return(df)
}

# Aplicar a função a todos os arquivos e unir os resultados
dados_unificados <- do.call(bind_rows, lapply(arquivos, ler_e_processar))

sinesp_vde_data <- dados_unificados |>
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
                masculino,nao_informado,total,total_peso,total_vitima) |>
  dplyr::arrange(uf,municipio,ano,mes,categoria,evento)

quantidade_linhas_atual <- nrow(sinesp_vde_data)

# Dados Populacionais ----------------------------------------------------------

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

# Salvar os resultados ---------------------------------------------------------

if(quantidade_linhas_atual > quantidade_linhas_anterior) {
  usethis::use_data(sinesp_vde_data, pop_mensal, pop_anual, compress = "xz",
                    internal = TRUE, overwrite = TRUE)
  writeLines("Existem novos dados no SINESP VDE", "data-raw/log_status.txt")
} else {
  writeLines("Os dados permanecem os mesmos", "data-raw/log_status.txt")
}

# TODO: Faz sentido baixar os anos anteriores? Ou não é melhor baixar apenas o ano atual?



















teste <- "https://www.gov.br/mj/pt-br/assuntos/sua-seguranca/seguranca-publica/estatistica/download/dnsp-base-de-dados/bancovde-2015.xlsx/@@download/file"
arquivo <- "bancovde-2015.xlsx"

download.file(teste,arquivo,mode = "wb")

dados <- read_xlsx(arquivo)
