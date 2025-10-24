library(openxlsx)
library(dplyr)
library(readxl)
library(usethis)

options(timeout = 500)
devtools::load_all()

# Dados anteriores
tabela_anterior <- get_sinesp_vde_data()
quantidade_linhas_anterior <- nrow(tabela_anterior)

current_year <- as.numeric(format(Sys.Date(), "%Y"))
anos <- 2015:current_year

base_url <- "https://www.gov.br/mj/pt-br/assuntos/sua-seguranca/seguranca-publica/estatistica/download/dnsp-base-de-dados/"

# Função que monta o link correto
montar_link <- function(ano) {
  if (ano == 2017) {
    return(paste0(base_url, "bancovde-2017.xlsx/@@download/file"))
  } else {
    return(paste0(base_url, "bancovde-", ano, ".xlsx/@@download/file"))
  }
}

# Função que baixa com retentativas
baixar_arquivo <- function(ano) {
  link <- montar_link(ano)
  destfile <- paste0("data-raw/raw-sinesp-vde-data/bancovde-", ano, ".xlsx")
  dir.create(dirname(destfile), recursive = TRUE, showWarnings = FALSE)

  tentativas <- 0
  max_tentativas <- 3

  repeat {
    tentativas <- tentativas + 1
    cat("Baixando ano", ano, "- tentativa", tentativas, "\n")
    resultado <- tryCatch({
      download.file(link, destfile, mode = "wb", quiet = TRUE)
      if (file.exists(destfile) && file.info(destfile)$size > 10000) {
        return(destfile)
      } else {
        stop("Arquivo baixado vazio ou corrompido.")
      }
    }, error = function(e) {
      message("Erro ao baixar ", ano, ": ", e$message)
      return(NULL)
    })

    if (!is.null(resultado)) {
      return(destfile)
    }

    if (tentativas >= max_tentativas) {
      writeLines(paste("Falha ao baixar o ano", ano), "data-raw/log_status.txt")
      stop("Não foi possível baixar os dados do ano ", ano)
    }

    Sys.sleep(5)
  }
}

# Processa arquivo
processar_dados <- function(destfile, ano) {
  df <- read_xlsx(destfile, guess_max = 10000) %>%
    mutate(
      data = as.Date(data_referencia, origin = "1899-12-30"),
      mes = format(data, "%m"),
      ano = ano
    )
  return(df)
}

# Loop de anos
lista_dfs <- lapply(anos, function(ano) {
  destfile <- baixar_arquivo(ano)
  if (!is.null(destfile)) processar_dados(destfile, ano) else NULL
})

# Unir e classificar
dados_unificados <- bind_rows(lista_dfs)

sinesp_vde_data <- dados_unificados %>%
  mutate(
    categoria = case_when(
      evento %in% c("Apreensão de Cocaína", "Apreensão de Maconha", "Tráfico de drogas") ~ "drogas",
      evento %in% c("Arma de Fogo Apreendida") ~ "arma de fogo",
      evento %in% c("Feminicídio", "Homicídio doloso", "Lesão corporal seguida de morte",
                    "Morte no trânsito ou em decorrência dele (exceto homicídio doloso)",
                    "Mortes a esclarecer (sem indício de crime)", "Roubo seguido de morte (latrocínio)",
                    "Suicídio", "Tentativa de homicídio", "Estupro",
                    "Morte por intervenção de Agente do Estado","Tentativa de feminicídio") ~ "vitimas",
      evento %in% c("Furto de veículo", "Roubo a instituição financeira", "Roubo de carga",
                    "Roubo de veículo") ~ "ocorrencias",
      evento %in% c("Pessoa Desaparecida", "Pessoa Localizada") ~ "desaparecidos/localizados",
      evento == "Mandado de prisão cumprido" ~ "mandado de prisao cumprido",
      evento %in% c("Morte de Agente do Estado", "Suicídio de Agente do Estado") ~ "profissionais de seguranca",
      evento %in% c("Atendimento pré-hospitalar", "Busca e salvamento",
                    "Combate a incêndios", "Emissão de Alvarás de licença", "Realização de vistorias") ~ "bombeiros",
      TRUE ~ NA_character_
    )
  ) %>%
  select(uf, municipio, ano, mes, categoria, evento, agente, arma, faixa_etaria,
         feminino, masculino, nao_informado, total, total_peso, total_vitima) %>%
  arrange(uf, municipio, ano, mes, categoria, evento)

# Salva apenas se há novos dados
quantidade_linhas_atual <- nrow(sinesp_vde_data)

if (quantidade_linhas_atual > quantidade_linhas_anterior) {
  usethis::use_data(sinesp_vde_data,
                    compress = "xz", internal = TRUE, overwrite = TRUE)
  writeLines("Existem novos dados no SINESP VDE", "data-raw/log_status.txt")
} else {
  writeLines("Os dados permanecem os mesmos", "data-raw/log_status.txt")
}

# Limpa ambiente
rm(dados_unificados, tabela_anterior, anos, quantidade_linhas_anterior,
   quantidade_linhas_atual, current_year, base_url)
