#' get_sinesp_vde_data Function
#'
#' This function collects criminal data from the SINESP VDE database (2015–2024).
#' It supports filtering by state, city, year, crime category and typology.
#'
#' @param state State to be filtered. Character or vector. Default is "all".
#' @param city City to be filtered. Character or vector. Default is "all".
#' @param year Year(s) of the crime. Integer or vector. Default is "all".
#' @param category Crime category. Character or vector. Default is "all".
#' @param typology Crime typology (e.g., "Furto de veículo"). Character or vector. Default is "all".
#' @param granularity Level of temporal granularity: "year" or "month". Default is "month".
#'
#' @return A data frame with the filtered or summarized data.
#'
#' @examples
#' \donttest{
#' dados <- get_sinesp_vde_data(state = "SP", year = 2020:2022)
#' }
#' @export
get_sinesp_vde_data <- function(state = 'all', city = "all",
                                year = 'all', category = "all",
                                typology = 'all', granularity = 'month') {
  uf <- municipio <- ano <- categoria <- evento <- NULL
  df <- sinesp_vde_data

  # --- Corrigir colunas de vítimas com cuidado
  colunas_vitimas <- c("feminino", "masculino", "nao_informado")
  colunas_existentes <- colunas_vitimas[colunas_vitimas %in% names(df)]

  if (length(colunas_existentes) == 3) {
    df[colunas_existentes] <- lapply(df[colunas_existentes], function(x) {
      if (is.factor(x)) x <- as.character(x)
      suppressWarnings(as.numeric(x))
    })

    # Corrigir NA se tudo for NA — manter NA, não forçar 0
    df$total_vitimas <- apply(df[colunas_existentes], 1, function(row) {
      if (all(is.na(row))) NA else sum(row, na.rm = TRUE)
    })
  }

  # --- Função de filtro mais robusta
  filter_data <- function(df, category, typology, city, state, year) {
    if (!identical(state, "all")) {
      df <- df |>
        dplyr::filter(tolower(uf) %in% tolower(state))

      if (!identical(city, "all") && !identical(city, FALSE)) {
        df <- df |>
          dplyr::filter(tolower(municipio) %in% tolower(city))
      }
    } else {
      if (!identical(city, "all")) {
        stop("To select a city, it is necessary to select a state first.")
      }
    }

    if (!identical(year, "all")) {
      df <- df |>
        dplyr::filter(ano %in% year)
    }

    if (!identical(category, "all")) {
      df <- df |>
        dplyr::filter(tolower(categoria) %in% tolower(category))
    }

    if (!identical(typology, "all")) {
      df <- df |>
        dplyr::filter(tolower(evento) %in% tolower(typology))
    }

    df
  }

  # --- Função de agregação
  summarize_data <- function(df, group_vars, summarize_vars) {
    group_vars <- group_vars[group_vars %in% colnames(df)]
    summarize_vars <- summarize_vars[summarize_vars %in% colnames(df)]

    df |>
      dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) |>
      dplyr::summarize(dplyr::across(dplyr::all_of(summarize_vars), ~ sum(.x, na.rm = TRUE)),
                       .groups = 'drop')
  }

  # --- Aplicar filtros
  df <- filter_data(df, category, typology, city, state, year)

  # --- Agregação por ano, se solicitado
  if (granularity == 'year') {
    group_vars <- c("uf", "municipio", "ano", "categoria", "evento", "agente", "arma", "faixa_etaria")
    summarize_vars <- c("feminino", "masculino", "nao_informado", "total", "total_peso", "total_vitimas")
    df <- summarize_data(df, group_vars, summarize_vars)
  }

  message("Query completed.")
  old <- options(timeout = 60)
  on.exit(options(old))
  return(df)
}



