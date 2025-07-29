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

  uf <- municipio <- ano <- mes <- categoria <- evento <- NULL
  df <- sinesp_vde_data

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

  # --- Aplicar filtros
  df <- filter_data(df, category, typology, city, state, year)

  # Criar coluna 'data' no formato YYYY-MM como Date
  if ("ano" %in% names(df) && "mes" %in% names(df)) {
    df$data <- lubridate::ym(paste(df$ano, stringr::str_pad(df$mes, 2, pad = "0")))
  }

  message("Query completed.")
  old <- options(timeout = 60)
  on.exit(options(old))
  return(df)
}
