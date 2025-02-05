#' get_sinesp_vde_data Function
#'
#' This function is responsible for collecting criminal data directly from the SINESP VDE database.
#' The data collected by this function starts temporally in January 2015 and goes until December 2022 for each brazilian city,
#' covering crimes such as: Estupro (Rape), Furto de veículo (Vehicle Theft), Homicídio doloso (Intentional Homicide),
#' Lesão corporal seguida de morte (Bodily Injury Followed by Death), Roubo a instituição financeira (Robbery of Financial Institution),
#' Roubo de carga (Cargo Theft), Roubo de veículo (Vehicle Robbery), Roubo seguido de morte (Latrocínio) (Robbery Followed by Death)
#' and Tentativa de homicídio (Attempted Homicide).
#'
#' @param state State to be filtered. Character.
#' @param city City to be filtered. Character.
#' @param year Year of the crime. Integer.
#' @param category Crime category. Character.
#' @param typology Crime typology. For example: Furto de veículo, Roubo de carga. Character.
#' @param granularity Level of temporal granularity. Can be "year" or "month". Character.
#'
#' @return A data frame with the filtered data.
#'
#'
#' @examples
#' \donttest{
#' dados <- get_sinesp_vde_data()
#' }
#'
#' @export
get_sinesp_vde_data <- function(state = 'all', city = "all",
                                 year = 'all', category = "all",
                                 typology = 'all', granularity = 'month') {

  uf <- municipio <- ano <- categoria <- evento <- NULL

  df <- sinesp_vde_data


  filter_data <- function(df, category, typology, city, state, year) {
    if (state != "all") {
      df <- df |>
        dplyr::filter(tolower(uf) %in% tolower(state))

      if (city != "all" & city != FALSE) {
        df <- df |>
          dplyr::filter(tolower(municipio) %in% tolower(city))
      }
    } else {
      if (city != "all") {
        stop("To select a city, it is necessary to select a state first.")
      }
    }

    if (year != "all") {
      df <- df |>
        dplyr::filter(ano %in% year)
    }

    if (category != "all") {
      df <- df |>
        dplyr::filter(tolower(categoria) %in% tolower(category))
    }

    if (typology != "all") {
      df <- df |>
        dplyr::filter(tolower(evento) %in% tolower(typology))
    }

    df
  }

  summarize_data <- function(df, group_vars, summarize_vars) {
    group_vars <- group_vars[group_vars %in% colnames(df)]
    summarize_vars <- summarize_vars[summarize_vars %in% colnames(df)]

    df <- df |>
      dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) |>
      dplyr::summarize(dplyr::across(dplyr::all_of(summarize_vars), ~ sum(.x,
                                                                          na.rm = TRUE)),
                       .groups = 'drop')

    df
  }

  df <- filter_data(df, category, typology, city, state, year)

  if (granularity == 'year') {
    group_vars <- c("uf", "municipio", "ano", "categoria", "evento", "agente", "arma", "faixa_etaria")
    summarize_vars <- c("feminino", "masculino", "nao_informado", "total", "total_peso", "total_vitima")
    df <- summarize_data(df, group_vars, summarize_vars)
  }

  message("Query completed.")
  old <- options(timeout = 60)
  on.exit(options(old))
  return(df)
}
