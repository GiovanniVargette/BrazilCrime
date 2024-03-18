#' get_sinesp_data Function
#'
#'This function is responsible for collecting criminal data directly from the SINESP database.
#'The data collected by this function starts temporally in January 2015 and goes until December 2022,
#'covering crimes such as: Estupro (Rape), Furto de veículo (Vehicle Theft), Homicídio doloso (Intentional Homicide),
#'Lesão corporal seguida de morte (Bodily Injury Followed by Death), Roubo a instituição financeira (Robbery of Financial Institution),
#'Roubo de carga (Cargo Theft), Roubo de veículo (Vehicle Robbery), Roubo seguido de morte (Latrocínio) (Robbery Followed by Death)
#' and Tentativa de homicídio (Attempted Homicide).
#'
#'@examples
#' dados <- get_sinesp_data()
#'
#'
#' @export

get_sinesp_data <- function(state = 'all', typology = 'all', year = 'all',
                            granularity = 'month', pivot = F, geom = F) {

  options(scipen = 999, timeout = 1500)

  link <- "http://dados.mj.gov.br/dataset/210b9ae2-21fc-4986-89c6-2006eb4db247/resource/feeae05e-faba-406c-8a4a-512aec91a9d1/download/indicadoressegurancapublicauf.xlsx"

  ufs <- read.csv('data-raw/ufs.csv')

  suppressWarnings({
    # download and initial data treatment
    tryCatch({

      df <- openxlsx::read.xlsx(link) |>
        janitor::clean_names() |>
        dplyr::arrange(uf, tipo_crime, ano) |>
        dplyr::left_join(ufs,
                         by = 'uf') |>
        dplyr::select(uf, uf_abrev, tipo_crime, ano, mes, ocorrencias)

      message("Download completed.")

    }, error = function(e1) {
      message("Error downloading file. Try again later.")

    })
  })

  # Function arguments ---------------------------------------------------------

    # argument state
    if (!"all" %in% state) {
      df <- df |>
        dplyr::filter(uf_abrev %in% state)
    }

    # argument typology
    if (!"all" %in% typology) {
      df <- df |>
        dplyr::filter(tipo_crime %in% typology)
    }

    # argument year
    if (!"all" %in% year) {
      df <- df |>
        dplyr::filter(ano %in% year)
    }

    # argument granularity
    if (granularity != 'month' & granularity == 'year') {
      df <- df |>
        dplyr::group_by(uf, uf_abrev, tipo_crime, ano) |>
        dplyr::summarise(ocorrencias = sum(ocorrencias, na.rm = T))
    }

    # argument geom
    if (geom == T) {
      ufs_geom <- geobr::read_state() |>
        dplyr::select(code_state, abbrev_state) |>
        dplyr::rename('uf_abrev' = abbrev_state)

       df <- ufs_geom |>
          dplyr::left_join(df,
                           by = 'uf_abrev') |>
          dplyr::rename(geometry = geom) |>
          na.omit()

    }

    # argument pivot
    if (pivot == T) {

       df <- df |>
         tidyr::pivot_wider(
           names_from = tipo_crime,
           values_from = ocorrencias) |>
         dplyr::select(-geometry, everything(), geometry) |>
         janitor::clean_names()
    }

    message("Query completed.")

  old <- options(timeout = 60)
  on.exit(options(old))

  return(df)

}
