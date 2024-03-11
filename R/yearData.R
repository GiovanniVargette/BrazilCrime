#' yearData Function
#'
#'The yearData function filters the data based on the years provided.
#'
#' @examples
#' y2016 = yearData(2016)
#'
#' y2016_18 <- yearData(2016:2018)
#'
#'
#' @export
#'
#'
#' @param year Give years to be selected.
#'


yearData <- function (year) {
  openxlsx::read.xlsx("http://dados.mj.gov.br/dataset/210b9ae2-21fc-4986-89c6-2006eb4db247/resource/feeae05e-faba-406c-8a4a-512aec91a9d1/download/indicadoressegurancapublicauf.xlsx") |>
    tidyr::pivot_wider(
      names_from = Tipo.Crime, #As colunas
      values_from = Ocorrências)|> #Os valores
    dplyr::filter(Ano %in% year)
}


