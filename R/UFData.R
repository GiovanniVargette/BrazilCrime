#' UFData Function
#'
#'The UFData function filters the data based on the name of the Brazilian
#'Federative Unit provided.
#'
#' @example SP = UFData("São Paulo")
#'
#'
#'
#' @export
#'
#'
#' @param c(Estado) Use the UF's name to select the one you want.
#'


UFData <- function (Estado) {
    openxlsx::read.xlsx("http://dados.mj.gov.br/dataset/210b9ae2-21fc-4986-89c6-2006eb4db247/resource/feeae05e-faba-406c-8a4a-512aec91a9d1/download/indicadoressegurancapublicauf.xlsx") |>
    tidyr::pivot_wider(
      names_from = Tipo.Crime, #As colunas
      values_from = Ocorrências)|> #Os valores
    dplyr::filter(UF %in% Estado)

}
