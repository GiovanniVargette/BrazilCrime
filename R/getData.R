#' @export

getData <- {
  openxlsx::read.xlsx("http://dados.mj.gov.br/dataset/210b9ae2-21fc-4986-89c6-2006eb4db247/resource/feeae05e-faba-406c-8a4a-512aec91a9d1/download/indicadoressegurancapublicauf.xlsx") |>
    tidyr::pivot_wider(
      names_from = Tipo.Crime, #As colunas
      values_from = Ocorrências #Os valores
    )
}



