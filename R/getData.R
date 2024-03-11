#' getData Function
#'
#'This function is responsible for collecting criminal data directly from the SINESP database.
#'The data collected by this function starts temporally in January 2015 and goes until December 2022,
#'covering crimes such as: Estupro (Rape), Furto de Veículos (Vehicle Theft), Homicídio Doloso (Intentional Homicide),
#'Lesão Corporal Seguida de Morte (Bodily Injury Followed by Death), Roubo a Insituição Financeira (Robbery of Financial Institution),
#'Roubo de Carga (Cargo Theft), Roubo de Veículos (Vehicle Robbery), Roubo Seguido de Morte - Latrocínio (Robbery Followed by Death)
#' and Tentativa de Homicídio (Attempted Homicide).
#'
#'@examples
#' dados <- getData
#'
#'
#' @export

getData <- {
  openxlsx::read.xlsx("http://dados.mj.gov.br/dataset/210b9ae2-21fc-4986-89c6-2006eb4db247/resource/feeae05e-faba-406c-8a4a-512aec91a9d1/download/indicadoressegurancapublicauf.xlsx") |>
    tidyr::pivot_wider(
      names_from = Tipo.Crime, #As colunas
      values_from = Ocorrências #Os valores
    )
}



