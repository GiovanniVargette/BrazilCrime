#' allFilters Function
#'
#' The allFilters function allows simultaneous filtering through the UF,
#' year, and criminal typology.
#'
#'
#' @examples
#' acre_15_homc <- allFilters("Acre",2015,"Homicídio doloso")
#'
#' acre_sp_2016_18_estp_homic <- allFilters(c("Acre","São Paulo"),
#' 2016:2018,c("Estupro","Homicídio doloso"))
#'
#'
#'
#' @export
#'
#' @param Estado Enter the States you want.
#' @param year Enter the years you want.
#' @param typology Enter the criminal types you want.
#'


allFilters <- function (Estado,year,typology) {

  dados <- BrazilCrime::getData

  estado_ano <- filter(dados,UF %in% Estado & Ano %in% year)


  tipologia <- select(estado_ano,"UF","Ano","Mês",typology)

}
