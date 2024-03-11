#' crimeType Function
#'
#' The crimeType function filters the data based on the criminal typology provided.
#'
#' @examples
#' estupro <- crimeType("Estupro")
#'
#' estu_e_homic <- crimeType(c("Estupro","Homicídio doloso"))
#'
#'
#'
#' @export
#'
#'
#' @param typology Enter the criminal types you want.
#'


crimeType <- function (typology) {

  dados <- BrazilCrime::getData

  tipologia <- select(dados,"UF","Ano","Mês",typology)

}
