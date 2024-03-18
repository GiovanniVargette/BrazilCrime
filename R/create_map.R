#' create_map Function
#'
#'
#' Creates a map ...
#'
#'
#'@examples
#' create_map()
#'
#'
#' @export

create_map <- function(dataframe) {

  mapa <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = dataframe)

  mapa
}


# função create map com o ggplot (em formato para publicação)
# se forem vários anos, imprimir vários mapas

#
# a <- ggplot() +
#   geom_sf(data = rj) +
#   geom_sf(data = df1)+  # Whether to order the factor result or not
#   geom_sf(data = brt_corredor) +
#   coord_sf(xlim = c(-43.55, -43.30), ylim = c(-22.83, -23.02)) +
#   annotation_scale(location = 'br')+
#   annotation_north_arrow(location = 'tl',
#                          style = north_arrow_fancy_orienteering()) +
#   theme_classic()
#
# a
# # mapa 45 min
# b <- ggplot() +
#   geom_sf(data = rj) +
#   geom_sf(data = df2)+  # Whether to order the factor result or not
#   geom_sf(data = brt_corredor) +
#   coord_sf(xlim = c(-43.55, -43.30), ylim = c(-22.83, -23.02)) +
#   annotation_scale(location = 'br')+
#   annotation_north_arrow(location = 'tl',
#                          style = north_arrow_fancy_orienteering()) +
#   theme_classic()
#
# b
#
# (a|b)
#
# ggsave('output/01_entorno_mapas/transolimpica_15_45.png', scale = 1.2, width = 9, height = 6, dpi = 600)

