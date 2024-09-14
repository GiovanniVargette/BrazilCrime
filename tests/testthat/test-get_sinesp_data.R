library(testthat)
library(BrazilCrime)

# test_that("get_sinesp_data downloads and processes data correctly", {
#
#   dados <- get_sinesp_data()
#   expect_true(is.data.frame(dados))
#
#   expected_columns <- c("uf", "uf_abrev", "tipo_crime", "ano", "mes", "ocorrencias")
#   expect_true(all(expected_columns %in% colnames(dados)))
#
#
#   dados_sp <- get_sinesp_data(state = "SP")
#   expect_true(all(dados_sp$uf_abrev == "SP"))
#
#   dados_roubo <- get_sinesp_data(typology = "Roubo de veículo")
#   expect_true(all(dados_roubo$tipo_crime == "Roubo de veículo"))
#
#   dados_2020 <- get_sinesp_data(year = 2020)
#   expect_true(all(dados_2020$ano == 2020))
#
#   dados_anual <- get_sinesp_data(granularity = "year")
#   expect_true("ano" %in% colnames(dados_anual))
#   expect_false("mes" %in% colnames(dados_anual))
#
#   dados_relativos <- get_sinesp_data(relative_values = TRUE)
#   expect_true("ocorrencias_100k_hab" %in% colnames(dados_relativos))
#
#   dados_geom <- get_sinesp_data(geom = TRUE)
#   expect_true("geometry" %in% colnames(dados_geom))
#
#   dados_pivot <- get_sinesp_data(pivot = TRUE)
#   expect_true("roubo_de_veiculo" %in% colnames(dados_pivot))
# })
