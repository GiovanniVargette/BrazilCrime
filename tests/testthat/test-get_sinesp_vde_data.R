library(testthat)
library(BrazilCrime)

test_that("get_sinesp_vde_data returns a data frame", {
  result <- get_sinesp_vde_data()
  expect_s3_class(result, "data.frame")
  print('Test data type')
})

test_that("get_sinesp_vde_data filters correctly by state", {
  result <- get_sinesp_vde_data(state = "SP")
  unique_states <- unique(result$uf)
  expect_true(all(unique_states == "SP"))
  print('Test state argument')
})

test_that("get_sinesp_vde_data filters correctly by city", {
  result <- get_sinesp_vde_data(state = "SP", city = "São Paulo")
  unique_cities <- tolower(unique(result$municipio))
  expect_true(all(unique_cities == "são paulo"))
  print('Test city argument')
})

test_that("get_sinesp_vde_data stops if city is selected without state", {
  expect_error(get_sinesp_vde_data(city = "São Paulo"), "To select a city, it is necessary to select a state first.")
  print('Test city and state arguments')
  })

test_that("get_sinesp_vde_data filters correctly by category", {
  result <- get_sinesp_vde_data(category = "vitimas")
  unique_categories <- unique(result$categoria)
  expect_true(all(unique_categories == "vitimas"))
  print('Test category argument')
})

test_that("get_sinesp_vde_data filters correctly by typology", {
  result <- get_sinesp_vde_data(category = "vitimas", typology = "Estupro")
  unique_typologies <- unique(result$evento)
  expect_true(all(unique_typologies == "Estupro"))
  print('Test typology argument')
})

test_that("get_sinesp_vde_data summarizes correctly by year", {
  result <- get_sinesp_vde_data(granularity = 'year')
  expect_true("ano" %in% colnames(result))
  print('Test granularity argument')
})
