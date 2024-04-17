## code to prepare `DATASET` dataset goes here

populacao <- readxl::read_xlsx("C:/Users/giovannivargette/BrazilCrime/data-raw/pop_projetada_mensal_dia_15.xlsx")|>
  dplyr::filter(data >= 43480 )|>
  for(i in seq_along(data)) {}
    pop_data <- as.date



  dplyr::mutate(pop_data=dplyr::case_when(
    data == 43480 ~ 2015/01, data==43511 ~ 2015/02, data==43539 ~2015/03, data==43570

  ))

usethis::use_data(populacao, compress = "xz", overwrite = TRUE)
