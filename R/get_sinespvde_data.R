#' get_sinesp-vde_data Function
#'
#'This function is responsible for collecting criminal data directly from the SINESP database.
#'The data collected by this function starts temporally in January 2015 and goes until December 2022,
#'covering crimes such as: Estupro (Rape), Furto de veículo (Vehicle Theft), Homicídio doloso (Intentional Homicide),
#'Lesão corporal seguida de morte (Bodily Injury Followed by Death), Roubo a instituição financeira (Robbery of Financial Institution),
#'Roubo de carga (Cargo Theft), Roubo de veículo (Vehicle Robbery), Roubo seguido de morte (Latrocínio) (Robbery Followed by Death)
#' and Tentativa de homicídio (Attempted Homicide).
#'
#'@examples
#' dados <- get_sinespvde_data()
#'
#'
#' @export

get_sinespvde_data <- function(state = 'all', city = "all", category = "all", typology = 'all',
                                year = 'all', granularity = 'month', pivot = F, geom = F) {

  options(scipen = 999, timeout = 1500)

  link19 <- "https://www.gov.br/mj/pt-br/assuntos/sua-seguranca/seguranca-publica/estatistica/download/dnsp-base-de-dados/banco-vde-2019.xlsx/"

  link20 <- "https://www.gov.br/mj/pt-br/assuntos/sua-seguranca/seguranca-publica/estatistica/download/dnsp-base-de-dados/banco-vde-2020.xlsx/"

  link21 <- "https://www.gov.br/mj/pt-br/assuntos/sua-seguranca/seguranca-publica/estatistica/download/dnsp-base-de-dados/banco-vde-2021.xlsx/"

  link22 <- "https://www.gov.br/mj/pt-br/assuntos/sua-seguranca/seguranca-publica/estatistica/download/dnsp-base-de-dados/banco-vde-2022.xlsx/"

  link23 <- "https://www.gov.br/mj/pt-br/assuntos/sua-seguranca/seguranca-publica/estatistica/download/dnsp-base-de-dados/banco-vde-2023.xlsx/"

  link24 <- "https://www.gov.br/mj/pt-br/assuntos/sua-seguranca/seguranca-publica/estatistica/download/dnsp-base-de-dados/banco-vde-2024.xlsx/"



  #ufs <- read.csv("BrazilCrime/data-raw/ufs.csv")

  #suppressWarnings({
    # download and initial data treatment
    tryCatch({

      df19 <- openxlsx::read.xlsx(link19) |>
        dplyr::mutate(mes = dplyr::case_when(
          data_referencia == 43466 ~ 1,
          data_referencia == 43497 ~ 2,
          data_referencia == 43525 ~ 3,
          data_referencia == 43556 ~ 4,
          data_referencia == 43586 ~ 5,
          data_referencia == 43617 ~ 6,
          data_referencia == 43647 ~ 7,
          data_referencia == 43678 ~ 8,
          data_referencia == 43709 ~ 9,
          data_referencia == 43739 ~ 10,
          data_referencia == 43770 ~ 11,
          data_referencia == 43800 ~ 12))|>
        dplyr::mutate(ano=2019)

      df20 <- openxlsx::read.xlsx(link20) |>
        dplyr::mutate(mes = dplyr::case_when(
          data_referencia == 43831 ~ 1,
          data_referencia == 43862 ~ 2,
          data_referencia == 43891 ~ 3,
          data_referencia == 43922 ~ 4,
          data_referencia == 43952 ~ 5,
          data_referencia == 43983 ~ 6,
          data_referencia == 44013 ~ 7,
          data_referencia == 44044 ~ 8,
          data_referencia == 44075 ~ 9,
          data_referencia == 44105 ~ 10,
          data_referencia == 44136 ~ 11,
          data_referencia == 44166 ~ 12))|>
        dplyr::mutate(ano=2020)

      df21 <- openxlsx::read.xlsx(link21) |>
        dplyr::mutate(mes = dplyr::case_when(
          data_referencia == 44197 ~ 1,
          data_referencia == 44228 ~ 2,
          data_referencia == 44256 ~ 3,
          data_referencia == 44287 ~ 4,
          data_referencia == 44317 ~ 5,
          data_referencia == 44348 ~ 6,
          data_referencia == 44378 ~ 7,
          data_referencia == 44409 ~ 8,
          data_referencia == 44440 ~ 9,
          data_referencia == 44470 ~ 10,
          data_referencia == 44501 ~ 11,
          data_referencia == 44531 ~ 12))|>
        dplyr::mutate(ano=2021)

      df22 <- openxlsx::read.xlsx(link22)|>
        dplyr::mutate(mes = dplyr::case_when(
          data_referencia == 44562 ~ 1,
          data_referencia == 44593 ~ 2,
          data_referencia == 44621 ~ 3,
          data_referencia == 44652 ~ 4,
          data_referencia == 44682 ~ 5,
          data_referencia == 44713 ~ 6,
          data_referencia == 44743 ~ 7,
          data_referencia == 44774 ~ 8,
          data_referencia == 44805 ~ 9,
          data_referencia == 44835 ~ 10,
          data_referencia == 44866 ~ 11,
          data_referencia == 44896 ~ 12))|>
        dplyr::mutate(ano=2022)

      df23 <- openxlsx::read.xlsx(link23)|>
        dplyr::mutate(mes = dplyr::case_when(
          data_referencia == 44927 ~ 1,
          data_referencia == 44958 ~ 2,
          data_referencia == 44986 ~ 3,
          data_referencia == 45017 ~ 4,
          data_referencia == 45047 ~ 5,
          data_referencia == 45078 ~ 6,
          data_referencia == 45108 ~ 7,
          data_referencia == 45139 ~ 8,
          data_referencia == 45170 ~ 9,
          data_referencia == 45200 ~ 10,
          data_referencia == 45231 ~ 11,
          data_referencia == 45261 ~ 12))|>
        dplyr::mutate(ano=2023)

      df24 <- openxlsx::read.xlsx(link24)|>
        dplyr::mutate(mes = dplyr::case_when(
          data_referencia == 45292 ~ 1,
          data_referencia == 45323 ~ 2))|>
        dplyr::mutate(ano=2024)

      df <- dplyr::bind_rows(df19,df20,df21,df22,df23,df24) |>
        dplyr::mutate(categoria = dplyr::case_when(
          evento == "Apreensão de Cocaína" ~ "drogas",
          evento == "Apreensão de Maconha" ~ "drogas",
          evento == "Tráfico de drogas" ~ "drogas",
          evento == "Arma de Fogo Apreendida" ~ "arma de fogo",
          evento == "Feminicídio" ~ "vitimas",
          evento == "Homicídio doloso" ~ "vitimas",
          evento == "Lesão corporal seguida de morte" ~ "vitimas",
          evento == "Morte no trânsito ou em decorrência dele (exceto homicídio doloso)" ~ "vitimas",
          evento == "Mortes a esclarecer (sem indício de crime)" ~ "vitimas",
          evento == "Roubo seguido de morte (latrocínio)" ~ "vitimas",
          evento == "Suicídio" ~ "vitimas",
          evento == "Tentativa de homicídio" ~ "vitimas",
          evento == "Estupro" ~ "vitimas",
          evento == "Morte por intervenção de Agente do Estado" ~ "vitimas",
          evento == "Furto de veículo" ~ "ocorrencias",
          evento == "Roubo a instituição financeira" ~ "ocorrencias",
          evento == "Roubo de carga" ~ "ocorrencias",
          evento == "Roubo de veículo" ~ "ocorrencias",
          evento == "Pessoa Desaparecida" ~ "desaparecidos/localizados",
          evento == "Pessoa Localizada" ~ "desaparecidos/localizados",
          evento == "Mandado de prisão cumprido" ~ "mandado de prisao cumprido",
          evento == "Morte de Agente do Estado" ~ "profissionais de seguranca",
          evento == "Suicídio de Agente do Estado" ~ "profissionais de seguranca",
          evento == "Atendimento pré-hospitalar" ~ "bombeiros",
          evento == "Busca e salvamento" ~ "bombeiros",
          evento == "Combate a incêndios" ~ "bombeiros",
          evento == "Emissão de Alvarás de licença" ~ "bombeiros",
          evento == "Realização de vistorias" ~ "bombeiros")) |>
        dplyr::select(uf,municipio,ano,mes,categoria,evento,agente,arma,faixa_etaria,feminino,
                      masculino,nao_informado,total,total_peso,total_vitimas) |>
        dplyr::arrange(uf,municipio,ano,mes,categoria,evento)

      message("Download completed.")

    }, error = function(e1) {
      message("Error downloading file. Try again later.")

    })
  #})

  # Function arguments ---------------------------------------------------------

  # argument state & city
  if(state == "all" & city == "all"){
    df <- df
  }

  if(state != "all"){
    ifelse(city == "all",
           df <- df |>
             dplyr::filter(uf %in% state),
           ifelse(city != "all",
                  df <- df |>
                    dplyr::filter(uf %in% state) |>
                    dplyr::filter(municipio %in% city),
                  stop("To use the cities filter, it's necessary to select a state.")))}

  #argument category & typology
  if(category == "all"){
    df <- df
  }

  if(category == "vitimas"){
    ifelse(typology=="all",
           df <- df |>
             dplyr::select(uf,municipio,ano,mes,categoria,evento,feminino,masculino,
                           nao_informado,total_vitimas) |>
             dplyr::filter(categoria %in% category),
           ifelse(typology == "Feminicídio" | typology == "Homicídio doloso" |
                  typology == "Lesão corporal seguida de morte" |
                  typology == "Morte no trânsito ou em decorrência dele (exceto homicídio doloso)" |
                  typology == "Mortes a esclarecer (sem indício de crime)" | typology == "Roubo seguido de morte (latrocínio)" |
                  typology == "Suicídio" | typology == "Tentativa de homicídio" | typology == "Estupro" |
                  typology == "Morte por intervenção de Agente do Estado",
              df <- df |>
                dplyr::select(uf,municipio,ano,mes,categoria,evento,feminino,masculino,
                  nao_informado,total_vitimas) |>
                  dplyr::filter(categoria %in% category) |>
                  dplyr::filter(evento %in% typology),
              stop("A tipologia introduzida não corresponde à categoria fornecida")))}

  if(category == "drogas"){
    ifelse(typology=="all",
           df <- df |>
             dplyr::select(uf,municipio,ano,mes,categoria,evento,total,total_peso) |>
             dplyr::filter(categoria %in% category),
           ifelse(typology == "Apreensão de Cocaína" | typology == "Apreensão de Maconha" |
                  typology == "Tráfico de drogas",
              df <- df |>
                dplyr::select(uf,municipio,ano,mes,categoria,evento,feminino,masculino,
                                  nao_informado,total_vitimas) |>
                  dplyr::filter(categoria %in% category) |>
                  dplyr::filter(evento %in% typology),
                stop("A tipologia introduzida não corresponde à categoria fornecida")))}

  if(category == "arma de fogo"){
    ifelse(typology=="all",
           df <- df |>
             dplyr::select(uf,municipio,ano,mes,categoria,evento,arma,total) |>
             dplyr::filter(categoia %in% category),
           ifelse(typology == "Arma de Fogo Apreendida",
                  df <- df |>
                    dplyr::select(uf,municipio,ano,mes,categoria,evento,arma,total) |>
                    dplyr::filter(categoria %in% category) |>
                    dplyr::filter(evento %in% typology),
                  stop("A tipologia introduzida não corresponde à categoria fornecida")))}

  if(category == "ocorrencias"){
    ifelse(typology=="all",
           df <- df |>
             dplyr::select(uf,municipio,ano,mes,categoria,evento,total) |>
             dplyr::filter(categoria %in% category),
           ifelse(typology == "Furto de veículo" | typology == "Roubo a instituição financeira" |
                  typology == "Roubo de carga" | typology == "Roubo de veículo",
                  df <- df |>
                    dplyr::select(uf,municipio,ano,mes,categoria,evento,total) |>
                    dplyr::filter(categoria %in% category) |>
                    dplyr::filter(evento %in% typology),
                  stop("A tipologia introduzida não corresponde à categoria fornecida")))}

  if(category == "desaparecidos/localizados"){
    ifelse(typology=="all",
           df <- df |>
             dplyr::select(uf,municipio,ano,mes,categoria,evento,faixa_etaria,feminino,
                    masculino,nao_informado,total_vitimas) |>
             dplyr::filter(categoria %in% category),
           ifelse(typology == "Pessoa Desaparecida" | typology == "Pessoa Localizada",
                  df <- df |>
                    dplyr::select(uf,municipio,ano,mes,categoria,evento,faixa_etaria,feminino,
                                  masculino,nao_informado,total_vitimas) |>
                    dplyr::filter(categoria %in% category) |>
                    dplyr::filter(evento %in% typology),
                  stop("A tipologia introduzida não corresponde à categoria fornecida")))}

  if(category == "mandado de prisao cumprido"){
    ifelse(typology=="all",
           df <- df |>
             dplyr::select(uf,municipio,ano,mes,categoria,evento,total) |>
             dplyr::filter(categoria %in% category),
           ifelse(typology == "Mandado de prisão cumprido",
                  df <- df |>
                    dplyr::select(uf,municipio,ano,mes,categoria,evento,total) |>
                    dplyr::filter(categoria %in% category) |>
                    dplyr::filter(evento %in% typology),
                  stop("A tipologia introduzida não corresponde à categoria fornecida")))}

  if(category == "profissionais de seguranca"){
    ifelse(typology=="all",
           df <- df |>
             dplyr::select(uf,municipio,ano,mes,categoria,evento,agente,feminino,
                    masculino,nao_informado,total_vitimas) |>
             dplyr::filter(categoria %in% category),
           ifelse(typology == "Morte de Agente do Estado" | typology == "Suicídio de Agente do Estado",
                  df <- df |>
                    dplyr::select(uf,municipio,ano,mes,categoria,evento,agente,feminino,
                           masculino,nao_informado,total_vitimas) |>
                    dplyr::filter(categoria %in% category) |>
                    dplyr::filter(evento %in% typology),
                  stop("A tipologia introduzida não corresponde à categoria fornecida")))}

  if(category == "bombeiros"){
    ifelse(typology=="all",
           df <- df |>
             dplyr::select(uf,municipio,ano,mes,categoria,evento,total) |>
             dplyr::filter(categoria %in% category),
           ifelse(typology == "Atendimento pré-hospitalar" | typology == "Busca e salvamento" |
                  typology == "Combate a incêndios" | typology == "Emissão de Alvarás de licença" |
                  typology == "Realização de vistorias",
                  df <- df |>
                    dplyr::select(uf,municipio,ano,mes,categoria,evento,total) |>
                    dplyr::filter(categoria %in% category) |>
                    dplyr::filter(evento %in% typology),
                  stop("A tipologia introduzida não corresponde à categoria fornecida")))}

    # argument year
    if (!"all" %in% year) {
      df <- df |>
        dplyr::filter(ano %in% year)
    }

    # argument granularity
    if (granularity != 'month' & granularity == 'year') {
      df <- df |>
        dplyr::group_by(uf, evento, ano) |>
        dplyr::summarise(total = sum(total, na.rm = T))
    }

    # argument relative_values
    #if (relative_values == T & granularity == 'month') {

     # pop <- readxl::read_excel('data-raw/pop_projetada_mensal_dia_15.xlsx') |>
      #  dplyr::mutate(data = as.Date(data, origin = "1899-12-30"))

      #col_names <- names(pop)[2:29]

      #pop_long <- pop |>
       # tidyr::pivot_longer(
        #  cols = all_of(col_names),
         # names_to = "uf",
          #values_to = "populacao"
        #) |>
        #dplyr::select(uf, data, populacao)

      #df <- df |>
       # dplyr::mutate(data = as.Date(paste(ano, mes, "15", sep = "-"),
        #                             format = "%Y-%m-%d")) |>
        #dplyr::left_join(pop_long, by = c('data', 'uf')) |>
        #dplyr::mutate(ocorrencias_100k_hab = round(ocorrencias / populacao * 100000, 2)) |>
        #dplyr::select(-data)
    #}

    #if (relative_values == T & granularity == 'year') {
      # dados da tabela 7358 do Sidra - projeção anual de 2018
     # pop <- readxl::read_excel('data-raw/pop_projetada_anual.xlsx') |>
      #  dplyr::select(-cod)

      #df <- df |>
       # dplyr::mutate(ano = as.character(ano)) |>
        #dplyr::left_join(pop, by = c('ano', 'uf')) |>
        #dplyr::mutate(ocorrencias_100k_hab = round(ocorrencias / populacao * 100000, 2))
    #}

    # argument geom
    if (geom == T) {
      ufs_geom <- geobr::read_state() |>
        dplyr::select(code_state, abbrev_state) |>
        dplyr::rename('uf_abrev' = abbrev_state)

       df <- ufs_geom |>
          dplyr::left_join(df,
                           by = 'uf_abrev') |>
          dplyr::rename(geometry = geom) |>
          na.omit()

    }

    # argument pivot
    if (pivot == T) {

       df <- df |>
         tidyr::pivot_wider(
           names_from = evento,
           values_from = total) |>
         janitor::clean_names()

       if (geom == T) {
         df <- df |>
         dplyr::select(-geometry, everything(), geometry)
       }
    }

  #if (pivot == T & relative_values == T) {

   # df <- df |>
   #   dplyr::select(-ocorrencias) |>
   #   tidyr::pivot_wider(
  #      names_from = tipo_crime,
   #     values_from = ocorrencias_100k_hab) |>
    #  janitor::clean_names()

    #if (geom == T) {
   #   df <- df |>
   #     dplyr::select(-geometry, everything(), geometry)
   # }


    message("Query completed.")

  old <- options(timeout = 60)
  on.exit(options(old))

  return(df)
}


