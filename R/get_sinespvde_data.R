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

  load("data/df.rda")

  # Function arguments ---------------------------------------------------------

  # argument state & city
  if(state == "all" & city == "all"){
    df <- df
  }

  if(state != "all"){
    ifelse({{city}} == "all",
           df <- df |>
             dplyr::filter({uf} %in% {{state}}),
           ifelse(city != "all",
                  df <- df |>
                    dplyr::filter({uf} %in% {{state}}) |>
                    dplyr::filter({municipio} %in% {{city}}),
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
                  dplyr::filter({categoria} %in% {{category}}) |>
                  dplyr::filter({evento} %in% {{typology}}),
              stop("A tipologia introduzida não corresponde à categoria fornecida")))
    }

  if(category == "drogas"){
    ifelse(typology=="all",
           df <- df |>
             dplyr::select(uf,municipio,ano,mes,categoria,evento,total,total_peso) |>
             dplyr::filter({categoria} %in% {{category}}),
           ifelse(typology == "Apreensão de Cocaína" | typology == "Apreensão de Maconha" |
                  typology == "Tráfico de drogas",
              df <- df |>
                dplyr::select(uf,municipio,ano,mes,categoria,evento,feminino,masculino,
                                  nao_informado,total_vitimas) |>
                  dplyr::filter({categoria} %in% {{category}}) |>
                  dplyr::filter({evento} %in% {{typology}}),
                stop("A tipologia introduzida não corresponde à categoria fornecida")))
    }

  if(category == "arma de fogo"){
    ifelse(typology=="all",
           df <- df |>
             dplyr::select(uf,municipio,ano,mes,categoria,evento,arma,total) |>
             dplyr::filter({categoria} %in% {{category}}),
           ifelse(typology == "Arma de Fogo Apreendida",
                  df <- df |>
                    dplyr::select(uf,municipio,ano,mes,categoria,evento,arma,total) |>
                    dplyr::filter({categoria} %in% {{category}}) |>
                    dplyr::filter({evento} %in% {{typology}}),
                  stop("A tipologia introduzida não corresponde à categoria fornecida")))
    }

  if(category == "ocorrencias"){
    ifelse(typology=="all",
           df <- df |>
             dplyr::select(uf,municipio,ano,mes,categoria,evento,total) |>
             dplyr::filter({categoria} %in% {{category}}),
           ifelse(typology == "Furto de veículo" | typology == "Roubo a instituição financeira" |
                  typology == "Roubo de carga" | typology == "Roubo de veículo",
                  df <- df |>
                    dplyr::select(uf,municipio,ano,mes,categoria,evento,total) |>
                    dplyr::filter({categoria} %in% {{category}}) |>
                    dplyr::filter({evento} %in% {{typology}}),
                  stop("A tipologia introduzida não corresponde à categoria fornecida")))
    }

  if(category == "desaparecidos/localizados"){
    ifelse(typology=="all",
           df <- df |>
             dplyr::select(uf,municipio,ano,mes,categoria,evento,faixa_etaria,feminino,
                    masculino,nao_informado,total_vitimas) |>
             dplyr::filter({categoria} %in% {{category}}),
           ifelse(typology == "Pessoa Desaparecida" | typology == "Pessoa Localizada",
                  df <- df |>
                    dplyr::select(uf,municipio,ano,mes,categoria,evento,faixa_etaria,feminino,
                                  masculino,nao_informado,total_vitimas) |>
                    dplyr::filter({categoria} %in% {{category}}) |>
                    dplyr::filter({evento} %in% {{typology}}),
                  stop("A tipologia introduzida não corresponde à categoria fornecida")))
    }

  if(category == "mandado de prisao cumprido"){
    ifelse(typology=="all",
           df <- df |>
             dplyr::select(uf,municipio,ano,mes,categoria,evento,total) |>
             dplyr::filter({categoria} %in% {{category}}),
           ifelse(typology == "Mandado de prisão cumprido",
                  df <- df |>
                    dplyr::select(uf,municipio,ano,mes,categoria,evento,total) |>
                    dplyr::filter({categoria} %in% {{category}}) |>
                    dplyr::filter({evento} %in% {{typology}}),
                  stop("A tipologia introduzida não corresponde à categoria fornecida")))
    }

  if(category == "profissionais de seguranca"){
    ifelse(typology=="all",
           df <- df |>
             dplyr::select(uf,municipio,ano,mes,categoria,evento,agente,feminino,
                    masculino,nao_informado,total_vitimas) |>
             dplyr::filter({categoria} %in% {{category}}),
           ifelse(typology == "Morte de Agente do Estado" | typology == "Suicídio de Agente do Estado",
                  df <- df |>
                    dplyr::select(uf,municipio,ano,mes,categoria,evento,agente,feminino,
                           masculino,nao_informado,total_vitimas) |>
                    dplyr::filter({categoria} %in% {{category}}) |>
                    dplyr::filter({evento} %in% {{typology}}),
                  stop("A tipologia introduzida não corresponde à categoria fornecida")))
    }

  if(category == "bombeiros"){
    ifelse(typology=="all",
           df <- df |>
             dplyr::select(uf,municipio,ano,mes,categoria,evento,total) |>
             dplyr::filter({categoria} %in% {{category}}),
           ifelse(typology == "Atendimento pré-hospitalar" | typology == "Busca e salvamento" |
                  typology == "Combate a incêndios" | typology == "Emissão de Alvarás de licença" |
                  typology == "Realização de vistorias",
                  df <- df |>
                    dplyr::select(uf,municipio,ano,mes,categoria,evento,total) |>
                    dplyr::filter({categoria} %in% {{category}}) |>
                    dplyr::filter({evento} %in% {{typology}}),
                  stop("A tipologia introduzida não corresponde à categoria fornecida")))
    }

    # argument year
    if (!"all" %in% year) {
      df <- df |>
        dplyr::filter({ano} %in% {{year}})
    }

    # argument granularity
    if (granularity == 'year' & category == "all") {
      stop("Para utilizar a granularidade anual, selecione previamente uma categoria")
    }

    if (granularity=="year" & category == "vitimas"){
      ifelse(typology=="all",
             df <- df |>
               dplyr::group_by(uf,evento,ano)|>
               dplyr::summarize(total_fem = (sum(feminino, na.rm = T)),
                                total_masc = (sum(masculino, na.rm = T)),
                                total_n_inf = (sum(nao_informado, na.rm = T)),
                                total = sum(total_vitimas, na.rm = T)),
             ifelse(typology == "Feminicídio" | typology == "Homicídio doloso" |
                      typology == "Lesão corporal seguida de morte" |
                      typology == "Morte no trânsito ou em decorrência dele (exceto homicídio doloso)" |
                      typology == "Mortes a esclarecer (sem indício de crime)" | typology == "Roubo seguido de morte (latrocínio)" |
                      typology == "Suicídio" | typology == "Tentativa de homicídio" | typology == "Estupro" |
                      typology == "Morte por intervenção de Agente do Estado",
                    df <- df |>
                      dplyr::group_by(uf,{{typology}},ano) |>
                      dplyr::summarize(total_fem = (sum(feminino, na.rm = T)),
                                       total_masc = (sum(masculino, na.rm = T)),
                                       total_n_inf = (sum(nao_informado, na.rm = T)),
                                       total = sum(total_vitimas, na.rm = T)),
                    stop("A tipologia introduzida não corresponde à categoria fornecida")))
    }

    if (granularity=="year" & category == "ocorrencias"){
      ifelse(typology=="all",
             df <- df |>
               dplyr::group_by(uf,evento,ano)|>
               dplyr::summarize(total = sum(total, na.rm = T)),
             ifelse(typology == "Furto de veículo" | typology == "Roubo a instituição financeira" |
                      typology == "Roubo de carga" | typology == "Roubo de veículo",
                    df <- df |>
                      dplyr::group_by(uf,{{typology}},ano) |>
                      dplyr::summarize(total = sum(total, na.rm = T)),
                    stop("A tipologia introduzida não corresponde à categoria fornecida")))
    }

    if (granularity=="year" & category == "drogas"){
      ifelse(typology=="all",
             df <- df |>
               dplyr::group_by(uf,evento,ano)|>
               dplyr::summarize(total_casos = sum(total),
                                total_peso = sum(total_peso)),
             ifelse(typology == "Apreensão de Cocaína" | typology == "Apreensão de Maconha" |
                    typology == "Tráfico de drogas",
                    df <- df |>
                      dplyr::group_by(uf,{{typology}},ano) |>
                      dplyr::summarize(total_casos = sum(total),
                                       total_peso = sum(total_peso)),
                    stop("A tipologia introduzida não corresponde à categoria fornecida")))
    }

    if (granularity=="year" & category == "arma de fogo"){
      ifelse(typology=="all",
             df <- df |>
               dplyr::group_by(uf,evento,ano,arma)|>
               dplyr::summarize(total = sum(total)),
             ifelse(typology == "Arma de Fogo Apreendida",
                    df <- df |>
                      dplyr::group_by(uf,evento,ano,arma) |>
                      dplyr::summarize(total),
                    stop("A tipologia introduzida não corresponde à categoria fornecida")))
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


