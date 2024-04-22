#' get_sinesp-vde_data Function
#'
#'This function is responsible for collecting criminal data directly from the SINESP database.
#'The data collected by this function starts temporally in January 2015 and goes until December 2022,
#'covering crimes such as: Estupro (Rape), Furto de veículo (Vehicle Theft), Homicídio doloso (Intentional Homicide),
#'Lesão corporal seguida de morte (Bodily Injury Followed by Death), Roubo a instituição financeira (Robbery of Financial Institution),
#'Roubo de carga (Cargo Theft), Roubo de veículo (Vehicle Robbery), Roubo seguido de morte (Latrocínio) (Robbery Followed by Death)
#'and Tentativa de homicídio (Attempted Homicide).
#'
#'@examples
#' dados <- get_sinespvde_data()
#'
#'
#' @export

get_sinespvde_data <- function(state = 'all', city = "all", category = "all", typology = 'all', year = 'all',
                               granularity = 'month', pivot = F, geom = F, relative_values = F) {

  load("data/df.rda")

  # Function arguments ---------------------------------------------------------

  #argument state & city-----------------
    if(state == "all"){
      ifelse({{city}} == "all",
      df <- df,
      ifelse(city == F,
             df <- df|>
               dplyr::select(-municipio)|>
               dplyr::group_by(uf,ano,mes,categoria,evento,agente,arma,faixa_etaria)|>
               dplyr::summarize(feminino=sum(feminino,na.rm = T),
                                masculino=sum(masculino,na.rm = T),
                                nao_informado=sum(nao_informado,na.rm = T),
                                total = sum(total,na.rm = T),
                                total_peso = sum(total_peso,na.rm = T),
                                total_vitimas=sum(total_vitimas,na.rm = T)),
             stop("To select a city, is necessary fist select a state")))
    }

  if(state != "all"){
    ifelse({{city}} == "all",
           df <- df |>
             dplyr::filter({uf} %in% {{state}}),
           ifelse(city != "all" & city != F,
                  df <- df |>
                    dplyr::filter({uf} %in% {{state}}) |>
                    dplyr::filter({municipio} %in% {{city}}),
                  ifelse(city == F,
                         df <- df |>
                           dplyr::filter({uf} %in% {{state}}),
             stop("To use the cities filter, it's necessary to select a state."))))
  }

  #argument category & typology-----------------------
  if(category == "all"){
    df <- df
  }

  if(category == "vitimas" & city!=F){
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

    if(category == "vitimas" & city==F){
      ifelse(typology=="all",
             df <- df |>
               dplyr::group_by(uf,ano,mes,categoria,evento,feminino,masculino,
                               nao_informado,total_vitimas) |>
               dplyr::filter(categoria %in% category)|>
               dplyr::summarize(feminino=sum(feminino,na.rm = T),
                                masculino=sum(masculino,na.rm = T),
                                nao_informado=sum(nao_informado,na.rm = T),
                                total_vitimas=sum(total_vitimas,na.rm = T)),
             ifelse(typology == "Feminicídio" | typology == "Homicídio doloso" |
                      typology == "Lesão corporal seguida de morte" |
                      typology == "Morte no trânsito ou em decorrência dele (exceto homicídio doloso)" |
                      typology == "Mortes a esclarecer (sem indício de crime)" | typology == "Roubo seguido de morte (latrocínio)" |
                      typology == "Suicídio" | typology == "Tentativa de homicídio" | typology == "Estupro" |
                      typology == "Morte por intervenção de Agente do Estado",
                    df <- df |>
                      dplyr::group_by(uf,ano,mes,categoria,evento,feminino,masculino,
                                    nao_informado,total_vitimas) |>
                      dplyr::filter({categoria} %in% {{category}}) |>
                      dplyr::filter({evento} %in% {{typology}})|>
                      dplyr::summarize(feminino=sum(feminino,na.rm = T),
                                        masculino=sum(masculino,na.rm = T),
                                        nao_informado=sum(nao_informado,na.rm = T),
                                        total_vitimas=sum(total_vitimas,na.rm = T)),
                    stop("A tipologia introduzida não corresponde à categoria fornecida")))
    }

  if(category == "drogas" & city!=F){
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

  if(category == "drogas" & city==F){
    ifelse(typology=="all",
           df <- df |>
             dplyr::group_by(uf,ano,mes,categoria,evento,total,total_peso) |>
             dplyr::filter({categoria} %in% {{category}})|>
             dplyr::rename(total_casos=total)|>
             dplyr::summarize(total_peso = sum(total_peso)),
           ifelse(typology == "Apreensão de Cocaína" | typology == "Apreensão de Maconha" |
                    typology == "Tráfico de drogas",
                  df <- df |>
                    dplyr::group_by(uf,ano,mes,categoria,evento,total,total_peso) |>
                    dplyr::filter({categoria} %in% {{category}}) |>
                    dplyr::filter({evento} %in% {{typology}})|>
                    dplyr::rename(total_casos=total)|>
                    dplyr::summarize(total_peso = sum(total_peso)),
                  stop("A tipologia introduzida não corresponde à categoria fornecida")))
  }

  if(category == "arma de fogo" & city!=F){
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

  if(category == "arma de fogo" & city==F){
    ifelse(typology=="all",
           df <- df |>
             dplyr::group_by(uf,ano,mes,categoria,evento,arma,total) |>
             dplyr::filter({categoria} %in% {{category}})|>
             dplyr::summarize(total),
           ifelse(typology == "Arma de Fogo Apreendida",
                  df <- df |>
                    dplyr::group_by(uf,ano,mes,categoria,evento,arma,total) |>
                    dplyr::filter({categoria} %in% {{category}}) |>
                    dplyr::filter({evento} %in% {{typology}})|>
                    dplyr::summarize(total),
                  stop("A tipologia introduzida não corresponde à categoria fornecida")))
  }

  if(category == "ocorrencias" & city!=F){
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

  if(category == "ocorrencias" & city==F){
    ifelse(typology=="all",
           df <- df |>
             dplyr::group_by(uf,ano,mes,categoria,evento,total) |>
             dplyr::filter({categoria} %in% {{category}})|>
             dplyr::summarize(total),
           ifelse(typology == "Furto de veículo" | typology == "Roubo a instituição financeira" |
                    typology == "Roubo de carga" | typology == "Roubo de veículo",
                  df <- df |>
                    dplyr::group_by(uf,ano,mes,categoria,evento,total) |>
                    dplyr::filter({categoria} %in% {{category}}) |>
                    dplyr::filter({evento} %in% {{typology}})|>
                    dplyr::summarize(total),
                  stop("A tipologia introduzida não corresponde à categoria fornecida")))
  }

  if(category == "desaparecidos/localizados" & city!=F){
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

  if(category == "desaparecidos/localizados" & city==F){
    ifelse(typology=="all",
           df <- df |>
             dplyr::group_by(uf,ano,mes,categoria,evento,faixa_etaria,feminino,
                             masculino,nao_informado,total_vitimas) |>
             dplyr::filter({categoria} %in% {{category}})|>
             dplyr::summarize(total_vitimas = (sum(total_vitimas, na.rm = T))),
           ifelse(typology == "Pessoa Desaparecida" | typology == "Pessoa Localizada",
                  df <- df |>
                    dplyr::group_by(uf,ano,mes,categoria,evento,faixa_etaria,feminino,
                                  masculino,nao_informado,total_vitimas) |>
                    dplyr::filter({categoria} %in% {{category}}) |>
                    dplyr::filter({evento} %in% {{typology}})|>
                    dplyr::summarize(total = (sum(total_vitimas, na.rm = T))),
                  stop("A tipologia introduzida não corresponde à categoria fornecida")))
  }

  if(category == "mandado de prisao cumprido" & city!=F){
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

  if(category == "mandado de prisao cumprido" & city==F){
    ifelse(typology=="all",
           df <- df |>
             dplyr::group_by(uf,ano,mes,categoria,evento,total) |>
             dplyr::filter({categoria} %in% {{category}})|>
             dplyr::summarize(total),
           ifelse(typology == "Mandado de prisão cumprido",
                  df <- df |>
                    dplyr::group_by(uf,ano,mes,categoria,evento,total) |>
                    dplyr::filter({categoria} %in% {{category}}) |>
                    dplyr::filter({evento} %in% {{typology}})|>
                    dplyr::summarize(total),
                  stop("A tipologia introduzida não corresponde à categoria fornecida")))
  }

  if(category == "profissionais de seguranca" & city!=F){
    ifelse(typology=="all",
           df <- df |>
             dplyr::select(uf,municipio,ano,mes,categoria,evento,agente,feminino,
                    masculino,nao_informado,total_vitimas) |>
             dplyr::filter(categoria %in% {{category}}),
           ifelse(typology == "Morte de Agente do Estado" | typology == "Suicídio de Agente do Estado",
                  df <- df |>
                    dplyr::select(uf,municipio,ano,mes,categoria,evento,agente,feminino,
                           masculino,nao_informado,total_vitimas) |>
                    dplyr::filter({categoria} %in% {{category}}) |>
                    dplyr::filter({evento} %in% {{typology}}),
                  stop("A tipologia introduzida não corresponde à categoria fornecida")))
  }

  if(category == "profissionais de seguranca" & city==F){
    ifelse(typology=="all",
           df <- df |>
             dplyr::group_by(uf,ano,mes,categoria,evento,agente,feminino,
                           masculino,nao_informado,total_vitimas) |>
             dplyr::filter(categoria %in% {{category}})|>
             dplyr::summarize(total_vitimas = (sum(total_vitimas, na.rm = T))),
           ifelse(typology == "Morte de Agente do Estado" | typology == "Suicídio de Agente do Estado",
                  df <- df |>
                    dplyr::group_by(uf,ano,mes,categoria,evento,agente,feminino,
                                  masculino,nao_informado,total_vitimas) |>
                    dplyr::filter({categoria} %in% {{category}}) |>
                    dplyr::filter({evento} %in% {{typology}})|>
                    dplyr::summarize(total_vitimas = (sum(total_vitimas, na.rm = T))),
                  stop("A tipologia introduzida não corresponde à categoria fornecida")))
  }

  if(category == "bombeiros" & city!=F){
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

  if(category == "bombeiros" & city==F){
    ifelse(typology=="all",
           df <- df |>
             dplyr::group_by(uf,ano,mes,categoria,evento,total) |>
             dplyr::filter({categoria} %in% {{category}})|>
             dplyr::summarize(total),
           ifelse(typology == "Atendimento pré-hospitalar" | typology == "Busca e salvamento" |
                    typology == "Combate a incêndios" | typology == "Emissão de Alvarás de licença" |
                    typology == "Realização de vistorias",
                  df <- df |>
                    dplyr::group_by(uf,ano,mes,categoria,evento,total) |>
                    dplyr::filter({categoria} %in% {{category}}) |>
                    dplyr::filter({evento} %in% {{typology}})|>
                    dplyr::summarize(total),
                  stop("A tipologia introduzida não corresponde à categoria fornecida")))
  }

    # argument year-------------------------------
    if (!"all" %in% year) {
      df <- df |>
        dplyr::filter({ano} %in% {{year}})
    }

    # argument granularity--------------------------------------
    if (granularity == 'year' & category == "all") {
      stop("To set granularity = 'year', please select a category fisrt")
    }

    if (granularity=="year" & category == "vitimas" & relative_values==F & pivot==F){
      ifelse(typology=="all",
             df <- df |>
               dplyr::group_by(uf,municipio,categoria,evento,ano)|>
               dplyr::filter({categoria} %in% {{category}})|>
               dplyr::summarize(total_fem = (sum(feminino, na.rm = T)),
                                total_masc = (sum(masculino, na.rm = T)),
                                total_n_inf = (sum(nao_informado, na.rm = T)),
                                total = sum(total_vitimas, na.rm = T))|>
               dplyr::ungroup(),
             ifelse(typology == "Feminicídio" | typology == "Homicídio doloso" |
                      typology == "Lesão corporal seguida de morte" |
                      typology == "Morte no trânsito ou em decorrência dele (exceto homicídio doloso)" |
                      typology == "Mortes a esclarecer (sem indício de crime)" | typology == "Roubo seguido de morte (latrocínio)" |
                      typology == "Suicídio" | typology == "Tentativa de homicídio" | typology == "Estupro" |
                      typology == "Morte por intervenção de Agente do Estado",
                    df <- df |>
                      dplyr::group_by(uf,categoria,{{typology}},ano,municipio) |>
                      dplyr::summarize(total_fem = (sum(feminino, na.rm = T)),
                                       total_masc = (sum(masculino, na.rm = T)),
                                       total_n_inf = (sum(nao_informado, na.rm = T)),
                                       total = sum(total_vitimas, na.rm = T))|>
                      dplyr::ungroup(),
                    stop("A tipologia introduzida não corresponde à categoria fornecida")))
    }

    if (granularity=="year" & category == "ocorrencias" & relative_values==F & pivot==F){
      ifelse(typology=="all",
             df <- df |>
               dplyr::group_by(uf,evento,ano,municipio)|>
               dplyr::summarize(total = sum(total, na.rm = T)),
             ifelse(typology == "Furto de veículo" | typology == "Roubo a instituição financeira" |
                      typology == "Roubo de carga" | typology == "Roubo de veículo",
                    df <- df |>
                      dplyr::group_by(uf,{{typology}},ano) |>
                      dplyr::summarize(total = sum(total, na.rm = T)),
                    stop("A tipologia introduzida não corresponde à categoria fornecida")))
    }

    if (granularity=="year" & category == "drogas" & relative_values==F & pivot==F){
      ifelse(typology=="all",
             df <- df |>
               dplyr::group_by(uf,evento,ano,municipio)|>
               dplyr::summarize(total_casos = sum(total),
                                total_peso = sum(total_peso)),
             ifelse(typology == "Apreensão de Cocaína" | typology == "Apreensão de Maconha" |
                    typology == "Tráfico de drogas",
                    df <- df |>
                      dplyr::group_by(uf,{{typology}},ano,municipio) |>
                      dplyr::summarize(total_casos = sum(total),
                                       total_peso = sum(total_peso)),
                    stop("A tipologia introduzida não corresponde à categoria fornecida")))
    }

    if (granularity=="year" & category == "arma de fogo" & relative_values==F & pivot==F){
      ifelse(typology=="all",
             df <- df |>
               dplyr::group_by(uf,evento,ano,arma,municipio)|>
               dplyr::summarize(total = sum(total, na.rm = T)),
             ifelse(typology == "Arma de Fogo Apreendida",
                    df <- df |>
                      dplyr::group_by(uf,evento,ano,arma,municipio) |>
                      dplyr::summarize(total= sum(total, na.rm = T)),
                    stop("A tipologia introduzida não corresponde à categoria fornecida")))
    }

    if (granularity=="year" & category == "desaparecidos/localizados" & relative_values==F & pivot==F){
      ifelse(typology=="all",
             df <- df |>
               dplyr::group_by(uf,evento,ano,faixa_etaria,municipio)|>
               dplyr::summarize(total_fem = (sum(feminino, na.rm = T)),
                                total_masc = (sum(masculino, na.rm = T)),
                                total_n_inf = (sum(nao_informado, na.rm = T)),
                                total = (sum(total_vitimas, na.rm = T))),
             ifelse(typology == "Pessoa Desaparecida" | typology == "Pessoa Localizada",
                    df <- df |>
                      dplyr::group_by(uf,{{typology}},ano,faixa_etaria,municipio) |>
                      dplyr::summarize(total_fem = (sum(feminino, na.rm = T)),
                                       total_masc = (sum(masculino, na.rm = T)),
                                       total_n_inf = (sum(nao_informado, na.rm = T)),
                                       total = (sum(total_vitimas, na.rm = T))),
                    stop("A tipologia introduzida não corresponde à categoria fornecida")))
    }

    if (granularity=="year" & category == "mandado de prisao cumprido" & relative_values==F & pivot==F){
      ifelse(typology=="all",
             df <- df |>
               dplyr::group_by(uf,evento,ano,municipio)|>
               dplyr::summarize(total=(sum(total, na.rm = T))),
             ifelse(typology == "Mandado de prisão cumprido",
                    df <- df |>
                      dplyr::group_by(uf,evento,ano,municipio)|>
                      dplyr::summarize(total=(sum(total, na.rm = T))),
                    stop("A tipologia introduzida não corresponde à categoria fornecida")))
    }

    if (granularity=="year" & category == "profissionais de seguranca" & relative_values==F & pivot==F){
      ifelse(typology=="all",
             df <- df |>
               dplyr::group_by(uf,evento,ano,agente,municipio)|>
               dplyr::summarize(total_fem = (sum(feminino, na.rm = T)),
                                total_masc = (sum(masculino, na.rm = T)),
                                total_n_inf = (sum(nao_informado, na.rm = T)),
                                total = (sum(total_vitimas, na.rm = T))),
             ifelse(typology == "Morte de Agente do Estado" | typology == "Suicídio de Agente do Estado",
                    df <- df |>
                      dplyr::group_by(uf,{{typology}},ano,agente,municipio)|>
                      dplyr::summarize(total_fem = (sum(feminino, na.rm = T)),
                                       total_masc = (sum(masculino, na.rm = T)),
                                       total_n_inf = (sum(nao_informado, na.rm = T)),
                                       total = (sum(total_vitimas, na.rm = T))),
                    stop("A tipologia introduzida não corresponde à categoria fornecida")))
    }

    if (granularity=="year" & category == "bombeiros" & relative_values==F & pivot==F){
      ifelse(typology=="all",
             df <- df |>
               dplyr::group_by(uf,evento,ano,municipio)|>
               dplyr::summarize(total=sum(total,na.rm = T)),
             ifelse(typology == "Atendimento pré-hospitalar" | typology == "Busca e salvamento" |
                      typology == "Combate a incêndios" | typology == "Emissão de Alvarás de licença" |
                      typology == "Realização de vistorias",
                    df <- df |>
                      dplyr::group_by(uf,{{typology}},ano,agente,municipio)|>
                      dplyr::summarize(total=sum(total,na.rm = T)),
                    stop("A tipologia introduzida não corresponde à categoria fornecida")))
    }


  #Argument Relative_Values == T & granularity == "month" --------------------------------

    if (relative_values == T & granularity == 'month' & city != F & pivot==F) {
      stop("A city only can be picked when relative_values = F")
    }

  if (relative_values == T & granularity == 'month' & city == F & pivot==F) {

      load("data/pop.rda")

        col_names <- names(pop)

      pop_long <- pop |>
        tidyr::pivot_longer(
          cols = col_names[2:28],
          names_to = "uf",
          values_to = "populacao")|>
        dplyr::select(-TOTAL)|>
        dplyr::mutate(mes=as.factor(mes))

      if (category=="all"){
             df <- df |>
               dplyr::mutate(mes=as.factor(mes))|>
               dplyr::inner_join(pop_long, by = c("uf", "ano", "mes"))|>
               dplyr::group_by(uf,ano,mes,categoria,evento,agente,arma,faixa_etaria,populacao)|>
               dplyr::summarize(feminino=sum(feminino,na.rm = T),
                                masculino=sum(masculino,na.rm = T),
                                nao_informado=sum(nao_informado,na.rm = T),
                                total = sum(total,na.rm = T),
                                total_peso = sum(total_peso,na.rm = T),
                                total_vitimas=sum(total_vitimas,na.rm = T))
      }
  }

  if (relative_values == T & granularity == 'month' & city == F & category!="all" & pivot==F){

    load("data/pop.rda")

    col_names <- names(pop)

    pop_long <- pop |>
      tidyr::pivot_longer(
        cols = col_names[2:28],
        names_to = "uf",
        values_to = "populacao")|>
      dplyr::select(-TOTAL)|>
      dplyr::mutate(mes=as.factor(mes))

    df <- df |>
      dplyr::mutate(mes=as.factor(mes))|>
      dplyr::inner_join(pop_long, by = c("uf", "ano", "mes"))

      if (category == "vitimas"){
        ifelse(typology=="all",
               df <- df |>
                 dplyr::group_by(uf,ano,mes,categoria,evento,feminino,masculino,
                                 nao_informado,total_vitimas,populacao) |>
                 dplyr::filter(categoria %in% category)|>
                 dplyr::summarize(feminino=sum(feminino,na.rm = T),
                                  masculino=sum(masculino,na.rm = T),
                                  nao_informado=sum(nao_informado,na.rm = T),
                                  total_vitimas=sum(total_vitimas,na.rm = T))|>
                 dplyr::mutate(vitimas_100k_hab = round(total_vitimas / populacao * 100000, 2)),
                 ifelse(typology == "Feminicídio" | typology == "Homicídio doloso" |
                        typology == "Lesão corporal seguida de morte" |
                        typology == "Morte no trânsito ou em decorrência dele (exceto homicídio doloso)" |
                        typology == "Mortes a esclarecer (sem indício de crime)" | typology == "Roubo seguido de morte (latrocínio)" |
                        typology == "Suicídio" | typology == "Tentativa de homicídio" | typology == "Estupro" |
                        typology == "Morte por intervenção de Agente do Estado",
                        df <- df |>
                      dplyr::group_by(uf,ano,mes,categoria,evento,feminino,masculino,
                                      nao_informado,total_vitimas,populacao) |>
                        dplyr::filter({categoria} %in% {{category}}) |>
                        dplyr::filter({evento} %in% {{typology}})|>
                        dplyr::summarize(feminino=sum(feminino,na.rm = T),
                                         masculino=sum(masculino,na.rm = T),
                                         nao_informado=sum(nao_informado,na.rm = T),
                                         total_vitimas=sum(total_vitimas,na.rm = T))|>
                        dplyr::mutate(vitimas_100k_hab = round(total_vitimas / populacao * 100000, 2)),
                      stop("A tipologia introduzida não corresponde à categoria fornecida")))
      }

      if (category == "drogas"){
        ifelse(typology=="all",
               df <- df |>
                 dplyr::group_by(uf,evento,ano,mes,populacao)|>
                 dplyr::summarize(total = sum(total_casos, na.rm = T),
                                  total_peso = sum(total_peso,na.rm = T))|>
                 dplyr::mutate(casos_100k_hab = round(total / populacao * 100000, 2))|>
                 dplyr::mutate(apreensao_100k_hab = round(total_peso / populacao * 100000, 2)),
               ifelse(typology == "Apreensão de Cocaína" | typology == "Apreensão de Maconha" |
                 typology == "Tráfico de drogas",
                 df <- df |>
                   dplyr::group_by(uf,{{typology}},ano,mes,populacao) |>
                   dplyr::summarize(total = sum(total_casos, na.rm = T),
                                    total_peso = sum(total_peso,na.rm = T))|>
                   dplyr::mutate(casos_100k_hab = round(total / populacao * 100000, 2))|>
                   dplyr::mutate(apreensao_100k_hab = round(total_peso / populacao * 100000, 2)),
                 stop("A tipologia introduzida não corresponde à categoria fornecida")))
      }

      if (category == "ocorrencias"){
        ifelse(typology=="all",
               df <- df |>
                 dplyr::group_by(uf,evento,ano,mes,populacao)|>
                 dplyr::summarize(total = sum(total, na.rm = T))|>
                 dplyr::mutate(ocorrencia_100k_hab = round(total / populacao * 100000, 2)),
               ifelse(typology == "Furto de veículo" | typology == "Roubo a instituição financeira" |
                        typology == "Roubo de carga" | typology == "Roubo de veículo",
                      df <- df |>
                        dplyr::group_by(uf,{{typology}},ano,mes,populacao) |>
                        dplyr::summarize(total = sum(total, na.rm = T))|>
                        dplyr::mutate(ocorrencia_100k_hab = round(total / populacao * 100000, 2)),
                      stop("A tipologia introduzida não corresponde à categoria fornecida")))
      }

      if(category == "arma de fogo"){
        ifelse(typology=="all",
               df <- df |>
                 dplyr::group_by(uf,evento,ano,mes,arma,populacao)|>
                 dplyr::summarize(total = sum(total, na.rm = T))|>
                 dplyr::mutate(arma_apreend_100k_hab = round(total / populacao * 100000, 2)),
               ifelse(typology == "Arma de Fogo Apreendida",
                      df <- df |>
                        dplyr::group_by(uf,evento,ano,mes,arma,populacao) |>
                        dplyr::summarize(total= sum(total, na.rm = T)) |>
                        dplyr::mutate(arma_apreend_100k_hab = round(total / populacao * 100000, 2)),
                      stop("A tipologia introduzida não corresponde à categoria fornecida")))
      }

    if(category == "desaparecidos/localizados"){
      ifelse(typology=="all",
             df <- df |>
               dplyr::group_by(uf,ano,mes,categoria,evento,faixa_etaria,feminino,
                               masculino,nao_informado,total_vitimas,populacao) |>
               dplyr::filter({categoria} %in% {{category}})|>
               dplyr::summarize(total_vitimas = (sum(total_vitimas, na.rm = T)))|>
               dplyr::mutate(vitimas_100k_hab = round(total_vitimas / populacao * 100000, 2)),
             ifelse(typology == "Pessoa Desaparecida" | typology == "Pessoa Localizada",
                    df <- df |>
                      dplyr::group_by(uf,ano,mes,categoria,evento,faixa_etaria,feminino,
                                      masculino,nao_informado,total_vitimas,populacao) |>
                      dplyr::filter({categoria} %in% {{category}}) |>
                      dplyr::filter({evento} %in% {{typology}})|>
                      dplyr::summarize(total_vitimas = (sum(total_vitimas, na.rm = T)))|>
                      dplyr::mutate(vitimas_100k_hab = round(total_vitimas / populacao * 100000, 2)),
                    stop("A tipologia introduzida não corresponde à categoria fornecida")))
    }

    if (category == "mandado de prisao cumprido"){
      ifelse(typology=="all",
             df <- df |>
               dplyr::group_by(uf,ano,mes,categoria,evento,total,populacao) |>
               dplyr::filter({categoria} %in% {{category}})|>
               dplyr::summarize(total)|>
               dplyr::mutate(total_100k_hab = round(total / populacao * 100000, 2)),
             ifelse(typology == "Mandado de prisão cumprido",
                    df <- df |>
                      dplyr::group_by(uf,ano,mes,categoria,evento,total,populacao) |>
                      dplyr::filter({categoria} %in% {{category}}) |>
                      dplyr::filter({evento} %in% {{typology}})|>
                      dplyr::summarize(total)|>
                      dplyr::mutate(total_100k_hab = round(total / populacao * 100000, 2)),
                    stop("A tipologia introduzida não corresponde à categoria fornecida")))
    }

    if(category == "profissionais de seguranca"){
      ifelse(typology=="all",
             df <- df |>
               dplyr::group_by(uf,evento,ano,mes,agente,populacao)|>
               dplyr::summarize(total_fem = (sum(feminino, na.rm = T)),
                                total_masc = (sum(masculino, na.rm = T)),
                                total_n_inf = (sum(nao_informado, na.rm = T)),
                                total = (sum(total_vitimas, na.rm = T)))|>
               dplyr::mutate(vitimas_100k_hab = round(total / populacao * 100000, 2)),
             ifelse(typology == "Morte de Agente do Estado" | typology == "Suicídio de Agente do Estado",
                    df <- df |>
                      dplyr::group_by(uf,{{typology}},ano,agente,populacao)|>
                      dplyr::summarize(total_fem = (sum(feminino, na.rm = T)),
                                       total_masc = (sum(masculino, na.rm = T)),
                                       total_n_inf = (sum(nao_informado, na.rm = T)),
                                       total = (sum(total_vitimas, na.rm = T)))|>
                      dplyr::mutate(vitimas_100k_hab = round(total / populacao * 100000, 2)),
                    stop("A tipologia introduzida não corresponde à categoria fornecida")))
    }

    if(category == "bombeiros"){
      ifelse(typology=="all",
             df <- df |>
               dplyr::group_by(uf,evento,ano,populacao)|>
               dplyr::summarize(total=sum(total,na.rm = T))|>
               dplyr::mutate(total_100k_hab = round(total / populacao * 100000, 2)),
             ifelse(typology == "Atendimento pré-hospitalar" | typology == "Busca e salvamento" |
                      typology == "Combate a incêndios" | typology == "Emissão de Alvarás de licença" |
                      typology == "Realização de vistorias",
                    df <- df |>
                      dplyr::group_by(uf,{{typology}},ano,populacao)|>
                      dplyr::summarize(total=sum(total,na.rm = T))|>
                      dplyr::mutate(total_100k_hab = round(total / populacao * 100000, 2)),
                    stop("A tipologia introduzida não corresponde à categoria fornecida")))
    }

  }


  #Argument Relative_Values & granularity == "year" --------------------------------

    if (relative_values == T & granularity == 'year' & city != F & pivot==F) {
      stop("A city only can be picked when relative_values = F")
    }


    if (relative_values == T & granularity == 'year' & city == F & category!="all" & pivot==F){

      load("data/pop_anual.rda")

      df <- df |>
        dplyr::mutate(ano=as.character(ano))|>
        dplyr::select(-mes)|>
        dplyr::left_join(pop_anual, by = c("uf", "ano"))

        if (category == "vitimas"){
          ifelse(typology=="all",
                 df <- df |>
                   dplyr::group_by(uf,ano,categoria,evento,populacao_anual)|>
                   dplyr::filter({categoria} %in% {{category}}) |>
                   dplyr::summarize(feminino=sum(feminino,na.rm = T),
                                    masculino=sum(masculino,na.rm = T),
                                    nao_informado=sum(nao_informado,na.rm = T),
                                    total_vitimas=sum(total_vitimas,na.rm = T))|>
                   dplyr::mutate(vitimas_100k_hab = round(total_vitimas / populacao_anual * 100000, 2)),
                 ifelse(typology == "Feminicídio" | typology == "Homicídio doloso" |
                        typology == "Lesão corporal seguida de morte" |
                        typology == "Morte no trânsito ou em decorrência dele (exceto homicídio doloso)" |
                        typology == "Mortes a esclarecer (sem indício de crime)" | typology == "Roubo seguido de morte (latrocínio)" |
                        typology == "Suicídio" | typology == "Tentativa de homicídio" | typology == "Estupro" |
                        typology == "Morte por intervenção de Agente do Estado",
                      df <- df |>
                        dplyr::group_by(uf,ano,categoria,evento,populacao_anual) |>
                        dplyr::filter({categoria} %in% {{category}}) |>
                        dplyr::filter({evento} %in% {{typology}})|>
                        dplyr::summarize(feminino=sum(feminino,na.rm = T),
                                         masculino=sum(masculino,na.rm = T),
                                         nao_informado=sum(nao_informado,na.rm = T),
                                         total_vitimas=sum(total_vitimas,na.rm = T))|>
                        dplyr::mutate(vitimas_100k_hab = round(total_vitimas / populacao_anual * 100000, 2)),
                      stop("A tipologia introduzida não corresponde à categoria fornecida")))
        }

    if (category == "drogas"){
      ifelse(typology=="all",
             df <- df |>
               dplyr::group_by(uf,evento,ano,populacao_anual)|>
               dplyr::summarize(total = sum(total_casos, na.rm = T),
                                total_peso = sum(total_peso,na.rm = T))|>
               dplyr::mutate(casos_100k_hab = round(total / populacao_anual * 100000, 2))|>
               dplyr::mutate(apreensao_100k_hab = round(total_peso / populacao_anual * 100000, 2)),
             ifelse(typology == "Apreensão de Cocaína" | typology == "Apreensão de Maconha" |
                      typology == "Tráfico de drogas",
                    df <- df |>
                      dplyr::group_by(uf,{{typology}},ano,populacao_anual) |>
                      dplyr::summarize(total = sum(total_casos, na.rm = T),
                                       total_peso = sum(total_peso,na.rm = T))|>
                      dplyr::mutate(casos_100k_hab = round(total / populacao_anual * 100000, 2))|>
                      dplyr::mutate(apreensao_100k_hab = round(total_peso / populacao_anual * 100000, 2)),
                    stop("A tipologia introduzida não corresponde à categoria fornecida")))
    }

    if (category == "ocorrencias"){
      ifelse(typology=="all",
             df <- df |>
               dplyr::group_by(uf,evento,ano,populacao_anual)|>
               dplyr::summarize(total = sum(total, na.rm = T))|>
               dplyr::mutate(ocorrencia_100k_hab = round(total / populacao_anual * 100000, 2)),
             ifelse(typology == "Furto de veículo" | typology == "Roubo a instituição financeira" |
                      typology == "Roubo de carga" | typology == "Roubo de veículo",
                    df <- df |>
                      dplyr::group_by(uf,{{typology}},ano,populacao_anual) |>
                      dplyr::summarize(total = sum(total, na.rm = T))|>
                      dplyr::mutate(ocorrencia_100k_hab = round(total / populacao_anual * 100000, 2)),
                    stop("A tipologia introduzida não corresponde à categoria fornecida")))
    }

    if(category == "arma de fogo"){
      ifelse(typology=="all",
             df <- df |>
               dplyr::group_by(uf,evento,ano,arma,populacao_anual)|>
               dplyr::summarize(total = sum(total, na.rm = T))|>
               dplyr::mutate(arma_apreend_100k_hab = round(total / populacao_anual * 100000, 2)),
             ifelse(typology == "Arma de Fogo Apreendida",
                    df <- df |>
                      dplyr::group_by(uf,evento,ano,arma,populacao_anual) |>
                      dplyr::summarize(total= sum(total, na.rm = T)) |>
                      dplyr::mutate(arma_apreend_100k_hab = round(total / populacao_anual * 100000, 2)),
                    stop("A tipologia introduzida não corresponde à categoria fornecida")))
    }

    if(category == "desaparecidos/localizados"){
      ifelse(typology=="all",
             df <- df |>
               dplyr::group_by(uf,ano,categoria,evento,faixa_etaria,populacao_anual) |>
               dplyr::filter({categoria} %in% {{category}})|>
               dplyr::summarize( feminino=sum(feminino,na.rm = T),
                                 masculino=sum(masculino,na.rm = T),
                                 nao_informado=sum(nao_informado,na.rm = T),
                                 total_vitimas=sum(total_vitimas,na.rm = T))|>
               dplyr::mutate(vitimas_100k_hab = round(total_vitimas / populacao_anual * 100000, 2)),
             ifelse(typology == "Pessoa Desaparecida" | typology == "Pessoa Localizada",
                    df <- df |>
                      dplyr::group_by(uf,ano,categoria,evento,faixa_etaria,populacao_anual) |>
                      dplyr::filter({categoria} %in% {{category}}) |>
                      dplyr::filter({evento} %in% {{typology}})|>
                      dplyr::summarize( feminino=sum(feminino,na.rm = T),
                                        masculino=sum(masculino,na.rm = T),
                                        nao_informado=sum(nao_informado,na.rm = T),
                                        total_vitimas=sum(total_vitimas,na.rm = T))|>
                      dplyr::mutate(vitimas_100k_hab = round(total_vitimas / populacao_anual * 100000, 2)),
                    stop("A tipologia introduzida não corresponde à categoria fornecida")))
    }

    if (category == "mandado de prisao cumprido"){
      ifelse(typology=="all",
             df <- df |>
               dplyr::group_by(uf,ano,categoria,evento,populacao_anual) |>
               dplyr::filter({categoria} %in% {{category}})|>
               dplyr::summarize(total=sum(total,na.rm = T))|>
               dplyr::mutate(total_100k_hab = round(total / populacao_anual * 100000, 2)),
             ifelse(typology == "Mandado de prisão cumprido",
                    df <- df |>
                      dplyr::group_by(uf,ano,categoria,evento,populacao_anual) |>
                      dplyr::filter({categoria} %in% {{category}}) |>
                      dplyr::filter({evento} %in% {{typology}})|>
                      dplyr::summarize(total=sum(total,na.rm = T))|>
                      dplyr::mutate(total_100k_hab = round(total / populacao_anual * 100000, 2)),
                    stop("A tipologia introduzida não corresponde à categoria fornecida")))
    }

    if(category == "profissionais de seguranca"){
      ifelse(typology=="all",
             df <- df |>
               dplyr::group_by(uf,evento,ano,agente,populacao_anual)|>
               dplyr::summarize(total_fem = (sum(feminino, na.rm = T)),
                                total_masc = (sum(masculino, na.rm = T)),
                                total_n_inf = (sum(nao_informado, na.rm = T)),
                                total = (sum(total_vitimas, na.rm = T)))|>
               dplyr::mutate(vitimas_100k_hab = round(total / populacao_anual * 100000, 2)),
             ifelse(typology == "Morte de Agente do Estado" | typology == "Suicídio de Agente do Estado",
                    df <- df |>
                      dplyr::group_by(uf,{{typology}},ano,agente,populacao_anual)|>
                      dplyr::summarize(total_fem = (sum(feminino, na.rm = T)),
                                       total_masc = (sum(masculino, na.rm = T)),
                                       total_n_inf = (sum(nao_informado, na.rm = T)),
                                       total = (sum(total_vitimas, na.rm = T)))|>
                      dplyr::mutate(vitimas_100k_hab = round(total / populacao_anual * 100000, 2)),
                    stop("A tipologia introduzida não corresponde à categoria fornecida")))
    }

    if(category == "bombeiros"){
      ifelse(typology=="all",
             df <- df |>
               dplyr::group_by(uf,evento,ano,populacao_anual)|>
               dplyr::summarize(total=sum(total,na.rm = T))|>
               dplyr::mutate(total_100k_hab = round(total / populacao_anual * 100000, 2)),
             ifelse(typology == "Atendimento pré-hospitalar" | typology == "Busca e salvamento" |
                      typology == "Combate a incêndios" | typology == "Emissão de Alvarás de licença" |
                      typology == "Realização de vistorias",
                    df <- df |>
                      dplyr::group_by(uf,{{typology}},ano,populacao_anual)|>
                      dplyr::summarize(total=sum(total,na.rm = T))|>
                      dplyr::mutate(total_100k_hab = round(total / populacao_anual * 100000, 2)),
                    stop("A tipologia introduzida não corresponde à categoria fornecida")))
    }

  }

# argument geom--------------------------

    if (geom == T & city == F) {
      ufs_geom <- geobr::read_state() |>
        dplyr::select(code_state, abbrev_state) |>
        dplyr::rename('uf' = abbrev_state)

       df <- ufs_geom |>
          dplyr::left_join(df,
                           by = 'uf') |>
          dplyr::rename(geometry = geom) |>
          na.omit()

    }

    if(geom == T & city !=F){
      mncp_geom <- geobr::read_municipality()|>
        dplyr::select(code_muni, name_muni,abbrev_state)|>
        dplyr::rename(municipio = name_muni,
                      uf = abbrev_state)

      mncp_geom$municipio <-  toupper(mncp_geom$municipio)

      df <- mncp_geom|>
        dplyr::left_join(df,
                         by=c("uf", "municipio"))|>
        na.omit()
    }

# argument pivot = T & realtive_values = F -----------------------

    if (pivot == T & category=="all" ){
      stop("To use pivot = T, please select a category first")
    }

    if (pivot == T & category!="all") {

      if(category=="vitimas" & relative_values==F & granularity=="month"){
        ifelse(typology=="all",
               df <- df|>
                 dplyr::group_by(uf,categoria,evento,ano,mes)|>
                 dplyr::filter(categoria %in% {{category}})|>
                 tidyr::pivot_wider(
                   names_from = evento,
                   values_from = c(feminino,masculino,nao_informado,total_vitimas)),
              ifelse(typology == "Feminicídio" | typology == "Homicídio doloso" |
                       typology == "Lesão corporal seguida de morte" |
                       typology == "Morte no trânsito ou em decorrência dele (exceto homicídio doloso)" |
                       typology == "Mortes a esclarecer (sem indício de crime)" | typology == "Roubo seguido de morte (latrocínio)" |
                       typology == "Suicídio" | typology == "Tentativa de homicídio" | typology == "Estupro" |
                       typology == "Morte por intervenção de Agente do Estado",
                     df <- df|>
                       dplyr::group_by(uf,categoria,evento,ano,mes)|>
                       dplyr::filter(categoria %in% {{category}})|>
                       dplyr::filter(evento %in% {{typology}}) |>
                       tidyr::pivot_wider(
                         names_from = evento,
                         values_from = c(feminino,masculino,nao_informado,total_vitimas)),
                     stop("A tipologia introduzida não corresponde à categoria fornecida")))
      }

      if(category=="vitimas" & relative_values==F & granularity=="year"){
        ifelse(typology=="all",
               df <- df|>
                 dplyr::group_by(uf,municipio,categoria,evento,ano)|>
                 dplyr::filter(categoria %in% {{category}})|>
                 dplyr::summarize(feminino=sum(feminino,na.rm = T),
                                  masculino=sum(masculino,na.rm = T),
                                  nao_informado=sum(nao_informado,na.rm = T),
                                  total_vitimas=sum(total_vitimas,na.rm = T))|>
                 tidyr::pivot_wider(
                   names_from = evento,
                   values_from = c(feminino,masculino,nao_informado,total_vitimas)),
               ifelse(typology == "Feminicídio" | typology == "Homicídio doloso" |
                        typology == "Lesão corporal seguida de morte" |
                        typology == "Morte no trânsito ou em decorrência dele (exceto homicídio doloso)" |
                        typology == "Mortes a esclarecer (sem indício de crime)" | typology == "Roubo seguido de morte (latrocínio)" |
                        typology == "Suicídio" | typology == "Tentativa de homicídio" | typology == "Estupro" |
                        typology == "Morte por intervenção de Agente do Estado",
                      df <- df|>
                        dplyr::group_by(uf,categoria,evento,ano,mes,municipio)|>
                        dplyr::filter(categoria %in% {{category}})|>
                        dplyr::filter(evento %in% {{typology}})|>
                        dplyr::summarize(feminino=sum(feminino,na.rm = T),
                                         masculino=sum(masculino,na.rm = T),
                                         nao_informado=sum(nao_informado,na.rm = T),
                                         total_vitimas=sum(total_vitimas,na.rm = T))|>
                        tidyr::pivot_wider(
                          names_from = evento,
                          values_from = c(feminino,masculino,nao_informado,total_vitimas)),
                      stop("A tipologia introduzida não corresponde à categoria fornecida")))
      }

        if(category=="drogas" & relative_values==F & granularity=="month"){
          ifelse(typology=="all",
                 df <- df|>
                   dplyr::group_by(uf,categoria,evento,ano,mes,municipio)|>
                   dplyr::filter(categoria %in% {{category}})|>
                   tidyr::pivot_wider(
                     names_from = evento,
                     values_from = c(total_casos,total_peso)),
                 ifelse(typology == "Apreensão de Cocaína" | typology == "Apreensão de Maconha" |
                          typology == "Tráfico de drogas",
                        df <- df|>
                          dplyr::group_by(uf,categoria,evento,ano,mes,municipio)|>
                          dplyr::filter(categoria %in% {{category}})|>
                          dplyr::filter(evento %in% {{typology}}) |>
                          tidyr::pivot_wider(
                            names_from = evento,
                            values_from = c(total_casos,total_peso)),
                        stop("A tipologia introduzida não corresponde à categoria fornecida")))
        }

        if(category=="drogas" & relative_values==F & granularity=="year"){
          ifelse(typology=="all",
                 df <- df|>
                   dplyr::group_by(uf,categoria,evento,ano,municipio)|>
                   dplyr::filter(categoria %in% {{category}})|>
                   dplyr::summarize(total_casos=sum(total_casos,na.rm = T),
                                    total_peso=sum(total_peso,na.rm = T))|>
                   tidyr::pivot_wider(
                     names_from = evento,
                     values_from = c(total_casos,total_peso)),
                 ifelse(typology == "Apreensão de Cocaína" | typology == "Apreensão de Maconha" |
                          typology == "Tráfico de drogas",
                        df <- df|>
                          dplyr::group_by(uf,categoria,evento,ano,mes,municipio)|>
                          dplyr::filter(categoria %in% {{category}})|>
                          dplyr::filter(evento %in% {{typology}})|>
                          tidyr::pivot_wider(
                            names_from = evento,
                            values_from = c(total_casos,total_peso)),
                        stop("A tipologia introduzida não corresponde à categoria fornecida")))
        }

        if(category=="ocorrencias" & relative_values==F & granularity=="month"){
          ifelse(typology=="all",
                 df <- df |>
                   dplyr::group_by(uf,evento,ano,mes,municipio)|>
                   dplyr::filter(categoria %in% {{category}})|>
                   tidyr::pivot_wider(
                     names_from = evento,
                     values_from = total),
                 ifelse(typology == "Furto de veículo" | typology == "Roubo a instituição financeira" |
                          typology == "Roubo de carga" | typology == "Roubo de veículo",
                        df <- df |>
                          dplyr::group_by(uf,{{typology}},ano,mes,municipio)|>
                          dplyr::filter(categoria %in% {{category}})|>
                          dplyr::filter(evento %in% {{typology}})|>
                          tidyr::pivot_wider(
                            names_from = evento,
                            values_from = total),
                        stop("A tipologia introduzida não corresponde à categoria fornecida")))
        }

        if(category=="ocorrencias" & relative_values==F & granularity=="year"){
          ifesle(typology=="all",
            df <- df |>
              dplyr::group_by(uf,evento,ano,municipio)|>
              dplyr::filter(categoria %in% {{category}})|>
              dplyr::summarize(total = sum(total, na.rm = T))|>
              tidyr::pivot_wider(
                names_from = evento,
                values_from = total),
          ifelse(typology == "Furto de veículo" | typology == "Roubo a instituição financeira" |
                 typology == "Roubo de carga" | typology == "Roubo de veículo",
                 df <- df |>
                   dplyr::group_by(uf,{{typology}},ano,municipio)|>
                   dplyr::filter(categoria %in% {{category}})|>
                   dplyr::filter(evento %in% {{typology}})|>
                   dplyr::summarize(total = sum(total, na.rm = T))|>
                   tidyr::pivot_wider(
                     names_from = evento,
                     values_from = total),
                 stop("A tipologia introduzida não corresponde à categoria fornecida")))
        }

        if(category=="arma de fogo" & relative_values==F & granularity=="month"){
          ifelse(typology=="all",
                 df <- df|>
                   dplyr::group_by(uf,categoria,evento,ano,mes,arma,municipio)|>
                   dplyr::filter(categoria %in% {{category}})|>
                   tidyr::pivot_wider(
                     names_from = c(evento,arma),
                     values_from = total),
                 ifelse(typology == "Arma de Fogo Apreendida",
                        df <- df|>
                          dplyr::group_by(uf,categoria,evento,ano,mes,municipio)|>
                          dplyr::filter(categoria %in% {{category}})|>
                          dplyr::filter(evento %in% {{typology}}) |>
                          tidyr::pivot_wider(
                            names_from = c(evento,arma),
                            values_from = total),
                        stop("A tipologia introduzida não corresponde à categoria fornecida")))
        }

        if(category=="arma de fogo" & relative_values==F & granularity=="year"){
          ifelse(typology=="all",
                 df <- df|>
                   dplyr::group_by(uf,categoria,evento,ano,arma,municipio)|>
                   dplyr::filter(categoria %in% {{category}})|>
                   dplyr::summarize(total=sum(total,na.rm = T))|>
                   tidyr::pivot_wider(
                     names_from = c(evento,arma),
                     values_from = total),
                 ifelse(typology == "Arma de Fogo Apreendida",
                        df <- df|>
                          dplyr::group_by(uf,categoria,evento,ano,mes,arma,municipio)|>
                          dplyr::filter(categoria %in% {{category}})|>
                          dplyr::filter(evento %in% {{typology}})|>
                          dplyr::summarize(total=sum(total,na.rm = T))|>
                          tidyr::pivot_wider(
                            names_from = c(arma,evento),
                            values_from = total),
                        stop("A tipologia introduzida não corresponde à categoria fornecida")))
        }

        if(category=="desaparecidos/localizados" & relative_values==F & granularity=="month"){
          ifelse(typology=="all",
                 df <- df|>
                   dplyr::group_by(uf,categoria,evento,ano,mes,faixa_etaria,municipio)|>
                   dplyr::filter(categoria %in% {{category}})|>
                   tidyr::pivot_wider(
                     names_from = c(evento,faixa_etaria),
                     values_from = c(feminino,masculino,nao_informado,total_vitimas)),
                 ifelse(typology == "Pessoa Desaparecida" | typology == "Pessoa Localizada",
                        df <- df|>
                          dplyr::group_by(uf,categoria,evento,ano,mes,faixa_etaria,municipio)|>
                          dplyr::filter(categoria %in% {{category}})|>
                          dplyr::filter(evento %in% {{typology}}) |>
                          tidyr::pivot_wider(
                            names_from = c(evento,faixa_etaria),
                            values_from = c(feminino,masculino,nao_informado,total_vitimas)),
                        stop("A tipologia introduzida não corresponde à categoria fornecida")))
        }

        if(category=="desaparecidos/localizados" & relative_values==F & granularity=="year"){
          ifelse(typology=="all",
                 df <- df|>
                   dplyr::group_by(uf,categoria,evento,ano,faixa_etaria,municipio)|>
                   dplyr::filter(categoria %in% {{category}})|>
                   dplyr::summarize(feminino = (sum(feminino, na.rm = T)),
                                    masculino = (sum(masculino, na.rm = T)),
                                    nao_informado = (sum(nao_informado, na.rm = T)),
                                    total_vitimas = (sum(total_vitimas, na.rm = T)))|>
                   tidyr::pivot_wider(
                     names_from = c(evento,faixa_etaria),
                     values_from = c(feminino,masculino,nao_informado,total_vitimas)),
                 ifelse(typology == "Pessoa Desaparecida" | typology == "Pessoa Localizada",
                        df <- df|>
                          dplyr::group_by(uf,categoria,evento,ano,faixa_etaria,municipio)|>
                          dplyr::filter(categoria %in% {{category}})|>
                          dplyr::filter(evento %in% {{typology}})|>
                          dplyr::summarize(feminino = sum(feminino, na.rm = T),
                                           masculino = sum(masculino, na.rm = T),
                                           nao_informado =sum(nao_informado, na.rm = T),
                                           total_vitimas = sum(total_vitimas, na.rm = T))|>
                          tidyr::pivot_wider(
                            names_from = c(evento,faixa_etaria),
                            values_from = c(feminino,masculino,nao_informado,total_vitimas)),
                        stop("A tipologia introduzida não corresponde à categoria fornecida")))
        }

        if(category=="mandado de prisao cumprido" & relative_values==F & granularity=="month"){
          ifelse(typology=="all",
                 df <- df|>
                   dplyr::group_by(uf,categoria,evento,ano,mes,municipio)|>
                   dplyr::filter(categoria %in% {{category}})|>
                   tidyr::pivot_wider(
                     names_from = evento,
                     values_from = total),
                 ifelse(typology == "Mandado de prisão cumprido",
                        df <- df|>
                          dplyr::group_by(uf,categoria,evento,ano,mes,municipio)|>
                          dplyr::filter(categoria %in% {{category}})|>
                          dplyr::filter(evento %in% {{typology}}) |>
                          tidyr::pivot_wider(
                            names_from = evento,
                            values_from = total),
                        stop("A tipologia introduzida não corresponde à categoria fornecida")))
        }

        if(category=="mandado de prisao cumprido" & relative_values==F & granularity=="year"){
          ifelse(typology=="all",
                 df <- df|>
                   dplyr::group_by(uf,categoria,evento,ano,municipio)|>
                   dplyr::filter(categoria %in% {{category}})|>
                   dplyr::summarize(total=sum(total,na.rm = T))|>
                   tidyr::pivot_wider(
                     names_from = evento,
                     values_from = total),
                 ifelse(typology == "Mandado de prisão cumprido",
                        df <- df|>
                          dplyr::group_by(uf,categoria,evento,ano,municipio)|>
                          dplyr::filter(categoria %in% {{category}})|>
                          dplyr::filter(evento %in% {{typology}})|>
                          dplyr::summarize(total=sum(total,na.rm = T))|>
                          tidyr::pivot_wider(
                            names_from = evento,
                            values_from = total),
                        stop("A tipologia introduzida não corresponde à categoria fornecida")))
        }

        if(category=="profissionais de seguranca" & relative_values==F & granularity=="month"){
          ifelse(typology=="all",
                 df <- df|>
                   dplyr::group_by(uf,categoria,evento,ano,mes,agente,municipio)|>
                   dplyr::filter(categoria %in% {{category}})|>
                   tidyr::pivot_wider(
                     names_from = c(evento,agente),
                     values_from = c(feminino,masculino,nao_informado,total_vitimas)),
                 ifelse(typology == "Morte de Agente do Estado" | typology == "Suicídio de Agente do Estado",
                        df <- df|>
                          dplyr::group_by(uf,categoria,evento,ano,mes,agente,municipio)|>
                          dplyr::filter(categoria %in% {{category}})|>
                          dplyr::filter(evento %in% {{typology}}) |>
                          tidyr::pivot_wider(
                            names_from = c(evento,agente),
                            values_from = c(feminino,masculino,nao_informado,total_vitimas)),
                        stop("A tipologia introduzida não corresponde à categoria fornecida")))
        }

        if(category=="profissionais de seguranca" & relative_values==F & granularity=="year"){
          ifelse(typology=="all",
                 df <- df|>
                   dplyr::group_by(uf,categoria,evento,ano,agente,municipio)|>
                   dplyr::filter(categoria %in% {{category}})|>
                   dplyr::summarize(feminino = sum(feminino, na.rm = T),
                                    masculino = sum(masculino, na.rm = T),
                                    nao_informado =sum(nao_informado, na.rm = T),
                                    total_vitimas = sum(total_vitimas, na.rm = T))|>
                   tidyr::pivot_wider(
                     names_from = c(evento,agente),
                     values_from = c(feminino,masculino,nao_informado,total_vitimas)),
                 ifelse(typology == "Morte de Agente do Estado" | typology == "Suicídio de Agente do Estado",
                        df <- df|>
                          dplyr::group_by(uf,categoria,evento,ano,agente,municipio)|>
                          dplyr::filter(categoria %in% {{category}})|>
                          dplyr::filter(evento %in% {{typology}})|>
                          dplyr::summarize(feminino = sum(feminino, na.rm = T),
                                           masculino = sum(masculino, na.rm = T),
                                           nao_informado =sum(nao_informado, na.rm = T),
                                           total_vitimas = sum(total_vitimas, na.rm = T))|>
                          tidyr::pivot_wider(
                            names_from = c(evento,agente),
                            values_from = c(feminino,masculino,nao_informado,total_vitimas)),
                        stop("A tipologia introduzida não corresponde à categoria fornecida")))
        }

        if(category=="bombeiros" & relative_values==F & granularity=="month"){
          ifelse(typology=="all",
                 df <- df|>
                   dplyr::group_by(uf,categoria,evento,ano,mes,municipio)|>
                   dplyr::filter(categoria %in% {{category}})|>
                   tidyr::pivot_wider(
                     names_from = evento,
                     values_from = total),
                 ifelse(typology == "Atendimento pré-hospitalar" | typology == "Busca e salvamento" |
                          typology == "Combate a incêndios" | typology == "Emissão de Alvarás de licença" |
                          typology == "Realização de vistorias",
                        df <- df|>
                          dplyr::group_by(uf,categoria,evento,ano,mes,municipio)|>
                          dplyr::filter(categoria %in% {{category}})|>
                          dplyr::filter(evento %in% {{typology}}) |>
                          tidyr::pivot_wider(
                            names_from = evento,
                            values_from = total),
                        stop("A tipologia introduzida não corresponde à categoria fornecida")))
        }

        if(category=="bombeiros" & relative_values==F & granularity=="year"){
          ifelse(typology=="all",
                 df <- df|>
                   dplyr::group_by(uf,categoria,evento,ano,municipio)|>
                   dplyr::filter(categoria %in% {{category}})|>
                   dplyr::summarize(total=sum(total,na.rm = T))|>
                   tidyr::pivot_wider(
                     names_from = evento,
                     values_from = total),
                 ifelse(typology == "Atendimento pré-hospitalar" | typology == "Busca e salvamento" |
                          typology == "Combate a incêndios" | typology == "Emissão de Alvarás de licença" |
                          typology == "Realização de vistorias",
                        df <- df|>
                          dplyr::group_by(uf,categoria,evento,ano,municipio)|>
                          dplyr::filter(categoria %in% {{category}})|>
                          dplyr::filter(evento %in% {{typology}})|>
                          dplyr::summarize(total=sum(total,na.rm = T))|>
                          tidyr::pivot_wider(
                            names_from = evento,
                            values_from = total),
                        stop("A tipologia introduzida não corresponde à categoria fornecida")))
        }

      # Relative_values = T -----------------------

      if(relative_values==T & granularity=="month"){

        load("data/pop.rda")

        col_names <- names(pop)

      pop_long <- pop |>
        tidyr::pivot_longer(
          cols = col_names[2:28],
          names_to = "uf",
          values_to = "populacao")|>
        dplyr::select(-TOTAL)|>
        dplyr::mutate(mes=as.factor(mes))

      df <- df |>
        dplyr::mutate(mes=as.factor(mes))|>
        dplyr::inner_join(pop_long, by = c("uf", "ano", "mes"))

         if(category=="vitimas"){
            ifelse(typology=="all",
                   df <- df|>
                     dplyr::group_by(uf,categoria,evento,ano,mes,populacao)|>
                     dplyr::filter(categoria %in% {{category}})|>
                     dplyr::summarize(feminino=sum(feminino,na.rm = T),
                                      masculino=sum(masculino,na.rm = T),
                                      nao_informado=sum(nao_informado,na.rm = T),
                                      total_vitimas=sum(total_vitimas,na.rm = T))|>
                     dplyr::mutate(vitimas_100k_hab = round(total_vitimas / populacao * 100000, 2))|>
                     tidyr::pivot_wider(
                       names_from = evento,
                       values_from = c(feminino,masculino,nao_informado,total_vitimas,vitimas_100k_hab)),
                   ifelse(typology == "Feminicídio" | typology == "Homicídio doloso" |
                            typology == "Lesão corporal seguida de morte" |
                            typology == "Morte no trânsito ou em decorrência dele (exceto homicídio doloso)" |
                            typology == "Mortes a esclarecer (sem indício de crime)" | typology == "Roubo seguido de morte (latrocínio)" |
                            typology == "Suicídio" | typology == "Tentativa de homicídio" | typology == "Estupro" |
                            typology == "Morte por intervenção de Agente do Estado",
                          df <- df|>
                            dplyr::group_by(uf,categoria,evento,ano,mes)|>
                            dplyr::filter(categoria %in% {{category}})|>
                            dplyr::filter(evento %in% {{typology}}) |>
                            dplyr::summarize(feminino=sum(feminino,na.rm = T),
                                             masculino=sum(masculino,na.rm = T),
                                             nao_informado=sum(nao_informado,na.rm = T),
                                             total_vitimas=sum(total_vitimas,na.rm = T))|>
                            dplyr::mutate(vitimas_100k_hab = round(total_vitimas / populacao * 100000, 2))|>
                            tidyr::pivot_wider(
                              names_from = evento,
                              values_from = c(feminino,masculino,nao_informado,total_vitimas)),
                          stop("A tipologia introduzida não corresponde à categoria fornecida")))
          }

          if(category=="drogas"){
            ifelse(typology=="all",
                   df <- df|>
                     dplyr::group_by(uf,categoria,evento,ano,mes,municipio)|>
                     dplyr::filter(categoria %in% {{category}})|>
                     dplyr::summarize(total = sum(total_casos, na.rm = T),
                                      total_peso = sum(total_peso,na.rm = T))|>
                     dplyr::mutate(casos_100k_hab = round(total / populacao * 100000, 2))|>
                     dplyr::mutate(apreensao_100k_hab = round(total_peso / populacao * 100000, 2))|>
                     tidyr::pivot_wider(
                       names_from = evento,
                       values_from = c(total_casos,total_peso,casos_100k_hab,apreensao_100k_hab)),
                   ifelse(typology == "Apreensão de Cocaína" | typology == "Apreensão de Maconha" |
                            typology == "Tráfico de drogas",
                          df <- df|>
                            dplyr::group_by(uf,categoria,evento,ano,mes,municipio)|>
                            dplyr::filter(categoria %in% {{category}})|>
                            dplyr::filter(evento %in% {{typology}}) |>
                            dplyr::summarize(total = sum(total_casos, na.rm = T),
                                             total_peso = sum(total_peso,na.rm = T))|>
                            dplyr::mutate(casos_100k_hab = round(total / populacao * 100000, 2))|>
                            dplyr::mutate(apreensao_100k_hab = round(total_peso / populacao * 100000, 2))|>
                            tidyr::pivot_wider(
                              names_from = evento,
                              values_from = c(total_casos,total_peso,casos_100k_hab,apreensao_100k_hab)),
                          stop("A tipologia introduzida não corresponde à categoria fornecida")))
          }

          if(category=="ocorrencias"){
            ifelse(typology=="all",
                   df <- df |>
                     dplyr::group_by(uf,evento,ano,mes,municipio)|>
                     dplyr::filter(categoria %in% {{category}})|>
                     dplyr::summarize(total = sum(total, na.rm = T))|>
                     dplyr::mutate(ocorrencia_100k_hab = round(total / populacao * 100000, 2))|>
                     tidyr::pivot_wider(
                       names_from = evento,
                       values_from = total,ocorrencia_100k_hab),
                   ifelse(typology == "Furto de veículo" | typology == "Roubo a instituição financeira" |
                            typology == "Roubo de carga" | typology == "Roubo de veículo",
                          df <- df |>
                            dplyr::group_by(uf,{{typology}},ano,mes,municipio)|>
                            dplyr::filter(categoria %in% {{category}})|>
                            dplyr::filter(evento %in% {{typology}})|>
                            dplyr::summarize(total = sum(total, na.rm = T))|>
                            dplyr::mutate(ocorrencia_100k_hab = round(total / populacao * 100000, 2))|>
                            tidyr::pivot_wider(
                              names_from = evento,
                              values_from = total,ocorrencia_100k_hab),
                          stop("A tipologia introduzida não corresponde à categoria fornecida")))
          }

          if(category=="arma de fogo"){
            ifelse(typology=="all",
                   df <- df|>
                     dplyr::group_by(uf,categoria,evento,ano,mes,arma,municipio)|>
                     dplyr::filter(categoria %in% {{category}})|>
                     dplyr::summarize(total = sum(total, na.rm = T))|>
                     dplyr::mutate(arma_apreend_100k_hab = round(total / populacao * 100000, 2))|>
                     tidyr::pivot_wider(
                       names_from = c(evento,arma),
                       values_from = total,arma_apreend_100k_hab),
                   ifelse(typology == "Arma de Fogo Apreendida",
                          df <- df|>
                            dplyr::group_by(uf,categoria,evento,ano,mes,municipio)|>
                            dplyr::filter(categoria %in% {{category}})|>
                            dplyr::filter(evento %in% {{typology}}) |>
                            dplyr::summarize(total = sum(total, na.rm = T))|>
                            dplyr::mutate(arma_apreend_100k_hab = round(total / populacao * 100000, 2))|>
                            tidyr::pivot_wider(
                              names_from = c(evento,arma),
                              values_from = total,arma_apreend_100k_hab),
                          stop("A tipologia introduzida não corresponde à categoria fornecida")))
          }

          if(category=="desaparecidos/localizados"){
            ifelse(typology=="all",
                   df <- df|>
                     dplyr::group_by(uf,categoria,evento,ano,mes,faixa_etaria,municipio)|>
                     dplyr::filter(categoria %in% {{category}})|>
                     dplyr::filter({categoria} %in% {{category}})|>
                     dplyr::summarize(total_vitimas = (sum(total_vitimas, na.rm = T)))|>
                     dplyr::mutate(vitimas_100k_hab = round(total_vitimas / populacao * 100000, 2))|>
                     tidyr::pivot_wider(
                       names_from = c(evento,faixa_etaria),
                       values_from = c(feminino,masculino,nao_informado,total_vitimas),vitimas_100k_hab),
                   ifelse(typology == "Pessoa Desaparecida" | typology == "Pessoa Localizada",
                          df <- df|>
                            dplyr::group_by(uf,categoria,evento,ano,mes,faixa_etaria,municipio)|>
                            dplyr::filter(categoria %in% {{category}})|>
                            dplyr::filter(evento %in% {{typology}}) |>
                            dplyr::summarize(total_vitimas = (sum(total_vitimas, na.rm = T)))|>
                            dplyr::mutate(vitimas_100k_hab = round(total_vitimas / populacao * 100000, 2))|>
                            tidyr::pivot_wider(
                              names_from = c(evento,faixa_etaria),
                              values_from = c(feminino,masculino,nao_informado,total_vitimas),vitimas_100k_hab),
                          stop("A tipologia introduzida não corresponde à categoria fornecida")))
          }

          if(category=="mandado de prisao cumprido"){
            ifelse(typology=="all",
                   df <- df|>
                     dplyr::group_by(uf,categoria,evento,ano,mes,municipio)|>
                     dplyr::filter(categoria %in% {{category}})|>
                     dplyr::summarize(total)|>
                     dplyr::mutate(total_100k_hab = round(total / populacao * 100000, 2))|>
                     tidyr::pivot_wider(
                       names_from = evento,
                       values_from = total,total_100k_hab),
                   ifelse(typology == "Mandado de prisão cumprido",
                          df <- df|>
                            dplyr::group_by(uf,categoria,evento,ano,mes,municipio)|>
                            dplyr::filter(categoria %in% {{category}})|>
                            dplyr::filter(evento %in% {{typology}}) |>
                            dplyr::summarize(total)|>
                            dplyr::mutate(total_100k_hab = round(total / populacao * 100000, 2))|>
                            tidyr::pivot_wider(
                              names_from = evento,
                              values_from = total,total_100k_hab),
                          stop("A tipologia introduzida não corresponde à categoria fornecida")))
          }

          if(category=="profissionais de seguranca"){
            ifelse(typology=="all",
                   df <- df|>
                     dplyr::group_by(uf,categoria,evento,ano,mes,agente,municipio)|>
                     dplyr::filter(categoria %in% {{category}})|>
                     dplyr::summarize(total_fem = (sum(feminino, na.rm = T)),
                                      total_masc = (sum(masculino, na.rm = T)),
                                      total_n_inf = (sum(nao_informado, na.rm = T)),
                                      total = (sum(total_vitimas, na.rm = T)))|>
                     dplyr::mutate(vitimas_100k_hab = round(total / populacao * 100000, 2))|>
                     tidyr::pivot_wider(
                       names_from = c(evento,agente),
                       values_from = c(feminino,masculino,nao_informado,total_vitimas,vitimas_100k_hab)),
                   ifelse(typology == "Morte de Agente do Estado" | typology == "Suicídio de Agente do Estado",
                          df <- df|>
                            dplyr::group_by(uf,categoria,evento,ano,mes,agente,municipio)|>
                            dplyr::filter(categoria %in% {{category}})|>
                            dplyr::filter(evento %in% {{typology}}) |>
                            dplyr::summarize(total_fem = (sum(feminino, na.rm = T)),
                                             total_masc = (sum(masculino, na.rm = T)),
                                             total_n_inf = (sum(nao_informado, na.rm = T)),
                                             total = (sum(total_vitimas, na.rm = T)))|>
                            dplyr::mutate(vitimas_100k_hab = round(total / populacao * 100000, 2))|>
                            tidyr::pivot_wider(
                              names_from = c(evento,agente),
                              values_from = c(feminino,masculino,nao_informado,total_vitimas,vitimas_100k_hab)),
                          stop("A tipologia introduzida não corresponde à categoria fornecida")))
          }

          if(category=="bombeiros"){
            ifelse(typology=="all",
                   df <- df|>
                     dplyr::group_by(uf,categoria,evento,ano,mes,municipio)|>
                     dplyr::filter(categoria %in% {{category}})|>
                     dplyr::summarize(total=sum(total,na.rm = T))|>
                     dplyr::mutate(total_100k_hab = round(total / populacao * 100000, 2))|>
                     tidyr::pivot_wider(
                       names_from = evento,
                       values_from = total,total_100k_hab),
                   ifelse(typology == "Atendimento pré-hospitalar" | typology == "Busca e salvamento" |
                            typology == "Combate a incêndios" | typology == "Emissão de Alvarás de licença" |
                            typology == "Realização de vistorias",
                          df <- df|>
                            dplyr::group_by(uf,categoria,evento,ano,mes,municipio)|>
                            dplyr::filter(categoria %in% {{category}})|>
                            dplyr::filter(evento %in% {{typology}}) |>
                            dplyr::summarize(total=sum(total,na.rm = T))|>
                            dplyr::mutate(total_100k_hab = round(total / populacao * 100000, 2))|>
                            tidyr::pivot_wider(
                              names_from = evento,
                              values_from = total,total_100k_hab),
                          stop("A tipologia introduzida não corresponde à categoria fornecida")))
          }
      }

      if (relative_values==T & granularity == "year"){

        load("data/pop_anual.rda")

        df <- df |>
          dplyr::mutate(ano=as.character(ano))|>
          dplyr::select(-mes)|>
          dplyr::left_join(pop_anual, by = c("uf", "ano"))

        if(city!=F){
          stop("To use granularity = 'year', plese set city = F")
        }

        if(category=="vitimas"){
          ifelse(typology=="all",
                 df <- df|>
                   dplyr::group_by(uf,categoria,evento,ano,populacao_anual)|>
                   dplyr::filter(categoria %in% {{category}})|>
                   dplyr::summarize(feminino=sum(feminino,na.rm = T),
                                    masculino=sum(masculino,na.rm = T),
                                    nao_informado=sum(nao_informado,na.rm = T),
                                    total_vitimas=sum(total_vitimas,na.rm = T))|>
                   dplyr::mutate(vitimas_100k_hab = round(total_vitimas / populacao_anual * 100000, 2))|>
                   tidyr::pivot_wider(
                     names_from = evento,
                     values_from = c(feminino,masculino,nao_informado,total_vitimas,vitimas_100k_hab)),
                 ifelse(typology == "Feminicídio" | typology == "Homicídio doloso" |
                          typology == "Lesão corporal seguida de morte" |
                          typology == "Morte no trânsito ou em decorrência dele (exceto homicídio doloso)" |
                          typology == "Mortes a esclarecer (sem indício de crime)" | typology == "Roubo seguido de morte (latrocínio)" |
                          typology == "Suicídio" | typology == "Tentativa de homicídio" | typology == "Estupro" |
                          typology == "Morte por intervenção de Agente do Estado",
                        df <- df|>
                          dplyr::group_by(uf,categoria,evento,ano,populacao_anual)|>
                          dplyr::filter(categoria %in% {{category}})|>
                          dplyr::filter(evento %in% {{typology}}) |>
                          dplyr::summarize(feminino=sum(feminino,na.rm = T),
                                           masculino=sum(masculino,na.rm = T),
                                           nao_informado=sum(nao_informado,na.rm = T),
                                           total_vitimas=sum(total_vitimas,na.rm = T))|>
                          dplyr::mutate(vitimas_100k_hab = round(total_vitimas / populacao_anual * 100000, 2))|>
                          tidyr::pivot_wider(
                            names_from = evento,
                            values_from = c(feminino,masculino,nao_informado,total_vitimas)),
                        stop("A tipologia introduzida não corresponde à categoria fornecida")))
        }

        if(category=="drogas"){
          ifelse(typology=="all",
                 df <- df|>
                   dplyr::group_by(uf,categoria,evento,ano,populacao_anual,municipio)|>
                   dplyr::filter(categoria %in% {{category}})|>
                   dplyr::summarize(total = sum(total_casos, na.rm = T),
                                    total_peso = sum(total_peso,na.rm = T))|>
                   dplyr::mutate(casos_100k_hab = round(total / populacao_anual * 100000, 2))|>
                   dplyr::mutate(apreensao_100k_hab = round(total_peso / populacao_anual * 100000, 2))|>
                   tidyr::pivot_wider(
                     names_from = evento,
                     values_from = c(total_casos,total_peso,casos_100k_hab,apreensao_100k_hab)),
                 ifelse(typology == "Apreensão de Cocaína" | typology == "Apreensão de Maconha" |
                          typology == "Tráfico de drogas",
                        df <- df|>
                          dplyr::group_by(uf,categoria,evento,ano,populacao_anual,municipio)|>
                          dplyr::filter(categoria %in% {{category}})|>
                          dplyr::filter(evento %in% {{typology}}) |>
                          dplyr::summarize(total = sum(total_casos, na.rm = T),
                                           total_peso = sum(total_peso,na.rm = T))|>
                          dplyr::mutate(casos_100k_hab = round(total / populacao_anual * 100000, 2))|>
                          dplyr::mutate(apreensao_100k_hab = round(total_peso / populacao_anual * 100000, 2))|>
                          tidyr::pivot_wider(
                            names_from = evento,
                            values_from = c(total_casos,total_peso,casos_100k_hab,apreensao_100k_hab)),
                        stop("A tipologia introduzida não corresponde à categoria fornecida")))
        }

        if(category=="ocorrencias"){
          ifelse(typology=="all",
                 df <- df |>
                   dplyr::group_by(uf,evento,ano,populacao_anual,municipio)|>
                   dplyr::filter(categoria %in% {{category}})|>
                   dplyr::summarize(total = sum(total, na.rm = T))|>
                   dplyr::mutate(ocorrencia_100k_hab = round(total / populacao_anual * 100000, 2))|>
                   tidyr::pivot_wider(
                     names_from = evento,
                     values_from = total,ocorrencia_100k_hab),
                 ifelse(typology == "Furto de veículo" | typology == "Roubo a instituição financeira" |
                          typology == "Roubo de carga" | typology == "Roubo de veículo",
                        df <- df |>
                          dplyr::group_by(uf,{{typology}},ano,populacao_anual,municipio)|>
                          dplyr::filter(categoria %in% {{category}})|>
                          dplyr::filter(evento %in% {{typology}})|>
                          dplyr::summarize(total = sum(total, na.rm = T))|>
                          dplyr::mutate(ocorrencia_100k_hab = round(total / populacao_anual * 100000, 2))|>
                          tidyr::pivot_wider(
                            names_from = evento,
                            values_from = total,ocorrencia_100k_hab),
                        stop("A tipologia introduzida não corresponde à categoria fornecida")))
        }

        if(category=="arma de fogo"){
          ifelse(typology=="all",
                 df <- df|>
                   dplyr::group_by(uf,categoria,evento,ano,populacao_anual,arma,municipio)|>
                   dplyr::filter(categoria %in% {{category}})|>
                   dplyr::summarize(total = sum(total, na.rm = T))|>
                   dplyr::mutate(arma_apreend_100k_hab = round(total / populacao_anual * 100000, 2))|>
                   tidyr::pivot_wider(
                     names_from = c(evento,arma),
                     values_from = total,arma_apreend_100k_hab),
                 ifelse(typology == "Arma de Fogo Apreendida",
                        df <- df|>
                          dplyr::group_by(uf,categoria,evento,ano,populacao_anual,municipio)|>
                          dplyr::filter(categoria %in% {{category}})|>
                          dplyr::filter(evento %in% {{typology}}) |>
                          dplyr::summarize(total = sum(total, na.rm = T))|>
                          dplyr::mutate(arma_apreend_100k_hab = round(total / populacao_anual * 100000, 2))|>
                          tidyr::pivot_wider(
                            names_from = c(evento,arma),
                            values_from = total,arma_apreend_100k_hab),
                        stop("A tipologia introduzida não corresponde à categoria fornecida")))
        }

        if(category=="desaparecidos/localizados"){
          ifelse(typology=="all",
                 df <- df|>
                   dplyr::group_by(uf,categoria,evento,ano,populacao_anual,faixa_etaria,municipio)|>
                   dplyr::filter(categoria %in% {{category}})|>
                   dplyr::filter({categoria} %in% {{category}})|>
                   dplyr::summarize(total_vitimas = (sum(total_vitimas, na.rm = T)))|>
                   dplyr::mutate(vitimas_100k_hab = round(total_vitimas / populacao_anual * 100000, 2))|>
                   tidyr::pivot_wider(
                     names_from = c(evento,faixa_etaria),
                     values_from = c(feminino,masculino,nao_informado,total_vitimas),vitimas_100k_hab),
                 ifelse(typology == "Pessoa Desaparecida" | typology == "Pessoa Localizada",
                        df <- df|>
                          dplyr::group_by(uf,categoria,evento,ano,populacao_anual,faixa_etaria,municipio)|>
                          dplyr::filter(categoria %in% {{category}})|>
                          dplyr::filter(evento %in% {{typology}}) |>
                          dplyr::summarize(total_vitimas = (sum(total_vitimas, na.rm = T)))|>
                          dplyr::mutate(vitimas_100k_hab = round(total_vitimas / populacao_anual * 100000, 2))|>
                          tidyr::pivot_wider(
                            names_from = c(evento,faixa_etaria),
                            values_from = c(feminino,masculino,nao_informado,total_vitimas),vitimas_100k_hab),
                        stop("A tipologia introduzida não corresponde à categoria fornecida")))
        }

        if(category=="mandado de prisao cumprido"){
          ifelse(typology=="all",
                 df <- df|>
                   dplyr::group_by(uf,categoria,evento,ano,populacao_anual,municipio)|>
                   dplyr::filter(categoria %in% {{category}})|>
                   dplyr::summarize(total)|>
                   dplyr::mutate(total_100k_hab = round(total / populacao_anual * 100000, 2))|>
                   tidyr::pivot_wider(
                     names_from = evento,
                     values_from = total,total_100k_hab),
                 ifelse(typology == "Mandado de prisão cumprido",
                        df <- df|>
                          dplyr::group_by(uf,categoria,evento,ano,populacao_anual,municipio)|>
                          dplyr::filter(categoria %in% {{category}})|>
                          dplyr::filter(evento %in% {{typology}}) |>
                          dplyr::summarize(total)|>
                          dplyr::mutate(total_100k_hab = round(total / populacao_anual * 100000, 2))|>
                          tidyr::pivot_wider(
                            names_from = evento,
                            values_from = total,total_100k_hab),
                        stop("A tipologia introduzida não corresponde à categoria fornecida")))
        }

        if(category=="profissionais de seguranca"){
          ifelse(typology=="all",
                 df <- df|>
                   dplyr::group_by(uf,categoria,evento,ano,populacao_anual,agente,municipio)|>
                   dplyr::filter(categoria %in% {{category}})|>
                   dplyr::summarize(total_fem = (sum(feminino, na.rm = T)),
                                    total_masc = (sum(masculino, na.rm = T)),
                                    total_n_inf = (sum(nao_informado, na.rm = T)),
                                    total = (sum(total_vitimas, na.rm = T)))|>
                   dplyr::mutate(vitimas_100k_hab = round(total / populacao_anual * 100000, 2))|>
                   tidyr::pivot_wider(
                     names_from = c(evento,agente),
                     values_from = c(feminino,masculino,nao_informado,total_vitimas,vitimas_100k_hab)),
                 ifelse(typology == "Morte de Agente do Estado" | typology == "Suicídio de Agente do Estado",
                        df <- df|>
                          dplyr::group_by(uf,categoria,evento,ano,populacao_anual,agente,municipio)|>
                          dplyr::filter(categoria %in% {{category}})|>
                          dplyr::filter(evento %in% {{typology}}) |>
                          dplyr::summarize(total_fem = (sum(feminino, na.rm = T)),
                                           total_masc = (sum(masculino, na.rm = T)),
                                           total_n_inf = (sum(nao_informado, na.rm = T)),
                                           total = (sum(total_vitimas, na.rm = T)))|>
                          dplyr::mutate(vitimas_100k_hab = round(total / populacao_anual * 100000, 2))|>
                          tidyr::pivot_wider(
                            names_from = c(evento,agente),
                            values_from = c(feminino,masculino,nao_informado,total_vitimas,vitimas_100k_hab)),
                        stop("A tipologia introduzida não corresponde à categoria fornecida")))
        }

        if(category=="bombeiros"){
          ifelse(typology=="all",
                 df <- df|>
                   dplyr::group_by(uf,categoria,evento,ano,populacao_anual,municipio)|>
                   dplyr::filter(categoria %in% {{category}})|>
                   dplyr::summarize(total=sum(total,na.rm = T))|>
                   dplyr::mutate(total_100k_hab = round(total / populacao_anual * 100000, 2))|>
                   tidyr::pivot_wider(
                     names_from = evento,
                     values_from = total,total_100k_hab),
                 ifelse(typology == "Atendimento pré-hospitalar" | typology == "Busca e salvamento" |
                          typology == "Combate a incêndios" | typology == "Emissão de Alvarás de licença" |
                          typology == "Realização de vistorias",
                        df <- df|>
                          dplyr::group_by(uf,categoria,evento,ano,populacao_anual,municipio)|>
                          dplyr::filter(categoria %in% {{category}})|>
                          dplyr::filter(evento %in% {{typology}}) |>
                          dplyr::summarize(total=sum(total,na.rm = T))|>
                          dplyr::mutate(total_100k_hab = round(total / populacao_anual * 100000, 2))|>
                          tidyr::pivot_wider(
                            names_from = evento,
                            values_from = total,total_100k_hab),
                        stop("A tipologia introduzida não corresponde à categoria fornecida")))
        }
      }
    }

    message("Query completed.")

  old <- options(timeout = 60)
  on.exit(options(old))

  return(df)
}
