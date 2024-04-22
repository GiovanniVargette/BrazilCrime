## code to prepare `DATASET` dataset goes here

pop <- readxl::read_xlsx("C:/Users/giovannivargette/BrazilCrime/data-raw/pop_projetada_mensal_dia_15.xlsx")|>
  janitor::clean_names()|>
  dplyr::rename(TOTAL=brasil,RO=rondonia,AC=acre,AM=amazonas,RR=roraima,PA=para,AP=amapa,TO=tocantins,MA=maranhao,PI=piaui,
                CE=ceara,RN=rio_grande_do_norte,PB=paraiba,PE=pernambuco,AL=alagoas,SE=sergipe,BA=bahia,MG=minas_gerais,
                ES=espirito_santo,RJ=rio_de_janeiro,SP=sao_paulo,PR=parana,SC=santa_catarina,RS=rio_grande_do_sul,MS=mato_grosso_do_sul,
                MT=mato_grosso,GO=goias,DF=distrito_federal)|>
  dplyr::filter(data >= 43480 & data<=47467 )|>
  dplyr::mutate(ano = rep(2019:2029,each=12))|>
  dplyr::mutate(mes = gl(12,1,length=132))|>
  dplyr::select(!data)

usethis::use_data(pop, compress = "xz", overwrite = TRUE)

pop_anual <- readxl::read_xlsx("C:/Users/giovannivargette/BrazilCrime/data-raw/pop_projetada_anual.xlsx")|>
  dplyr::select(-cod)|>
  dplyr::filter(uf!="Brasil")|>
  dplyr::filter(ano>2018 & ano <2030)|>
  dplyr::mutate(uf = dplyr::case_when(
    uf=="Rondônia" ~ "RO",uf=="Acre"~"AC",uf=="Amazonas"~"AM",uf=="Roraima"~"RR",uf=="Pará"~"PA",uf=="Amapá"~"AP",
    uf=="Tocantins"~"TO",uf=="Maranhão"~"MA",uf=="Piauí"~"PI",uf=="Ceará"~"CE",uf=="Rio Grande do Norte"~"RN",uf=="Paraíba"~"PB",
    uf=="Pernambuco"~"PE",uf=="Alagoas"~"AL",uf=="Sergipe"~"SE",uf=="Bahia"~"BA",uf=="Minas Gerais"~"MG",uf=="Espírito Santo"~"ES",
    uf=="Rio de Janeiro"~"RJ",uf=="São Paulo"~"SP",uf=='Paraná'~'PR',uf=='Santa Catarina'~"SC",uf=='Rio Grande do Sul'~'RS',
    uf=="Mato Grosso do Sul"~'MS',uf=="Mato Grosso"~'MT',uf=="Goiás"~"GO",uf=="Distrito Federal"~"DF"))|>
  dplyr::rename(populacao_anual=populacao)

usethis::use_data(pop_anual, compress = "xz", overwrite = TRUE)

