


# Conversão para lista ----------------------------------------------------

abjutils::chrome_to_body(
  "__RequestVerificationToken: AlJVY4p4ujrcH-pMuCAtC6qSTUurFj0hn_pPd0SDzBWFRxkE0TVr-8_fFhJArQ_WGYAQCrMfiyecAEV0DfnWKKuzSbc-uJ28vsHjgeDeyZE1
  Escolha: 2
  DataInicial: 01/01/2021
  DataFinal: 31/12/2021
  XMaisViajados: 500
  IdOrgao: 0
  Classificacao: a
  TipoViagem: TD"
)


# Função para o Scraping --------------------------------------------------

diarias_desejadas <- function(ano) {

  # imitação para pesquisa das viagens feitas dentro do estado do RN (internas)

  body_rn <- list(
    "__RequestVerificationToken" = "AlJVY4p4ujrcH-pMuCAtC6qSTUurFj0hn_pPd0SDzBWFRxkE0TVr-8_fFhJArQ_WGYAQCrMfiyecAEV0DfnWKKuzSbc-uJ28vsHjgeDeyZE1",
    "Escolha" = "2",
    "DataInicial" = paste0("01/01/", ano),
    "DataFinal" = paste0("31/12/", ano),
    "XMaisViajados" = "6500",
    "IdOrgao" = "0",
    "Classificacao" = "b",
    "TipoViagem" = "RN")

  # imitação para pesquisa das viagens feitas para fora estado do RN (externas)

  body_ou <- list(
    "__RequestVerificationToken" = "AlJVY4p4ujrcH-pMuCAtC6qSTUurFj0hn_pPd0SDzBWFRxkE0TVr-8_fFhJArQ_WGYAQCrMfiyecAEV0DfnWKKuzSbc-uJ28vsHjgeDeyZE1",
    "Escolha" = "2",
    "DataInicial" = paste0("01/01/", ano),
    "DataFinal" = paste0("31/12/", ano),
    "XMaisViajados" = "6500",
    "IdOrgao" = "0",
    "Classificacao" = "b",
    "TipoViagem" = "OU")

  u_diarias <- "http://servicos.searh.rn.gov.br/searh/Diaria/Diaria"

  # Requisição dos dados das viagens internas e externas

  r_diarias_rn <- httr::POST(u_diarias, body = body_rn)

  r_diarias_ou <- httr::POST(u_diarias, body = body_ou)

  # Fluxo condicional para evitar erros na aquisição dos dados

  if(r_diarias_rn$status_code == 200) {

    # Aquisição dos dados

    tabela_diarias_bruta_rn <- r_diarias_rn |>
      xml2::read_html() |>
      xml2::xml_find_all("//table//tr//td") |>
      xml2::xml_text() |>
      stringr::str_squish()

    # Primeiras modificações nos dados

    tabela_diarias_modificada_rn <- tabela_diarias_bruta_rn |>
      c() |>
      matrix(ncol = 9, byrow = T) |>
      tibble::as_tibble() |>
      dplyr::select(-V4) |>
      dplyr::rename(
        ID = V1,
        CPF = V2,
        Nome = V3,
        Cargo_Funcao = V5,
        Orgao = V6,
        PCDs = V7,
        Diarias = V8,
        Valor = V9
      ) |>
      dplyr::mutate(
        Tipo_de_Viagem = "Interna",
        Ano = ano
      )

  } else {

    tabela_diarias_modificada_rn <- NULL

  }

  if(r_diarias_ou$status_code == 200) {

    tabela_diarias_bruta_ou <- r_diarias_ou |>
      xml2::read_html() |>
      xml2::xml_find_all("//table//tr//td") |>
      xml2::xml_text() |>
      stringr::str_squish()

    tabela_diarias_modificada_ou <- tabela_diarias_bruta_ou |>
      c() |>
      matrix(ncol = 9, byrow = T) |>
      tibble::as_tibble() |>
      dplyr::select(-V4) |>
      dplyr::rename(
        ID = V1,
        CPF = V2,
        Nome = V3,
        Cargo_Funcao = V5,
        Orgao = V6,
        PCDs = V7,
        Diarias = V8,
        Valor = V9
      ) |>
      dplyr::mutate(
        Tipo_de_Viagem = "Externa",
        Ano = ano
      )

  } else {

    tabela_diarias_modificada_ou <- NULL

  }

  # Junção dos dois data frames

  dplyr::bind_rows(tabela_diarias_modificada_rn, tabela_diarias_modificada_ou)

}

# Execução da função criada através de iteração

anos <- c(2004:2022)

diarias <- purrr::map_dfr(anos, diarias_desejadas)

diarias |>
  dplyr::glimpse()

diarias |>
  readr::write_rds("data/diarias")



