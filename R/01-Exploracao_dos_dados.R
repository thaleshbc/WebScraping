

# Releitura dos dados -----------------------------------------------------

diarias_modificada <- readr::read_rds("data/diarias")


# Alteração dos dados -----------------------------------------------------

diarias_modificada <- diarias_modificada |>
  dplyr::mutate(
    ID = NULL,
    Nome = stringr::str_to_title(Nome),
    Cargo_Funcao = forcats::as_factor(Cargo_Funcao),
    Cargo_Funcao = forcats::fct_infreq(Cargo_Funcao),
    Orgao = forcats::as_factor(Orgao),
    Orgao = forcats::fct_infreq(Orgao),
    PCDs = as.integer(PCDs),
    Diarias = stringr::str_replace(Diarias, ",", "."),
    Diarias = as.numeric(Diarias),
    Valor = stringr::str_remove(Valor, "R\\$ "),
    Valor = stringr::str_remove(Valor, "\\."),
    Valor = stringr::str_replace(Valor, ",", "."),
    Valor = as.numeric(Valor),
    Tipo_de_Viagem = forcats::as_factor(Tipo_de_Viagem),
    Tipo_de_Viagem = forcats::fct_infreq(Tipo_de_Viagem),
    Governador = dplyr::case_when(
      Ano %in% c(2004:2010) ~ "Wilma de Faria",
      Ano %in% c(2011:2014) ~ "Rosalba Ciarlini",
      Ano %in% c(2015:2018) ~ "Robinson Faria",
      Ano %in% c(2019:2022) ~ "Fátima Bezerra"
    ),
    Valor_por_Diaria = Valor / Diarias
  )


# Salvamento dos dados ----------------------------------------------------

diarias_modificada |>
  readr::write_rds("data/diarias_modificada")


# Releitura dos dados modificados -----------------------------------------

diarias_modificada <- readr::read_rds("data/diarias_modificada")



# Exploração dos dados ----------------------------------------------------

diarias_modificada |>
  dplyr::group_by(Orgao) |>
  dplyr::summarise(
    Total_Diarias = sum(Diarias)
  ) |>
  ggplot2::ggplot() +
  ggplot2::aes(
    x = forcats::fct_reorder(Orgao, Total_Diarias),
    y = Total_Diarias
  ) +
  ggplot2::geom_col() +
  ggplot2::theme_light() +
  ggplot2::coord_flip() +
  ggplot2::labs(
    title = "Órgão X Total de Diárias",
    subtitle = "Número total de diárias por órgão ou instituição do\ngoverno estadual desde o ano de 2004",
    x = "Órgão",
    y = "Diárias"
  ) +
  ggplot2::scale_y_continuous(
    breaks = seq(0, 300000, 100000),
    labels = c("0", "100 mil", "200 mil", "300 mil")
  )



diarias_modificada |>
  dplyr::group_by(Governador) |>
  dplyr::summarise(
    Total_Diarias = sum(Diarias)
  ) |>
  ggplot2::ggplot() +
  ggplot2::aes(
    x = forcats::fct_reorder(Governador, -Total_Diarias),
    y = Total_Diarias
  ) +
  ggplot2::geom_col() +
  ggplot2::theme_light() +
  ggplot2::labs(
    title = "Governador X Total de Diárias",
    subtitle = "Número total de diárias por governo eleito desde o ano de 2004",
    x = "Governador",
    y = "Diárias"
  ) +
  ggplot2::scale_y_continuous(
    breaks = seq(0, 1000000, 500000),
    labels = c("0", "500 mil", "1 milhão")
  )



diarias_modificada |>
  dplyr::group_by(Ano) |>
  dplyr::summarise(
    Total_Diarias = sum(Diarias)
  ) |>
  ggplot2::ggplot() +
  ggplot2::aes(
    x = Ano,
    y = Total_Diarias
  ) +
  ggplot2::geom_col() +
  ggplot2::theme_light() +
  ggplot2::coord_flip() +
  ggplot2::labs(
    title = "Ano X Total de Diárias",
    subtitle = "Número total de diárias por ano desde 2004",
    x = "Ano",
    y = "Diárias"
  ) +
  ggplot2::scale_y_continuous(
    breaks = seq(0, 200000, 50000),
    labels = c("0", "50 mil", "100 mil", "150 mil", "200 mil")
  )



diarias_modificada |>
  dplyr::group_by(Tipo_de_Viagem) |>
  dplyr::summarise(
    Total_Diarias = sum(Diarias)
  ) |>
  ggplot2::ggplot() +
  ggplot2::aes(
    x = forcats::fct_reorder(Tipo_de_Viagem, -Total_Diarias),
    y = Total_Diarias
  ) +
  ggplot2::geom_col(width = 0.65) +
  ggplot2::theme_light() +
  ggplot2::labs(
    title = "Tipo de Viagem X Total de Diárias",
    subtitle = "Número total de diárias por tipo de viagem desde o ano de 2004",
    x = "Tipo de Viagem",
    y = "Diárias"
  ) +
  ggplot2::scale_y_continuous(
    breaks = seq(0, 2000000, 500000),
    labels = c("0", "500 mil", "1 milhão", "1,5 milhões", "2 milhões")
  )



diarias_modificada |>
  dplyr::select(Diarias) |>
  ggplot2::ggplot() +
  ggplot2::aes(
    x = Diarias,
  ) +
  ggplot2::geom_histogram(binwidth = 25) +
  ggplot2::theme_light() +
  ggplot2::labs(
    title = "Distribuição da quantidade de diárias",
    subtitle = "Distribuição de frequência da quantidade de diárias por pessoa",
    x = "Diárias",
    y = "Frequência"
  )



diarias_modificada |>
  dplyr::select(Valor) |>
  ggplot2::ggplot() +
  ggplot2::aes(
    x = Valor,
  ) +
  ggplot2::geom_histogram(binwidth = 1000) +
  ggplot2::theme_light() +
  ggplot2::labs(
    title = "Distribuição do valor das diárias",
    subtitle = "Distribuição de frequência do valor das diárias por pessoa",
    x = "Diárias",
    y = "Freuência"
  )



diarias_modificada |>
  ggplot2::ggplot() +
  ggplot2::aes(
    x = Tipo_de_Viagem,
    y = Diarias
  ) +
  ggplot2::geom_boxplot() +
  ggplot2::theme_light() +
  ggplot2::labs(
    title = "Distribuição da quantidade de diárias",
    subtitle = "Distribuição de frequência da quantidade de diárias por pessoa",
    x = "Diárias",
  )


