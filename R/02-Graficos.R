diarias_modificada <- readr::read_rds("data/diarias_modificada")

# Primeiro gráfico --------------------------------------------------------

arrows <- tibble::tibble(
  x1 = c(3.95),
  x2 = c(4.19),
  y1 = c(390),
  y2 = c(316.5)
)


diarias_modificada |>
  ggplot2::ggplot() +
  ggplot2::aes(
    x = Governador,
    y = Diarias,
    color = Tipo_de_Viagem
  ) +
  ggplot2::geom_jitter(
    position = ggplot2::position_jitter(seed = 100, width = 0.25),
    size = 2,
    alpha = 0.3
  ) +
  ggplot2::theme_bw() +
  ggplot2::labs(
    title = "Como se comportaram as diárias durante os diferentes mandatos \nde governador?",
    subtitle = "Cada ponto mostra a quantidade de diárias recebidas por um servidor do Estado \nem viagens para dentro ou fora dos limites estaduais em um ano desde 2004 \ndurante mandatos de governadores diferentes.",
    x = "Governador",
    y = "Número de Diárias"
  ) +
  ggplot2::scale_y_continuous(
    limits = c(0, 450)
  ) +
  ggplot2::scale_color_manual(
    values = c("#2A788EFF", "#FDE725FF")
  ) +
  ggplot2::theme(
    legend.position = "top",
    legend.title = ggplot2::element_blank(),
    axis.title.x = ggplot2::element_text(vjust = -1),
    axis.title.y = ggplot2::element_text(vjust = 1.5),
  ) +
  ggplot2::annotate(
    "text", x = 3.75, y = 390, family = "Poppins", size = 2.8, color = "gray20",
    label = "306,5 diárias \nno ano de 2009"
  ) +
  ggplot2::geom_curve(
    data = arrows,
    ggplot2::aes(x = x1, y = y1, xend = x2, yend = y2),
    arrow = ggplot2::arrow(
      length = ggplot2::unit(0.08, "inch")
    ),
    size = 0.6,
    color = "gray20",
    curvature = -0.3
  )



# Segundo gráfico ---------------------------------------------------------


diarias_modificada |>
  dplyr::group_by(
    Ano,
    Orgao
  ) |>
  dplyr::summarise(
    Total_Valor = sum(Valor, na.rm = TRUE)
  ) |>
  dplyr::ungroup() |>
  ggplot2::ggplot() +
  ggplot2::geom_line(
    ggplot2::aes(
      x = Ano,
      y = Total_Valor,
      color = Orgao,
    ),
    size = 1
  ) +
  gghighlight::gghighlight(
    max(Total_Valor) >= 1500000,
    max_highlight = 10L,
    use_direct_label = FALSE,
    unhighlighted_params = list(size = 0.3, colour = "grey")
  ) +
  ggplot2::labs(
    title = "Gastos das insituições com diárias ao longo dos anos",
    subtitle = "Cada linha representa os gastos das instituições do governo estadual. \nEstão destacadas as que em algum ano tiveram gastos maiores a 1,5 milhões \nde reais.",
    x = "Ano",
    y = "Total Gasto (milhões R$)"
  ) +
  ggplot2::scale_color_viridis_d() +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.position = "top",
    legend.title = ggplot2::element_blank(),
    axis.title.x = ggplot2::element_text(vjust = -1),
    axis.title.y = ggplot2::element_text(vjust = 1.5)
  ) +
  ggplot2::scale_y_continuous(
    limits = c(0, 4000000),
    breaks = seq(0, 4000000, 2000000),
    labels = c("0", "2", "4")
  ) +
  ggplot2::scale_x_continuous(
    breaks = seq(2004, 2022, 3)
  )



# Terceiro gráfico --------------------------------------------------------


diarias_modificada |>
  dplyr::mutate(
    Valor_format = paste("R$", format(Valor, big.mark = ".", decimal.mark = ","))
  ) |>
  ggplot2::ggplot() +
  ggplot2::aes(x = Diarias, y = Valor) +
  ggplot2::geom_point() +
  gghighlight::gghighlight(
    Valor > 35000 & Diarias < 100,
    label_key = Valor_format
  ) +
  ggplot2::labs(
    title = "Há alguma incoerência nos valores pagos?",
    subtitle = "Alguns valores recebidos são extremamente altos quando se compara com a quantidade \nde diárias feitas para gerar esse valor. Podemos ver que alguns dos valores mais altos \nforam feitos a partir de menos de 100 diárias.",
    x = "Diárias",
    y = "Total Recebido (R$)"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.position = "none",
    legend.title = ggplot2::element_blank(),
    axis.title.x = ggplot2::element_text(vjust = -1),
    axis.title.y = ggplot2::element_text(vjust = 1.5)
  ) +
  ggplot2::scale_y_continuous(
    limits = c(0, 120000),
    breaks = seq(0, 120000, 30000),
    labels = c("0", "30 mil", "60 mil", "90 mil", "120 mil")
  )




