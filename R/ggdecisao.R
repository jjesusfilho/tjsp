#' Plota distribuição das decisões conforme a câmara e o recorrente
#'
#' @param data dataframe
#' @param title Título do gráfico
#' @param x orgao julgador, geralmente a câmara
#' @param facet facetas para facet_grid, geralmente o recorrente
#' @param fill preenchimento, geralmente a decisão
#' @param ordered se verdadeiro irá extrair somente os números e as siglas
#'     e ordená-la adequadaemente.
#' @param x_label etiqueta para órgão julgador, geralmente "câmara"
#' @param y_label etiqueta para número de casos, geralmente "número de decisões"
#' @param legend título da legenda
#' @param caption indicação da fonte
#'
#' @return ggplot
#' @export
#'
ggdecisao <-
  function(data,
           title = "",
           x = NULL,
           facet = NULL,
           fill = NULL ,
           ordered = TRUE,
           x_label = "Câmara",
           y_label = "Número de decisões",
           legend = "Decisão",
           caption = "Fonte: TJSP") {
    fill <- rlang::enquo(fill)
    facet <- rlang::enquo(facet)
    x <- rlang::enquo(x)

    data <- data %>%
      dplyr::select(x := !!x, facet := !!facet, fill := !!fill)

    if (ordered == TRUE) {
      data <- data %>%
        dplyr::mutate(x = stringr::str_remove_all(x, "(\\s+|[:lower:]+)"))

      l <- data %>%
        dplyr::distinct(x) %>%
        dplyr::mutate(number = stringr::str_extract(x, "\\d+") %>%  as.numeric) %>%
        dplyr::arrange(number) %>%
        dplyr::pull("x")

      data <- data %>%
        dplyr::mutate(x =  factor(x, levels = l)) %>%
        dplyr::count(x, facet, fill)

    } else {
      data <- data %>%
        dplyr::count(x, facet, fill)

    }

    lege <- paste0(legend, ":")


    ggplot2::ggplot(data, ggplot2::aes(x = x, y = n, fill = fill)) +
      ggplot2::geom_bar(stat = "identity",
                        position = "dodge",
                        colour = "black") +
      ggplot2::scale_fill_manual(values = c("red", "darkgreen"), name = lege) +
      #geom_text(aes(x=agravante,y=freq,label=freq),position=position_dodge(.9),vjust=-.5)+
      ggplot2::facet_grid( ~ facet) +
      ggplot2::coord_flip() +
      ggplot2::theme(
        strip.background = ggplot2::element_rect(fill = "lightblue", colour = "black"),
        legend.position = "bottom"
      ) +
      ggplot2::labs(
        title = title,
        x = x_label,
        y = y_label,
        caption = caption
      )
  }
