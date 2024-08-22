#' Subdivide as data em intervalos anuais, semestrais, trimestrais ou mensais.
#'
#' @param data_inicial Data inicial dd/mm/aaaa
#' @param data_final Data final dd/mm/aaaa
#' @param corte Informar se anual, semestral, trimestral ou mensal.
#' @param formato Formato de retorno. Default para dd/mm/aaaa.
#'
#' @return tibble com duas colunas com data inicial e final
#'
agrupar_datas <- function(data_inicial = NULL,
                          data_final = NULL,
                          corte = c('anual','semestral','trimestral','mensal'),
                          formato = "%d/%m/%Y"){


  recorte <- function(x, corte){

    switch(corte,
           anual = lubridate::year(x),
           semestral = lubridate::semester(x, with_year = TRUE),
           trimestral = lubridate::quarter(x, with_year = TRUE),
           mensal = lubridate::floor_date(x,"month"))

  }

  tibble::tibble(.datas = seq(lubridate::dmy(data_inicial),
                              lubridate::dmy(data_final),1)) |>
    dplyr::mutate(.var = recorte(.datas, corte)) |>
    dplyr::group_split(.var) |>
    purrr::map_dfr(~dplyr::pull(.x,".datas") |>
                     range() |>
                     setNames(c("data_inicial","data_final"))) |>
    dplyr::mutate_all(list(~as.Date(.,origin='1970-01-01') |>
                             format(formato)))

}
