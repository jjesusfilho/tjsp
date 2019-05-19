#' Retorna o lapso temporal na unidade especificada entre duas datas.
#'
#' @param data_inicial incluir a data no formato Date.
#' @param data_final  incluir a data no formato Date.
#' @param unidade  uma das opções: ano, mes, semana, dia.
#' @details Esta função é um mero wrapper das funções interval e time_length
#'     do pacote lubridate. Ela considera as variações de ano, meses e dias.
#'
#' @return double com o lapso temporal
#' @export
#'
#' @examples
#' lapso(data_inicial = as.Date("2018-01-01"), data_final = as.Date("2018-12-31"), unidade = "mes")
lapso <- function(data_inicial = NULL,
                  data_final = NULL,
                  unidade = "mes") {
  if (!lubridate::is.Date(data_inicial) |
    !lubridate::is.Date(data_final)) {
    stop("As datas devem estar no formato iso 8601: yyyy-mm-dd")
  }

  periodo <- switch(
    unidade,
    "ano" = "year",
    "mes" = "month",
    "semana" = "week",
    "dia" = "day"
  )

  lubridate::interval(data_inicial, data_final) %>%
    lubridate::time_length(periodo)
}
