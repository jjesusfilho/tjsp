#' Retorna o lapso temporal na unidade especificada entre duas datas.
#'
#' @param data_inicial data inicial
#' @param data_final data final
#' @param unidade ano, mes, semana ou dia.
#' @details Esta função é um mero wrapper das funções interval e time_length
#'     do pacote lubridate. Ela considera as variações de
#'     ano, mes,semana, dia, hora, minuto e segundo.
#'
#' @return double com o lapso temporal
#' @export
#'
#' @examples
#' lapso(data_inicial=as.Date("2018-01-01"),data_final=as.Date("2018-12-31"),unidade="mes")


lapso <- function (data_inicial = NULL, data_final = NULL, unidade = "mes")
{
  if (!lubridate::is.timepoint(data_inicial) | !lubridate::is.timepoint(data_final)) {
    stop("As datas devem no formato data")
  }
  periodo <- switch(unidade,
                    ano = "year",
                    mes = "month",
                    semana = "week",
                    dia = "day",
                    hora = "hour",
                    minuto = "minute",
                    segundo = "second")

  lubridate::interval(data_inicial, data_final) |>
    lubridate::time_length(periodo)
}
