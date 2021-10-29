#' Extrai data e horário da distribuicao
#'
#' @param x Coluna distribuicao
#'
#' @return data horário no formato yyymmddHHMMSS (Posixct)
#' @export
#'
 tjsp_parsear_distribuicao <- function(x){
  
  x |>
    stringr::str_extract(".+?(?=-)") |>
    lubridate::parse_date_time(orders = "dmYHM", tz = "America/Sao_Paulo")
  
}