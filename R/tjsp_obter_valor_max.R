#' Obtêm valor máximo do julgado
#'
#' @param julgado Julgado ou trecho do julgao de interesse
#'
#' @return Valor
#' @export
#'
tjsp_obter_valor_max <- function(julgado){
  julgado |>
    stringr::str_extract_all("R\\$[\\s.]?\\d\\S+") |>
    purrr::map_dbl(~{
      .x |>
        stringr::str_extract(".+?(?=\\D?$)") |>
        stringr::str_remove_all("(\\.|\\p{L}|\\$|\\s)+") |>
        stringr::str_replace(",", ".") |>
        as.numeric() |>
        max(na.rm = TRUE) |>
        unique()
    })
}
