#' Converte valor da ação para número ou qualquer outra string.
#'
#' @param str vetor de strings
#'
#' @return mesmo vetor porém numérico.
#' @export
#'
numero <- function(str) {
  str %>%
    stringr::str_remove_all("(\\.|\\p{L}|\\$|\\s)+") %>%
    stringr::str_replace(",", ".") %>%
    as.numeric()
}
