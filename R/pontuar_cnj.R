#' Pontuar número do CNJ
#'
#' @param numero Número pontuado ou não
#'
#' @return Número pontuado
#' @export
#'
#' @examples
#' pontuar_cnj("12345678920218260000")
pontuar_cnj <- function(numero = NULL){

  if (is.null(numero)){

    stop("Forne\u00e7a uma sequ\u00eancia num\u00e9rica")
  }

  numero %>%
    stringr::str_remove_all("\\D") %>%
    stringr::str_pad(width= 20, side = "left",pad = "0") %>%
    stringr::str_replace("(.{7})(.{2})(.{4})(.{1})(.{2})(.{4})","\\1-\\2.\\3.\\4.\\5.\\6")

}
