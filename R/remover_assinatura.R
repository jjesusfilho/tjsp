#' Remove texto com assinatura eletrônica
#'
#' @param x vetor de textos
#'
#' @return vetor de textos sem assinatura
#' @export
#'
remover_assinatura <- function(x) {
  stringr::str_remove_all(x, "(?i)(Este documento \u00e9 c\u00f3pia|para conferir)\\X+?(c\u00f3digo|registro|assinatura eletr\u00f4nica)\\X*?\n")
}
