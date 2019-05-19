#' Remove texto com assinatura eletrônica
#'
#' @param x vetor de textos
#'
#' @return vetor de textos sem assinatura
#' @export
#'
remover_assinatura <- function(x) {
  stringr::str_remove_all(x, "(?i)(Este documento é cópia|para conferir)\\X+?(código|registro|assinatura eletrônica)\\X*?\n")
}
