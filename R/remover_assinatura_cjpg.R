#' Remove assinutura das sentencas
#'
#' @param x Texto
#'
#' @return Texto com assinaturas removidas
#' @export
#'
remover_assinatura_cjpg <- function(x) {
  stringr::str_remove_all(x, "(?i)(Este documento \u00e9 c\u00f3pia|para conferir).+?\n")
}
