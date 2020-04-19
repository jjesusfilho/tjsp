#' Remove acentos
#'
#' @param x vetor de caracteres
#'
#' @return mesmo vetor com acentos removidos
#' @export
#'
remover_acentos <- function(x){

  stringi::stri_trans_general(x,"latin-ascii")


}
