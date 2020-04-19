#' Limpa e ordena câmaras de direito
#'
#' @param x Vetor de câmaras
#'
#' @return Mesmo vetor convertido em fator e ordenado da primeira à
#'     última câmara
#' @export
#'
ordenar_camaras <- function(x){

  stringr::str_remove_all(x,"(\\s|[:lower:]+)") %>%
    factor(levels= stringr::str_sort(unique(x),numeric=T))

}
