#' Filtra base cjsg por assunto e classe.
#'
#' @param base base parseada dos cjsg htmls
#' @param assunto regex do assunto a ser filtrado
#' @param classe regex da classe a ser filtrada
#' @param negate_assunto coloque TRUE se quiser excluir os assuntos especificados.
#'
#' @details Geralmente as classes são limitadas e as descrições mais claras que
#'     os assuntos. Por isso optei pela negação somente do assunto.
#'
#' @return Mesma base filtrada.
#' @export
#'
filtrar_cjsg <- function(base, assunto, classe, negate_assunto = FALSE) {
  base %>%
    dplyr::mutate_all(stringr::str_squish) %>%
    dplyr::filter(stringr::str_detect(assunto, !!assunto, negate = !!negate_assunto)) %>%
    dplyr::filter(stringr::str_detect(classe, !!classe))
}
