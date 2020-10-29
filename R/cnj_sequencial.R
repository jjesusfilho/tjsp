#' Creates cnj sequence numbers
#'
#' @param inicio integer 
#' @param fim   integer
#' @param ano   year
#' @param segmento integer
#' @param uf  federative unity.
#' @param distribuidor code of the court.
#'
#' @return vector of lawsuit numbers
#' @export
#' @examples 
#' \dontrun{
#' stj<-cnj_sequencial(0,100,2018,3,00,0000)
#' }
cnj_sequencial <- function(inicio, fim, ano, segmento, uf, distribuidor) {
  if (!is.numeric(inicio) | !is.numeric(fim)) {
    stop("inicio e fim devem ser num\u00e9ricos")
  }
  o <- stringr::str_pad(inicio:fim, width = 7, "left", "0")
  uf <- stringr::str_pad(uf, 2, "left", "0")
  distribuidor <- stringr::str_pad(distribuidor, 4, "left", "0")
  num <- paste0(o, ano, segmento, uf, distribuidor)
  abjutils::calc_dig(num, TRUE)
}