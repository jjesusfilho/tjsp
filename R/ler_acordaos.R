#' Lê pdfs dos acórdãos, remove assinatura e eventualmente combina
#'
#' @param diretorio diretório onde se encontram os pdfs
#' @param remover_assinatura default é FALSE
#' @param combinar default juntar todas as páginas em um único vetor.
#'
#' @return Lista com vetores das páginas dos pdfs
#' @export

ler_acordaos <- function(diretorio = ".", remover_assinatura = FALSE, combinar = FALSE) {
  pdfs <- list.files(diretorio, pattern = ".pdf$", full.names = TRUE)

  textos <- purrr::map(pdfs, pdftools::pdf_text)

  if (remover_assinatura == TRUE) {
    textos <- purrr::map(textos, ~remover_assinatura(.x))
  }

  if (combinar == TRUE) {
    textos <- purrr::map(textos, ~ stringr::str_c(.x, collapse = "\n"))
  }
  return(textos)
}
