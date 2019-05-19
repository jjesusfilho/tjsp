#' Lê pdfs dos acórdãos, remove assinatura e eventualmente combina
#'
#' @param diretorio diretório onde se encontram os pdfs
#' @param remover_assinatura default é FALSE
#' @param combinar default é manter cada página em um vetor
#'
#' @return Lista com vetores das páginas dos pdfs
#' @export

ler_acordaos <- function(diretorio = ".", remover_assinatura = FALSE, combinar = FALSE) {
  pdfs <- list.files(diretorio, pattern = ".pdf$", full.names = TRUE)

  textos <- purrr::map(pdfs, pdftools::pdf_text)

  if (remover_assinatura == TRUE) {
    texto <- purrr::map(textos, remover_assinatura)
  }

  if (combinar == TRUE) {
    textos <- purrr::map(textos, ~ stringr::str_c(.x, collapse = "\n"))
  }
  return(texto)
}
