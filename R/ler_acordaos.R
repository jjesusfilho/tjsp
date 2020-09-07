#' Lê pdfs dos acórdãos, remove assinatura e eventualmente combina
#'
#' @param arquivos Arquivos onde se encontram os pdfs
#' @param diretorio Diretório onde se encontram os pdfs (alternativa a arquivos)
#' @param remover_assinatura Padrão para FALSE
#' @param combinar Default para TRUE juntar todas as páginas em um único texto.
#'
#' @return Tibble com data do acórdão, processo e texto
#' @export

ler_acordaos <- function(arquivos = NULL, diretorio = ".", remover_assinatura = FALSE, combinar = TRUE) {

  if (is.null(arquivos)){

 arquivos   <- list.files(diretorio, pattern = "\\.pdf$", full.names = TRUE)

  }

  pb <- progress::progress_bar$new(total= length(arquivos))

purrr::map_dfr(arquivos, purrr::possibly(~{

  pb$tick()

  data_acordao <- stringr::str_extract(.x,"\\d{4}\\D\\d{2}\\D\\d{2}") %>%
                  lubridate::ymd()

  processo <- stringr::str_extract(.x,"\\d{20}")

  texto <- pdftools::pdf_text(.x)

  if (remover_assinatura) {
    texto <- remover_assinatura(texto)
  }

  if (combinar) {
    texto <- stringr::str_c(texto, collapse = "\n")
  }

  tibble::tibble(processo = processo, data_julgado = data_acordao, julgado = texto)

},NULL))

}
