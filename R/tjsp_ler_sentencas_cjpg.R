#' Lê pdfs das sentenças, remove assinatura e eventualmente combina
#'
#' @param arquivos Arquivos onde se encontram os pdfs
#' @param diretorio Diretório onde se encontram os pdffs
#'     (alternativa a arquivos)
#' @param remover_assinatura Padrão para FALSE
#' @param combinar Default para TRUE. Juntar todas as
#'     página num único texto
#'
#' @return Tibble
#' @export
#'
tjsp_ler_sentencas_cjpg <- function(arquivos = NULL, diretorio = ".", remover_assinatura = FALSE, combinar = TRUE){

  if (is.null(arquivos)){

    arquivos <- list.files(diretorio, full.names = TRUE, pattern = "pdf$")

  }

  pb <- progress::progress_bar$new(total = length(arquivos))

  purrr::map_dfr(arquivos, purrr::possibly(~{

    pb$tick()


    cd_doc <- stringr::str_extract(.x,"(?<=sentenca_).+?(?=\\.pdf)")

     suppressMessages({
    texto <- pdftools::pdf_text(.x)

     })
    if (remover_assinatura) {
      texto <- remover_assinatura_cjpg(texto)
    }


    if (combinar) {
      texto <- stringr::str_c(texto, collapse = "\n")
    }

   tibble::tibble(cd_doc, julgado = texto)


  }, NULL))



}
