#' Ler acórdãos baixados com tjsp_baixar_acordao_cjsg
#'
#' @param arquivos Arquivos dos pdfs
#' @param diretorio Informar se não informar arquivos
#' @param remover_assinatura Remover assinatura digital?
#' @param combinar Combinar as páginas em uma
#'
#' @return tibble
#' @export
#'
tjsp_ler_acordaos_cjsg <- function(arquivos = NULL, diretorio = ".", remover_assinatura = TRUE, combinar = TRUE){

  if (is.null(arquivos)){

    arquivos <- list.files(diretorio, full.names = TRUE, pattern = "pdf$")

  }

  pb <- progress::progress_bar$new(total = length(arquivos))

  purrr::map_dfr(arquivos, purrr::possibly(~{

     pb$tick()

    cdacordao <- stringr::str_extract(.x, "(?<=cdacordao_)\\d+")

    suppressMessages({

    texto <- pdftools::pdf_text(.x)


    if (remover_assinatura) {
      texto <- remover_assinatura(texto) %>%
        stringr::str_trim()
    }

    if (combinar) {
      texto <- stringr::str_c(texto, collapse = "\n")
    }

    })

    tibble::tibble(cdacordao = cdacordao, julgado = texto)

  },NULL))

}
