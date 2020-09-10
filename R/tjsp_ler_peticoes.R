#' Ler petições e outros dosc baixadas com tjsp_baixar_docs
#'
#' @param arquivos Vetor de arquivos
#' @param diretorio Diretório
#' @param remover_assinatura Remover assinatura, dafault para TRUE
#' @param combinar Juntar as páginas numa única string. Default para TRUE
#'
#' @return tibble
#' @export
#'
tjsp_ler_peticoes <- function(arquivos = NULL, diretorio = ".", remover_assinatura= TRUE, combinar= TRUE){

  if (is.null(arquivos)){

    arquivos <- list.files(diretorio, full.names = TRUE)

  }

  pb <- progress::progress_bar$new(total = length(arquivos))

  purrr::map_dfr(arquivos,purrr::possibly(~{

    pb$tick()

    processo <- stringr::str_extract(.x,"\\d{20}")

    doc_id <- stringr::str_extract(.x,"\\d+(?=\\.pdf$)") %>%
      as.integer()


    suppressMessages(

      texto <- pdftools::pdf_text(.x)

    )
    if (remover_assinatura){

      texto <- remover_assinatura(texto)

    }

    if (combinar){

      texto <- stringr::str_c(texto,collapse = "\n")

    }

    tibble::tibble(processo = processo, doc_id = doc_id, texto = texto)


  },NULL))

}
