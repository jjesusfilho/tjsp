#' Lê documentos baixados combinados com tjsp_combinar_docs
#'
#' @param arquivos Vetor de arquivos
#' @param diretorio Alternativamente informar diretorio
#' @param remover_assinatura  Remover assinatura? TRUE default
#' @param combinar Combinar o vetor em um documento? TRUE defalt
#' @param remover_cabecalho Remover o cabeçalho? TRUE default
#'
#' @return tibble
#' @export
#'
tjsp_ler_documentos_cd_processo <- function (arquivos = NULL, diretorio = ".", remover_assinatura = FALSE,
                                 combinar = TRUE,
                                 remover_cabecalho = FALSE)
{
  if (is.null(arquivos)) {
    arquivos <- list.files(diretorio, full.names = TRUE,
                           pattern = "pdf$")
  }


  purrr::map_dfr(arquivos, purrr::possibly(~{


    cd_processo <- stringr::str_extract(.x, "\\w+?(?=_id_doc)")

    id_doc <- stringr::str_extract(.x, "(?<=id_doc_)\\d+")

    pagina_inicial <- stringr::str_extract(.x, "(?<=pagina_inicial_)\\d+")
    
    pagina_final <- stringr::str_extract(.x, "(?<=pagina_final_)\\d+")

    suppressMessages({
      texto <- pdftools::pdf_text(.x)
    })


    data <- texto |>
      purrr::pluck(1) |>
      stringr::str_squish() |>
      stringr::str_extract("(?i)(?<=(liberado nos autos em |protocolado em ))\\S+") |>
      lubridate::dmy()

    if (remover_assinatura) {
      texto <- remover_assinatura_cjpg(texto)
    }
    if (combinar) {
      texto <- stringr::str_c(texto, collapse = "\n")
    }

    if(remover_cabecalho){

      texto <- stringr::str_remove(texto, 'TRIBUNAL DE JUSTI\u00C7A DO ESTADO DE S\u00C3O PAULO\\X+?(?=\\s{100})')
    }


    tibble::tibble(cd_processo, id_doc, pagina_inicial, pagina_final, doc = texto, data_documento = data)
    
  }, NULL), .progress = TRUE)
}
