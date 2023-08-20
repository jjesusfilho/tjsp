#' Lê xmls baixados com mni_consultar_processo como jsons
#'
#' @param arquivos Vetor de arquivos.
#' @param diretorio Alternativamente informar diretório.
#'
#' @return Tibble com coluna processo e coluna json(jq)
#' @export
#'
mni_ler_xml_as_json <- function(arquivos = NULL, diretorio = "."){

  if (is.null(arquivos)){
    arquivos <- list.files(diretorio, full.names = T)
  }


pb <- progress::progress_bar$new(total = length(arquivos))

purrr::map_dfr(arquivos, purrr::possibly(~{

  pb$tick()

  processo <- stringr::str_extract(.x, "\\d{20}")

   suppressWarnings(
   json <- .x |>
   readLines() |>
   JSON2XML::xml2JSON() |>
   stringr::str_remove_all("\\w+:") |>
   jqr::jq(".Envelope.Body.consultarProcessoResposta.processo")
   )

   tibble::tibble(processo, json)
}, NULL))

}
