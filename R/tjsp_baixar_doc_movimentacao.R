#' Baixa documentos da movimentação a partir da url
#'
#' @param urls Vetor de urls
#' @param diretorio
#'
#' @return PDF
#' @export
#'
tjsp_baixar_doc_movimentacao <- function(urls, diretorio = "."){


  purrr::walk(urls, purrr::possibly(~{

    cd_processo <- stringr::str_extract(.x, "(?<=codigo=)\\w+")

    invisible(httr::GET(paste0("https://esaj.tjsp.jus.br/cpopg/show.do?processo.codigo=",cd_processo,"&gateway=true")))

    query <-  .x |>
      httr::GET() |>
      httr::content("text") |>
      stringr::str_extract('(?<=parametros":").+?"') |>
      stringr::str_remove('\\"') |>
      httr:::parse_query()

    url_parseada <-
      structure(
        list(
          scheme = "https",
          hostname = "esaj.tjsp.jus.br",
          port = NULL,
          path = "pastadigital/getPDF.do",
          params = NULL,
          fragment = NULL,
          username = NULL,
          password = NULL
        ),
        class = "url"
      )


    url_parseada$query <- query


    url1 <- httr::build_url(url_parseada)


    arquivo <- file.path(diretorio, paste0("cd_processo_", cd_processo, "_cd_documento_", query$cdDocumento,".pdf"))

    httr::GET(url1, httr::write_disk(arquivo, overwrite = T))

  }, NULL), .progress = TRUE)

}
