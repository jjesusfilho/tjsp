#' Baixar julgados do banco de sentenças em pdf em pdf
#'
#' @param cd_doc Código da sentença obtido de tjsp_ler_cjpg
#' @param diretorio Diretório onde armazenar os arquivos
#'
#' @return pdf
#' @export
#'
tjsp_baixar_sentenca_cjpg <- function(cd_doc, diretorio = "."){


  parseado <-  structure(list(scheme = "https", hostname = "esaj.tjsp.jus.br",
                              port = NULL, path = "pastadigital/getPDF.do", query = NULL,
                              params = NULL, fragment = NULL, username = NULL, password = NULL), class = "url")

  pb <- progress::progress_bar$new(total = length(cd_doc))

  purrr::walk(cd_doc, purrr::possibly(~{

    pb$tick()

    parametros <- stringr::str_split(.x,"-") %>% unlist()


    query <-
      list(
        nuSeqRecurso = "00000",
        cdDocumento = parametros[[4]],
        conferenciaDocEdigOriginal = "false",
        nmAlias = parametros[[3]],
        origemDocumento = "M",
        nuPagina = "0",
        #numInicial = "122",
        tpOrigem = "2",
        flOrigem = "P",
        deTipoDocDigital = "Senten\xe7as",
        cdProcesso = parametros[[1]],
        cdFormatoDoc = "5",
        cdForo = parametros[[2]],
        #idDocumento = "55277239-122-0",
        #numFinal = "122",
        sigiloExterno = "N"
      )

    parseado$query <- query

    url <- httr::build_url(parseado)

    arquivo <- file.path(diretorio,paste0("sentenca_",.x,".pdf"))

    httr::GET(url, httr::write_disk(arquivo, overwrite = TRUE))

  },NULL))

}
