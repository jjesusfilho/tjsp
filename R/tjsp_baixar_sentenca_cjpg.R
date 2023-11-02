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


  purrr::walk(cd_doc, purrr::possibly(~{


    parametros <- stringr::str_split(.x,"-") %>% unlist()


    conteudo <- parametros[[1]] |>
      paste0("https://esaj.tjsp.jus.br/cpopg/show.do?processo.codigo=", ... = _, "&gateway=true") |>
      httr::GET() |>
      httr::content()


    if (
      conteudo |>
      xml2::xml_find_first("boolean(//a[@class='linkConsultaSG btn btn-secondary btn-space'])")
    ) {

      url1 <- paste0("https://esaj.tjsp.jus.br/cposg/verificarAcessoPastaDigital.do?cdProcesso=", parametros[[1]],"&conversationId=&_=1599440192646")

    } else {


      url1 <- paste0("https://esaj.tjsp.jus.br/cpopg/abrirPastaDigital.do?processo.codigo=",parametros[[1]])

    }

    r2 <-  url1 |>
      httr::GET() |>
      httr::content("text") |>
      httr::GET()

    query <-
      list(
        nuSeqRecurso = "00000",
        cdDocumento = parametros[[4]],
        conferenciaDocEdigOriginal = "false",
        nmAlias = parametros[[3]],
        origemDocumento = "M",
        nuPagina = "0",
        numInicial = "1",
        tpOrigem = "2",
        flOrigem = "P",
        deTipoDocDigital = "Senten\xe7as",
        cdProcesso = parametros[[1]],
        cdFormatoDoc = "5",
        cdForo = parametros[[2]],
        #idDocumento = "55277239-122-0",
        numFinal = "163",
        sigiloExterno = "N"
      )

    parseado$query <- query

    url <- httr::build_url(parseado)

    arquivo <- file.path(diretorio,paste0("sentenca_",.x,".pdf"))

    httr::GET(url, httr::write_disk(arquivo, overwrite = TRUE))

  },NULL))

}
