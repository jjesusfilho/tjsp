#' Baixar petições e docs a partir da tabela lida com tjsp_ler_tabela_docs
#'
#' @param df dataframe com colunas processo, id_doc e url_doc
#' @param diretorio Diretório
#'
#' @return pdfs
#' @export
#'
tjsp_baixar_cpopg_docs_cd_processo <- function(df,diretorio = "."){

  lista <- df |>
    dplyr::group_split(cd_processo)


  httr::set_config(httr::config(ssl_verifypeer = FALSE))


  uri1 <- "https://esaj.tjsp.jus.br/cpopg/search.do?gateway=true"

  pb <- progress::progress_bar$new(total = length(lista))


  purrr::walk(lista, purrr::possibly(~{

    pb$tick()

   dd <- .x

   cd_processo <- .x$cd_processo |> unique()

   conteudo <- cd_processo |>
      paste0("https://esaj.tjsp.jus.br/cpopg/show.do?processo.codigo=", ... = _, "&gateway=true") |>
      httr::GET() |>
      httr::content()


    if (
      conteudo |>
      xml2::xml_find_first("boolean(//a[@class='linkConsultaSG btn btn-secondary btn-space'])")
    ) {

    url1 <- paste0("https://esaj.tjsp.jus.br/cposg/verificarAcessoPastaDigital.do?cdProcesso=", cd_processo,"&conversationId=&_=1599440192646")

  } else {


    url1 <- paste0("https://esaj.tjsp.jus.br/cpopg/abrirPastaDigital.do?processo.codigo=",cd_processo)

  }

  r2 <-  url1 |>
    httr::GET() |>
    httr::content("text") |>
    httr::GET()

  tjsp_baixar_docs_cd_processo(dd$cd_processo, dd$id_doc, dd$pagina_inicial,  dd$url_doc, diretorio)


},NULL))

}



#' Função interna do tjsp_baixar_cpopg_docs
#' @param cd_processo Número do processo
#' @param id_doc Id do documento
#' @param pagina_inicial Pagina
#' @param urls Urls dos documentos
#' @param diretorio Diretório
#'
#' @return pdfs
#'
tjsp_baixar_docs_cd_processo <- function(cd_processo  = NULL,
                             id_doc = NULL,
                             pagina_inicial = NULL,
                             urls = NULL,
                             diretorio = NULL){



  id <- stringr::str_c(cd_processo,
                       "_id_doc_",id_doc,
                       "_pagina_inicial_",pagina_inicial
  )

  pb <- progress::progress_bar$new(total = length(cd_processo))


  purrr::walk2(urls,id,purrr::possibly(~{

    pb$tick()

    arquivo <- file.path(diretorio,paste0(.y,".pdf"))

    httr::GET(.x,httr::write_disk(arquivo,overwrite = TRUE))

  },NULL))

}
