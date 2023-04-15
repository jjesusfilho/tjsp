#' Busca os números de processos no primeiro grau no parametro especificado
#'
#' @param consulta Valor da consulta
#' @param parametro Uma dessas: "NUMOAB", "NMPARTE", "DOCPARTE"
#' @param distribuidor Informar código do distribuidor.
#'      Você pode obtê-los com `tjsp_varas()`
#' @param diretorio Informar diretorio onde armazenar os htmls
#' @return htmls
#' @export
#'
#' @examples
#' \dontrun{
#' tjsp_baixar_cpopg_par(consulta="123456SP", parametro = "NUMOAB")
#' obter_baixar_cpopg_par(consulta = "José da Silva Pereira", parametro = "NMPARTE")
#' }
tjsp_baixar_cpopg_par <- function(consulta = NULL,
                                  parametro = NULL,
                                  distribuidor = "-1",
                                  diretorio = ".") {

  httr::set_config(httr::config(ssl_verifypeer = FALSE))

  url1<-"https://esaj.tjsp.jus.br/cpopg/search.do?"

  url2<- "https://esaj.tjsp.jus.br/cpopg/trocarPagina.do?"


  if(parametro == "NUMOAB" | parametro == "DOCPARTE"){

    consulta <- stringr::str_remove_all(consulta, "\\W")
  }

  pb <- progress::progress_bar$new(total = length(consulta))


  purrr::pmap(list(x = consulta, y = parametro, z = distribuidor), purrr::possibly(function(x, y, z){

    pb$tick()

  query1 <-
    list(
      conversationId = "",
      #dadosConsulta.localPesquisa.cdLocal = "-1",
      cbPesquisa = y,
      dadosConsulta.tipoNuProcesso = "UNIFICADO",
      dadosConsulta.valorConsulta = x,
      cdForo = z,
      uuidCaptcha = ""
    )

  resposta <- httr::RETRY(verb="GET",url=url1,query=query1,httr::timeout(30))

  max_pag <- resposta |>
    httr::content() |>
    xml2::xml_find_first(xpath = "//span[@id='contadorDeProcessos']") |>
    xml2::xml_text(trim=TRUE) |>
    stringr::str_extract("\\d+") |>
    as.numeric() |>
    (\(x) x/25)() |>
    ceiling()

  paginas <- 1:max_pag |>  as.character()


  purrr::walk(paginas,purrr::possibly(~{

  arquivo <- file.path(diretorio, paste0("consulta_", x,"_parametro_", y,"_distribuidor_", z,"_pagina_", .x, ".html"))

    query2 <-
      list(
        paginaConsulta = .x,
        conversationId = "",
        #dadosConsulta.localPesquisa.cdLocal = "-1",
        cbPesquisa = y,
        dadosConsulta.tipoNuProcesso = "UNIFICADO",
        dadosConsulta.valorConsulta = x,
        cdForo = z,
        uuidCaptcha = ""
      )


   httr::RETRY("GET",
                            url = url2, query = query2,
                            quiet = TRUE, httr::timeout(2),
                            httr::write_disk(arquivo)
    )


  },NULL))

  },NULL))

}
