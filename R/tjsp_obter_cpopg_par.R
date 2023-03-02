#' Busca os números de processos no primeiro grau no parametro especificado
#'
#' @param consulta Valor da consulta
#' @param parametro Uma dessas: "NUMOAB", "NMPARTE", "DOCPARTE"
#' @return tibble com os números dos processos
#' @export
#'
#' @examples
#' \dontrun{
#' obter_cpopg_par(consulta="123456SP", parametro = "NUMOAB")
#' obter_cpopg_par(consulta = "José da Silva Pereira", parametro = "NMPARTE")
#' }
tjsp_obter_cpopg_par <- function(consulta = NULL, parametro = NULL) {





  httr::set_config(httr::config(ssl_verifypeer = FALSE))

  url1<-"https://esaj.tjsp.jus.br/cpopg/search.do?"

  url2<- "https://esaj.tjsp.jus.br/cpopg/trocarPagina.do?"

  query1 <-
    list(
      conversationId = "",
      dadosConsulta.localPesquisa.cdLocal = "-1",
      cbPesquisa = parametro,
      dadosConsulta.tipoNuProcesso = "UNIFICADO",
      dadosConsulta.valorConsulta = consulta,
      uuidCaptcha = ""
    )

  resposta<-httr::RETRY(verb="GET",url=url1,query=query1,httr::timeout(30))

  max_pag <- resposta %>%
    httr::content() %>%
    xml2::xml_find_all(xpath = "//span[@class='resultadoPaginacao']") %>%
    xml2::xml_text(trim=TRUE) %>%
    .[[1]] %>%
   # stringr::str_trim() %>%
    stringr::str_extract("\\d+$") %>%
    as.numeric() %>%
    `/`(25) %>%
    ceiling()

  paginas <- 1:max_pag %>% as.character()

  pb <- progress::progress_bar$new(total = max_pag)

purrr::map_dfr(paginas,purrr::possibly(~{

  pb$tick()

     query2 <-
       list(
         paginaConsulta = .x,
         conversationId = "",
         dadosConsulta.localPesquisa.cdLocal = "-1",
         cbPesquisa = parametro,
         dadosConsulta.tipoNuProcesso = "UNIFICADO",
         dadosConsulta.valorConsulta = consulta,
         uuidCaptcha = ""
       )


    resposta <- httr::RETRY("GET",
                             url = url2, query = query2,
                             quiet = TRUE, httr::timeout(2)
    )

    conteudo <- httr::content(resposta)

    processo<- xml2::xml_find_all(conteudo,"//div[@class='nuProcesso']") %>%
      xml2::xml_text(trim=TRUE) %>%
      stringr::str_remove_all("\\s.+") %>%
      rep(each=2)


    coluna<- xml2::xml_find_all(conteudo,"//div[@class='espacamentoLinhas']/span") %>%
      xml2::xml_text(trim=T) %>%
      stringr::str_remove_all("\\:.*")

    valor<- xml2::xml_find_all(conteudo,"//div[@class='espacamentoLinhas']") %>%
      xml2::xml_text(trim=TRUE) %>%
      stringr::str_extract("(?<=:\\s{1,5}).+") %>%
      stringr::str_squish()

    tibble::tibble(processo,coluna,valor) %>%
     tidyr::pivot_wider(names_from=coluna, values_from = valor) %>%
     janitor::clean_names() %>%
      tidyr::separate(recebido_em,c("data_entrada","unidade"), sep = " - ") %>%
      dplyr::mutate(data_entrada= lubridate::dmy(data_entrada)) %>%
      dplyr::select(processo,data_entrada, unidade, dplyr::everything())

         }, NULL))
}
