#' Busca os números de processos no primeiro grau com base na oab do advogado
#'
#' @param oab Número da oab seguido pela uf, ex. 123456SP
#'
#' @return tibble com os números dos processos
#' @export
#'
#' @examples
#' \dontrun{
#' obter_cpopg_oab(oab="123456SP")
#' }
obter_cpopg_oab <- function(oab = NULL) {
  
  if (is.null(oab)) {
    stop("Você deve informar o número da oab")
  }
  
  oab<-stringr::str_remove_all(oab,"\\W+") %>% 
    stringr::str_to_upper()
  

  httr::set_config(httr::config(ssl_verifypeer = FALSE))
  
  url1<-"https://esaj.tjsp.jus.br/cpopg/search.do?"

  url2<- "https://esaj.tjsp.jus.br/cpopg/trocarPagina.do?"
  
  query1 <-
    list(
      conversationId = "",
      dadosConsulta.localPesquisa.cdLocal = "-1",
      cbPesquisa = "NUMOAB",
      dadosConsulta.tipoNuProcesso = "UNIFICADO",
      dadosConsulta.valorConsulta = oab,
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
  
 processos<- purrr::map_dfr(paginas,purrr::possibly(~{
    
     query2 <-
       list(
         paginaConsulta = .x,
         conversationId = "",
         dadosConsulta.localPesquisa.cdLocal = "-1",
         cbPesquisa = "NUMOAB",
         dadosConsulta.tipoNuProcesso = "UNIFICADO",
         dadosConsulta.valorConsulta = oab,
         uuidCaptcha = ""
       )
     
     
    resposta <- httr::RETRY("GET",
                             url = url2, query = query2,
                             quiet = TRUE, httr::timeout(2)
    )
    
    conteudo <- httr::content(resposta)
    
    processo<- xml2::xml_find_all(conteudo,"//div[@class='nuProcesso']") %>% 
      xml2::xml_text(trim=TRUE) %>% 
      stringr::str_remove_all("\\s.+")
    
   tibble::tibble(processo)
         }, NULL))
}