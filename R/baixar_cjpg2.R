#' Baixa decisões de primeiro grau do TJSP a partir da url criada na busca no próprio site.
#'
#' @param url Faça a busca no próprio site: (https://esaj.tjsp.jus.br/cjpg/)
#' @param diretorio Diretório onde serão armazenados os htmls.
#'
#' @return Baixa os htmls com metadados das decisãoes de primeiro grau.
#' @export
#'
#' @examples
baixar_cjpg2<-function(
  url="",
  diretorio="."
){

  httr::set_config(httr::config(ssl_verifypeer = FALSE ))


  resposta <- httr::GET(url)

  paginas <- resposta  %>%
    httr::content() %>%
    xml2::xml_find_first(xpath="//*[@bgcolor='#EEEEEE']") %>%
    xml2::xml_text(trim = T) %>%
    stringr::str_extract( "\\d+$") %>%
    as.numeric()

  max_pag <- ceiling(paginas/10)

  purrr::map(1:max_pag,purrr::possibly(~{

    httr::GET(paste0("http://esaj.tjsp.jus.br/cjpg/trocarDePagina.do?pagina=",
                     .x, "&conversationId="), httr::set_cookies(unlist(resposta$cookies)),
              httr::write_disk(paste0(diretorio,"/pagina_",.x,".html"),overwrite = T))
  }),otherwise=NULL)

}

