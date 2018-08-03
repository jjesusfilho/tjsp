#' Lê conteúdo das decisões monocráticas
#'
#' @param path Diretório onde se encontram os htmls
#'
#' @return tibble com os htmls.
#' @export
#'
#' @examples
ler_decisoes_monocraticas<-function(path="."){
  a<- list.files(path=path,pattern=".html",full.names = T)

  processo<-stringr::str_extract(a,"\\d{20}")

  purrr::map2_dfr(a,processo,purrr::possibly(~{
    decisao<-xml2::read_html(.x) %>%
      rvest::html_nodes(xpath="//a[contains(text(),'Decisão Monocrática')]/following-sibling::span") %>%
      rvest::html_text()
    tibble::tibble(processo=.y,decisao=decisao)

  },otherwise=NULL))

}
