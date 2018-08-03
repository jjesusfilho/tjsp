#' Aplica parser para extrair informa\u00e7\u00f5es sobre as partes do
#'     processo.
#'
#' @param path diret\u00f3rio onde se encontram os htmls baixados.
#'
#' @return tabela com informa\u00e7\u00f5es das partes.
#' @export
#'
#' @examples
#' partes<-ler_partes(path=".")

ler_partes<-function(path="."){
  a<- list.files(path=path,pattern=".html",full.names = T)

  processo<-stringr::str_extract(a,"\\d{20}")

  purrr::map2_dfr(a,processo,purrr::possibly(~{

    parte_nome<-xml2::read_html(.x) %>%
      xml2::xml_find_all('//td/*[contains(@class,"mensagemExibindo")]/../following-sibling::td') %>%
      #xml2::xml_find_all('//table[1]//td') %>%
      xml2::xml_text() %>%
      stringr::str_trim() %>%
      stringr::str_squish() %>%
      stringr::str_split("\\w+:&nbsp") %>%
      unlist()

    parte<- xml2::read_html(.x) %>%
      xml2::xml_find_all('//td/span[@class="mensagemExibindo"]') %>%
      #xml2::xml_find_all('//table[1]//td') %>%
      xml2::xml_text() %>%
      stringr::str_extract("\\w+")

    tibble::tibble(processo=.y, parte_nome=parte_nome,parte=parte)
  },otherwise=NULL))
}

