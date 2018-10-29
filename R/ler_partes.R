#' Aplica parser para extrair informações sobre as partes do
#'     processo de primeira ou de segunda instância.
#'
#' @param diretorio Diretório onde se encontram os htmls baixados.
#'
#' @return tabela com informações das partes
#' @export
#'
#' @examples
#' \dontrun{
#' partes<-ler_partes(path=".")
#' }
ler_partes<-ler_partes_cpopg<-ler_partes_cposg<-function(diretorio="."){
  a<- list.files(path=diretorio,pattern=".html",full.names = T)

  processo<-stringr::str_extract(a,"\\d{20}") %>%
    abjutils::build_id()

  future::plan("multiprocess")

  furrr::future_map2_dfr(a,processo,purrr::possibly(~{

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

