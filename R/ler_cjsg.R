#' Ler metadados das decisões
#'
#' @param path Diretório onde se encontram os htmls.
#'
#' @return tabela com metadados jurisprudenciais.
#' @export
#'
#' @examples
#' cjsg<-ler_cjsg()
#'
ler_cjsg<-function(path="."){

  a<- list.files(path=path,pattern=".html",full.names = T)

 df<- purrr::map_dfr(a,purrr::possibly(~{

  resposta<-xml2::read_html(.x)

  aC <-xml2::xml_find_all(resposta,'//*[@class="assuntoClasse"]') %>%
    xml2::xml_text(trim=T) %>%
    stringr::str_match("(?:Classe.Assunto.\\s+)(\\w.*?)(?: / )(.*)")
  classe<-aC[,2]
  assunto<-aC[,3]
  relator<-xml2::xml_find_all(resposta,'//tr[2][@class="ementaClass2"][1]') %>%
    xml2::xml_text(trim=T)
  comarca<-xml2::xml_find_all(resposta,'//*[@class="ementaClass2"][2]') %>%
    xml2::xml_text(trim=T)
  orgao_julgador<-xml2::xml_find_all(resposta,'//*[@class="ementaClass2"][3]') %>%
    xml2::xml_text(trim=T)
  data_julgamento<-xml2::xml_find_all(resposta,'//*[@class="ementaClass2"][4]') %>%
    xml2::xml_text(trim=T)
  data_publicacao<-xml2::xml_find_all(resposta,'//*[@class="ementaClass2"][5]') %>%
    xml2::xml_text(trim=T)
  ementa<-xml2::xml_find_all(resposta,'//*[@class="mensagemSemFormatacao"]') %>%
    xml2::xml_text(trim=T)
  processo<-xml2::xml_find_all(resposta,'//*[@class="esajLinkLogin downloadEmenta"]') %>%
    xml2::xml_text(trim=T)
  cdacordao<-xml2::xml_find_all(resposta,'//a[1]/@cdacordao') %>%
    xml2::xml_text()
  tibble::tibble(classe,assunto,relator,comarca,orgao_julgador,data_julgamento,data_publicacao,processo,ementa,cdacordao)
},otherwise=NULL)
)
}
