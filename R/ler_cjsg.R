#' Ler metadados das decisões de segunda instância do TJSP
#'
#' @param diretorio Diretório onde se encontram os htmls.
#'
#' @return tabela com metadados jurisprudenciais.
#' @export
#'
#' @examples
#' \dontrun{
#' df<-ler_cjsg()
#' }
ler_cjsg<-function(diretorio="."){

  arquivos<- list.files(path=diretorio,pattern=".html",full.names = T)

 df<- purrr::map_dfr(arquivos,purrr::possibly(~{

  resposta<-xml2::read_html(.x)

  aC <-xml2::xml_find_all(resposta,'//*[@class="assuntoClasse"]') %>%
    xml2::xml_text(trim=T) %>%
    stringr::str_match("(?:Classe.Assunto.\\s+)(\\w.*?)(?: / )(.*)")
  classe<-aC[,2]
  assunto<-aC[,3]
  tabela<-xml2::xml_find_all(resposta,"//table//table[tr[@class]]")

  relator<-tabela %>%
    purrr::map_chr(~xml2::xml_find_first(.x,'.//td/strong[contains(.,"Relator(a):")]/following-sibling::text()') %>%
    xml2::xml_text(trim=T))

  comarca<-tabela %>%
    purrr::map_chr(~xml2::xml_find_first(.x,'.//*[@class="ementaClass2"][2]') %>%
    xml2::xml_text(trim=T))

  orgao_julgador<-tabela %>%
    purrr::map_chr(~xml2::xml_find_first(.x,'.//*[@class="ementaClass2"][3]') %>%
    xml2::xml_text(trim=T))

  data_julgamento<-tabela %>%
    purrr::map_chr(~xml2::xml_find_first(.x,'.//td/strong[contains(.,"Data do julgamento:")]/following-sibling::text()') %>%
    xml2::xml_text(trim=T))

  data_publicacao<-tabela %>%
      purrr::map_chr(~xml2::xml_find_first(.x,'.//*[contains(.,"Data de publica")]/following-sibling::text()') %>%
                       xml_text(trim=TRUE))

  ementa<-tabela %>%
    purrr::map_chr(~xml2::xml_find_first(.x,'.//*[@class="mensagemSemFormatacao"]') %>%
    xml2::xml_text(trim=T))

  processo<-tabela %>%
    purrr::map_chr(~xml2::xml_find_first(.x,'.//*[@class="esajLinkLogin downloadEmenta"]') %>%
    xml2::xml_text(trim=T))

  cdacordao<-tabela %>%
    purrr::map_chr(~xml2::xml_find_first(.x,'.//a[1]/@cdacordao') %>%
    xml2::xml_text(trim=TRUE))

  tibble::tibble(classe,assunto,relator,comarca,orgao_julgador,data_julgamento,data_publicacao,processo,ementa,cdacordao)
},otherwise=NULL)) %>%
   dplyr::mutate_at(dplyr::vars(c("relator","comarca","orgao_julgador","data_julgamento","data_publicacao")),
                     dplyr::funs(stringr::str_remove(.,".+\\:"))) %>%
   dplyr::mutate_at(dplyr::vars(c("data_julgamento","data_publicacao")),
                    dplyr::funs(lubridate::dmy(.))) %>%
   dplyr::mutate_all(dplyr::funs(stringr::str_squish(.)))
}
