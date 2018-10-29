#' Lê o dispositivo das decisões de segunda instância a partir dos htmls
#'
#' @param diretorio Diretório onde se encontram os htmls baixados
#'
#' @return tibble com as os numéros dos processos e respectivas decisões
#' @export
#'
#' @examples
#' \dontrun{
#' decisoes<-ler_decisoes_cposg().
#' }
ler_decisoes_cposg <- function(diretorio = ".") {

  a <- list.files(path = diretorio,
                  pattern = ".html",
                  full.names = T)

  processo<-stringr::str_extract(a,"\\d{20}") %>%
    abjutils::build_id()

  lista <- listenv::listenv()

  ## Controle de erro e retorno da informação constante no html,
  ## quando não aparece a decisão
  tentativa <- function(x,y){
    tryCatch({
     x[ii] %>%
      xml2::read_html() %>%
      xml2::xml_find_all(
        "//table/tr/td/h2[@class='subtitle'][contains(.,'Julgamentos')]/following::table[position()=2]") %>%
      rvest::html_table() %>%
      purrr::pluck(1) %>%
      stats::setNames(c("data_julgamento", "situacao_julgamento", "decisao")) %>%
      cbind(processo = y[ii], .,stringsAsFactors=F)

  },error=function(e){
    x[ii] %>%
      xml2::read_html() %>%
      xml2::xml_find_all("//table/tr/td/h2[@class='subtitle'][contains(.,'Julgamentos')]/following::table[position()=1]") %>%
      rvest::html_text() %>%
      stringr::str_trim() %>%
      tibble::tibble(processo=y[ii],data_julgamento=NA_character_,situacao_julgamento=.,decisao="a decisao não foi disponibilizada no andamento")
    })
  }

future::plan("multiprocess")

for (ii in length(a)){

 lista[[ii]]  %<-% {
   tentativa(a,processo)
 }
}
lista<-as.list(lista)
do.call(rbind,lista)
}
