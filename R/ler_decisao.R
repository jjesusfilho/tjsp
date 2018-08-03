ler_decisao <- function(path = ".") {

  a <- list.files(path = path,
                  pattern = ".html",
                  full.names = T)

  processo <- stringr::str_extract(a, "\\d{20}")

  lista<-vector("list",length(a))

  ## Controle de erro e retorno da informação constante no html,
  ## quando não aparece a decisão
  tentativa<-function(x,y){
    tryCatch({
     x[i] %>%
      xml2::read_html() %>%
      xml2::xml_find_all(
        "//table/tr/td/h2[@class='subtitle'][contains(.,'Julgamentos')]/following::table[position()=2]") %>%
      rvest::html_table() %>%
      purrr::pluck(1) %>%
      setNames(c("data_julgamento", "situacao_julgamento", "decisao")) %>%
      cbind(processo = y[i], .,stringsAsFactors=F)

  },error=function(e){
    x[i] %>%
      xml2::read_html() %>%
      xml2::xml_find_all("//table/tr/td/h2[@class='subtitle'][contains(.,'Julgamentos')]/following::table[position()=1]") %>%
      rvest::html_text() %>%
      stringr::str_trim() %>%
      tibble::tibble(processo=y[i],data_julgamento=NA_character_,situacao_julgamento=.,decisao="a decisao não foi disponibilizada no andamento")
    })
  }

  for (i in seq_along(lista)){
 lista[[i]]<- tentativa(a,processo)
 }
 do.call(rbind,lista)
}
