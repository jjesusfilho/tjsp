#' Autenticar no tjsp
#'
#' @return Estabelece uma sessão, não é necessário salvar.
#' @export
#'
#' @details Ao chamar esta função, abrirá duas janelas sucessivas para
#'     digitar cpf e senha.

autenticar<- function(){

cpf<-askpass::askpass("Digite seu cpf")

senha<-askpass::askpass("Digite sua senha")

cpf<-  stringr::str_remove_all(cpf,"\\D+")
#cpf<-  stringr::str_c(stringr::str_extract(cpf,"\\d{3}"),".",stringr::str_extract(cpf,"(?=\\d{3})\\d{3}"),".",stringr::str_extract(cpf,"(?<=\\d{6})\\d{3}"),"-",stringr::str_extract(cpf,"\\d{2}$"))

senha<-as.character(senha)

#httr::handle("https://esaj.tjsp.jus.br/esaj/identificacao.do?retorno=https%3A//esaj.tjsp.jus.br/cpopg/open.do")

url<-"https://esaj.tjsp.jus.br/esaj/identificacao.do?retorno=https%3A//esaj.tjsp.jus.br/esaj/portal.do%3Fservico%3D740000"

h<-httr::handle(url)

e<-url %>%
  httr::GET(handle=h) %>%
  httr::content()

execucao<- e %>%
  xml2::xml_find_first("//*[@name='execution']") %>%
  xml2::xml_attr("value")

form<-list(username=cpf,
           password=senha,
           lt="",
           execution=execucao,
           `_eventId`="submit",
           # submit="",
           pbEntrar="Entrar",
           signature="",
           certificadoSelecionado="",
           certificado="")

httr::POST("https://esaj.tjsp.jus.br/sajcas/login?service=https%3A%2F%2Fesaj.tjsp.jus.br%2Fesaj%2Fj_spring_cas_security_check",
               body=form,encode="form")

}

