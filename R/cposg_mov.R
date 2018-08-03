#' Extrai a movimenta\\u00e7\\u00e3o processual de segunda inst\\u00e2ncia
#'
#' @param processos n\\u00famero do processo de segunda inst\\u00e2ncia no formato NNNNNNN.DD.AAAA.J.TR.OOOO
#'
#' @return data frame com a movimenta\u00e7\u00e3o processual.
#' @export
#'
#' @examples
#'
#' movimentacao<-cposg_mov("1049363-11.2015.8.26.0002")

cposg_mov<-function(processos=NULL){

  unificado<-processos %>% str_extract(".{15}")
  foro<- processos %>% str_extract("\\d{4}$")

  httr::set_config(httr::config(ssl_verifypeer = FALSE ))
  url<-"https://esaj.tjsp.jus.br/cposg/search.do?"

  l<-vector("list",length(processos))

  for (i in seq_along(processos)) {


    tryCatch({

      if (stringr::str_count(processos[i]) != 25 ) l[[i]]<-NULL

      else

      query<-list(cbPesquisa= "NUMPROC",
           conversationId = "",
           dePesquisat="",
           dePesquisaNuUnificadot=processos[i],
           foroNumeroUnificadot= foro[i],
           localPesquisa.cdLocalt="-1",
           numeroDigitoAnoUnificadot=unificado[i],
           paginaConsultat="1",
           tipoNuProcesso="UNIFICADO",
           uuidCaptchat="")

  l[[i]]<-httr::GET(url,query=query) %>%
    httr::content("text") %>%
    xml2::read_html() %>%
    rvest::html_node(xpath="//table[@id='tabelaTodasMovimentacoes']") %>%
    rvest::html_table(fill=TRUE) %>%
    dplyr::mutate(X1=lubridate::dmy(X1),
           X3=stringr::str_squish(X3),
           X2=NULL) %>%
    magrittr::set_names(c("data","movimento")) %>%
    cbind(processo=processos[i],.)

  }, error=function(e){

      e

    },finally ={

      next

    })

    }

  return(l)
}

