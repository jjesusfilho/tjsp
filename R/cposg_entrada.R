#' #' Vou manter esta função comentada  por ora para assegurar que que não estou matando algo que não deveria.
#' #'
#' #' @param processos
#' #'
#' #' @return data de entada do recurso
#' #'
#' #'
#' #' examples
#' #' \dontrun{
#' #' cposg_entrada
#' #'
#' cposg_entrada<-function(processos=NULL){
#'
#'   unificado<-processos %>% str_extract(".{15}")
#'   foro<- processos %>% str_extract("\\d{4}$")
#'
#'   httr::set_config(httr::config(ssl_verifypeer = FALSE ))
#'   url<-"https://esaj.tjsp.jus.br/cposg/search.do?"
#'
#'   l<-vector("list",length(processos))
#'
#'   for (i in seq_along(processos)) {
#'
#'
#'     tryCatch({
#'
#'       if (stringr::str_count(processos[i]) != 25 ) l[[i]]<-NULL
#'
#'       else
#'
#'         query<-list(cbPesquisa= "NUMPROC",
#'                     conversationId = "",
#'                     dePesquisat="",
#'                     dePesquisaNuUnificadot=processos[i],
#'                     foroNumeroUnificadot= foro[i],
#'                     localPesquisa.cdLocalt="-1",
#'                     numeroDigitoAnoUnificadot=unificado[i],
#'                     paginaConsultat="1",
#'                     tipoNuProcesso="UNIFICADO",
#'                     uuidCaptchat="")
#'
#'       l[[i]]<-httr::GET(url,query=query) %>%
#'         httr::content("text") %>%
#'         xml2::read_html() %>%
#'         rvest::html_nodes(xpath="//div[@class='espacamentoLinhas']") %>%
#'         rvest::html_text() %>%
#'         stringr::str_extract("\\d{2}/\\d{2}/\\d{4}") %>%
#'         lubridate::dmy() %>%
#'         max() %>%
#'         data.frame(processo=processos[i],data=.)
#'
#'     }, error=function(e){
#'
#'       e
#'
#'     },finally ={
#'
#'       next
#'
#'     })
#'
#'   }
#'
#'   return(l)
#' }
