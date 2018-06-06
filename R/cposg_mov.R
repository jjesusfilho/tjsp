#' Extrai a movimentação processual de segunda instância
#'
#' @param processos número do processo de segunda instância no formato NNNNNNN.DD.AAAA.J.TR.OOOO
#'
#' @return
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
           dePesquisa	="",
           dePesquisaNuUnificado	=processos[i],
           foroNumeroUnificado	= foro[i],
           localPesquisa.cdLocal	="-1",
           numeroDigitoAnoUnificado	=unificado[i],
           paginaConsulta	="1",
           tipoNuProcesso="UNIFICADO",
           uuidCaptcha	="")
  
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

