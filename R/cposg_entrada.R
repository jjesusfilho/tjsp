#' Extrai data de entrada/recebimento da ação ou recurso em segunda instância
#' 
#' @param processos 
#'
#' @return
#' @export
#' @details Esta função foi criada somente para resolver retirar a entrada de 
#'     processos que entraram mais de uma vez. Ela pega somente a última data.
#'     Por ora, não vejo necessidade de pegar todas as entradas, vez que a finalidade
#'     é somente calcular o tempo do processo.
#' 
#' @examples
cposg_entrada<-function(processos=NULL){
  
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
        rvest::html_nodes(xpath="//div[@class='espacamentoLinhas']") %>% 
        rvest::html_text() %>%
        stringr::str_extract("\\d{2}/\\d{2}/\\d{4}") %>% 
        lubridate::dmy() %>% 
        max() %>% 
        data.frame(processo=processos[i],data=.)
      
    }, error=function(e){
      
      e
      
    },finally ={
      
      next
      
    })
    
  }
  
  return(l)
}
