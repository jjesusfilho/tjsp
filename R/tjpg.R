#' Função tjpg
#'
#' Esta função extraí dados de busca livre por processos de primeira instância no TJSP
#' @param url Faça primeiramente a busca na página do TJSP, depois copie e cole a url aqui.
#' @keywords tjsp primeira instância
#' @import httr
#' @import XML
#' @import stringr
#' @export
#' @examples
#' tjsg(url)

tjpg<-function (url){
  httr::set_config( httr::config( ssl_verifypeer = FALSE ))
  a<- GET(url)
  b<-htmlParse(content(a,as="text"))
  val<-xpathApply(b,"//*[@bgcolor='#EEEEEE']",xmlValue,trim=T)[[1]]
  val<-str_extract(val,"\\d+$")
  num<-as.numeric(val)
  max_pag<-ceiling(num/10)

  df <- data.frame()
  for (i in 1:max_pag) {
    tryCatch({
      c <- GET(paste0("https://esaj.tjsp.jus.br/cjpg/trocarDePagina.do?pagina=",
                      i,"&conversationId="), set_cookies(unlist(a$cookies)))
      d <- htmlParse(content(c, as = "text"), encoding = "UTF-8")
      s<-xpathApply(d,"//*[@id='divDadosResultado']/table//td//td[@align='left']",xmlValue,trim=T)
      s<-matrix(s,nrow=15,ncol=9,byrow=T)
      df1<-as.data.frame(s)
      names(df1)<-c("processo","classe","assunto","magistrado","comarca","foro","vara","disponibilizacao","decisao")
      df1$pagina<-i
      df1[2:8]<-lapply(df1[2:8],function(x) str_replace(x,".*:\\s?",""))
      df1[]<-lapply(df1[],str_trim)
      df <- rbind(df, df1)
    }, error = function(m) {
      m
    }, finally = {
      next
    })
    sys.sleep(1)
  }
  return(df)
}
