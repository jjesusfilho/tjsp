#' Função tjsg_meta
#'
#' Esta função extrai metadados das decisões de segundo grau do TJSP
#' @param livre palavra ou texto a ser buscado nas ementas e nos acórdãos
#' @param quote logical. Colocar a expressão entre aspas?
#' @param classes.value Código
#' @param inicio  Data inicial
#' @param fim  Data final
#' @keywords tjsp
#' @import XML
#' @import httr
#' @import stringr
#' @export
#' @examples
#' tjsg_meta(livre="Lei Maria da Penha")

tjsg_meta<-function(livre,quote=TRUE,classes.value="",inicio="",fim=""){
  set_config(config(ssl_verifypeer = FALSE ))
  body <- list(dados.buscaInteiroTeor ="", dados.pesquisarComSinonimos = "S",
               dados.pesquisarComSinonimos = "S", dados.buscaEmenta = "",
               dados.nuProcOrigem = "", dados.nuRegistro = "", agenteSelectedEntitiesList = "",
               contadoragente = "0", contadorMaioragente = "0", codigoCr = "",
               codigoTr = "", nmAgente = "", juizProlatorSelectedEntitiesList = "",
               contadorjuizProlator = "0", contadorMaiorjuizProlator = "0",
               codigoJuizCr = "", codigoJuizTr = "", nmJuiz = "", classesTreeSelection.values = "",
               classesTreeSelection.text = "", assuntosTreeSelection.values = "",
               assuntosTreeSelection.text = "", comarcaSelectedEntitiesList = "",
               contadorcomarca = "0", contadorMaiorcomarca = "0", cdComarca = "",
               nmComarca = "", secoesTreeSelection.values = "",
               secoesTreeSelection.text = "1",
               dados.dtJulgamentoInicio = "", dados.dtJulgamentoFim = "",
               dados.dtRegistroInicio = "", dados.dtRegistroFim = "",
               dados.origensSelecionadas = "T", tipoDecisaoSelecionados = "A",
               #tipoDecisaoSelecionados = "", tipoDecisaoSelecionados = "",
               dados.ordenacao = "data")
  if(quote==TRUE) livre<-deparse(livre)
  body[[1]]<-livre
  body[[19]]<-classes.value ##
  body[[30]]<-inicio ## colocar a data no formato dd/mm/aaa
  body[[31]]<-fim # idem
  a<-POST("https://esaj.tjsp.jus.br/cjsg/resultadoCompleta.do",encode="form",
          body=body)
  b<- htmlParse(content(a,as="text"), encoding = "UTF-8")
  val <- xmlGetAttr(getNodeSet(b, "//*[@id='totalResultadoAba-A']")[[1]],"value")
  num<-as.numeric(val)
  max_pag <- ceiling(num/20)
  df<-data.frame()
  for (i in 1:max_pag){
    tryCatch({
      c <- GET(paste0("https://esaj.tjsp.jus.br/cjsg/trocaDePagina.do?tipoDeDecisao=A&pagina=",i), set_cookies(unlist(a$cookies)))
      d <- htmlParse(content(c,as="text"), encoding = "UTF-8")
      aC <-xpathSApply(d,'//*[@class="assuntoClasse"]',xmlValue,trim=T)
      assunto.processo<-str_trim(str_extract(aC,".*?(?=/)"))
      assunto.materia<-str_trim(str_extract(aC,"(?<=/\\s{0,100}).*"))
      relator<-xpathSApply(d,'//*[@class="ementaClass2"][1]',xmlValue,trim=T)
      comarca<-xpathSApply(d,'//*[@class="ementaClass2"][2]',xmlValue,trim=T)
      orgao.julgador<-xpathSApply(d,'//*[@class="ementaClass2"][3]',xmlValue,trim=T)
      data.julgamento<-xpathSApply(d,'//*[@class="ementaClass2"][4]',xmlValue,trim=T)
      data.registro<-xpathSApply(d,'//*[@class="ementaClass2"][5]',xmlValue,trim=T)
      ementa<-xpathSApply(d,'//textarea',xmlValue,trim=T)
      processo<-xpathSApply(d,'//*[@class="esajLinkLogin downloadEmenta"]',xmlValue,trim=T)
      cdacordao<-xpathSApply(d,'//*[@class="downloadEmenta"]',xmlAttrs,trim=T)[2,]
      df1<-data.frame(pagina=i,assunto.processo,assunto.materia,relator,comarca,orgao.julgador,data.julgamento,data.registro,processo,ementa,cdacordao)
      df<-rbind(df,df1)
    },
    error=function(m){
      m
    },
    finally={
      next
    })
    sys.sleep(2)
  }
  df[4:8]<-lapply(df[4:8],function(x) str_replace(x,".*:\\s*",""))
  df$url<-paste0("https://esaj.tjsp.jus.br/cjsg/getArquivo.do?cdAcordao=",df$cdacordao,"&cdForo=0")
  return(df)
}

