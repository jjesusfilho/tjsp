#' Função tjsg_meta
#'
#' Esta função extrai metadados das decisões de segundo grau do TJSP
#' @param livre palavra ou texto a ser buscado nas ementas e nos acórdãos
#' @param quote logical. Colocar a expressão entre aspas?
#' @param classes.value Código
#' @param inicio  data inicial
#' @param fim  Data final
#' @param paginas paginas a serem buscadas
#' @keywords tjsp
#' @export
#' @examples
#' tjsg_meta(livre="Lei Maria da Penha")

tjsg_meta<-function(livre,quote=TRUE,classes.value="",inicio="",fim="",paginas=NULL,tipo="A"){
  httr::set_config(httr::config(ssl_verifypeer = FALSE ))
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
  body[[35]]<-tipo
  a<-httr::POST("https://esaj.tjsp.jus.br/cjsg/resultadoCompleta.do",encode="form",
          body=body)
  if(length(paginas)==0){
    max_pag<-a %>%
      httr::content("parsed") %>%
      xml2::xml_find_all(xpath="//*[@id='totalResultadoAba-A']") %>%
      xml2::xml_attrs() %>%
      .[[1]] %>%
      .[3] %>%
      as.numeric() %>%
      `/`(20) %>%
      ceiling()
    paginas<-1:max_pag
  }
  l<-vector("list",max_pag)

  for (i in seq_along(l)){
    tryCatch({
      c <- httr::GET(paste0("https://esaj.tjsp.jus.br/cjsg/trocaDePagina.do?tipoDeDecisao=A&pagina=",i), set_cookies(unlist(a$cookies)))
      d <- httr::content(c)
      aC <-xml2::xml_find_all(d,'//*[@class="assuntoClasse"]') %>%
        xml2::xml_text(trim=T) %>%
        stringr::str_match("(?:Classe.Assunto.\\s+)(\\w.*?)(?: / )(.*)")
      classe<-aC[,2]
      assunto<-aC[,3]
      relator<-xml2::xml_find_all(d,'//tr[2][@class="ementaClass2"][1]') %>%
      xml2::xml_text(trim=T)
      comarca<-xml2::xml_find_all(d,'//*[@class="ementaClass2"][2]') %>%
      xml2::xml_text(trim=T)
      orgao_julgador<-xml2::xml_find_all(d,'//*[@class="ementaClass2"][3]') %>%
      xml2::xml_text(trim=T)
      data_julgamento<-xml2::xml_find_all(d,'//*[@class="ementaClass2"][4]') %>%
      xml2::xml_text(trim=T)
      data_publicacao<-xml2::xml_find_all(d,'//*[@class="ementaClass2"][5]') %>%
      xml2::xml_text(trim=T)
      ementa<-xml2::xml_find_all(d,'//*[@class="mensagemSemFormatacao"]') %>%
      xml2::xml_text(trim=T)
      processo<-xml2::xml_find_all(d,'//*[@class="esajLinkLogin downloadEmenta"]') %>%
      xml2::xml_text(trim=T)
      cdacordao<-xml2::xml_find_all(d,'//a[1]/@cdacordao') %>%
      xml2::xml_text()
      l[[i]]<-data.frame(pagina=i,classe,assunto,relator,comarca,orgao_julgador,data_julgamento,data_publicacao,processo,ementa,cdacordao)
    },
    error=function(m){
      m
    },
    finally={
      next
    })
    sys.sleep(2)
  }
  df<-do.call(rbind,l)

  df %<>% dplyr::modify_at(vars(4:8),funs(str_replace(.,".*:\\s*","")))
  df$url<-paste0("https://esaj.tjsp.jus.br/cjsg/getArquivo.do?cdAcordao=",df$cdacordao,"&cdForo=0")
  return(df)
}
