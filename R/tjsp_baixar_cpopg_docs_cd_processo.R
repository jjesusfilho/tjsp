#' Baixar petições e docs a partir da tabela lida com tjsp_ler_tabela_docs
#'
#' @param df dataframe com colunas processo, id_doc e url_doc
#' @param diretorio Diretório
#'
#' @return pdfs
#' @export
#'
tjsp_baixar_docs_cd_processo <- function(df,diretorio = "."){
  
  lista <- df |>
    dplyr::group_split(cd_processo_pg)
  
  
  httr::set_config(httr::config(ssl_verifypeer = FALSE))
  
  
  uri_pg <- "https://esaj.tjsp.jus.br/cpopg/search.do?gateway=true"
  
  uri_sg <- "https://esaj.tjsp.jus.br/cposg/search.do?gateway=true"
  
  
  
  
  purrr::walk(lista, purrr::possibly(~{
        
    
    cd_processo <- .x$cd_processo_pg |> unique()
    
    
    if (!all(is.na(.x$cd_processo_sg))) {
      
      sg <- unique(.x$cd_processo_sg)
      
      r1 <- httr::GET(paste0("https://esaj.tjsp.jus.br/cposg/show.do?processo.codigo=",sg , "&gateway=true"))
      
      url2 <- paste0("https://esaj.tjsp.jus.br/cposg/verificarAcessoPastaDigital.do?cdProcesso=", sg,"&conversationId=&_=1599440192646")
      

    } else {
      
      url1 <- paste0("https://esaj.tjsp.jus.br/cpopg/show.do?processo.codigo=",cd_processo , "&gateway=true")

      url2 <- paste0("https://esaj.tjsp.jus.br/cpopg/abrirPastaDigital.do?processo.codigo=",cd_processo)
      
      if (stringr::str_detect(cd_processo, "^DW")){
        url1 <- paste0(url1,"&consultaDeRequisitorios=true")
      }
      
      r1 <- httr::GET(url1)
      
    }
    
    r2 <-  url2 |>
      httr::GET() |>
      httr::content("text") |>
      httr::GET()
    
    tjsp_baixar_docs_cd_processo1(.x$cd_processo_pg, .x$id_doc, .x$pagina_inicial, .x$pagina_final, .x$url_doc, diretorio)
    
    
  },NULL))
  
}



#' Função interna do tjsp_baixar_cpopg_docs
#' @param cd_processo Número do processo
#' @param id_doc Id do documento
#' @param pagina_inicial pagina inicial
#' @param pagina_final pagina final
#' @param urls Urls dos documentos
#' @param diretorio Diretório
#'
#' @return pdfs
#'
tjsp_baixar_docs_cd_processo1 <- function(cd_processo  = NULL,
                                          id_doc = NULL,
                                          pagina_inicial = NULL,
                                          pagina_final = NULL,
                                          urls = NULL,
                                          diretorio = NULL){
  
  
  
  id <- stringr::str_c(cd_processo,
                       "_id_doc_",id_doc,
                       "_pagina_inicial_",pagina_inicial,
                       "_pagina_final_", pagina_final
  )
  
  
  
  purrr::walk2(urls,id,purrr::possibly(~{
    
    
    arquivo <- file.path(diretorio,paste0(.y,".pdf"))
    
    httr::GET(.x,httr::write_disk(arquivo,overwrite = TRUE))
    
  },NULL), .progress = TRUE)
  
}
