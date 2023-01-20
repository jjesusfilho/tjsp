#' Ler tabelas baixadas com tjsp_baixar_tabela_docs
#'
#' @param arquivos Vetor de arquivos
#' @param diretorio Informar se não informar diretório
#'
#' @return tibble
#' @export
#'
tjsp_ler_tabela_docs <- function(arquivos = NULL, diretorio = "."){
  
  if (is.null(arquivos)) {
    
    arquivos <- list.files(diretorio,full.names = TRUE)
    
  }
  
  pb <- progress::progress_bar$new(total = length(arquivos))
  
  purrr::map_dfr(arquivos,purrr::possibly(~{
    
    pb$tick()
    
    processo <- stringr::str_extract(.x,"\\d{20}")
    
    suppressMessages({
      
       doc <-   .x %>%
        xml2::read_html() %>%
        xml2::xml_text() %>%
        stringr::str_extract("(?<=requestScope = )\\X+?(?=;)") |> 
        jsonlite::fromJSON()
       
  
  doc_name <- tibble::tibble(doc_name= doc$data$title) |> 
              tibble::rownames_to_column("id_doc")
       
  paginas  <- doc$children[[2]]$data$indicePagina
  
 df <- purrr::imap_dfr(doc$children,~{
    
   url_doc <-  .x$data$parametros
   pagina <- .x$data$indicePagina
   
  tibble::tibble(id_doc = .y, pagina, url_doc) |> 
          dplyr::mutate(doc_id = as.character(doc_id))
  
  }) |> 
   dplyr::left_join(doc_name) |> 
   dplyr::select(id_doc, doc_name, pagina, url_doc) |> 
   dplyr::mutate(url_doc = paste0("https://esaj.tjsp.jus.br/pastadigital/getPDF.do?",url_doc)) |> 
      dplyr::group_by(doc_id) |> 
   dplyr::mutate(pagina_inicial = dplyr::first(pagina),
          pagina_final = dplyr::last(pagina)) |> 
   dplyr::ungroup() |> 
   tibble::add_column(processo, .before =1)
    
    }) 
    
  }, NULL))
}
