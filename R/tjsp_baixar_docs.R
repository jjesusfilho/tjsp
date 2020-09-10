#' Baixar petições e docs a partir da tabela lida com tjsp_ler_tabela_docs
#'
#' @param processos Número do processo
#' @param doc_id Id do documento
#' @param urls Urls dos documentos
#' @param diretorio Diretório
#'
#' @return pdfs
#' @export
#'
tjsp_baixar_docs <- function(processos  = NULL, doc_id = NULL, urls = NULL, diretorio = NULL){


  processos <- stringr::str_remove_all(processos,"\\D")

  id <- stringr::str_c(processos,"_","doc_id",doc_id)

  pb <- progress::progress_bar$new(total = length(processos))


  purrr::walk2(urls,id,purrr::possibly(~{

    pb$tick()

    arquivo <- file.path(diretorio,paste0(.y,".pdf"))

    httr::GET(.x,httr::write_disk(arquivo,overwrite = TRUE))




  },NULL))

}
