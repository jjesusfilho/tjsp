#' Converte documentos em base 64 para pdf.
#'
#' @param arquivos Vetor de arquivos
#' @param dir_origem Alternativamente informar origem
#' @param dir_destino Diretório destino.
#'
#' @return pdfs
#' @export
#'
mni_base64_doc <- function(arquivos = NULL, dir_origem  = ".", dir_destino = "."){
  
  
  if(is.null(arquivos)){
    
    
    arquivos <- list.files(dir_origem, full.names = TRUE, pattern = "xml$")
    
  }
  
  
  purrr::walk(arquivos, purrr::possibly(~{
    
    
    
    processo <- stringr::str_extract(.x, "\\d{20}")
    
    x <- .x |>
      xml2::read_xml() |>
      xml2::xml_find_all("//ns2:dadosBasicos/following-sibling::ns2:documento")
    
    x |>
      purrr::walk(purrr::possibly(~{
        
        
        
        id_documento <-   .x |>
          xml2::xml_attr("idDocumento") |> 
          stringr::str_split_1("\\D+")
        
        digito <- id_documento[2]
        
        id_documento <- id_documento[1]
        
        arquivo <- file.path(dir_destino, paste0("processo_", processo, "_id_documento_",id_documento,"_digito_", digito, ".pdf"))
        
        .x |>
          xml2::xml_find_all(".//ns2:conteudo") |>
          xml2::xml_text() |>
          base64enc::base64decode() |>
          writeBin(arquivo)

      }, NULL))
    
    
  }, NULL))
  
  
}
