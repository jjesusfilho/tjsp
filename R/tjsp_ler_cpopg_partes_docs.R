#' Lê dados das partes baixados com tjsp_baixar_cpopg_partes_docs
#'
#' @param arquivos Vetor de arquivos
#' @param diretorio Diretório se não forem informados os arquivos
#' @param rm_listas Remover colunas listas? Geralmente estão vazias.
#' @param rm_na Remover colunas em todas as linhas são NA?
#'
#' @return Tibble
#' @export
#'
tjsp_ler_cpopg_partes_docs <- function(arquivos = NULL,
                                       diretorio = ".",
                                       rm_listas = FALSE,
                                       rm_na = FALSE){
  
  
  if (is.null(arquivos)){
    
    arquivos <- list.files(diretorio, full.names = T, pattern= "json$")
  }
  
  
  pb <- progress::progress_bar$new(total = length(arquivos))
  
  dados <-   purrr::map_dfr(arquivos, purrr::possibly(~{
    
    pb$tick()
    
    cd_processo <- stringr::str_extract(.x, "(?<=processo_)[a-zA-Z0-9]+")
    
    jsonlite::fromJSON(.x, flatten = TRUE) |> 
      tibble::add_column(cd_processo,.before = 1)
    
  },NULL))
  
  if (rm_listas){
    
    dados <- dados |> 
      dplyr::select(!dplyr::where(is.list))
    
  }
  
  
  if (rm_na){
    
    dados <- dados |> 
      dplyr::select(!dplyr::where(~is.na(.x) |> all()))
  }
  
  return(dados)
  
}
