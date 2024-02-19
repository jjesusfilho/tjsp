#' Extrai a movimentação processual de primeira e de segunda instância
#'
#' @param arquivos se não informados, informar diretório
#' @param diretorio objeto ou diretorio  onde se encontram os htmls
#'
#' @return tibble com a movimentação processual.
#' @export
#' @examples
#' \dontrun{
#' andamento_cposg <- ler_movimentacao_cposg()
#' andamento_cpopg <- ler_movimentacao_cpopg()
#' }
#'

tjsp_ler_movimentacao <- function (arquivos = NULL, diretorio = ".") {
  if (is.null(arquivos)) {
    arquivos <- list.files(
      path = diretorio,
      pattern = ".html",
      full.names = TRUE
    )
  }
  
  purrr::map_dfr(arquivos, purrr::possibly(~{
    
    resposta <- xml2::read_html(.x)
    
    processo <- resposta |>
      xml2::xml_find_first("//span[contains(@class,'unj-larger')]") |>
      xml2::xml_text() |>
      stringr::str_squish() |>
      stringr::str_remove_all("[^\\d+\\s]") |>
      stringr::str_trim()
    
    cd_processo <- resposta |>
      xml2::xml_find_first("//script[contains(text(),'processo.codigo')]|//script[contains(text(),'cdProcesso')]") |>
      xml2::xml_text() |>
      stringr::str_extract("(?<=(processo.codigo|cdProcesso)=)\\w+")
    
    movs <- resposta |>
      xml2::xml_find_first("//table/tbody[@id='tabelaTodasMovimentacoes']") |>
      xml2::xml_find_all("./tr")
    
    dt_mov <- movs |>
      xml2::xml_find_first("./td[contains(@class,'dataMovimentacao')]") |>
      xml2::xml_text(trim = TRUE) |>
      lubridate::dmy()
    
    anexo <- movs |>
      xml2::xml_find_first("./td[contains(@class,'descricaoMovimentacao')]/a") |>
      xml2::xml_attr("href")
    
    cd_documento <- anexo |>
      stringr::str_extract("(?<=cdDocumento=)\\d+")
    
    
    recurso_acessado <- anexo |>
      stringr::str_extract("(?<=Acessado=).+") |>
      URLdecode() |>
      stringr::str_replace_all("\\+", " ")

  url <- anexo |> 
       purrr::map_chr(~{
         
 if (is.na(.x)){
      
      
    url <-  NA_character_ 
 
 } else if(startsWith(cd_processo, "RI")) {

   
   p <- processo |> 
        abjutils::build_id()
   
   unificado <- p |> 
     stringr::str_sub(1,15)
   
   foro <- stringr::str_sub(p, -4)
   
   parseada <-
     structure(
       list(
         scheme = "https",
         hostname = "esaj.tjsp.jus.br",
         port = NULL,
         path = "cposg/search.do",
         query = list(
           conversationId = "",
           paginaConsulta = "0",
           cbPesquisa = "NUMPROC",
           numeroDigitoAnoUnificado = unificado,
           foroNumeroUnificado = foro,
           dePesquisaNuUnificado = p,
           dePesquisaNuUnificado = "UNIFICADO",
           dePesquisa = "",
           tipoNuProcesso = "UNIFICADO"
         ),
         params = NULL,
         fragment = stringr::str_remove(.x,"#"),
         username = NULL,
         password = NULL
       ),
       class = "url"
     )
   
   url <- httr::build_url(parseada)
   
 } else {
   
   url <- xml2::url_absolute(.x,"https://esaj.tjsp.jus.br")
   
 }
    url
       })
  
  
    mov <- movs |>
      xml2::xml_find_first("./td[contains(@class,'descricaoMovimentacao')]") |>
      xml2::xml_text(trim=TRUE)
    
    tibble::tibble(
      processo,
      cd_processo,
      dt_mov,
      mov,
      cd_documento,
      recurso_acessado,
      url
    ) |>
      tidyr::separate(
        col = mov,
        into = c("movimento", "descricao"),
        sep = "\n\\s+"
      )
  }, otherwise = NULL), .progress = TRUE)
}
