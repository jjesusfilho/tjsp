#' Extrai o histórico o link para histórico das partes
#'
#' @param arquivos se não informados, informar diretório
#' @param diretorio objeto ou diretorio  onde se encontram os htmls
#'
#' @return tibble com urls das partes
#' @export

#'
tjsp_ler_url_historico_partes <- function(arquivos = NULL,diretorio = ".") {
  
  if (is.null(arquivos)){
    arquivos <- list.files(
      path = diretorio, pattern = ".html",
      full.names = TRUE
    )
  }
  
  
  purrr::map_dfr(arquivos, purrr::possibly(~{
    
    
    
    doc <-  xml2::read_html(.x)
    
    
    processo <- doc |>
      xml2::xml_find_first("//span[contains(@class,'unj-larger')]") |>
      xml2::xml_text() |>
      stringr::str_squish() |>
      stringr::str_remove_all("[^\\d+\\s]") |>
      stringr::str_trim()
    
    cd_processo <- doc |>
      xml2::xml_find_first("//a[contains(@href,'processo.codigo')]/@href|//form[contains(@action,'processo.codigo')]/@action") |>
      xml2::xml_text() |>
      stringr::str_extract("(?<=processo.codigo=)\\w+")
    
    parte <- doc |>
      xml2::xml_find_all(xpath = "//td[@class='nomeParteEAdvogado']/a") |>
      xml2::xml_text(trim = T)
    
    url_parte <- doc |> 
      xml2::xml_find_all(xpath = "//td[@class='nomeParteEAdvogado']/a") |>
      xml2::xml_attr("href") |> 
      xml2::url_absolute("https://esaj.tjsp.jus.br")
    
    tibble::tibble(processo, cd_processo, parte, url_parte)
    
  }, otherwise = NULL), .progress = TRUE)
}
