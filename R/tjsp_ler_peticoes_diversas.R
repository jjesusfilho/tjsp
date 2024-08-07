#' Extrai tabela de petições diversas da busca cpopg
#'
#' @param arquivos se não informados, informar diretório
#' @param diretorio objeto ou diretorio  onde se encontram os htmls
#'
#' @return tibble com nomes das petições e datas
#' @export

tjsp_ler_peticoes_diversas <- function(arquivos = NULL,diretorio = ".") {

   if (is.null(arquivos)){
   arquivos <- list.files(
    path = diretorio, pattern = ".html",
    full.names = TRUE
  )  
}

  pb <- progress::progress_bar$new(total = length(arquivos))

  purrr::map_dfr(arquivos, purrr::possibly(~{


   doc <-    xml2::read_html(.x) 
    
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


       doc |>
      xml2::xml_find_all(xpath = "//div/h2[contains(text(),'Peti\u00E7\u00F5es diversas')]/../following-sibling::table[1]/tbody/tr") |>
      xml2::xml_text() |>
      stringr::str_trim() |>
      stringr::str_split("\n\\s+") |>
      purrr::map_dfr(stats::setNames, c("data","tipo")) |>
      dplyr::mutate(data = lubridate::dmy(data)) |>
      tibble::add_column(processo  = processo, cd_processo = cd_processo,  .before = 1)

  }, otherwise = NULL))
}
