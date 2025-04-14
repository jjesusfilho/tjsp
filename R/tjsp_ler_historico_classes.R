#' Extrai o histórico das classes processuais
#'
#' @param arquivos se não informados, informar diretório
#' @param diretorio objeto ou diretorio  onde se encontram os htmls
#'
#' @return tibble com alterações das classes processuais
#' @export

#'
tjsp_ler_historico_classes <- function(arquivos = NULL,diretorio = ".") {

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

    nomes <- doc |>
      xml2::xml_find_all(xpath = "//div/h2[contains(text(),'Hist\u00f3rico de classes')]/../following-sibling::table//th") |>
      xml2::xml_text() |>
      janitor::make_clean_names()

    doc |>
      xml2::xml_find_all(xpath = "//div/h2[contains(text(),'Hist\u00f3rico de classes')]/../following-sibling::table/tbody") |>
      xml2::xml_text() |>
      stringr::str_trim() |>
      stringr::str_split("\\s*\n\t\\s*") |>
      purrr::map_dfr(stats::setNames, nomes) |>
      dplyr::mutate(data = lubridate::dmy(data)) |>
      tibble::add_column(processo  = processo,cd_processo,  .before = 1)


  }, otherwise = NULL), .progress = TRUE)
}
