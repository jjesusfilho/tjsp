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

  pb <- progress::progress_bar$new(total = length(arquivos))

  purrr::map_dfr(arquivos, purrr::possibly(~{


    pb$tick()

    processo <- stringr::str_extract(.x, "\\d{20}")

    nomes <- xml2::read_html(.x) |>
      xml2::xml_find_all(xpath = "//div/h2[contains(text(),'Hist\u00f3rico de classes')]/../following-sibling::table//th") |>
      xml2::xml_text() |>
      janitor::make_clean_names()

    xml2::read_html(.x) |>
      xml2::xml_find_all(xpath = "//div/h2[contains(text(),'Hist\u00f3rico de classes')]/../following-sibling::table/tbody") |>
      xml2::xml_text() |>
      stringr::str_trim() |>
      stringr::str_split("\\s*\n\t\\s*") |>
      purrr::map_dfr(stats::setNames, nomes) |>
      dplyr::mutate(data = lubridate::dmy(data)) |>
      tibble::add_column(processo  = processo, .before = 1)


  }, otherwise = NULL))
}