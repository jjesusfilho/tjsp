#' Extrai a tabela de incidentes
#'
#' @param arquivos se não informados, informar diretório
#' @param diretorio objeto ou diretorio  onde se encontram os htmls
#'
#' @return Tibble com urls dos incidentes
#' @export

#'
tjsp_ler_tabela_incidentes <- function(arquivos = NULL,diretorio = ".") {

  if (is.null(arquivos)){
    arquivos <- list.files(
      path = diretorio, pattern = ".html",
      full.names = TRUE
    )
  }

  pb <- progress::progress_bar$new(total = length(arquivos))

  purrr::map_dfr(arquivos, purrr::possibly(~{


    pb$tick()

    doc <- .x |>
      xml2::read_html()

    processo <- doc |>
      xml2::xml_find_first("//span[contains(@class,'unj-larger')]") |>
      xml2::xml_text() |>
      stringr::str_squish() |>
      stringr::str_remove_all("[^\\d+\\s]") |>
      stringr::str_trim()

    codigo_processo <- doc |>
      xml2::xml_find_first("//a[contains(@href,'processo.codigo')]/@href|//form[contains(@action,'processo.codigo')]/@action") |>
      xml2::xml_text() |>
      stringr::str_extract("(?<=processo.codigo=)\\w+")

    data_recebimento <- doc |>
      xml2::xml_find_all(xpath = "//div/h2[contains(text(),'Incidentes')]/../following-sibling::table[1]//td[@width=140]") |>
      xml2::xml_text(trim = T) |>
      lubridate::dmy()


    classe <-  doc |>
      xml2::xml_find_all(xpath = "//div/h2[contains(text(),'Incidentes')]/../following-sibling::table[1]//a") |>
      xml2::xml_text() |>
      stringr::str_squish()


    url <-  doc |>
      xml2::xml_find_all(xpath = "//div/h2[contains(text(),'Incidentes')]/../following-sibling::table[1]//a") |>
      xml2::xml_attr("href") |>
      xml2::url_absolute("https://esaj.tjsp.jus.br")

    tibble::tibble(processo, codigo_processo,data_recebimento, classe, url) |>
      dplyr::mutate(codigo_incidente = stringr::str_extract(url, "(?<=codigo=)[^&]+"), .after = codigo_processo) |>
      dplyr::mutate(codigo_local = stringr::str_extract(url, "(?i)(?<=cdLocal=)\\d+"), .after = codigo_incidente)


  }, otherwise = NULL))
}
