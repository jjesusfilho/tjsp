#' Lê municípios e respectivos fóruns baixados com baixar_municipios
#'
#' @param arquivos Vetor de arquivos
#' @param diretorio Alternativamente informar diretório
#'
#' @return tibble
#' @export
#'
ler_municipios <- function(arquivos = NULL, diretorio = "."){

  if (is.null(arquivos)){

    arquivos <- list.files(diretorio, full.names  = T, pattern = "htm$")

  }

  pb <- progress::progress_bar$new(total = length(arquivos))

  purrr::map_dfr(arquivos, purrr::possibly(~{

    pb$tick()

    codigo <- stringr::str_extract(.x, "(?<=sp_)\\d+")

    x <- xml2::read_html(.x)

    municipio <- x |>
      xml2::xml_find_first("//a[contains(text(), 'entre S\u00E3o Paulo')]") |>
      xml2::xml_text() |>
      stringr::str_squish() |>
      stringr::str_extract("(?<=Paulo e ).+")

    if (is.na(municipio)){

      municipio <- x |>
        xml2::xml_find_first("//h1[@id = 'h2']") |>
        xml2::xml_text()

    }

    forum <- x |>
      xml2::xml_find_all("//b[contains(text(),'F\u00F3rum ')]") |>
      xml2::xml_text() |>
      stringr::str_squish()

    if (length(forum)==0){

      forum <- x |>
        xml2::xml_find_all("//p[contains(text(),'F\u00D3RUM ')]") |>
        xml2::xml_text() |>
        stringr::str_squish() |>
        stringr::str_extract("(?i)f\u00F3rum.+?(?= -)")
    }

    tibble::tibble(codigo, municipio, forum)

  },NULL))
}
