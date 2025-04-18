#' Ler Partes do cpopg ou cposg, quando baixados por cd_processo.
#'
#' @param arquivos Vetor de arquivos
#' @param diretorio Diretório se não informar arquivos
#' @details São criadas cinco colunas : processo,cd_processo
#'     tipo_parte, parte e representante. Esta última
#'     pode ser tanto o procurador quanto o representante
#'     legal.
#' @return tibble
#' @export
#'
tjsp_ler_partes_cd_processo <- function(arquivos = NULL,diretorio = ".") {

  if (is.null(arquivos)) {

    arquivos <- list.files(
      path = diretorio, pattern = ".html",
      full.names = TRUE
    )
  }


  lista <- purrr::map_dfr(arquivos,purrr::possibly(~{

    x <- xml2::read_html(.x)

    processo <- x |>
       xml2::xml_find_first("//span[@id='numeroProcesso']") |>
       xml2::xml_text() |>
       stringr::str_remove_all("\\D+")

    cd_processo <- .x |>
      stringr::str_extract("(?<=cd_processo_)\\w+")

    if (
      xml2::xml_find_first(x,"boolean(//table[@id='tableTodasPartes'])")
    ) {

      x |>
        xml2::xml_find_first("//table[@id='tableTodasPartes']") |>
        rvest::html_table() |>
        setNames(c("tipo_parte","parte")) |>
        tidyr::separate(parte,c("parte","representante"),sep = "(?<=\\S)\\s{10,}", extra = "merge") |>
        tibble::add_column(processo = processo, .before = 1) |>
        tibble::add_column(cd_processo = cd_processo, .after = 1)

    } else {

      x |>
        xml2::xml_find_first("//table[@id='tablePartesPrincipais']") |>
        rvest::html_table() |>
        setNames(c("tipo_parte","parte")) |>
        tidyr::separate(parte,c("parte","representante"),sep = "(?<=\\S)\\s{10,}", extra = "merge") |>
        tibble::add_column(processo = processo, .before = 1) |>
        tibble::add_column(cd_processo = cd_processo, .after = 1)

    }

  },NULL), .progress = TRUE)

}
