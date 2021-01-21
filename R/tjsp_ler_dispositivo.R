#' Ler dispositivo do cposg
#'
#' @param arquivos Vetor de arquivos
#' @param diretorio Diretorio se não informar arquivos
#'
#' @return Tibble
#' @export
#'
tjsp_ler_dispositivo <- function(arquivos = NULL,
                                 diretorio = ".") {
  if (is.null(arquivos)) {
    arquivos <- list.files(diretorio, full.names = TRUE)
  }
  pb <- progress::progress_bar$new(total = length(arquivos))
  purrr::map_dfr(arquivos, purrr::possibly( ~ {
    pb$tick()
    processo <- stringr::str_extract(.x, "\\d{20}")
    .x %>%
      xml2::read_html() %>%
      xml2::xml_find_all(
        "//div//h2[@class='subtitle'][contains(.,'Julgamentos')]/following::table[position()=2]"
      ) %>%
      rvest::html_text() %>%
      stringr::str_trim() %>%
      tibble::tibble(
        data = stringr::str_extract(., "\\d.+"),
        dispositivo = stringr::str_extract(., "(?<=Julgado)\\X+")
      ) %>%
      dplyr::mutate(dispositivo = stringr::str_squish(dispositivo)) %>%
      dplyr::select(data, dispositivo) %>%
      dplyr::mutate(processo = !!processo, .before = 1) %>%
      dplyr::mutate(data = lubridate::dmy(data))
  }, NULL))
}
