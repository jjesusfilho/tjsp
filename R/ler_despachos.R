#' Extrai despachos judiciais, que podem incluir decisões monocráticas, da movimentação.
#'
#' @param fonte objeto ou diretório onde se encontram os htmls.
#'
#' @return tibble com o número do processo, o despacho e a data do despacho.
#' @export
#'
#' @examples
#' \dontrun{
#' ler_despachos()
#' }
#'
ler_despachos <- function(fonte = ".") {

    arquivos <- list.files(
      path = fonte, pattern = ".html",
      full.names = TRUE
    )





  processo <- stringr::str_extract(arquivos, "\\d{20}")


  purrr::map2_dfr(arquivos, processo, purrr::possibly(~ {
    despacho <- xml2::read_html(.x) %>%
      rvest::html_nodes(xpath = "//td[@style='vertical-align: top; padding-bottom: 5px'][contains(text(),'Despacho')]//span") %>%
      rvest::html_text()

    data_despacho <- xml2::read_html(.x) %>%
      rvest::html_nodes(xpath = "//td[@style='vertical-align: top; padding-bottom: 5px'][contains(text(),'Despacho')]/parent::tr/td[1]") %>%
      rvest::html_text(trim = TRUE) %>%
      lubridate::dmy()

    tibble::tibble(processo = .y, despacho = despacho, data_despacho = data_despacho) %>%
      dplyr::filter(stringr::str_detect(despacho, "\\w\\X+"))
  }, otherwise = NULL))
}
