#' Extrai despachos judiciais, que podem incluir decisões monocráticas, da movimentação.
#'
#' @param diretorio Diretório onde se encontram os htmls.
#'
#' @return tibble com o número do processo, o despacho e a data do despacho.
#' @export
#'
#' @examples
#' \dontrun{
#' ler_despachos()
#' }
#'
ler_despachos <- function(diretorio = "."){
  a <- list.files(path = diretorio, pattern = ".html", full.names = T)
  processo <- stringr::str_extract(a, "\\d{20}")

  future::plan("multiprocess")

  furrr::future_map2_dfr(a, processo, purrr::possibly(~{
    despacho <- xml2::read_html(.x) %>%
      rvest::html_nodes(xpath = "//td[@style='vertical-align: top; padding-bottom: 5px'][contains(text(),'Despacho')]//span") %>%
      rvest::html_text()

    data_despacho <- xml2::read_html(.x) %>%
      rvest::html_nodes(xpath = "//td[@style='vertical-align: top; padding-bottom: 5px'][contains(text(),'Despacho')]/parent::tr/td[1]") %>%
      rvest::html_text(trim=TRUE) %>%
      lubridate::dmy()

    tibble::tibble(processo = .y, despacho = despacho,data_despacho=data_despacho) %>%
      dplyr::filter(stringr::str_detect(despacho,"\\w\\X+"))
  }, otherwise = NULL))
}
