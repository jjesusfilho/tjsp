#' Lê conteúdo das decisões monocráticas
#'
#' @param diretorio Diretório onde se encontram os htmls.
#'
#' @return tibble com o número do processo, o texto da decisão e a data da
#'     decisão.
#'
#' @details Em alguns processos, as decisões monocráticas estão etiquetadas como
#'     despachos. Complemente a busca com a função ler_despachos.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' ler_decisoes_monocraticas()
#' }
#' 
ler_decisoes_monocraticas <- function(diretorio = ".") {
  a <- list.files(path = diretorio, pattern = ".html", full.names = T)

  processo <- stringr::str_extract(a, "\\d{20}")

  purrr::map2_dfr(a, processo, purrr::possibly(~ {
    decisao <- xml2::read_html(.x) %>%
      rvest::html_nodes(xpath = "//a[contains(text(),'Decisão Monocrática')]/following-sibling::span") %>%
      rvest::html_text()

    data_decisao <- xml2::read_html(.x) %>%
      rvest::html_nodes(xpath = "//tr[td[a[contains(text(),'Decisão Monocrática')]]]/td[1]") %>%
      rvest::html_text(trim = TRUE) %>%
      lubridate::dmy()

    tibble::tibble(processo = .y, decisao = decisao, data_decisao = data_decisao)
  }, otherwise = NULL))
}
