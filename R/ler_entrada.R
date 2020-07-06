#' Data da entrada do processo
#'
#' @param diretorio objeto ou diretório onde se encontram os htmls dos processos
#'
#' @return tibble com o número dos processos e respectivas decisões
#' @export
#'
#' @examples
#' \dontrun{
#' entrada <- ler_entrada_cposg()
#' entrada <- ler_entrada_cpopg()
#' }
#'
ler_entrada <- ler_entrada_cposg <- ler_entrada_cpopg <- function(diretorio = ".") {
  arquivos <- list.files(
    path = diretorio, pattern = ".html",
    full.names = TRUE
  )


  processo <- stringr::str_extract(arquivos, "\\d{20}") %>%
    abjutils::build_id()


  pb <- progress::progress_bar$new(total = length(arquivos))

  purrr::map2_dfr(arquivos, processo, purrr::possibly(~ {

    pb$tick()

    data <- xml2::read_html(.x) %>%
      rvest::html_nodes(xpath = "//td[@width='120']") %>%
      rvest::html_text() %>%
      dplyr::last() %>%
      stringr::str_extract("\\d{2}/\\d{2}/\\d{4}") %>%
      lubridate::dmy() %>%
      max()

    tibble::tibble(processo = .y, data = data)
  }, otherwise = NULL))
}
