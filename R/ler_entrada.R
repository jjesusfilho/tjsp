#' Data da entrada do processo
#'
#' @param diretorio Diretório onde se encontram os htmls dos processos
#'
#' @return tibble com o número dos processos e respectivas decisões
#' @export
#'
#' @examples
#' \dontrun{
#' entrada<-ler_entrada()
#' }

ler_entrada <- function(diretorio = ".") {

  a <- list.files(path = diretorio,
                  pattern = ".html",
                  full.names = T)

  processo <- stringr::str_extract(a, "\\d{20}")

  purrr::map2_dfr(a, processo, purrr::possibly( ~ {

    data <- xml2::read_html(.x) %>%

      rvest::html_nodes(xpath = "//td[@width='120']") %>%
      rvest::html_text() %>%
      last() %>%
      stringr::str_extract("\\d{2}/\\d{2}/\\d{4}") %>%
      lubridate::dmy() %>%
      max()

    tibble::tibble(processo = .y, data = data)

  }, otherwise = NULL))
}
