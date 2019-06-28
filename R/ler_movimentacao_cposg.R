#' Extrai a movimentação processual de primeira e de segunda instância
#'
#' @param diretorio objeto ou diretorio  onde se encontram os htmls
#'
#' @return tibble com a movimentação processual.
#' @export
#' @examples
#' \dontrun{
#' andamento_cposg <- ler_movimentacao_cposg()
#' andamento_cpopg <- ler_movimentacao_cpopg()
#' }
#'
ler_movimentacao_cposg <- ler_movimentacao_cpopg <- function(diretorio = ".") {
  arquivos <- list.files(
    path = diretorio, pattern = ".html",
    full.names = TRUE
  )



  purrr::map2_dfr(arquivos, processo, purrr::possibly(~ {
    texto <- xml2::read_html(.x) %>%
      xml2::xml_find_first(xpath = "//table/tbody[@id='tabelaTodasMovimentacoes']")


    data <- xml2::xml_find_all(texto, ".//td[@width='120']") %>%
      xml2::xml_text(trim = TRUE)

    mov <- xml2::xml_find_all(texto, ".//td[@style='vertical-align: top; padding-bottom: 5px']") %>%
      xml2::xml_text(trim = TRUE)



    tibble::tibble(processo = .y, data = data, movimentacao = mov)
  }, otherwise = NULL))
}
